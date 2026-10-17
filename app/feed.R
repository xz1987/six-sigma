# Deterministic synthetic event replay for the Shiny application. This module
# owns simulation state only. Completed records can be handed to qc_analyze();
# it never receives or changes baseline observations, reviews, or limits.

feed_label <- 'Synthetic replay data'
feed_event_types <- c('dispatched', 'approaching_scene', 'on_scene')

feed_with_seed <- function(seed, code) {
  had_seed <- exists('.Random.seed', envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get('.Random.seed', envir = .GlobalEnv)
  on.exit({
    if (had_seed) assign('.Random.seed', old_seed, envir = .GlobalEnv)
    else if (exists('.Random.seed', envir = .GlobalEnv, inherits = FALSE))
      rm('.Random.seed', envir = .GlobalEnv)
  })
  set.seed(seed)
  force(code)
}

feed_build_events <- function(records, seed = 20261016, count = 20,
                              start_time = '2026-02-01T12:00:00Z',
                              dispatch_gap_seconds = 30,
                              include_approaching = TRUE,
                              source_name = 'existing synthetic fixture') {
  if (!exists('qc_validate', mode = 'function') ||
      !exists('qc_parse_time', mode = 'function'))
    stop('source app/qc.R before building feed events')
  if (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed))
    stop('seed must be one finite number')
  if (!is.numeric(count) || length(count) != 1 || is.na(count) || count < 1 || count %% 1 != 0)
    stop('count must be a positive whole number')
  if (!is.numeric(dispatch_gap_seconds) || length(dispatch_gap_seconds) != 1 ||
      is.na(dispatch_gap_seconds) || dispatch_gap_seconds < 0)
    stop('dispatch_gap_seconds must be nonnegative')
  origin <- qc_parse_time(start_time)
  if (length(origin) != 1 || is.na(origin)) stop('start_time must be ISO 8601 with an offset')

  validated <- qc_validate(records, mode = 'completed')
  available <- validated$completed
  if (nrow(available) < count)
    stop('not enough valid completed synthetic records for requested count')
  picked <- feed_with_seed(seed, sample(seq_len(nrow(available)), count, replace = FALSE))
  selected <- available[picked, , drop = FALSE]

  rows <- vector('list', count * if (isTRUE(include_approaching)) 3 else 2)
  at <- 0L
  for (i in seq_len(count)) {
    duration <- as.numeric(difftime(selected$on_scene_time[i],
      selected$dispatch_time[i], units = 'secs'))
    dispatch <- origin + (i - 1) * dispatch_gap_seconds
    common <- list(
      incident_id = as.character(selected$incident_id[i]),
      zone = as.character(selected$zone[i]),
      call_priority = as.character(selected$call_priority[i]),
      weather_condition = as.character(selected$weather_condition[i]),
      data_classification = feed_label
    )
    make_row <- function(kind, when) data.frame(
      event_id = paste0('replay-', sprintf('%04d', i), '-', kind),
      incident_id = common$incident_id,
      event_type = kind,
      event_time = as.POSIXct(when, origin = '1970-01-01', tz = 'UTC'),
      zone = common$zone,
      call_priority = common$call_priority,
      weather_condition = common$weather_condition,
      data_classification = common$data_classification,
      stringsAsFactors = FALSE
    )
    at <- at + 1L
    rows[[at]] <- make_row('dispatched', dispatch)
    if (isTRUE(include_approaching) && duration > 30) {
      at <- at + 1L
      rows[[at]] <- make_row('approaching_scene', dispatch + duration - 30)
    }
    at <- at + 1L
    rows[[at]] <- make_row('on_scene', dispatch + duration)
  }
  events <- do.call(rbind, rows[seq_len(at)])
  type_order <- match(events$event_type, feed_event_types)
  events <- events[order(events$event_time, type_order, events$event_id), , drop = FALSE]
  rownames(events) <- NULL
  attr(events, 'feed_metadata') <- list(
    data_classification = feed_label,
    source = source_name,
    seed = seed,
    record_count = count,
    simulated_time_zone = 'UTC'
  )
  events
}

feed_empty_active <- function() data.frame(
  incident_id = character(), zone = character(), call_priority = character(),
  weather_condition = character(), dispatch_time = as.POSIXct(character(), tz = 'UTC'),
  status = character(), data_classification = character(),
  stringsAsFactors = FALSE
)

feed_empty_completed <- function() data.frame(
  incident_id = character(), zone = character(), call_priority = character(),
  weather_condition = character(), dispatch_time = as.POSIXct(character(), tz = 'UTC'),
  on_scene_time = as.POSIXct(character(), tz = 'UTC'), response_minutes = numeric(),
  data_classification = character(), stringsAsFactors = FALSE
)

feed_empty_issues <- function() data.frame(
  event_id = character(), incident_id = character(), reason = character(),
  stringsAsFactors = FALSE
)

feed_new <- function(events) {
  if (!is.data.frame(events)) stop('events must be a data frame')
  required <- c('event_id', 'incident_id', 'event_type', 'event_time', 'zone',
    'call_priority', 'weather_condition', 'data_classification')
  if (length(setdiff(required, names(events)))) stop('events are missing required columns')
  if (!inherits(events$event_time, 'POSIXt')) {
    if (!exists('qc_parse_time', mode = 'function')) stop('source app/qc.R before parsing event times')
    events$event_time <- qc_parse_time(events$event_time)
  }
  if (!nrow(events) || any(is.na(events$event_time))) stop('events need valid event times')
  if (any(!events$event_type %in% feed_event_types)) stop('unknown event_type')

  state <- new.env(parent = emptyenv())
  state$events <- events
  state$metadata <- attr(events, 'feed_metadata')
  if (is.null(state$metadata)) state$metadata <- list(data_classification = feed_label)
  state$initial_time <- min(events$event_time) - 1
  feed_reset(state)
  state
}

feed_reset <- function(state) {
  state$running <- FALSE
  state$simulated_time <- state$initial_time
  state$processed <- rep(FALSE, nrow(state$events))
  state$seen_event_ids <- character()
  state$active <- feed_empty_active()
  state$completed <- feed_empty_completed()
  state$issues <- feed_empty_issues()
  invisible(state)
}

feed_start <- function(state) {
  state$running <- TRUE
  invisible(state)
}

feed_pause <- function(state) {
  state$running <- FALSE
  invisible(state)
}

feed_issue <- function(state, event, reason) {
  state$issues <- rbind(state$issues, data.frame(
    event_id = as.character(event$event_id),
    incident_id = as.character(event$incident_id),
    reason = reason, stringsAsFactors = FALSE
  ))
}

feed_apply_event <- function(state, event) {
  event_id <- as.character(event$event_id)
  incident_id <- as.character(event$incident_id)
  if (event_id %in% state$seen_event_ids) {
    feed_issue(state, event, 'duplicate event_id')
    return(invisible(state))
  }
  state$seen_event_ids <- c(state$seen_event_ids, event_id)
  active_index <- match(incident_id, state$active$incident_id)
  completed_already <- incident_id %in% state$completed$incident_id

  if (event$event_type == 'dispatched') {
    if (!is.na(active_index) || completed_already) {
      feed_issue(state, event, 'duplicate incident dispatch')
      return(invisible(state))
    }
    state$active <- rbind(state$active, data.frame(
      incident_id = incident_id,
      zone = as.character(event$zone),
      call_priority = as.character(event$call_priority),
      weather_condition = as.character(event$weather_condition),
      dispatch_time = as.POSIXct(event$event_time, origin = '1970-01-01', tz = 'UTC'),
      status = 'dispatched',
      data_classification = feed_label,
      stringsAsFactors = FALSE
    ))
    return(invisible(state))
  }

  if (is.na(active_index)) {
    feed_issue(state, event, if (completed_already) 'event after completion' else 'event before dispatch')
    return(invisible(state))
  }
  if (event$event_time < state$active$dispatch_time[active_index]) {
    feed_issue(state, event, 'event time before dispatch time')
    return(invisible(state))
  }
  if (event$event_type == 'approaching_scene') {
    state$active$status[active_index] <- 'approaching scene (simulated)'
    return(invisible(state))
  }

  dispatch <- state$active$dispatch_time[active_index]
  arrival <- as.POSIXct(event$event_time, origin = '1970-01-01', tz = 'UTC')
  state$completed <- rbind(state$completed, data.frame(
    incident_id = incident_id,
    zone = state$active$zone[active_index],
    call_priority = state$active$call_priority[active_index],
    weather_condition = state$active$weather_condition[active_index],
    dispatch_time = dispatch,
    on_scene_time = arrival,
    response_minutes = as.numeric(difftime(arrival, dispatch, units = 'mins')),
    data_classification = feed_label,
    stringsAsFactors = FALSE
  ))
  state$active <- state$active[-active_index, , drop = FALSE]
  invisible(state)
}

feed_advance <- function(state, seconds = 1) {
  if (!is.numeric(seconds) || length(seconds) != 1 || is.na(seconds) || seconds < 0)
    stop('seconds must be one nonnegative number')
  if (!isTRUE(state$running)) return(invisible(state))
  state$simulated_time <- state$simulated_time + seconds
  due <- which(!state$processed & state$events$event_time <= state$simulated_time)
  for (i in due) {
    feed_apply_event(state, state$events[i, , drop = FALSE])
    state$processed[i] <- TRUE
  }
  invisible(state)
}

feed_pending <- function(state) state$active
feed_completed <- function(state) state$completed
feed_qc_records <- function(state) state$completed

feed_snapshot <- function(state) list(
  running = state$running,
  simulated_time = state$simulated_time,
  processed_events = sum(state$processed),
  total_events = nrow(state$events),
  pending = state$active,
  completed = state$completed,
  issues = state$issues,
  metadata = state$metadata
)

feed_replay_to_end <- function(events) {
  state <- feed_new(events)
  feed_start(state)
  seconds <- as.numeric(difftime(max(events$event_time), state$simulated_time,
    units = 'secs'))
  feed_advance(state, seconds)
  state
}
