# Response-system integration boundary for the Supervisor dashboard.
#
# Future systems can pass the canonical timestamps listed in
# response_system_fields(). The existing hackathon fixtures do not contain
# those fields, so the local demo creates deterministic synthetic proxies. The
# adapter then maps either source to the unchanged QC input shape expected by
# qc.R; no control-chart formula lives here.

response_metric_catalog <- function() {
  data.frame(
    id = c('call_to_scene', 'transport_to_hospital'),
    label = c('Call received → scene arrival',
      'Transport departure → hospital arrival'),
    short_label = c('Call to scene', 'Transport to hospital'),
    start_field = c('call_received_time', 'transport_departure_time'),
    end_field = c('scene_arrival_time', 'hospital_arrival_time'),
    start_label = c('Call received', 'Transport departed'),
    end_label = c('Confirmed at scene', 'Confirmed at hospital'),
    definition = c(
      'Elapsed minutes from the response system receiving the call until the vehicle is confirmed at the incident scene.',
      'Elapsed minutes from the vehicle beginning transport until the response system confirms the vehicle has stopped near the hospital.'
    ),
    stringsAsFactors = FALSE
  )
}

response_system_fields <- function() {
  c('incident_id', 'zone', 'call_priority', 'weather_condition',
    'call_received_time', 'scene_arrival_time',
    'transport_departure_time', 'hospital_arrival_time')
}

# Empty canonical stream used by ordinary mode until Caller and Responder
# workflows emit their first shared response record.
response_metric_empty_records <- function() {
  fields <- c(response_system_fields(), 'responder_id', 'response_system_source')
  as.data.frame(setNames(replicate(length(fields), character(), simplify = FALSE), fields),
    stringsAsFactors = FALSE)
}

response_metric_info <- function(metric_id) {
  catalog <- response_metric_catalog()
  row <- catalog[catalog$id == metric_id, , drop = FALSE]
  if (nrow(row) != 1L) stop('unknown response metric')
  row
}

response_metric_code <- function(ids) {
  vapply(as.character(ids), function(id) {
    if (is.na(id) || !nzchar(id)) return(0L)
    sum(utf8ToInt(id))
  }, integer(1))
}

# Build explicit demo-only timestamps from the legacy assignment/scene fixture.
# These proxies are deterministic and are never described as operational data.
response_metric_mock_contract <- function(records) {
  d <- records
  n <- nrow(d)
  for (field in c('dispatch_time', 'on_scene_time', 'incident_id'))
    if (!field %in% names(d)) d[[field]] <- rep(NA_character_, n)
  assigned <- qc_parse_time(d$dispatch_time)
  scene <- qc_parse_time(d$on_scene_time)
  code <- response_metric_code(d$incident_id)
  legacy_minutes <- as.numeric(difftime(scene, assigned, units = 'mins'))

  call_offset_seconds <- 45 + (code %% 136L)
  scene_dwell_seconds <- 8 * 60 + (code %% (8 * 60L))
  hospital_travel_minutes <- pmax(4,
    legacy_minutes * 0.72 + ((code %% 9L) - 4L) * 0.25)

  d$call_received_time <- assigned - call_offset_seconds
  d$scene_arrival_time <- scene
  d$transport_departure_time <- scene + scene_dwell_seconds
  d$hospital_arrival_time <- d$transport_departure_time + hospital_travel_minutes * 60
  d$response_system_source <- rep('Synthetic adapter from legacy fixture', n)
  d
}

response_metric_has_contract <- function(records) {
  all(response_system_fields() %in% names(records))
}

# Convert one selected metric to the legacy QC column names. In production,
# call with canonical response-system fields and allow_mock = FALSE.
response_metric_to_qc <- function(records, metric_id, allow_mock = TRUE) {
  if (!is.data.frame(records)) stop('records must be a data frame')
  info <- response_metric_info(metric_id)
  source <- records
  if (!response_metric_has_contract(source)) {
    if (!isTRUE(allow_mock)) stop('canonical response-system timestamp fields are missing')
    source <- response_metric_mock_contract(source)
  } else if (!'response_system_source' %in% names(source)) {
    source$response_system_source <- rep('Canonical response-system contract', nrow(source))
  }

  common <- c('incident_id', 'zone', 'call_priority', 'weather_condition')
  for (field in common) if (!field %in% names(source)) source[[field]] <- NA_character_
  passthrough <- c(common, intersect('responder_id', names(source)))
  out <- source[, passthrough, drop = FALSE]
  out$dispatch_time <- source[[info$start_field]]
  out$on_scene_time <- source[[info$end_field]]
  out$metric_id <- rep(metric_id, nrow(out))
  out$metric_label <- rep(info$label, nrow(out))
  out$response_system_source <- source$response_system_source
  out
}

# Expand canonical incident rows into append-style completion events for the
# Supervisor activity feed. One incident can contribute a scene-arrival event
# and, later, a hospital-arrival event without duplicating the incident in the
# canonical integration contract.
response_metric_completion_events <- function(records, allow_mock = FALSE) {
  if (!is.data.frame(records) || !nrow(records)) {
    return(data.frame(
      completion_id = character(), incident_id = character(), metric_id = character(),
      metric_label = character(), zone = character(), call_priority = character(),
      responder_id = character(), completed_at = as.POSIXct(character()),
      duration_minutes = numeric(), stringsAsFactors = FALSE))
  }

  events <- lapply(response_metric_catalog()$id, function(metric_id) {
    d <- response_metric_to_qc(records, metric_id, allow_mock = allow_mock)
    started <- qc_parse_time(d$dispatch_time)
    completed <- qc_parse_time(d$on_scene_time)
    duration <- as.numeric(difftime(completed, started, units = 'mins'))
    keep <- !is.na(started) & !is.na(completed) & is.finite(duration) & duration >= 0
    if (!any(keep)) return(NULL)
    responder <- if ('responder_id' %in% names(d)) as.character(d$responder_id)
      else rep(NA_character_, nrow(d))
    data.frame(
      completion_id = paste(d$incident_id[keep], metric_id, sep = '::'),
      incident_id = as.character(d$incident_id[keep]),
      metric_id = metric_id,
      metric_label = as.character(d$metric_label[keep]),
      zone = as.character(d$zone[keep]),
      call_priority = as.character(d$call_priority[keep]),
      responder_id = responder[keep],
      completed_at = completed[keep],
      duration_minutes = duration[keep],
      stringsAsFactors = FALSE)
  })
  events <- Filter(Negate(is.null), events)
  if (!length(events)) return(response_metric_completion_events(records[FALSE, , drop = FALSE]))
  result <- do.call(rbind, events)
  result[order(result$completed_at, result$completion_id), , drop = FALSE]
}

response_metric_connection_status <- function(records) {
  if (response_metric_has_contract(records))
    'Canonical response-system fields detected'
  else
    'Demo adapter active · ready for canonical response-system timestamps'
}
