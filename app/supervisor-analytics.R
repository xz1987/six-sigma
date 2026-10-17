# Supervisor-only analytics helpers. These functions decorate the validated QC
# stream; they do not refit or modify the fixed I-MR baseline limits in qc.R.

supervisor_rule_labels <- c(
  beyond_3sigma = '1 point beyond 3σ',
  two_of_three_2sigma = '2 of 3 beyond 2σ',
  four_of_five_1sigma = '4 of 5 beyond 1σ',
  eight_same_side = '8 on one side',
  six_point_trend = '6-point trend',
  fifteen_centered = '15 within 1σ',
  moving_range = 'Large moving range',
  target_miss = 'Contextual target miss'
)

supervisor_add_rule <- function(labels, rows, label) {
  rows <- unique(rows[rows >= 1L & rows <= length(labels)])
  for (i in rows) labels[i] <- paste(c(labels[i], label)[nzchar(c(labels[i], label))],
    collapse = '; ')
  labels
}

# Six commonly used supplementary SPC rules plus the existing MR signal and
# the separately calculated contextual target miss.
supervisor_signal_rules <- function(m, center, sigma, targets = NULL) {
  n <- nrow(m)
  if (!n) return(data.frame(incident_id = character(), rule_labels = character(),
    primary_rule = character(), performance_signal = logical(),
    target_miss = logical(), stringsAsFactors = FALSE))
  x <- as.numeric(m$response_minutes)
  z <- if (is.finite(sigma) && sigma > 0) (x - center) / sigma else rep(NA_real_, n)
  labels <- rep('', n)
  labels <- supervisor_add_rule(labels, which(abs(z) > 3), supervisor_rule_labels[['beyond_3sigma']])
  if (n >= 3L) for (i in 3:n) {
    w <- z[(i - 2L):i]
    if (sum(w > 2, na.rm = TRUE) >= 2L || sum(w < -2, na.rm = TRUE) >= 2L)
      labels <- supervisor_add_rule(labels, i, supervisor_rule_labels[['two_of_three_2sigma']])
  }
  if (n >= 5L) for (i in 5:n) {
    w <- z[(i - 4L):i]
    if (sum(w > 1, na.rm = TRUE) >= 4L || sum(w < -1, na.rm = TRUE) >= 4L)
      labels <- supervisor_add_rule(labels, i, supervisor_rule_labels[['four_of_five_1sigma']])
  }
  if (n >= 8L) for (i in 8:n) {
    w <- z[(i - 7L):i]
    if (all(w > 0, na.rm = FALSE) || all(w < 0, na.rm = FALSE))
      labels <- supervisor_add_rule(labels, i, supervisor_rule_labels[['eight_same_side']])
  }
  if (n >= 6L) for (i in 6:n) {
    dx <- diff(x[(i - 5L):i])
    if (all(dx > 0, na.rm = FALSE) || all(dx < 0, na.rm = FALSE))
      labels <- supervisor_add_rule(labels, i, supervisor_rule_labels[['six_point_trend']])
  }
  if (n >= 15L) for (i in 15:n) {
    if (all(abs(z[(i - 14L):i]) < 1, na.rm = FALSE))
      labels <- supervisor_add_rule(labels, i, supervisor_rule_labels[['fifteen_centered']])
  }
  if ('variation_signal' %in% names(m))
    labels <- supervisor_add_rule(labels, which(!is.na(m$variation_signal) & m$variation_signal),
      supervisor_rule_labels[['moving_range']])
  if (is.null(targets)) targets <- rep(NA_real_, n)
  miss <- is.finite(targets) & is.finite(x) & x > targets
  labels <- supervisor_add_rule(labels, which(miss), supervisor_rule_labels[['target_miss']])
  primary <- sub(';.*$', '', labels)
  data.frame(incident_id = as.character(m$incident_id), rule_labels = labels,
    primary_rule = primary, performance_signal = nzchar(labels), target_miss = miss,
    stringsAsFactors = FALSE)
}

supervisor_group_signals <- function(m, rules, metric_id, zone, priority) {
  keep <- rules$performance_signal
  if (!any(keep)) return(data.frame(group_id = character(), primary_rule = character(),
    count = integer(), latest_at = as.POSIXct(character()), max_minutes = numeric(),
    incident_ids = character(), stringsAsFactors = FALSE))
  d <- cbind(m[keep, , drop = FALSE], rules[keep, , drop = FALSE])
  day <- format(d$dispatch_time, '%Y-%m-%d', tz = 'America/New_York')
  d$group_id <- paste(metric_id, zone, priority, day, d$primary_rule, sep = '::')
  groups <- split(d, d$group_id)
  out <- lapply(groups, function(g) data.frame(group_id = g$group_id[[1L]],
    primary_rule = g$primary_rule[[1L]], count = nrow(g),
    latest_at = max(g$on_scene_time), max_minutes = max(g$response_minutes),
    incident_ids = paste(g$incident_id, collapse = ', '), stringsAsFactors = FALSE))
  result <- do.call(rbind, out)
  result[order(result$latest_at, decreasing = TRUE), , drop = FALSE]
}

supervisor_target_model <- function(records, metric_id) {
  d <- response_metric_to_qc(records, metric_id, allow_mock = TRUE)
  start <- qc_parse_time(d$dispatch_time)
  end <- qc_parse_time(d$on_scene_time)
  d$response_minutes <- as.numeric(difftime(end, start, units = 'mins'))
  d$hour_band <- factor(cut(as.integer(format(start, '%H', tz = 'America/New_York')),
    breaks = c(-1, 5, 11, 17, 23), labels = c('Overnight', 'Morning', 'Afternoon', 'Evening')))
  d$weekday <- factor(format(start, '%u', tz = 'America/New_York'))
  d$zone <- factor(d$zone, levels = qc_zones)
  d$call_priority <- factor(d$call_priority, levels = qc_priorities)
  d$weather_condition <- factor(d$weather_condition, levels = qc_weather)
  period <- dashboard_period()
  keep <- start >= period$baseline_start & start < period$baseline_end &
    is.finite(d$response_minutes) & d$response_minutes > 0 & !is.na(d$hour_band) &
    !is.na(d$weekday) & !is.na(d$zone) & !is.na(d$call_priority) & !is.na(d$weather_condition)
  d <- d[keep, , drop = FALSE]
  if (nrow(d) < 40L) return(list(status = 'unavailable', reason = 'insufficient reference data'))
  fit <- try(stats::lm(log(response_minutes) ~ zone + call_priority + weather_condition +
    hour_band + weekday, data = d), silent = TRUE)
  if (inherits(fit, 'try-error') || fit$df.residual < 1L)
    return(list(status = 'unavailable', reason = 'context model could not be estimated'))
  list(status = 'synthetic_context_model', fit = fit, reference = d,
    factors = c('zone', 'priority', 'weather', 'hour', 'weekday'),
    traffic = 'unavailable in canonical stream')
}

supervisor_target_predict <- function(model, m) {
  n <- nrow(m)
  if (!n) return(numeric())
  if (!identical(model$status, 'synthetic_context_model')) return(rep(NA_real_, n))
  start <- qc_parse_time(m$dispatch_time)
  newdata <- data.frame(
    zone = factor(m$zone, levels = qc_zones),
    call_priority = factor(m$call_priority, levels = qc_priorities),
    weather_condition = factor(m$weather_condition, levels = qc_weather),
    hour_band = factor(cut(as.integer(format(start, '%H', tz = 'America/New_York')),
      breaks = c(-1, 5, 11, 17, 23), labels = c('Overnight', 'Morning', 'Afternoon', 'Evening')),
      levels = levels(model$reference$hour_band)),
    weekday = factor(format(start, '%u', tz = 'America/New_York'),
      levels = levels(model$reference$weekday)))
  out <- suppressWarnings(try(exp(stats::predict(model$fit, newdata = newdata) +
    stats::qnorm(.90) * stats::sigma(model$fit)), silent = TRUE))
  if (inherits(out, 'try-error')) rep(NA_real_, n) else as.numeric(out)
}

# A displayed goal for an empty live view: standardize the selected zone and
# priority over the frozen reference mix of weather, hour and weekday.
supervisor_target_for_scope <- function(model, zone, priority) {
  if (!identical(model$status, 'synthetic_context_model')) return(NA_real_)
  d <- model$reference
  d$zone <- factor(zone, levels = levels(d$zone))
  d$call_priority <- factor(priority, levels = levels(d$call_priority))
  prediction <- suppressWarnings(try(exp(stats::predict(model$fit, newdata = d) +
    stats::qnorm(.90) * stats::sigma(model$fit)), silent = TRUE))
  if (inherits(prediction, 'try-error') || !any(is.finite(prediction))) NA_real_
  else stats::median(prediction[is.finite(prediction)])
}

# Marginal standardization: set the comparison factor to each level while
# retaining the reference distribution of weather, time and the other factor.
supervisor_adjusted_comparison <- function(model, field = c('zone', 'call_priority')) {
  field <- match.arg(field)
  if (!identical(model$status, 'synthetic_context_model')) return(data.frame())
  ref <- model$reference
  levels_to_show <- levels(ref[[field]])
  beta <- stats::coef(model$fit); vc <- stats::vcov(model$fit)
  terms_no_response <- delete.response(stats::terms(model$fit))
  rows <- lapply(levels_to_show, function(level) {
    d <- ref; d[[field]] <- factor(level, levels = levels(ref[[field]]))
    mm <- stats::model.matrix(terms_no_response, d, contrasts.arg = model$fit$contrasts)
    xbar <- colMeans(mm)[names(beta)]
    estimate <- sum(xbar * beta)
    se <- sqrt(as.numeric(t(xbar) %*% vc %*% xbar))
    data.frame(group = level, estimate = exp(estimate), lower = exp(estimate - 1.96 * se),
      upper = exp(estimate + 1.96 * se), n = sum(ref[[field]] == level), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

supervisor_period_subgroups <- function(m, center, sigma,
                                        aggregation = c('day', 'week')) {
  aggregation <- match.arg(aggregation)
  if (!nrow(m)) return(data.frame())
  day <- as.Date(m$dispatch_time, tz = 'America/New_York')
  period <- if (identical(aggregation, 'week'))
    day - (as.integer(format(day, '%u')) - 1L) else day
  groups <- split(m$response_minutes, period)
  out <- lapply(names(groups), function(name) {
    x <- groups[[name]]; n <- sum(is.finite(x)); x <- x[is.finite(x)]
    data.frame(period = as.Date(name), n = n, mean = if (n) mean(x) else NA_real_,
      range = if (n >= 2L) diff(range(x)) else NA_real_,
      sd = if (n >= 2L) stats::sd(x) else NA_real_, stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, out)
  d$lcl <- pmax(0, center - 3 * sigma / sqrt(pmax(d$n, 1)))
  d$ucl <- center + 3 * sigma / sqrt(pmax(d$n, 1))
  d$signal <- d$n > 0 & (d$mean < d$lcl | d$mean > d$ucl)
  d
}

supervisor_daily_subgroups <- function(m, center, sigma) {
  d <- supervisor_period_subgroups(m, center, sigma, 'day')
  if (nrow(d)) names(d)[names(d) == 'period'] <- 'day'
  d
}

supervisor_history_period <- function(m, period = c('30', '90', 'full')) {
  period <- match.arg(period)
  if (!nrow(m) || identical(period, 'full')) return(m)
  day <- as.Date(m$dispatch_time, tz = 'America/New_York')
  available_through <- max(day, na.rm = TRUE)
  m[!is.na(day) & day >= available_through - (as.integer(period) - 1L), , drop = FALSE]
}

supervisor_empty_service_goals <- function() data.frame(
  metric_id = character(), priority = character(), zone = character(),
  minutes = numeric(), updated_at = as.POSIXct(character()), stringsAsFactors = FALSE)

supervisor_set_service_goal <- function(goals, metric_id, priority, zone, minutes,
                                        updated_at = Sys.time()) {
  stopifnot(is.data.frame(goals), length(metric_id) == 1L, length(priority) == 1L,
    length(zone) == 1L, length(minutes) == 1L)
  if (!is.finite(minutes) || minutes <= 0) stop('service goal must be a positive number')
  keep <- !(goals$metric_id == metric_id & goals$priority == priority & goals$zone == zone)
  rbind(goals[keep, , drop = FALSE], data.frame(metric_id = metric_id,
    priority = priority, zone = zone, minutes = as.numeric(minutes),
    updated_at = as.POSIXct(updated_at), stringsAsFactors = FALSE))
}

supervisor_clear_service_goal <- function(goals, metric_id, priority, zone) {
  keep <- !(goals$metric_id == metric_id & goals$priority == priority & goals$zone == zone)
  goals[keep, , drop = FALSE]
}

supervisor_service_goal_value <- function(goals, metric_id, zone, priority) {
  exact <- goals$metric_id == metric_id & goals$priority == priority & goals$zone == zone
  fallback <- goals$metric_id == metric_id & goals$priority == priority & goals$zone == 'All zones'
  row <- if (any(exact)) tail(which(exact), 1L) else if (any(fallback)) tail(which(fallback), 1L) else integer()
  if (!length(row)) NA_real_ else goals$minutes[row]
}

supervisor_service_performance <- function(m, goals, metric_id) {
  grid <- expand.grid(zone = qc_zones, priority = qc_priorities,
    stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    x <- m$response_minutes[m$zone == grid$zone[i] & m$call_priority == grid$priority[i]]
    x <- x[is.finite(x)]
    goal <- supervisor_service_goal_value(goals, metric_id, grid$zone[i], grid$priority[i])
    data.frame(zone = grid$zone[i], priority = grid$priority[i], n = length(x),
      p90 = if (length(x)) as.numeric(stats::quantile(x, .90, names = FALSE, type = 7)) else NA_real_,
      goal = goal, within_goal = if (length(x) && is.finite(goal)) mean(x <= goal) else NA_real_,
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

supervisor_data_quality <- function(records, metric_id) {
  empty <- data.frame(issue_id = character(), incident_id = character(), severity = character(),
    issue = character(), stringsAsFactors = FALSE)
  if (!is.data.frame(records) || !nrow(records)) return(empty)
  info <- response_metric_info(metric_id)
  value <- function(field) if (field %in% names(records)) as.character(records[[field]])
    else rep(NA_character_, nrow(records))
  id <- value('incident_id'); start_raw <- value(info$start_field); end_raw <- value(info$end_field)
  start <- qc_parse_time(start_raw); end <- qc_parse_time(end_raw)
  issues <- list(); add <- function(rows, label, severity = 'Warning') {
    for (i in rows) issues[[length(issues) + 1L]] <<- data.frame(
      issue_id = sprintf('DQ-%03d', length(issues) + 1L),
      incident_id = if (is.na(id[i]) || !nzchar(id[i])) 'Unknown incident' else id[i],
      severity = severity, issue = label, stringsAsFactors = FALSE)
  }
  add(which(is.na(id) | !nzchar(id)), 'Missing incident identifier', 'Error')
  add(which(duplicated(id) & !is.na(id) & nzchar(id)), 'Duplicate incident identifier', 'Error')
  add(which(is.na(start_raw) | !nzchar(start_raw)), paste('Missing', info$start_label), 'Error')
  add(which(!is.na(start_raw) & nzchar(start_raw) & is.na(start)), 'Invalid start timestamp', 'Error')
  add(which(!is.na(end_raw) & nzchar(end_raw) & is.na(end)), 'Invalid completion timestamp', 'Error')
  add(which(!is.na(start) & !is.na(end) & end < start), 'Completion precedes start', 'Error')
  weather <- value('weather_condition')
  add(which(is.na(weather) | !nzchar(weather) | weather == 'Unknown'), 'Weather context unavailable')
  if (!length(issues)) empty else do.call(rbind, issues)
}

supervisor_apply_demo_event <- function(call, event_type, metric_id, target_minutes, index) {
  if (event_type %in% c('stable', 'data_quality')) return(call)
  info <- response_metric_info(metric_id)
  start <- qc_parse_time(call[[info$start_field]])
  end <- qc_parse_time(call[[info$end_field]])
  minutes <- as.numeric(difftime(end, start, units = 'mins'))
  adjusted <- switch(event_type,
    extreme = max(minutes * 2.2, target_minutes * 1.5),
    shift = max(minutes * 1.45, target_minutes * 1.2),
    trend = max(minutes, target_minutes * (.82 + .08 * index)),
    target_miss = max(minutes, target_minutes * 1.25),
    improvement = max(1, min(minutes * .75, target_minutes * .85)), minutes)
  stamp <- function(t) format(t, '%Y-%m-%dT%H:%M:%S%z', tz = 'America/New_York')
  old_transport <- as.numeric(difftime(qc_parse_time(call$hospital_arrival_time),
    qc_parse_time(call$transport_departure_time), units = 'mins'))
  call[[info$end_field]] <- stamp(start + adjusted * 60)
  if (identical(metric_id, 'call_to_scene')) {
    dwell <- 10 * 60
    call$transport_departure_time <- stamp(start + adjusted * 60 + dwell)
    if (!is.finite(old_transport) || old_transport <= 0) old_transport <- 8
    call$hospital_arrival_time <- stamp(qc_parse_time(call$transport_departure_time) + old_transport * 60)
  }
  call
}
