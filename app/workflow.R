# Application orchestration. Statistical calculations remain in qc.R.

dashboard_period <- function() {
  qc_period(
    '2025-01-01T00:00:00-0500',
    '2026-01-01T00:00:00-0500',
    '2026-02-01T00:00:00-0500',
    '2026-03-01T00:00:00-0500'
  )
}
dashboard_load_fixture <- function(repo_root, scenario) {
  allowed <- c(stable = 'stable.csv', planted = 'planted_delay.csv')
  if (!scenario %in% names(allowed)) stop('unknown bundled scenario')
  path <- file.path(repo_root, 'data', unname(allowed[[scenario]]))
  if (!file.exists(path)) stop('bundled scenario not found: ', path)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

dashboard_source_monitoring <- function(records) {
  validated <- qc_validate(records, mode = 'completed')
  source_period <- qc_period(
    '2025-01-01T00:00:00-0500',
    '2026-01-01T00:00:00-0500',
    '2026-01-01T00:00:00-0500',
    '2026-02-01T00:00:00-0500'
  )
  d <- validated$completed
  d[d$dispatch_time >= source_period$monitoring_start &
      d$dispatch_time < source_period$monitoring_end, , drop = FALSE]
}

dashboard_baseline <- function(records) {
  validated <- qc_validate(records, mode = 'completed')
  period <- dashboard_period()
  d <- validated$completed
  d[d$dispatch_time >= period$baseline_start &
      d$dispatch_time < period$baseline_end, , drop = FALSE]
}

dashboard_build_feed <- function(records, seed, start_time = '2026-02-01T12:00:00-0500') {
  monitoring <- dashboard_source_monitoring(records)
  feed_build_events(
    monitoring,
    seed = as.integer(seed),
    count = nrow(monitoring),
    start_time = start_time,
    dispatch_gap_seconds = 10,
    include_approaching = TRUE,
    source_name = 'bundled synthetic monitoring fixture'
  )
}

dashboard_analysis_records <- function(records, completed) {
  baseline <- dashboard_baseline(records)
  fields <- c('incident_id', 'zone', 'call_priority', 'weather_condition',
    'dispatch_time', 'on_scene_time')
  rbind(baseline[, fields, drop = FALSE], completed[, fields, drop = FALSE])
}

dashboard_demo_reviews <- function(analysis) {
  lapply(analysis$groups, function(group) {
    qc_review(
      group,
      reviewer = 'Synthetic demo review fixture',
      rationale = paste(
        'Non-operational demonstration only. Eligibility and arithmetic were',
        'checked; this does not approve a real EMS baseline.'
      ),
      decision = 'approved',
      reviewed_at = '2026-02-01T00:00:00-0500'
    )
  })
}

dashboard_analyze <- function(records, completed, demo_review = FALSE) {
  period <- dashboard_period()
  combined <- dashboard_analysis_records(records, completed)
  preliminary <- qc_analyze(combined, period)
  if (!isTRUE(demo_review)) return(preliminary)
  qc_analyze(combined, period, reviews = dashboard_demo_reviews(preliminary))
}

dashboard_descriptive_records <- function(analysis, start_date, end_date) {
  d <- analysis$validation$completed
  start <- as.POSIXct(as.Date(start_date), tz = 'America/New_York')
  end <- as.POSIXct(as.Date(end_date) + 1, tz = 'America/New_York')
  d[d$dispatch_time >= start & d$dispatch_time < end, , drop = FALSE]
}
