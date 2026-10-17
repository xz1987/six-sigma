source(file.path('..', 'app', 'supervisor.R'), local = TRUE)

test_that('display windows use calendar boundaries and reject invalid dates', {
  expect_equal(as.character(supervisor_date_window('day', as.Date('2026-02-02'))),
    c('2026-02-02', '2026-02-02'))
  expect_equal(as.character(supervisor_date_window('week', as.Date('2026-02-01'))),
    c('2026-01-26', '2026-02-01'))
  expect_equal(as.character(supervisor_date_window('month', as.Date('2026-02-01'))),
    c('2026-02-01', '2026-02-28'))
  expect_equal(as.character(supervisor_date_window('custom', as.Date('2026-02-01'),
    as.Date(c('2026-02-03', '2026-02-07')))), c('2026-02-03', '2026-02-07'))
  expect_error(supervisor_date_window('custom', as.Date('2026-02-01'),
    as.Date(c('2026-02-07', '2026-02-03'))), 'invalid display window')
})

test_that('display filters retain fixed limits, original sequence and signals', {
  monitoring <- data.frame(
    incident_id = c('A', 'B', 'C'),
    dispatch_time = as.POSIXct(c('2026-02-01 12:00:00', '2026-02-02 12:00:00',
      '2026-02-02 13:00:00'), tz = 'America/New_York'),
    responder_id = c('Crew 1', 'Crew 2', 'Crew 1'),
    response_minutes = c(5, 8, 12),
    moving_range = c(1, 3, 4),
    individual_signal = c(FALSE, FALSE, TRUE),
    variation_signal = c(FALSE, FALSE, TRUE)
  )
  group <- list(baseline = data.frame(response_minutes = c(4, 5)),
    monitoring = monitoring, limits = list(i_ucl = 10, mr_ucl = 3))
  original <- group
  view <- supervisor_visible_monitoring(group, as.Date(c('2026-02-02', '2026-02-02')),
    'Crew 1')
  expect_equal(view$incident_id, 'C')
  expect_equal(view$plot_sequence, 5L)
  expect_true(view$individual_signal)
  expect_true(view$variation_signal)
  expect_identical(group, original)
  expect_equal(nrow(supervisor_visible_monitoring(group,
    as.Date(c('2026-02-03', '2026-02-03')))), 0L)
  group$monitoring$responder_id <- NULL
  expect_equal(nrow(supervisor_visible_monitoring(group,
    as.Date(c('2026-02-01', '2026-02-02')), 'Crew 1')), 0L)
})

test_that('rolling chart window and line segments preserve chronological gaps', {
  m <- data.frame(incident_id = paste0('I', 1:30),
    monitoring_sequence = 1:30, response_minutes = as.numeric(1:30),
    on_scene_time = as.POSIXct('2026-02-01 12:00:00', tz = 'UTC') + 1:30)
  expect_equal(supervisor_chart_window(m)$incident_id, paste0('I', 7:30))
  expect_equal(supervisor_chart_window(m, 'I8')$incident_id, paste0('I', 1:8))
  expect_equal(nrow(supervisor_chart_window(m[FALSE, ])), 0L)
  filtered <- m[c(1, 2, 5), ]
  segments <- supervisor_consecutive_segments(filtered, 'response_minutes')
  expect_equal(nrow(segments), 1L)
  expect_equal(c(segments$x, segments$xend, segments$y, segments$yend), c(1, 2, 1, 2))
})

test_that('chart window ends at the newest sequence row, not the latest completion', {
  m <- data.frame(incident_id = c('A', 'B'), monitoring_sequence = 1:2,
    response_minutes = c(14, 12),
    on_scene_time = as.POSIXct('2026-02-01 12:00:00', tz = 'UTC') + c(900, 600))
  expect_equal(supervisor_chart_window(m)$incident_id, c('A', 'B'))
})

test_that('simulated developer calls are deterministic, synthetic and leave the RNG alone', {
  records <- dashboard_load_fixture(repo_root, 'stable')
  start <- as.POSIXct('2026-10-17 09:00:00', tz = 'America/New_York')
  set.seed(1); before <- runif(1); set.seed(1)
  a <- supervisor_simulate_call(records, 'Astoria', 'Medium', 1L, start)
  expect_equal(runif(1), before)
  b <- supervisor_simulate_call(records, 'Astoria', 'Medium', 1L, start)
  expect_identical(a, b)
  expect_true(response_metric_has_contract(a))
  expect_equal(c(a$zone, a$call_priority, a$incident_id), c('Astoria', 'Medium', 'SIM-0001'))
  expect_match(a$response_system_source, 'Synthetic')
  expect_true(a$responder_id %in% supervisor_developer_responders)
  expect_equal(supervisor_simulate_call(records, 'Astoria', 'Medium', 2L, start,
    responder = 'EMS-102')$responder_id, 'EMS-102')
  events <- response_metric_completion_events(a, allow_mock = FALSE)
  expect_equal(sort(events$metric_id), c('call_to_scene', 'transport_to_hospital'))
  expect_true(all(events$duration_minutes >= 1))
  expect_error(supervisor_simulate_call(records, 'Nowhere', 'Medium', 1L, start), 'no baseline')
})

test_that('simulated calls form one consecutive group sequence without refitting limits', {
  records <- dashboard_load_fixture(repo_root, 'stable')
  start <- as.POSIXct('2026-10-17 09:00:00', tz = 'America/New_York')
  calls <- do.call(rbind, lapply(1:5, function(i)
    supervisor_simulate_call(records, 'Astoria', 'Medium', i, start + (i - 1) * 60)))
  historical <- response_metric_to_qc(records, 'call_to_scene')
  with_calls <- supervisor_analyze(historical,
    response_metric_to_qc(calls, 'call_to_scene', allow_mock = FALSE), TRUE,
    supervisor_live_period())
  without <- supervisor_analyze(historical,
    response_metric_to_qc(calls[FALSE, ], 'call_to_scene', allow_mock = FALSE), TRUE,
    supervisor_live_period())
  g <- with_calls$groups[['Astoria/Medium']]
  expect_equal(g$monitoring$incident_id, sprintf('SIM-%04d', 1:5))
  expect_equal(lapply(with_calls$groups, `[[`, 'limits'),
    lapply(without$groups, `[[`, 'limits'))
  view <- supervisor_visible_monitoring(g, as.Date(c('2026-10-17', '2026-10-17')))
  expect_equal(view$monitoring_sequence, 1:5)
  expect_equal(nrow(supervisor_consecutive_segments(supervisor_chart_window(view),
    'response_minutes')), 4L)
})

test_that('live canonical records enter the I-MR monitoring sequence with true durations', {
  records <- dashboard_load_fixture(repo_root, 'stable')
  historical <- response_metric_to_qc(records, 'call_to_scene')
  live <- data.frame(incident_id = c('LIVE-1', 'LIVE-2'), zone = 'Astoria',
    call_priority = 'High', weather_condition = 'Rain',
    call_received_time = c('2026-10-16T16:00:00Z', '2026-10-16T17:00:00Z'),
    scene_arrival_time = c('2026-10-16T16:12:00Z', '2026-10-16T17:12:30Z'),
    transport_departure_time = NA_character_, hospital_arrival_time = NA_character_,
    responder_id = 'Q-12', stringsAsFactors = FALSE)
  completed <- response_metric_to_qc(live, 'call_to_scene', allow_mock = FALSE)
  live_analysis <- supervisor_analyze(historical, completed, TRUE, supervisor_live_period())
  m <- live_analysis$groups[['Astoria/High']]$monitoring
  expect_equal(m$incident_id, c('LIVE-1', 'LIVE-2'))
  expect_equal(m$response_minutes, c(12, 12.5))
  fixture_analysis <- dashboard_analyze(historical, completed[FALSE, ], TRUE)
  expect_equal(live_analysis$groups[['Astoria/High']]$limits,
    fixture_analysis$groups[['Astoria/High']]$limits)
  # The February 2026 developer window excludes these October records.
  expect_equal(nrow(supervisor_analyze(historical, completed, TRUE)$groups[['Astoria/High']]$monitoring), 0L)
})

test_that('ordinary-mode today follows the dispatch area calendar', {
  expect_equal(supervisor_today(as.POSIXct('2026-10-17 02:30:00', tz = 'UTC')),
    as.Date('2026-10-16'))
})

test_that('history subgroups adapt between daily and weekly completed periods', {
  m <- data.frame(dispatch_time = as.POSIXct(c('2025-01-06 08:00:00',
    '2025-01-06 09:00:00', '2025-01-13 08:00:00'), tz = 'America/New_York'),
    response_minutes = c(8, 10, 14))
  expect_equal(nrow(supervisor_history_period(m, '7')), 1L)
  expect_equal(nrow(supervisor_history_period(m, '14')), 3L)
  daily <- supervisor_period_subgroups(m, center = 10, sigma = 2, 'day')
  weekly <- supervisor_period_subgroups(m, center = 10, sigma = 2, 'week')
  expect_equal(daily$n, c(2, 1))
  expect_equal(daily$mean, c(9, 14))
  expect_equal(weekly$n, c(2, 1))
  expect_equal(as.character(weekly$period), c('2025-01-06', '2025-01-13'))
  expect_equal(supervisor_daily_subgroups(m, 10, 2)$mean, daily$mean)
})

test_that('subgroup count legends use observed integer values only', {
  expect_equal(supervisor_count_breaks(c(2, 1, 2, 5)), c(1L, 2L, 5L))
  expect_equal(supervisor_count_breaks(c(1, NA, Inf, 2)), c(1L, 2L))
})

test_that('service goals use zone overrides and never alter observed performance', {
  goals <- supervisor_empty_service_goals()
  goals <- supervisor_set_service_goal(goals, 'call_to_scene', 'High', 'All zones', 12,
    as.POSIXct('2026-10-17', tz = 'UTC'))
  goals <- supervisor_set_service_goal(goals, 'call_to_scene', 'High', 'Astoria', 10,
    as.POSIXct('2026-10-17', tz = 'UTC'))
  expect_equal(supervisor_service_goal_value(goals, 'call_to_scene', 'Astoria', 'High'), 10)
  expect_equal(supervisor_service_goal_value(goals, 'call_to_scene', 'Flushing', 'High'), 12)
  expect_true(is.na(supervisor_service_goal_value(goals, 'call_to_scene', 'Astoria', 'Low')))
  m <- data.frame(zone = c('Astoria', 'Astoria'), call_priority = c('High', 'High'),
    response_minutes = c(8, 12))
  result <- supervisor_service_performance(m, goals, 'call_to_scene')
  astoria <- result[result$zone == 'Astoria' & result$priority == 'High', ]
  expect_equal(astoria$goal, 10)
  expect_equal(astoria$within_goal, .5)
  expect_equal(astoria$p90, 11.6)
  expect_equal(supervisor_clear_service_goal(goals, 'call_to_scene', 'High', 'Astoria')$zone,
    'All zones')
})

test_that('suggested service goals are reproducible editable baseline defaults', {
  records <- dashboard_load_fixture(repo_root, 'stable')
  a <- supervisor_suggested_service_goals(records)
  b <- supervisor_suggested_service_goals(records)
  expect_equal(a, b)
  expect_equal(nrow(a), length(response_metric_catalog()$id) * length(qc_priorities))
  expect_true(all(a$zone == 'All zones'))
  expect_true(all(a$source == 'synthetic baseline suggestion'))
  expect_true(all(is.finite(a$minutes) & a$minutes > 0))
})

test_that('selected-scope service summary reports one readable comparison', {
  goals <- supervisor_empty_service_goals()
  goals <- supervisor_set_service_goal(goals, 'call_to_scene', 'High', 'All zones', 10)
  m <- data.frame(zone = c('Astoria', 'Astoria', 'Flushing'),
    call_priority = c('High', 'High', 'High'), response_minutes = c(8, 12, 20))
  d <- supervisor_service_scope_summary(m, goals, 'call_to_scene', 'Astoria', 'High')
  expect_equal(d$n, 2L)
  expect_equal(d$p90, 11.6)
  expect_equal(d$goal, 10)
  expect_equal(d$within_goal, .5)
  expect_equal(d$status, 'Above goal')
})
