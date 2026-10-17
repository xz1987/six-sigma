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

test_that('developer replay adds exactly one completed call per step', {
  times <- as.POSIXct('2026-02-01 12:00:00', tz = 'UTC') + c(0, 10, 60, 80)
  events <- data.frame(
    event_id = paste0('event-', 1:4),
    incident_id = c('A', 'B', 'A', 'B'),
    event_type = c('dispatched', 'dispatched', 'on_scene', 'on_scene'),
    event_time = times,
    zone = rep('Astoria', 4), call_priority = rep('High', 4),
    weather_condition = rep('Dry', 4),
    data_classification = rep(feed_label, 4), stringsAsFactors = FALSE)
  feed <- feed_new(events)
  expect_equal(nrow(feed_completed(feed)), 0L)
  expect_equal(supervisor_advance_one_completion(feed), 1L)
  expect_equal(feed_completed(feed)$incident_id, 'A')
  expect_equal(feed_snapshot(feed)$processed_events, 3L)
  expect_equal(feed$simulated_time, times[3])
  expect_equal(supervisor_advance_one_completion(feed), 1L)
  expect_equal(feed_completed(feed)$incident_id, c('A', 'B'))
  expect_equal(feed$simulated_time, times[4])
  expect_equal(supervisor_advance_one_completion(feed), 0L)
  expect_true(all(feed$processed))
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

test_that('stepped arrivals never refit the 2025 control limits', {
  records <- dashboard_load_fixture(repo_root, 'stable')
  feed <- feed_new(dashboard_build_feed(records, seed = 20261016))
  before <- dashboard_analyze(records, feed_completed(feed), TRUE)
  expect_equal(supervisor_advance_one_completion(feed), 1L)
  after <- dashboard_analyze(records, feed_completed(feed), TRUE)
  before_limits <- lapply(before$groups, function(group) group$limits)
  after_limits <- lapply(after$groups, function(group) group$limits)
  expect_equal(after_limits, before_limits)
  expect_equal(nrow(feed_completed(feed)), 1L)
})
