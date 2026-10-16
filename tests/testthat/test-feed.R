library(testthat)

source(file.path('..', '..', 'app', 'qc.R'))
source(file.path('..', '..', 'app', 'feed.R'))

feed_records <- function() {
  data.frame(
    incident_id = paste0('SIM-', 1:4),
    zone = c('Astoria', 'Flushing', 'Jamaica', 'Forest Hills'),
    call_priority = c('High', 'Medium', 'Low', 'High'),
    weather_condition = c('Dry', 'Rain', 'Snow/Ice', 'Unknown'),
    dispatch_time = c(
      '2026-01-02T10:00:00-0500', '2026-01-02T10:02:00-0500',
      '2026-01-02T10:04:00-0500', '2026-01-02T10:06:00-0500'
    ),
    on_scene_time = c(
      '2026-01-02T10:04:00-0500', '2026-01-02T10:07:00-0500',
      '2026-01-02T10:10:00-0500', '2026-01-02T10:13:00-0500'
    ), stringsAsFactors = FALSE
  )
}

test_that('seeded fixture replay is deterministic and clearly synthetic', {
  input <- feed_records()
  original <- input
  a <- feed_build_events(input, seed = 41, count = 4,
    start_time = '2026-02-01T12:00:00Z', dispatch_gap_seconds = 30)
  b <- feed_build_events(input, seed = 41, count = 4,
    start_time = '2026-02-01T12:00:00Z', dispatch_gap_seconds = 30)
  c <- feed_build_events(input, seed = 99, count = 4,
    start_time = '2026-02-01T12:00:00Z', dispatch_gap_seconds = 30)

  expect_equal(a, b)
  expect_false(identical(a$incident_id[a$event_type == 'dispatched'],
    c$incident_id[c$event_type == 'dispatched']))
  expect_identical(input, original)
  expect_equal(attr(a, 'feed_metadata')$data_classification,
    'Synthetic replay data')
  expect_true(all(a$data_classification == 'Synthetic replay data'))
  expect_true(all(a$event_type %in% c('dispatched', 'approaching_scene', 'on_scene')))
  approaching <- a[a$event_type == 'approaching_scene', ]
  arrivals <- a[a$event_type == 'on_scene', ]
  matched_arrivals <- arrivals$event_time[match(approaching$incident_id, arrivals$incident_id)]
  expect_equal(as.numeric(difftime(matched_arrivals, approaching$event_time, units = 'secs')),
    rep(30, nrow(approaching)))
  short <- input[1, ]
  short$on_scene_time <- '2026-01-02T10:00:30-0500'
  short_events <- feed_build_events(short, seed = 1, count = 1)
  expect_false('approaching_scene' %in% short_events$event_type)
  expect_error(feed_build_events(input, seed = Inf, count = 1),
    'seed must be one finite number')
})

test_that('start pause resume and reset use simulated time only', {
  events <- feed_build_events(feed_records(), seed = 7, count = 2,
    start_time = '2026-02-01T12:00:00Z', dispatch_gap_seconds = 30)
  state <- feed_new(events)
  initial <- feed_snapshot(state)
  expect_false(initial$running)
  expect_equal(initial$processed_events, 0)

  feed_start(state)
  feed_advance(state, 1)
  after_start <- feed_snapshot(state)
  expect_true(after_start$running)
  expect_gt(after_start$processed_events, 0)

  feed_pause(state)
  paused_time <- feed_snapshot(state)$simulated_time
  paused_count <- feed_snapshot(state)$processed_events
  feed_advance(state, 10000)
  expect_equal(feed_snapshot(state)$simulated_time, paused_time)
  expect_equal(feed_snapshot(state)$processed_events, paused_count)

  feed_start(state)
  feed_advance(state, 10000)
  expect_equal(nrow(feed_completed(state)), 2)
  expect_equal(nrow(feed_pending(state)), 0)

  feed_reset(state)
  reset <- feed_snapshot(state)
  expect_false(reset$running)
  expect_equal(reset$processed_events, 0)
  expect_equal(nrow(reset$pending), 0)
  expect_equal(nrow(reset$completed), 0)
  expect_equal(reset$simulated_time, initial$simulated_time)

  feed_start(state)
  feed_advance(state, 10000)
  expect_equal(feed_completed(state)$incident_id,
    feed_completed(feed_replay_to_end(events))$incident_id)
})

test_that('pending calls stay out of QC handoff until arrival completes them', {
  events <- feed_build_events(feed_records()[1, ], seed = 1, count = 1,
    start_time = '2026-02-01T12:00:00Z', dispatch_gap_seconds = 30,
    include_approaching = FALSE)
  state <- feed_new(events)
  feed_start(state)
  feed_advance(state, 1)

  expect_equal(nrow(feed_pending(state)), 1)
  expect_equal(nrow(feed_completed(state)), 0)
  expect_equal(nrow(feed_qc_records(state)), 0)

  feed_advance(state, 4 * 60)
  completed <- feed_qc_records(state)
  expect_equal(nrow(completed), 1)
  expect_equal(completed$incident_id, 'SIM-1')
  expect_equal(completed$response_minutes, 4)
  expect_equal(qc_validate(completed)$counts[['accepted']], 1)
})

test_that('duplicate and out-of-order events are ignored and reported', {
  t0 <- qc_parse_time('2026-02-01T12:00:00Z')
  events <- data.frame(
    event_id = c('early-arrival', 'dispatch-1', 'dispatch-1', 'arrival-1'),
    incident_id = rep('SIM-X', 4),
    event_type = c('on_scene', 'dispatched', 'dispatched', 'on_scene'),
    event_time = t0 + c(0, 1, 1, 121),
    zone = rep('Astoria', 4),
    call_priority = rep('High', 4),
    weather_condition = rep('Dry', 4),
    data_classification = rep('Synthetic replay data', 4),
    stringsAsFactors = FALSE
  )
  state <- feed_new(events)
  feed_start(state)
  feed_advance(state, 200)

  expect_equal(nrow(feed_completed(state)), 1)
  expect_equal(feed_completed(state)$response_minutes, 2)
  expect_equal(sum(state$issues$reason == 'event before dispatch'), 1)
  expect_equal(sum(state$issues$reason == 'duplicate event_id'), 1)
})

test_that('completion and reset do not mutate source records or frozen limits', {
  input <- feed_records()
  input_before <- input
  frozen <- imr_fit(c(4, 5, 6, 5, 4))
  frozen_before <- serialize(frozen, NULL)
  state <- feed_replay_to_end(feed_build_events(input, seed = 10, count = 4,
    start_time = '2026-02-01T12:00:00Z', dispatch_gap_seconds = 10))

  expect_equal(nrow(feed_qc_records(state)), 4)
  expect_identical(input, input_before)
  expect_identical(serialize(frozen, NULL), frozen_before)
  expect_equal(frozen$center, 4.8)
  expect_equal(frozen$i_ucl, 4.8 + 3 / 1.128)
})
