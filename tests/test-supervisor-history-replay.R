source(file.path('..', 'app', 'supervisor.R'), local = TRUE)

test_that('2026 History replay days are deterministic, complete and varied', {
  baseline <- dashboard_load_fixture(repo_root, 'stable')
  days <- as.Date(c('2026-01-01', '2026-01-02'))
  stable <- supervisor_history_replay_days(baseline, 'Astoria', 'High',
    'call_to_scene', days, 'stable')
  repeat_stable <- supervisor_history_replay_days(baseline, 'Astoria', 'High',
    'call_to_scene', days, 'stable')
  delayed <- supervisor_history_replay_days(baseline, 'Astoria', 'High',
    'call_to_scene', days, 'abnormal')

  expect_identical(stable, repeat_stable)
  expect_equal(nrow(stable), 24L)
  expect_equal(anyDuplicated(stable$incident_id), 0L)
  expect_true(all(grepl('^HIST-2026010[12]-', stable$incident_id)))
  expect_true(response_metric_has_contract(stable))
  completed <- qc_validate(response_metric_to_qc(stable, 'call_to_scene',
    allow_mock = FALSE))$completed
  expect_equal(as.integer(table(as.Date(completed$dispatch_time,
    tz = 'America/New_York'))), c(12L, 12L))
  daily_means <- tapply(completed$response_minutes,
    as.Date(completed$dispatch_time, tz = 'America/New_York'), mean)
  expect_gt(length(unique(round(daily_means, 6))), 1L)
  delayed_completed <- qc_validate(response_metric_to_qc(delayed, 'call_to_scene',
    allow_mock = FALSE))$completed
  expect_equal(tail(delayed_completed$response_minutes, 3L), rep(30, 3L))
  expect_error(supervisor_history_replay_days(baseline, 'Astoria', 'High',
    'call_to_scene', as.Date('2027-01-01')), 'valid dates in 2026')
})

test_that('History replay closes days, updates outputs and preserves ordinary records', {
  live <- response_metric_empty_records()
  suppressWarnings(shiny::testServer(supervisor_server,
    args = list(production_records = function() live), {
      session$setInputs(metric = 'call_to_scene', zone = 'Astoria', priority = 'High',
        time_span = 'day', day_date = '2026-01-01', main_tabs = 'Active tracking',
        history_metric = 'call_to_scene', history_zone = 'Astoria',
        history_priority = 'High', history_period = 'full',
        history_aggregation = 'day', history_phase = 'both', demo_review = TRUE,
        developer_panel_open = TRUE, enter_developer = 1,
        history_replay_scenario = 'stable')
      frozen <- lapply(history_analysis()$groups, function(group) group$limits)
      expect_match(paste(output$developer_bar, collapse = ' '), 'Daily demo')
      expect_false(grepl('History scenario', paste(output$developer_bar, collapse = ' ')))

      session$setInputs(main_tabs = 'History analysis')
      expect_match(paste(output$developer_bar, collapse = ' '), 'History scenario')
      expect_false(grepl('Daily demo', paste(output$developer_bar, collapse = ' ')))

      session$setInputs(start_history_replay = 1)
      expect_equal(state$history_replay_days, 1L)
      expect_equal(state$history_replay_through, as.Date('2026-01-01'))
      expect_equal(subgroup_open_day(), as.Date('2026-01-02'))
      expect_equal(nrow(history_completed()), 12L)
      expect_equal(nrow(active_completed()), 0L)
      expect_equal(sum(history_all_records()$phase == 'Closed monitoring day'), 12L)
      expect_identical(frozen, lapply(history_analysis()$groups, function(group) group$limits))
      expect_match(paste(output$developer_bar_text, collapse = ' '),
        '1 closed 2026 day')

      session$setInputs(next_history_day = 1)
      expect_equal(state$history_replay_days, 2L)
      expect_equal(state$history_replay_through, as.Date('2026-01-02'))
      expect_equal(nrow(history_completed()), 24L)
      expect_equal(nrow(active_completed()), 0L)
      expect_equal(sum(history_all_records()$phase == 'Closed monitoring day'), 24L)

      session$setInputs(next_history_week = 1)
      expect_equal(state$history_replay_days, 9L)
      expect_equal(nrow(history_completed()), 108L)
      expect_equal(nrow(active_completed()), 0L)
      monitoring_subgroups <- history_subgroups()
      expect_equal(sum(monitoring_subgroups$phase == 'Closed monitoring'), 9L)
      expect_false(is.null(output$history_control_chart))
      expect_false(is.null(output$history_distribution_chart))
      expect_false(is.null(output$service_goal_status))

      session$setInputs(main_tabs = 'Active tracking', daily_demo_scenario = 'stable',
        load_daily_demo = 1, next_demo_case = 1)
      expect_match(paste(output$developer_bar, collapse = ' '), 'Daily demo')
      expect_false(grepl('History scenario', paste(output$developer_bar, collapse = ' ')))
      expect_equal(nrow(active_completed()), 1L)
      expect_equal(nrow(history_completed()), 108L)

      session$setInputs(main_tabs = 'Patterns')
      expect_true(is.null(output$developer_bar))

      session$setInputs(main_tabs = 'History analysis')

      session$setInputs(reset_history_replay = 1)
      expect_equal(state$history_replay_days, 0L)
      expect_equal(nrow(history_completed()), 0L)
      expect_equal(nrow(active_completed()), 1L)
      expect_equal(sum(history_all_records()$phase == 'Closed monitoring day'), 0L)
      expect_identical(frozen, lapply(history_analysis()$groups, function(group) group$limits))

      session$setInputs(end_developer = 1)
      expect_identical(state$mode, 'ordinary')
      expect_null(state$developer_calls)
      expect_null(state$history_replay_calls)
      expect_equal(nrow(active_completed()), nrow(live))
    }))
})
