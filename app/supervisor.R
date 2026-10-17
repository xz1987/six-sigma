library(ggplot2)

find_repo_root <- function() {
  candidates <- unique(normalizePath(c(getwd(), file.path(getwd(), '..')),
    mustWork = FALSE))
  hit <- candidates[file.exists(file.path(candidates, 'app', 'qc.R')) &
    dir.exists(file.path(candidates, 'data'))]
  if (!length(hit)) stop('Run this application from the repository root or app directory.')
  hit[[1]]
}

repo_root <- find_repo_root()
source(file.path(repo_root, 'app', 'qc.R'), local = TRUE)
source(file.path(repo_root, 'app', 'response-metrics.R'), local = TRUE)
source(file.path(repo_root, 'app', 'feed.R'), local = TRUE)
source(file.path(repo_root, 'app', 'workflow.R'), local = TRUE)

fmt_num <- function(x, digits = 1) {
  if (!length(x) || is.na(x)) '—' else formatC(x, digits = digits, format = 'f')
}

kpi <- function(label, value_id, unit = NULL, ns = identity) {
  div(class = 'kpi', div(class = 'kpi-label', label),
    div(class = 'kpi-value', textOutput(ns(value_id), inline = TRUE)),
    if (!is.null(unit)) div(class = 'kpi-unit', unit))
}

panel_card <- function(title, subtitle, ...) {
  div(class = 'panel-card', h3(title), div(class = 'subhead', subtitle), ...)
}

# Today's calendar date for the dispatch area, independent of the server's clock zone.
supervisor_today <- function(now = Sys.time()) as.Date(now, tz = 'America/New_York')

# Ordinary mode monitors the live Caller/Responder stream. It keeps the same fixed
# 2025 baseline as dashboard_period(), but its monitoring window stays open so
# records completed after the February 2026 developer fixture are not dropped.
supervisor_live_period <- function() {
  fixture <- dashboard_period()
  qc_period('2025-01-01T00:00:00-0500', '2026-01-01T00:00:00-0500',
    format(fixture$monitoring_start, '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC'),
    '2100-01-01T00:00:00-0500')
}

# Same orchestration as dashboard_analyze(), with an explicit period. Live
# canonical records arrive as ISO 8601 strings; rbind() with the parsed baseline
# would silently coerce them to midnight (zero-minute durations), so parse them
# with the strict QC parser first. Invalid stamps become NA and are rejected.
supervisor_analyze <- function(records, completed, demo_review = FALSE,
                               period = dashboard_period()) {
  for (field in c('dispatch_time', 'on_scene_time'))
    if (field %in% names(completed)) completed[[field]] <- qc_parse_time(completed[[field]])
  combined <- dashboard_analysis_records(records, completed)
  preliminary <- qc_analyze(combined, period)
  if (!isTRUE(demo_review)) return(preliminary)
  qc_analyze(combined, period, reviews = dashboard_demo_reviews(preliminary))
}

# Calendar windows are display-only. They never enter qc_analyze() or refit limits.
supervisor_date_window <- function(span, anchor, custom = NULL) {
  day <- as.Date(anchor)
  if (length(day) != 1L || is.na(day)) stop('anchor date is required')
  if (identical(span, 'day')) return(c(day, day))
  if (identical(span, 'week')) {
    monday <- day - ((as.POSIXlt(day)$wday + 6L) %% 7L)
    return(c(monday, monday + 6L))
  }
  if (identical(span, 'month')) {
    first <- as.Date(format(day, '%Y-%m-01'))
    return(c(first, seq(first, by = 'month', length.out = 2L)[2L] - 1L))
  }
  if (identical(span, 'custom') && length(custom) == 2L &&
      all(!is.na(as.Date(custom))) && custom[1L] <= custom[2L])
    return(as.Date(custom))
  stop('invalid display window')
}

supervisor_week_choices <- function(year = 2026L) {
  first <- as.Date(sprintf('%d-01-01', year))
  last <- as.Date(sprintf('%d-12-31', year))
  first_monday <- first - ((as.POSIXlt(first)$wday + 6L) %% 7L)
  starts <- seq(first_monday, last, by = 'week')
  labels <- sprintf('%s – %s', format(starts, '%b %d'),
    format(starts + 6L, '%b %d, %Y'))
  setNames(as.character(starts), labels)
}

supervisor_month_choices <- function(year = 2026L) {
  starts <- seq(as.Date(sprintf('%d-01-01', year)), by = 'month', length.out = 12L)
  setNames(as.character(starts), format(starts, '%B %Y'))
}

supervisor_period_values <- function(day) {
  day <- as.Date(day)
  list(day = day,
    week = supervisor_date_window('week', day)[1L],
    month = as.Date(format(day, '%Y-%m-01')))
}

# Filter points for display only; retain their full-group sequence and QC signals.
supervisor_visible_monitoring <- function(group, window, responder = 'all') {
  m <- group$monitoring
  m$monitoring_sequence <- seq_len(nrow(m))
  m$plot_sequence <- nrow(group$baseline) + seq_len(nrow(m))
  dates <- as.Date(m$dispatch_time, tz = 'America/New_York')
  keep <- dates >= window[1L] & dates <= window[2L]
  if (!is.null(responder) && !identical(responder, 'all')) {
    keep <- keep & if ('responder_id' %in% names(m))
      as.character(m$responder_id) == responder else rep(FALSE, nrow(m))
  }
  m[keep, , drop = FALSE]
}

# Display a readable moving window; this does not change QC order or limits.
supervisor_chart_window <- function(m, focus_id = NULL, width = 24L) {
  if (!nrow(m)) return(m)
  focus <- match(focus_id, m$incident_id)
  # Rows are in metric-start (QC) order, so the newest sequence position is the
  # last row even when an earlier-started call completed later.
  last <- if (length(focus) == 1L && !is.na(focus)) focus else nrow(m)
  first <- max(1L, last - width + 1L)
  m[seq.int(first, last), , drop = FALSE]
}

supervisor_consecutive_segments <- function(m, value_col) {
  empty <- data.frame(x = numeric(), xend = numeric(), y = numeric(), yend = numeric())
  if (nrow(m) < 2L) return(empty)
  adjacent <- which(diff(m$monitoring_sequence) == 1L)
  if (!length(adjacent)) return(empty)
  data.frame(x = m$monitoring_sequence[adjacent],
    xend = m$monitoring_sequence[adjacent + 1L],
    y = m[[value_col]][adjacent], yend = m[[value_col]][adjacent + 1L])
}

# QC calculations stay unchanged; this decorates monitoring rows with the
# responder identifier carried by the ordinary-mode canonical stream.
supervisor_attach_responder_ids <- function(analysis, completed) {
  if (!'responder_id' %in% names(completed)) return(analysis)
  for (key in names(analysis$groups)) {
    monitoring <- analysis$groups[[key]]$monitoring
    matched <- match(monitoring$incident_id, completed$incident_id)
    monitoring$responder_id <- completed$responder_id[matched]
    analysis$groups[[key]]$monitoring <- monitoring
  }
  analysis
}

supervisor_developer_responders <- c('EMS-101', 'EMS-102', 'EMS-103')

# Baseline (2025) mean and SD of one metric for one zone/priority group, read
# from the bundled synthetic fixture. Used only to draw developer simulations.
supervisor_baseline_moments <- function(historical, metric_id, zone, priority) {
  d <- response_metric_to_qc(historical, metric_id, allow_mock = TRUE)
  start <- qc_parse_time(d$dispatch_time)
  minutes <- as.numeric(difftime(qc_parse_time(d$on_scene_time), start, units = 'mins'))
  period <- dashboard_period()
  keep <- d$zone == zone & d$call_priority == priority & !is.na(minutes) &
    start >= period$baseline_start & start < period$baseline_end
  x <- minutes[keep]
  if (length(x) < 2L) stop('no baseline records for ', zone, '/', priority)
  list(mean = mean(x), sd = stats::sd(x))
}

# One explicitly synthetic completed call in canonical response-system shape.
# Both metric durations are drawn from the selected group's own 2025 baseline,
# so a stable process is simulated; no limit is refitted. Deterministic for a
# given seed and index, and the global RNG state is left untouched.
supervisor_simulate_call <- function(historical, zone, priority, index, start_time,
                                     responder = NULL, seed = 20261016L) {
  scene <- supervisor_baseline_moments(historical, 'call_to_scene', zone, priority)
  transport <- supervisor_baseline_moments(historical, 'transport_to_hospital', zone, priority)
  old_seed <- if (exists('.Random.seed', envir = globalenv())) get('.Random.seed', envir = globalenv())
  on.exit(if (is.null(old_seed)) rm('.Random.seed', envir = globalenv())
    else assign('.Random.seed', old_seed, envir = globalenv()))
  set.seed(as.integer(seed) + as.integer(index))
  scene_minutes <- max(1, stats::rnorm(1, scene$mean, scene$sd))
  transport_minutes <- max(1, stats::rnorm(1, transport$mean, transport$sd))
  dwell_minutes <- stats::runif(1, 8, 16)
  weather <- sample(c('Dry', 'Rain', 'Snow/Ice'), 1, prob = c(.7, .25, .05))
  if (is.null(responder) || identical(responder, 'all'))
    responder <- sample(supervisor_developer_responders, 1)
  received <- as.POSIXct(start_time, tz = 'America/New_York')
  arrived <- received + scene_minutes * 60
  departed <- arrived + dwell_minutes * 60
  hospital <- departed + transport_minutes * 60
  stamp <- function(t) format(t, '%Y-%m-%dT%H:%M:%S%z', tz = 'America/New_York')
  data.frame(incident_id = sprintf('SIM-%04d', as.integer(index)), zone = zone,
    call_priority = priority, weather_condition = weather,
    call_received_time = stamp(received), scene_arrival_time = stamp(arrived),
    transport_departure_time = stamp(departed), hospital_arrival_time = stamp(hospital),
    responder_id = responder,
    response_system_source = 'Synthetic developer simulation',
    stringsAsFactors = FALSE)
}

supervisor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = 'supervisor-pwa',
      div(id = 'connection-status', class = 'alert-note danger', role = 'status', hidden = NA,
        "You're offline. Reconnect to use login, incident records, and response updates."),
      div(class = 'pwa-tools',
        tags$button(id = 'install-app', type = 'button', class = 'btn btn-primary',
          hidden = NA, 'Install app'),
        tags$details(tags$summary('Add this supervisor dashboard to your device'),
          p("iPhone/iPad: use the browser Share menu → Add to Home Screen. Safari on Mac: File → Add to Dock. Chrome/Edge: use the install option in the address bar or browser menu."),
          p('Live features require a connection. This remains a synthetic hackathon demonstration.')),
        tags$span(id = 'pwa-setup-status', role = 'status'))),
    div(class = 'app-shell',
    tags$aside(class = 'control-rail',
      div(class = 'rail-heading', div(class = 'kpi-label', 'Supervisor workspace'), h2('1 · Set your view')),
      selectInput(ns('time_span'), 'Time span',
        choices = c('Day' = 'day', 'Week' = 'week', 'Month' = 'month'),
        selected = 'day'),
      selectInput(ns('metric'), 'Response metric',
        choices = setNames(response_metric_catalog()$id,
          response_metric_catalog()$label), selected = 'call_to_scene'),
      conditionalPanel(sprintf("input['%s'] === 'day'", ns('time_span')),
        dateInput(ns('day_date'), 'Date', value = supervisor_today(),
          min = as.Date('2026-01-01'), max = as.Date('2026-12-31'))),
      conditionalPanel(sprintf("input['%s'] === 'week'", ns('time_span')),
        selectInput(ns('week_start'), 'Week', choices = supervisor_week_choices(),
          selected = '2026-01-26')),
      conditionalPanel(sprintf("input['%s'] === 'month'", ns('time_span')),
        selectInput(ns('month_start'), 'Month', choices = supervisor_month_choices(),
          selected = '2026-02-01')),
      selectInput(ns('zone'), 'Incident zone', choices = qc_zones,
        selected = 'Far Rockaway'),
      selectInput(ns('priority'), 'Call priority · QC group', choices = qc_priorities),
      selectInput(ns('responder'), 'Responder',
        choices = c('All responders · IDs not yet available' = 'all'), selected = 'all'),
      uiOutput(ns('responder_note')),
      div(class = 'rail-note',
        strong('Response-time definition'), br(),
        textOutput(ns('metric_definition'), inline = TRUE), br(),
        tags$small('Each metric has its own fixed baseline limits. Display filters never refit them.')),
      tags$details(id = ns('developer_panel'), class = 'developer-settings',
        ontoggle = sprintf("Shiny.setInputValue('%s', this.open, {priority: 'event'});",
          ns('developer_panel_open')),
        tags$summary(title = 'Developer settings',
          onclick = sprintf("if (this.parentElement.open) Shiny.setInputValue('%s', Date.now(), {priority: 'event'});",
            ns('developer_panel_closed')),
          tags$span(class = 'sr-only', 'Developer settings')),
        uiOutput(ns('developer_controls')))
    ),
    tags$main(class = 'workspace',
      uiOutput(ns('developer_bar')),
      div(class = 'dashboard-heading',
        div(div(class = 'kpi-label', 'Response quality / Supervisor'),
          h1(textOutput(ns('dashboard_title'), inline = TRUE)),
          p('Completed response-system records enter the selected metric view. Historical limits stay fixed.')),
        uiOutput(ns('mode_chip'))),
      div(class = 'scope-strip',
        textOutput(ns('scope_label'), inline = TRUE),
        div(class = 'status-pill', strong('QC status: '), textOutput(ns('qc_status'), inline = TRUE))),
      uiOutput(ns('source_notice')),
      uiOutput(ns('shared_trip')),
      div(class = 'replay-timeline',
        div(class = 'timeline-clock', span(class = 'timeline-pulse'),
          strong(textOutput(ns('time_context_label'), inline = TRUE)),
          textOutput(ns('overview_sim_time'), inline = TRUE)),
        textOutput(ns('latest_arrival'), inline = TRUE)),
      tabsetPanel(id = ns('main_tabs'),
        tabPanel('2 · Review', value='review',
          div(class = 'kpi-grid',
            kpi('Completed records', 'kpi_completed', 'for selected metric / group', ns),
            kpi('Median duration', 'kpi_median', 'minutes', ns),
            kpi('90th percentile', 'kpi_p90', 'minutes', ns),
            kpi('QC signals', 'kpi_flags', 'in selected time / group', ns)),
          uiOutput(ns('review_notice')),
          panel_card('Latest completed response milestones',
            'Completion records for the selected response metric only: scene arrivals for call to scene, hospital arrivals for transport to hospital.',
            tableOutput(ns('milestone_table'))),
          actionButton(ns('investigate_next'), 'Next: investigate signals', class='btn-primary'),
        ),
        tabPanel('3 · Investigate', value='investigate',
          div(class = 'analysis-layout',
            div(class = 'chart-stack',
              panel_card('Individuals control chart',
                'New completed metric records appear one by one in metric-start order. The chart shows the latest 24 visible incidents; dashed lines are fixed 2025 baseline limits.',
                plotOutput(ns('i_chart'), height = 345)),
              panel_card('Moving-range control chart',
                'Each range uses its true predecessor in the full zone/priority sequence, even when this view hides other points.',
                plotOutput(ns('mr_chart'), height = 265))),
            div(class = 'flag-panel panel-card',
              div(class = 'flag-heading', h3('Signals to investigate'),
                span(class = 'flag-count', textOutput(ns('flag_count'), inline = TRUE))),
              tags$p(class = 'subhead', 'Select a signal to locate it on the charts and review the evidence.'),
              uiOutput(ns('flag_cards')),
              uiOutput(ns('flag_detail')))),
          panel_card('Completed metric records in this view',
            'The display filters change which completed metric records you see, not the baseline or control limits.',
            tableOutput(ns('completed_table')))
        ),
        tabPanel('4 · Explore patterns', value='patterns',
          panel_card('Neighborhood distribution',
            'Selected time span and priority. Type-7 quartiles and 1.5×IQR whiskers; descriptive only.',
            plotOutput(ns('zone_boxplot'), height = 380)),
          div(class = 'two-col',
            panel_card('Assignment hour × weekday',
              'Median response minutes in America/New_York; labels show median and sample count. Blank means no data.',
              plotOutput(ns('heatmap'), height = 420)),
            panel_card('Weather context',
              'Synthetic assignment-time weather. Descriptive only; weather does not change QC limits or establish cause.',
              plotOutput(ns('weather_boxplot'), height = 420)))
        )
      )
    )
  )
  )
}

supervisor_server <- function(id, production_records) {
  moduleServer(id, function(input, output, session) {
  observeEvent(input$investigate_next, { updateTabsetPanel(session, 'main_tabs', selected='investigate') })
  state <- reactiveValues(mode = 'ordinary', developer_calls = NULL,
    baseline_records = dashboard_load_fixture(repo_root, 'stable'),
    ui_tick = 0L, completed_tick = 0L, last_step = NULL)

  set_period_for_mode <- function(day) {
    values <- supervisor_period_values(day)
    updateDateInput(session, 'day_date', value = values$day)
    updateSelectInput(session, 'week_start', selected = as.character(values$week))
    updateSelectInput(session, 'month_start', selected = as.character(values$month))
  }

  enter_developer_mode <- function() {
    state$mode <- 'developer'
    state$developer_calls <- response_metric_empty_records()
    state$last_step <- NULL
    state$ui_tick <- state$ui_tick + 1L
    state$completed_tick <- state$completed_tick + 1L
    set_period_for_mode(supervisor_today())
  }

  leave_developer_mode <- function() {
    state$mode <- 'ordinary'
    state$developer_calls <- NULL
    state$last_step <- NULL
    state$ui_tick <- state$ui_tick + 1L
    state$completed_tick <- state$completed_tick + 1L
    set_period_for_mode(supervisor_today())
  }

  observeEvent(input$enter_developer, enter_developer_mode())
  observeEvent(input$end_developer, leave_developer_mode())
  observeEvent(input$developer_panel_open, {
    if (!isTRUE(input$developer_panel_open) && identical(state$mode, 'developer'))
      leave_developer_mode()
  }, ignoreInit = TRUE)
  observeEvent(input$developer_panel_closed, {
    if (identical(state$mode, 'developer')) leave_developer_mode()
  }, ignoreInit = TRUE)

  # Developer mode: each click adds one simulated completed call to the zone,
  # priority and responder currently selected, so it lands on the visible chart.
  observeEvent(input$add_call, {
    req(identical(state$mode, 'developer'), input$zone, input$priority)
    calls <- state$developer_calls
    # Each simulated call starts after the previous one reaches the hospital, so
    # both metrics keep start order = completion order and the newest call is
    # always the latest point on either chart.
    start_time <- Sys.time()
    if (nrow(calls)) start_time <- max(start_time,
      max(qc_parse_time(calls$hospital_arrival_time)) + 60)
    call <- supervisor_simulate_call(state$baseline_records, input$zone, input$priority,
      index = nrow(calls) + 1L, start_time = start_time, responder = input$responder)
    state$developer_calls <- rbind(calls, call[, names(calls), drop = FALSE])
    event <- response_metric_completion_events(call, allow_mock = FALSE)
    event <- event[event$metric_id == input$metric, , drop = FALSE]
    call$display_minutes <- event$duration_minutes[[1L]]
    call$display_metric <- metric_info()$short_label
    state$last_step <- call
    state$completed_tick <- isolate(state$completed_tick) + 1L
    state$ui_tick <- isolate(state$ui_tick) + 1L
  })

  snapshot <- reactive({
    state$ui_tick
    records <- if (identical(state$mode, 'developer')) state$developer_calls
      else production_records()
    as_of <- Sys.time()
    if (identical(state$mode, 'developer') && nrow(records))
      as_of <- max(qc_parse_time(records$call_received_time))
    list(simulated_time = as_of, running = FALSE, pending = records[FALSE, , drop = FALSE],
      completed = records, processed_events = nrow(records), total_events = nrow(records))
  })
  active_completed <- reactive({
    state$completed_tick
    if (identical(state$mode, 'developer')) state$developer_calls
    else production_records()
  })
  active_source_records <- reactive(active_completed())
  metric_info <- reactive({ req(input$metric); response_metric_info(input$metric) })
  analysis <- reactive({
    req(input$metric)
    state$completed_tick
    req(state$baseline_records)
    historical <- response_metric_to_qc(state$baseline_records, input$metric, allow_mock = TRUE)
    completed <- response_metric_to_qc(active_completed(), input$metric,
      allow_mock = FALSE)
    review_baseline <- if (identical(state$mode, 'developer'))
      isTRUE(input$demo_review) else TRUE
    result <- supervisor_analyze(historical, completed, review_baseline,
      supervisor_live_period())
    supervisor_attach_responder_ids(result, completed)
  })

  # Ordinary mode: when a new milestone for the selected metric arrives, show its
  # zone/priority group so the new point is visible on the Individuals chart.
  seen_live_completions <- reactiveVal(character())
  observe({
    if (!identical(state$mode, 'ordinary')) return()
    events <- response_metric_completion_events(production_records(), allow_mock = FALSE)
    fresh <- setdiff(events$completion_id, isolate(seen_live_completions()))
    seen_live_completions(events$completion_id)
    latest <- events[events$completion_id %in% fresh &
      events$metric_id == isolate(input$metric), , drop = FALSE]
    if (!nrow(latest)) return()
    latest <- latest[which.max(latest$completed_at), , drop = FALSE]
    updateSelectInput(session, 'zone', selected = latest$zone)
    updateSelectInput(session, 'priority', selected = latest$call_priority)
  })
  selected_group <- reactive({
    req(input$zone, input$priority)
    key <- paste(input$zone, input$priority, sep = '/')
    analysis()$groups[[key]]
  })

  output$developer_controls <- renderUI({
    if (!identical(state$mode, 'developer')) {
      return(tagList(
        tags$p('Ordinary mode is active. The dashboard is reading only the shared Caller and Responder response stream.'),
        actionButton(session$ns('enter_developer'), 'Enter developer mode',
          class = 'btn-default', width = '100%')))
    }
    tagList(
      div(class = 'developer-mode-card',
        tags$strong('Developer mode active'),
        tags$p('The ordinary stream is disconnected from this view. Use Add simulated call above the dashboard to add synthetic completed calls; they are discarded when you leave developer mode.')),
      div(class = 'review-box',
        checkboxInput(session$ns('demo_review'),
          'Use synthetic baseline-review fixture', value = TRUE),
        tags$small('Demonstration only. This is not approval of a real EMS baseline.')),
      actionButton(session$ns('end_developer'), 'End developer mode',
        class = 'btn-warning', width = '100%'))
  })

  # The bar itself renders only when the mode changes, so the button keeps focus
  # across clicks; the status text updates separately.
  output$developer_bar <- renderUI({
    if (!identical(state$mode, 'developer')) return(NULL)
    div(class = 'developer-bar', role = 'region', `aria-label` = 'Developer simulation',
      uiOutput(session$ns('developer_bar_text'), class = 'developer-bar-text'),
      actionButton(session$ns('add_call'), 'Add simulated call', class = 'btn-primary',
        icon = icon('plus')))
  })
  output$developer_bar_text <- renderUI({
    req(identical(state$mode, 'developer'))
    target <- sprintf('%s · %s priority%s', input$zone, input$priority,
      if (!is.null(input$responder) && !identical(input$responder, 'all'))
        paste(' ·', input$responder) else '')
    last <- state$last_step
    n <- nrow(state$developer_calls)
    tagList(
      div(class = 'kpi-label', 'Developer mode · synthetic simulation'),
      tags$strong(sprintf('%d simulated call%s', n, if (n == 1L) '' else 's')),
      tags$span(class = 'developer-bar-detail', role = 'status',
        if (is.null(last)) paste('Next call goes to', target)
        else sprintf('Last: %s · %s / %s · %s %.1f min · next call goes to %s',
          last$incident_id, last$zone, last$call_priority,
          tolower(last$display_metric), last$display_minutes, target)))
  })

  output$mode_chip <- renderUI({
    if (identical(state$mode, 'developer'))
      return(div(class = 'baseline-chip mode-chip-developer',
        'Developer mode', tags$small('Simulated calls only')))
    div(class = 'baseline-chip', 'Ordinary mode',
      tags$small('Caller + responder stream'))
  })

  output$source_notice <- renderUI({
    if (identical(state$mode, 'developer')) {
      return(div(class = 'alert-note',
        'Developer mode · simulated calls are isolated from ordinary response records and use the fixed 2025 synthetic baseline. Close Developer settings or select End developer mode to discard them and reconnect.'))
    }
    count <- nrow(production_records())
    milestones <- nrow(response_metric_completion_events(production_records(), allow_mock = FALSE))
    div(class = 'alert-note',
      if (count)
        sprintf('Ordinary mode · %d canonical incident%s and %d completed response milestone%s received from the shared Caller/Responder stream.',
          count, if (count == 1L) '' else 's', milestones,
          if (milestones == 1L) '' else 's')
      else
        'Ordinary mode · connected to the shared Caller/Responder stream. No completed response records have been emitted yet.')
  })

  responder_ids <- reactive({
    m <- selected_group()$monitoring
    if (!'responder_id' %in% names(m)) return(character())
    sort(unique(as.character(m$responder_id[!is.na(m$responder_id) & nzchar(as.character(m$responder_id))])))
  })
  observe({
    ids <- responder_ids()
    choices <- if (length(ids)) c('All responders' = 'all', setNames(ids, ids))
      else c('All responders · IDs not yet available' = 'all')
    current <- isolate(input$responder)
    updateSelectInput(session, 'responder', choices = choices,
      selected = if (!is.null(current) && current %in% c('all', ids)) current else 'all')
  })
  output$responder_note <- renderUI({
    if (length(responder_ids()))
      tags$p(class = 'filter-note', 'Responder selection changes visible completed metric records; fixed metric/zone/priority control limits remain unchanged.')
    else tags$p(class = 'filter-note',
      if (identical(state$mode, 'developer'))
        'Simulated calls are assigned synthetic responder IDs. This filter activates after the first simulated call in this group.'
      else
        'No responder IDs are present in the ordinary stream yet. This filter activates when responder records arrive.')
  })
  output$metric_definition <- renderText({ metric_info()$definition })
  output$dashboard_title <- renderText({ paste(metric_info()$short_label, 'overview') })
  display_window <- reactive({
    req(input$time_span)
    if (identical(input$time_span, 'day')) {
      req(input$day_date)
      return(supervisor_date_window('day', input$day_date))
    }
    if (identical(input$time_span, 'week')) {
      req(input$week_start)
      return(supervisor_date_window('week', as.Date(input$week_start)))
    }
    req(input$month_start)
    supervisor_date_window('month', as.Date(input$month_start))
  })
  visible_monitoring <- reactive({
    supervisor_visible_monitoring(selected_group(), display_window(), input$responder)
  })
  selected_monitor_summary <- reactive({ qc_summary(visible_monitoring()$response_minutes) })
  visible_flags <- reactive({
    if (!isTRUE(selected_group()$flags_enabled)) return(visible_monitoring()[FALSE, , drop = FALSE])
    m <- visible_monitoring()
    hit <- (!is.na(m$individual_signal) & m$individual_signal) |
      (!is.na(m$variation_signal) & m$variation_signal)
    m[hit, , drop = FALSE]
  })
  selected_flag_id <- reactiveVal(NULL)
  observeEvent(input$selected_flag, selected_flag_id(input$selected_flag), ignoreInit = TRUE)
  observeEvent(input$follow_live, selected_flag_id(NULL), ignoreInit = TRUE)

  output$sim_time <- renderText({
    format(snapshot()$simulated_time, '%b %d, %Y %H:%M:%S %Z', tz = 'America/New_York')
  })
  output$overview_sim_time <- renderText({
    format(snapshot()$simulated_time, '%b %d, %Y %H:%M:%S %Z', tz = 'America/New_York')
  })
  output$time_context_label <- renderText({
    if (identical(state$mode, 'developer')) 'Simulated as of ' else 'Production stream as of '
  })
  output$latest_arrival <- renderText({
    d <- selected_group()$monitoring
    if (!nrow(d)) return(if (identical(state$mode, 'developer'))
      'No simulated calls in this group yet · select Add simulated call'
    else 'Waiting for completed response records from Caller and Responder')
    row <- d[which.max(d$on_scene_time), , drop = FALSE]
    sprintf('Latest %s: %s · %s / %s · %.1f min',
      tolower(metric_info()$short_label), row$incident_id,
      row$zone, row$call_priority, row$response_minutes)
  })
  output$qc_status <- renderText({ selected_group()$status })
  output$scope_label <- renderText({
    window <- display_window()
    sprintf('%s – %s  ·  %s  ·  %s priority  ·  %s',
      format(window[1L], '%b %d, %Y'), format(window[2L], '%b %d, %Y'),
      input$zone, input$priority, metric_info()$short_label)
  })
  output$kpi_completed <- renderText({ selected_monitor_summary()$n })
  output$kpi_median <- renderText({ fmt_num(selected_monitor_summary()$median, 1) })
  output$kpi_p90 <- renderText({ fmt_num(selected_monitor_summary()$p90, 1) })
  output$kpi_flags <- renderText({ nrow(visible_flags()) })
  output$flag_count <- renderText({ nrow(visible_flags()) })

  output$shared_trip <- renderUI({
    if(!identical(state$mode,'ordinary') || !exists('responder_trip_snapshot',mode='function')) return(NULL)
    trip <- responder_trip_snapshot()
    if(is.null(trip)) return(NULL)
    label <- if(trip$phase=='hospital') 'At hospital' else if(trip$phase=='transport') 'Transporting to hospital' else responder_demo(trip$scene_elapsed,trip$scenario)$status
    div(class='panel-card',div(class='kpi-label','Shared live demo trip'),
      h3(paste(trip$incident_id,'·',trip$unit)),
      p(paste(trip$zone,'·',trip$priority,'·',label)),
      if(!is.null(trip$hospital)) p(strong('Confirmed hospital: '),trip$hospital$name),
      p(class='filter-note','Same trip as Responder and private Caller tracking. Synthetic event times; developer replay remains separate.'))
  })
  output$connection_contract <- renderUI({
    info <- metric_info()
    tags$div(class = 'connection-contract',
      div(class = 'status-pill', strong('Current source: '),
        response_metric_connection_status(active_source_records())),
      tags$p(strong('Required identifiers/context: '),
        tags$code('incident_id, zone, call_priority, weather_condition')),
      tags$p(strong('Required timestamps: '),
        tags$code('call_received_time, scene_arrival_time, transport_departure_time, hospital_arrival_time')),
      tags$p(sprintf('Selected mapping: %s → %s. The upstream response system must only emit hospital_arrival_time after it confirms the vehicle has stopped near the hospital.',
        info$start_field, info$end_field)),
      tags$small('Future integration calls response_metric_to_qc(records, metric_id, allow_mock = FALSE). The adapter validates the selected start/end pair through the existing QC pipeline.'))
  })
  output$review_notice <- renderUI({
    group <- selected_group()
    if (!identical(state$mode, 'developer')) {
      return(div(class = 'alert-note',
        'Ordinary mode evaluates only completed canonical Caller/Responder records against the fixed historical baseline.'))
    }
    if (isTRUE(group$flags_enabled)) {
      div(class = 'alert-note danger',
        'DEMO-ONLY REVIEW FIXTURE ACTIVE. Synthetic flags are visible for demonstration and are not operational approval.')
    } else {
      div(class = 'alert-note',
        paste('Flags disabled:', group$status,
          '— use the explicitly labeled demo-only control to preview synthetic flags.'))
    }
  })

  output$completed_table <- renderTable({
    d <- visible_monitoring()
    if (!nrow(d)) return(data.frame(Status = 'No completed metric records in this view yet.'))
    d <- d[order(d$on_scene_time, decreasing = TRUE), , drop = FALSE]
    shown <- data.frame(
      Incident = d$incident_id, Neighborhood = d$zone, Priority = d$call_priority,
      `Duration (minutes)` = sprintf('%.1f', d$response_minutes),
      `Metric completed` = format(d$on_scene_time, '%b %d %H:%M:%S', tz = 'America/New_York'),
      check.names = FALSE
    )
    head(shown, 12)
  }, striped = TRUE, hover = TRUE, spacing = 's', rownames = FALSE)

  completion_events <- reactive({
    response_metric_completion_events(active_completed(),
      allow_mock = identical(state$mode, 'developer'))
  })
  output$milestone_table <- renderTable({
    # Only milestones of the selected metric: scene arrivals for call to scene,
    # hospital arrivals for transport to hospital.
    d <- completion_events()
    d <- d[d$metric_id == input$metric, , drop = FALSE]
    if (!nrow(d)) return(data.frame(Status = sprintf(
      'No %s milestones have been completed yet.', tolower(metric_info()$short_label))))
    d <- d[order(d$completed_at, decreasing = TRUE), , drop = FALSE]
    shown <- data.frame(
      Incident = d$incident_id,
      Milestone = ifelse(d$metric_id == 'call_to_scene',
        'Arrived at destination / scene', 'Arrived at hospital'),
      Zone = d$zone,
      Responder = ifelse(is.na(d$responder_id) | !nzchar(d$responder_id), '—', d$responder_id),
      `Duration (minutes)` = sprintf('%.1f', d$duration_minutes),
      Completed = format(d$completed_at, '%b %d %H:%M:%S', tz = 'America/New_York'),
      check.names = FALSE)
    head(shown, 8L)
  }, striped = TRUE, hover = TRUE, spacing = 's', rownames = FALSE)

  selected_signal <- reactive({
    flags <- visible_flags()
    if (!nrow(flags)) return(flags)
    index <- match(selected_flag_id(), flags$incident_id)
    if (length(index) != 1L || is.na(index)) return(flags[FALSE, , drop = FALSE])
    flags[index, , drop = FALSE]
  })

  output$flag_cards <- renderUI({
    g <- selected_group()
    flags <- visible_flags()
    if (!isTRUE(g$flags_enabled))
      return(div(class = 'flag-empty', 'Signals are unavailable until the baseline review is documented.'))
    if (!nrow(flags))
      return(div(class = 'flag-empty', 'No control-chart signals among completed metric records in this view. This does not prove service is acceptable.'))
    flags <- flags[order(flags$dispatch_time, decreasing = TRUE), , drop = FALSE]
    current <- selected_signal()$incident_id
    cards <- lapply(seq_len(nrow(flags)), function(i) {
      row <- flags[i, , drop = FALSE]
      kind <- if (isTRUE(row$individual_signal)) 'Long duration' else 'Abrupt variation'
      tags$button(type = 'button',
        class = paste('flag-card', if (identical(row$incident_id, current)) 'is-selected'),
        onclick = sprintf('Shiny.setInputValue(%s,%s,{priority:"event"})',
          jsonlite::toJSON(session$ns('selected_flag'), auto_unbox = TRUE),
          jsonlite::toJSON(as.character(row$incident_id), auto_unbox = TRUE)),
        tags$strong(sprintf('%s · %.1f min', kind, row$response_minutes)),
        tags$small(sprintf('%s · %s', row$incident_id,
          format(row$dispatch_time, '%b %d %H:%M', tz = 'America/New_York'))))
    })
    div(class = 'flag-list', cards)
  })

  output$flag_detail <- renderUI({
    row <- selected_signal()
    if (!nrow(row)) return(NULL)
    g <- selected_group()
    long <- isTRUE(row$individual_signal)
    variation <- isTRUE(row$variation_signal)
    div(class = 'flag-detail',
      div(class = 'kpi-label', 'Selected signal / Evidence'),
      h4(row$incident_id),
      p(sprintf('%s %.2f min · fixed I UCL %.2f min · moving range %.2f min · fixed MR UCL %.2f min.',
        metric_info()$short_label,
        row$response_minutes, g$limits$i_ucl, row$moving_range, g$limits$mr_ucl)),
      p(sprintf('Metric started %s · %s · %s priority. Baseline: 2025-01-01 to 2025-12-31.',
        format(row$dispatch_time, '%Y-%m-%d %H:%M:%S %Z', tz = 'America/New_York'),
        g$zone, g$call_priority)),
      p(strong('Interpretation. '),
        if (long && variation) 'This completed response exceeds both the Individuals and moving-range upper control limits.'
        else if (long) 'This completed duration is unusually long relative to this metric group’s fixed baseline.'
        else 'The change from the preceding completed duration exceeds the moving-range upper control limit.'),
      p(strong('Supervisor check. '),
        'Verify timestamps and assignment details, then review dispatch, travel, weather and staffing context before documenting a cause or intervention.'),
      tags$small('A control-chart signal requests investigation; it is not a service target or proof of fault.'),
      actionButton(session$ns('follow_live'), 'Return to latest metric records', class = 'btn-default'))
  })

  output$i_chart <- renderPlot({
    g <- selected_group(); b <- g$baseline; m <- visible_monitoring()
    focus <- selected_signal()
    latest_id <- if (nrow(m)) m$incident_id[which.max(m$on_scene_time)] else NULL
    m <- supervisor_chart_window(m, selected_flag_id())
    mplot <- data.frame(sequence = m$monitoring_sequence, value = m$response_minutes,
      signal = !is.na(m$individual_signal) & m$individual_signal,
      selected = m$incident_id %in% focus$incident_id,
      new = m$incident_id %in% latest_id)
    segments <- supervisor_consecutive_segments(m, 'response_minutes')
    left <- if (nrow(m)) max(0.5, min(m$monitoring_sequence) - 0.5) else 0.5
    right <- if (nrow(m)) max(left + 24, max(m$monitoring_sequence) + 1.5) else 24.5
    ggplot() +
      geom_segment(data = segments, aes(x = x, xend = xend, y = y, yend = yend),
        color = '#5B6B7A', linewidth = .8) +
      geom_point(data = mplot, aes(sequence, value, color = signal), size = 2.7) +
      geom_point(data = mplot[mplot$new, , drop = FALSE], aes(sequence, value),
        shape = 21, size = 6.5, stroke = 1.4, fill = NA, color = '#2C3E50') +
      geom_point(data = mplot[mplot$selected, , drop = FALSE], aes(sequence, value),
        shape = 21, size = 8, stroke = 1.3, fill = NA, color = '#1F2D3B') +
      geom_hline(yintercept = g$limits$center, color = '#2C3E50', linewidth = .75) +
      geom_hline(yintercept = c(g$limits$i_lcl, g$limits$i_ucl),
        color = '#D15B4A', linetype = 'dashed', linewidth = .65) +
      scale_color_manual(values = c(`FALSE` = '#5B6B7A', `TRUE` = '#D15B4A'), guide = 'none') +
      coord_cartesian(xlim = c(left, right)) +
      labs(x = 'Monitoring incident #',
        y = 'Duration (minutes)',
        title = sprintf('%s · %s priority · %s', g$zone, g$call_priority,
          metric_info()$short_label),
        subtitle = sprintf('Baseline n=%d · visible n=%d',
          nrow(b), nrow(visible_monitoring())),
        caption = 'Ring: latest selected-group metric record · larger ring: selected signal') +
      theme_minimal(base_size = 11)
  })

  output$mr_chart <- renderPlot({
    g <- selected_group(); m <- visible_monitoring()
    focus <- selected_signal()
    latest_id <- if (nrow(m)) m$incident_id[which.max(m$on_scene_time)] else NULL
    m <- supervisor_chart_window(m, selected_flag_id())
    mmr <- data.frame(sequence = m$monitoring_sequence, value = m$moving_range,
      signal = !is.na(m$variation_signal) & m$variation_signal,
      selected = m$incident_id %in% focus$incident_id,
      new = m$incident_id %in% latest_id)
    segments <- supervisor_consecutive_segments(m, 'moving_range')
    left <- if (nrow(m)) max(0.5, min(m$monitoring_sequence) - 0.5) else 0.5
    right <- if (nrow(m)) max(left + 24, max(m$monitoring_sequence) + 1.5) else 24.5
    ggplot() +
      geom_segment(data = segments, aes(x = x, xend = xend, y = y, yend = yend),
        color = '#5B6B7A', linewidth = .8, na.rm = TRUE) +
      geom_point(data = mmr, aes(sequence, value, color = signal), size = 2.4) +
      geom_point(data = mmr[mmr$new, , drop = FALSE], aes(sequence, value),
        shape = 21, size = 6.5, stroke = 1.4, fill = NA, color = '#2C3E50') +
      geom_point(data = mmr[mmr$selected, , drop = FALSE], aes(sequence, value),
        shape = 21, size = 8, stroke = 1.3, fill = NA, color = '#1F2D3B') +
      geom_hline(yintercept = g$limits$mr_center, color = '#2C3E50', linewidth = .75) +
      geom_hline(yintercept = g$limits$mr_ucl,
        color = '#D15B4A', linetype = 'dashed', linewidth = .65) +
      scale_color_manual(values = c(`FALSE` = '#5B6B7A', `TRUE` = '#D15B4A'), guide = 'none') +
      coord_cartesian(xlim = c(left, right)) +
      labs(x = 'Monitoring incident #', y = 'Moving range (minutes)',
        subtitle = sprintf('MR UCL %.2f min · visible n=%d',
          g$limits$mr_ucl, nrow(visible_monitoring()))) +
      theme_minimal(base_size = 11)
  })

  descriptive <- reactive({
    window <- display_window()
    dashboard_descriptive_records(analysis(), window[1L], window[2L])
  })

  box_plot <- function(d, group_col, groups, title) {
    boxes <- qc_boxes(d, group_col)
    frame <- do.call(rbind, lapply(groups, function(name) {
      b <- boxes[[name]]
      data.frame(group = name, n = b$n, q1 = b$q1, median = b$median, q3 = b$q3,
        low = b$lower_whisker, high = b$upper_whisker)
    }))
    frame$x <- seq_len(nrow(frame))
    pts <- do.call(rbind, lapply(seq_along(groups), function(i) {
      values <- boxes[[groups[i]]]$points
      if (!length(values)) return(NULL)
      data.frame(x = i, value = values)
    }))
    if (is.null(pts)) pts <- data.frame(x = numeric(), value = numeric())
    ggplot() +
      geom_jitter(data = pts, aes(x, value), width = .12, color = '#2C3E50', alpha = .2, size = 1.2) +
      geom_segment(data = frame, aes(x = x, xend = x, y = low, yend = high), linewidth = .7, na.rm = TRUE) +
      geom_rect(data = frame, aes(xmin = x - .27, xmax = x + .27, ymin = q1, ymax = q3),
        fill = '#AFC6D9', color = '#2C3E50', linewidth = .7, na.rm = TRUE) +
      geom_segment(data = frame, aes(x = x - .27, xend = x + .27, y = median, yend = median),
        color = '#D15B4A', linewidth = 1, na.rm = TRUE) +
      scale_x_continuous(breaks = frame$x, labels = sprintf('%s\n(n=%d)', frame$group, frame$n)) +
      labs(x = NULL, y = 'Duration (minutes)', title = title) +
      theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 18, hjust = 1))
  }

  output$zone_boxplot <- renderPlot({
    d <- descriptive(); d <- d[d$call_priority == input$priority, , drop = FALSE]
    box_plot(d, 'zone', qc_zones,
      sprintf('%s priority · %s · %d completed incidents', input$priority,
        metric_info()$short_label, nrow(d)))
  })
  output$weather_boxplot <- renderPlot({
    d <- descriptive(); d <- d[d$zone == input$zone & d$call_priority == input$priority, , drop = FALSE]
    box_plot(d, 'weather_condition', qc_weather,
      sprintf('%s / %s · %s · %d completed incidents', input$zone,
        input$priority, metric_info()$short_label, nrow(d)))
  })
  output$heatmap <- renderPlot({
    d <- descriptive(); d <- d[d$zone == input$zone & d$call_priority == input$priority, , drop = FALSE]
    grid <- qc_heatmap(d)
    grid$weekday <- factor(grid$weekday, levels = 7:1,
      labels = c('Sun', 'Sat', 'Fri', 'Thu', 'Wed', 'Tue', 'Mon'))
    grid$label <- ifelse(grid$n == 0, '', sprintf('%.1f\nn=%d', grid$median, grid$n))
    ggplot(grid, aes(hour, weekday, fill = median)) +
      geom_tile(color = 'white', linewidth = .25) +
      geom_text(aes(label = label), size = 2.2, color = '#1F2D3B') +
      scale_fill_gradient(low = '#EFEAE2', high = '#6F93B4', na.value = '#FFFFFF',
        name = 'Median\nminutes') +
      scale_x_continuous(breaks = seq(0, 23, 3)) +
      labs(x = 'Assignment hour (America/New_York)', y = NULL,
        title = sprintf('%s / %s · %s · n=%d', input$zone, input$priority,
          metric_info()$short_label, nrow(d))) +
      theme_minimal(base_size = 10) + theme(panel.grid = element_blank())
  })

  output$validation_table <- renderTable({
    source_records <- active_source_records()
    req(source_records)
    metric_records <- response_metric_to_qc(source_records, input$metric,
      allow_mock = identical(state$mode, 'developer'))
    v <- qc_validate(metric_records, mode = 'completed')
    data.frame(
      Measure = c('Source rows', 'Accepted rows', 'Completed rows', 'Rejected rows',
        'Weather values normalized'),
      Count = c(nrow(source_records), unname(v$counts['accepted']),
        unname(v$counts['completed']), unname(v$counts['rejected']),
        unname(v$counts['weather_normalized']))
    )
  }, striped = TRUE, spacing = 's', rownames = FALSE)
  output$limits_table <- renderTable({
    g <- selected_group()
    data.frame(
      Measure = c('Baseline valid incidents', 'Baseline mean (minutes)',
        'Mean moving range (minutes)', 'Individuals LCL (minutes)',
        'Individuals UCL (minutes)', 'Moving-range UCL (minutes)', 'QC status'),
      Value = c(g$limits$n, fmt_num(g$limits$center, 3), fmt_num(g$limits$mr_center, 3),
        fmt_num(g$limits$i_lcl, 3), fmt_num(g$limits$i_ucl, 3),
        fmt_num(g$limits$mr_ucl, 3), g$status)
    )
  }, striped = TRUE, spacing = 's', rownames = FALSE)
  })
}
