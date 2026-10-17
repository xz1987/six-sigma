library(ggplot2)

find_repo_root <- function() {
  candidates <- unique(normalizePath(c(getwd(), file.path(getwd(), '..')),
    mustWork = FALSE))
  hit <- candidates[file.exists(file.path(candidates, 'app', 'qc.R')) &
    dir.exists(file.path(candidates, 'data'))]
  if(length(hit)) return(hit[[1]])
  standalone <- candidates[file.exists(file.path(candidates,'qc.R')) & dir.exists(file.path(candidates,'data'))]
  if(length(standalone)) return(standalone[[1]])
  stop('App source and bundled data were not found.')
}

repo_root <- find_repo_root()
app_source_dir <- if(file.exists(file.path(repo_root,'app','qc.R'))) file.path(repo_root,'app') else repo_root
source(file.path(app_source_dir, 'qc.R'), local = TRUE)
source(file.path(app_source_dir, 'response-metrics.R'), local = TRUE)
source(file.path(app_source_dir, 'feed.R'), local = TRUE)
source(file.path(app_source_dir, 'workflow.R'), local = TRUE)
source(file.path(app_source_dir, 'supervisor-analytics.R'), local = TRUE)

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

chart_help <- function(...) {
  tags$details(class = 'chart-reading',
    tags$summary(
      tags$span('How to read this chart'),
      tags$span(class = 'chart-reading-mark', `aria-hidden` = 'true')),
    ...)
}

reading_key <- function(...) {
  div(class = 'reading-key', ...)
}

key_item <- function(label, kind) {
  span(class = 'reading-key-item',
    span(class = paste('reading-key-symbol', kind), `aria-hidden` = 'true'),
    span(label))
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

supervisor_developer_responders <- 'EMS-101'

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
                                     responder = NULL, seed = 20261016L, baseline_moments = NULL) {
  scene <- if(is.null(baseline_moments)) supervisor_baseline_moments(historical, 'call_to_scene', zone, priority) else baseline_moments$scene
  transport <- if(is.null(baseline_moments)) supervisor_baseline_moments(historical, 'transport_to_hospital', zone, priority) else baseline_moments$transport
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

# One synthetic working day, revealed case by case in Active tracking.
supervisor_simulate_day <- function(historical, zone, priority, metric_id,
                                     abnormal=FALSE, as_of=Sys.time()) {
  days <- as.Date(as_of,tz='America/New_York')
  info <- response_metric_info(metric_id)
  moments <- list(scene=supervisor_baseline_moments(historical,'call_to_scene',zone,priority),
    transport=supervisor_baseline_moments(historical,'transport_to_hospital',zone,priority))
  rows <- lapply(seq_along(days),function(k) do.call(rbind,lapply(seq_len(12),function(j) {
    start <- as.POSIXct(paste(days[k],'08:00:00'),tz='America/New_York')+(j-1)*60*60
    call <- supervisor_simulate_call(historical,zone,priority,(k-1L)*12L+j,start,responder='EMS-101',baseline_moments=moments)
    call$incident_id <- sprintf('DAY-%s-%02d',format(days[k],'%Y%m%d'),j)
    call$response_system_source <- 'Synthetic active-day demonstration'
    if(isTRUE(abnormal) && j>=10L) {
      old_transport <- as.numeric(difftime(qc_parse_time(call$hospital_arrival_time),qc_parse_time(call$transport_departure_time),units='mins'))
      end <- qc_parse_time(call[[info$start_field]])+30*60
      stamp <- function(t)format(t,'%Y-%m-%dT%H:%M:%S%z',tz='America/New_York')
      call[[info$end_field]] <- stamp(end)
      if(metric_id=='call_to_scene') {
        call$transport_departure_time <- stamp(end+10*60)
        call$hospital_arrival_time <- stamp(end+10*60+old_transport*60)
      }
      call$response_system_source <- 'Synthetic active-day demonstration: planted 30-minute delay'
    }
    call
  })))
  do.call(rbind,rows)
}

# Presentation metadata only: no measurements or statistical formulas change.
# Interpret existing results; do not fit limits or infer a cause.
supervisor_plain_summary <- function(m, flags_enabled, status, target_misses, service) {
  n <- nrow(m)
  if (!n) return(list(history='No completed records in this view yet. Pending trips are not zero-minute responses.',
    target='Target performance is not available until records complete.',
    goal='Configured service-goal performance is not available yet.',
    action='Wait for completed records or choose another display period.'))
  history <- if (!isTRUE(flags_enabled)) paste('Historical signals unavailable:',status,
    '— do not interpret this as zero signals.') else sprintf(
      '%d of %d completed records exceed the historical upper duration limit; %d show an unusually large change from the preceding record.',
      sum(m$individual_signal %in% TRUE),n,sum(m$variation_signal %in% TRUE))
  known <- sum(!is.na(target_misses))
  target <- if (!known) 'Context target unavailable for these records.' else sprintf(
    '%d of %d records with an available context target missed it. A target miss can overlap a historical signal.',
    sum(target_misses %in% TRUE),known)
  goal <- if (!nrow(service) || !is.finite(service$goal[1]))
    'No service goal is configured for this metric / zone / priority.' else sprintf(
      '%.0f%% of %d completed records met the configured %.1f-minute service goal.',
      100*service$within_goal[1],service$n[1],service$goal[1])
  list(history=history,target=target,goal=goal,
    action='Review the signal details and verify timestamps and recorded circumstances. Signals do not establish a cause; no signal does not prove acceptable service. Display filters never change the fixed baseline limits.')
}

supervisor_history_summary <- function(d, service, aggregation, enabled, status, open_count) {
  unit <- if (identical(aggregation,'week')) 'weekly' else 'daily'
  known <- if(nrow(d)) sum(!is.na(d$signal)) else 0L
  history <- if(!nrow(d)) 'No completed subgroups are available for this selection.' else
    if(!isTRUE(enabled) || !known) paste('Subgroup limit comparisons unavailable:',status,
      '— this is not evidence of zero signals.') else sprintf(
      '%d of %d assessable %s means fall outside the baseline-derived control limits. These are period averages, not counts of unusually slow individual responses.',
      sum(d$signal %in% TRUE),known,unit)
  goal <- if(!nrow(service) || service$n[1]==0) 'No completed individual responses are available for service-goal comparison.' else
    if(!is.finite(service$goal[1])) sprintf('Individual-response p90 is %.1f minutes across %d responses; no service goal is configured.',service$p90[1],service$n[1]) else sprintf(
      'Individual-response p90 is %.1f minutes; %.0f%% of %d responses met the configured %.1f-minute goal. This goal is separate from control limits.',
      service$p90[1],100*service$within_goal[1],service$n[1],service$goal[1])
  list(history=history,goal=goal,coverage=sprintf(
    '%d completed %s subgroups are displayed. %d current-day responses are excluded from closed-period history. The distribution chart summarizes daily means even when the control chart uses weekly means.',
    nrow(d),unit,open_count),
    action='Investigate unusual periods using incident details in Active tracking. Baseline-period comparisons describe the reference data, not prospective monitoring. Adjusted comparisons show associations, not causes; no signal does not prove acceptable service. Historical baseline limits remain fixed.')
}

supervisor_patterns_summary <- function(records, zone, priority) {
  all_zones <- records[records$call_priority == priority,,drop=FALSE]
  selected <- all_zones[all_zones$zone == zone,,drop=FALSE]
  describe <- function(boxes) {
    present <- boxes[vapply(boxes,function(b)b$n > 0,logical(1))]
    if(!length(present)) return('No completed records for this selection.')
    paste(vapply(names(present),function(name) sprintf('%s: median %.1f min (n=%d)',
      name,present[[name]]$median,present[[name]]$n),character(1)),collapse='; ')
  }
  grid <- qc_heatmap(selected)
  populated <- grid[grid$n > 0,,drop=FALSE]
  time <- if(!nrow(populated)) 'No populated hour/weekday cells for this zone and priority.' else sprintf(
    '%d of 168 hour/weekday cells contain records; observed cell medians range from %.1f to %.1f minutes. Read each cell with its sample count; blank cells mean no data.',
    nrow(populated),min(populated$median),max(populated$median))
  list(zones=describe(qc_boxes(all_zones,'zone')),
    weather=describe(qc_boxes(selected,'weather_condition')),time=time,
    scope=sprintf('Neighborhood chart: %d completed records across all zones at %s priority. Weather and time charts: %d records in %s / %s.',nrow(all_zones),priority,nrow(selected),zone,priority),
    action='Use these patterns to choose incidents or periods to investigate in Active tracking. Small samples can give unstable comparisons. These are unadjusted descriptions, not proof of a weather effect, a zone ranking, or new control-chart signals. Changing these views never refits baseline limits.')
}

supervisor_chart_guides <- function() {
  key <- function(symbol,label,color='#2C3E50') c(symbol=symbol,label=label,color=color)
  box <- list(text='Compare completed durations across categories. The box spans the middle 50%; its line is the median. Whiskers extend to the most extreme values within 1.5 box lengths. Dots are individual records. Counts show sample size; comparisons are descriptive, not adjusted rankings or causes.',
    legend=list(key('▰','Middle 50%','#AFC6D9'),key('━','Median','#D15B4A'),key('│','Whiskers'),key('●','Individual record'),key('n','Sample count')))
  list(
    i_chart=list(text='Each point is one completed selected-metric duration in minutes, ordered within the zone/priority group. Red can mean an SPC rule or contextual-target miss; open its signal details to see which. Control limits describe historical variation, not a response-time promise. Green targets are separate from control limits. Only the latest 24 visible records are plotted unless a signal is selected.',
      legend=list(key('●','Completed duration','#5B6B7A'),key('●','SPC or target signal','#D15B4A'),key('━','Baseline mean'),key('┄','Control limits','#D15B4A'),key('┄','Dot-dash: contextual target','#6B8F7A'),key('◯','Latest record; larger ring = selected signal'))),
    mr_chart=list(text='Each point is the absolute change in minutes from the preceding completed duration in the same metric/zone/priority sequence. A large change signals variation, not necessarily a long response. The predecessor can be outside your displayed date filter. Limits stay fixed when filters change.',
      legend=list(key('●','Consecutive change','#5B6B7A'),key('●','Above MR upper limit','#D15B4A'),key('━','Baseline mean moving range'),key('┄','MR upper limit','#D15B4A'),key('◯','Latest record; larger ring = selected signal'))),
    zone_boxplot=box, weather_boxplot=box,
    heatmap=list(text='Columns are the selected metric’s start hour and rows are weekdays in New York time. Each cell shows median minutes and n, the number of completed records. Darker cells mean a higher median. Blank cells have no data; they do not mean zero minutes. These patterns do not identify causes.',
      legend=list(key('▰','Lighter = lower median','#EFEAE2'),key('▰','Darker = higher median','#6F93B4'),key('□','Blank = no records','#FFFFFF'),key('n','Records in cell'))),
    history_control_chart=list(text='Each point is the mean duration for one completed day or week, not one incident. The blue band shows baseline-based three-sigma limits adjusted for that period’s sample count; larger samples have narrower limits. The horizontal line is the fixed baseline center. Today’s open day is excluded. No point means no completed subgroup, not zero performance.',
      legend=list(key('●','Within subgroup limits','#6B8F7A'),key('●','Outside subgroup limits','#D15B4A'),key('▰','Variable control-limit band','#AFC6D9'),key('━','Baseline center'),key('●','Circle = 2025 baseline','#6F93B4'),key('▲','Triangle = closed monitoring','#6F93B4'),key('n','Larger point = more records'))),
    history_distribution_chart=list(text='Bars count how many completed daily/weekly subgroup means fall in each duration interval. Horizontal units are minutes; vertical units are subgroups. The baseline-center line is a reference, not a control limit. A configured service-goal line is a management threshold, not an SPC limit.',
      legend=list(key('▰','Count of subgroup means','#AFC6D9'),key('│','Baseline center'),key('┆','Configured service goal, when present','#E6A700'))),
    history_monthly_chart=list(text='Each monthly box summarizes completed daily means in minutes. It does not show individual incident durations. Boxes show the middle half; the line inside is the median. Red points are boxplot outliers, not automatically control-chart signals. The dashed horizontal reference is the baseline center.',
      legend=list(key('▰','Middle 50% of daily means','#E6EEF5'),key('━','Median; whiskers show spread'),key('●','Boxplot outlier','#D15B4A'),key('┄','Baseline center','#6B8F7A'))),
    service_goal_plot=list(text='Each bar is a zone/priority group’s observed 90th-percentile duration in minutes. The cross marks its configured service goal. Green means p90 is at or below the goal; red means above. Labels give the percent of incidents within the goal and n. Service goals are separate from statistical control limits. No observations cannot establish performance.',
      legend=list(key('▰','p90 meets goal','#6B8F7A'),key('▰','p90 misses goal','#D15B4A'),key('▰','Goal not configured','#AFC6D9'),key('×','Configured goal'),key('n','Completed records'))),
    adjusted_zone_plot=list(text='Dots are model-adjusted geometric means in minutes, standardized to the synthetic reference mix of weather and time. Horizontal bars are 95% model intervals; point size shows reference sample count. These are model-based associations, not causal effects or a validated ranking.',
      legend=list(key('●','Adjusted geometric mean','#D15B4A'),key('━','95% model interval','#5B6B7A'),key('n','Larger point = more reference records'))),
    adjusted_priority_plot=list(text='Dots compare model-adjusted geometric mean minutes across priorities using the synthetic reference mix. Bars are 95% model intervals and size shows reference count. Adjusting for recorded predictors does not remove unmeasured differences or establish causation.',
      legend=list(key('●','Adjusted geometric mean','#D15B4A'),key('━','95% model interval','#5B6B7A'),key('n','Larger point = more reference records')))
  )
}

supervisor_chart <- function(ns,id,height=345) {
  guide <- supervisor_chart_guides()[[id]]
  stopifnot(!is.null(guide))
  shiny::tagList(
    shiny::tags$details(class='supervisor-chart-guide',
      shiny::tags$summary('How to read this chart'), shiny::p(guide$text)),
    shiny::div(class='supervisor-chart-legend',role='list',`aria-label`='Chart legend',
      lapply(guide$legend,function(item) shiny::span(role='listitem',
        shiny::span(class='chart-key-symbol',style=paste0('color:',item[['color']]),`aria-hidden`='true',item[['symbol']]),
        item[['label']]))),
    shiny::plotOutput(ns(id),height=height))
}

supervisor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(src = 'history-tour.js?v=1', defer = NA),
    div(class = 'supervisor-pwa',
      div(id = 'connection-status', class = 'alert-note danger', role = 'status', hidden = NA,
        "You're offline. Reconnect to use login, incident records, and response updates."),
      div(class = 'pwa-tools',
        tags$button(id = 'supervisor-install-app', type = 'button', class = 'btn btn-primary',
          'Install app'),
        tags$details(id = 'supervisor-install-help',
          tags$summary('Installation help'),
          p("iPhone/iPad: use the browser Share menu → Add to Home Screen. Safari on Mac: File → Add to Dock. Chrome/Edge: use the install option in the address bar or browser menu."),
          p('Live features require a connection. This remains a synthetic hackathon demonstration.')),
        tags$span(id = 'supervisor-pwa-status', role = 'status'))),
    div(class = 'app-shell',
    tags$aside(class = 'control-rail',
      div(class = 'rail-heading', div(class = 'kpi-label', 'Supervisor workspace'), h2('Response quality')),
      div(class = 'rail-note',
        strong('Response-time definition'), br(),
        textOutput(ns('metric_definition'), inline = TRUE), br(),
        tags$small('Each metric has its own fixed baseline limits. Display filters never refit them.')),
      uiOutput(ns('workspace_nav')),
      tags$details(id = ns('developer_panel'), class = 'developer-settings',
        ontoggle = sprintf("Shiny.setInputValue('%s', this.open, {priority: 'event'});",
          ns('developer_panel_open')),
        tags$summary(title = 'Developer settings',
          onclick = sprintf("if (this.parentElement.open) Shiny.setInputValue('%s', Date.now(), {priority: 'event'});",
            ns('developer_panel_closed')),
          tags$span('Advanced demo settings')),
        uiOutput(ns('developer_controls')))
    ),
    tags$main(class = 'workspace',
      conditionalPanel(sprintf("input['%s'] !== 'History analysis'", ns('main_tabs')),
        div(class = 'supervisor-filter-bar',
        div(class = 'filter-field', selectInput(ns('time_span'), 'Time span',
          choices = c('Day' = 'day', 'Week' = 'week', 'Month' = 'month'), selected = 'day')),
        conditionalPanel(sprintf("input['%s'] === 'day'", ns('time_span')), class = 'filter-field',
          dateInput(ns('day_date'), 'Date', value = supervisor_today(),
            min = as.Date('2026-01-01'), max = as.Date('2026-12-31'))),
        conditionalPanel(sprintf("input['%s'] === 'week'", ns('time_span')), class = 'filter-field',
          selectInput(ns('week_start'), 'Week', choices = supervisor_week_choices(), selected = '2026-01-26')),
        conditionalPanel(sprintf("input['%s'] === 'month'", ns('time_span')), class = 'filter-field',
          selectInput(ns('month_start'), 'Month', choices = supervisor_month_choices(), selected = '2026-02-01')),
        div(class = 'filter-field filter-field-wide', selectInput(ns('metric'), 'Response metric',
          choices = setNames(response_metric_catalog()$id, response_metric_catalog()$label),
          selected = 'call_to_scene')),
        div(class = 'filter-field', selectInput(ns('zone'), 'Incident zone', choices = qc_zones,
          selected = 'Far Rockaway')),
        div(class = 'filter-field', selectInput(ns('priority'), 'Priority · QC group', choices = qc_priorities)),
        actionButton(ns('clear_filters'), 'Reset', class = 'btn-default filter-reset'))),
      uiOutput(ns('developer_bar')),
      div(class = 'dashboard-heading',
        div(div(class = 'kpi-label', 'Response quality / Supervisor'),
          h1(textOutput(ns('dashboard_title'), inline = TRUE)),
          p(textOutput(ns('dashboard_subtitle'), inline = TRUE))),
        uiOutput(ns('mode_chip'))),
      conditionalPanel(sprintf("input['%s'] !== 'History analysis'", ns('main_tabs')),
        div(class = 'scope-strip',
          textOutput(ns('scope_label'), inline = TRUE),
          div(class = 'status-pill', strong('QC status: '), textOutput(ns('qc_status'), inline = TRUE))),
        uiOutput(ns('source_notice')),
        div(class = 'replay-timeline',
          div(class = 'timeline-clock', span(class = 'timeline-pulse'),
            strong(textOutput(ns('time_context_label'), inline = TRUE)),
            textOutput(ns('overview_sim_time'), inline = TRUE)),
          textOutput(ns('latest_arrival'), inline = TRUE))),
      uiOutput(ns('shared_trip')),
      tabsetPanel(id = ns('main_tabs'), type = 'hidden', selected = 'Active tracking',
        tabPanel('Active tracking',
          div(class = 'kpi-grid',
            kpi('Completed records', 'kpi_completed', 'for selected metric / group', ns),
            kpi('Median duration', 'kpi_median', 'minutes', ns),
            kpi('Context target', 'kpi_target', 'minutes', ns),
            kpi('Performance signals', 'kpi_flags', 'SPC + target misses', ns)),
          uiOutput(ns('target_notice')),
          uiOutput(ns('review_notice')),
          uiOutput(ns('data_quality_panel')),
          panel_card('Latest completed response milestones',
            'Completion records for the selected response metric only: scene arrivals for call to scene, hospital arrivals for transport to hospital.',
            tableOutput(ns('milestone_table'))),
          div(class = 'analysis-layout',
            div(class = 'chart-stack',
              panel_card('Individuals control chart',
                'New completed metric records appear one by one in metric-start order. The chart shows the latest 24 visible incidents; dashed lines are fixed 2025 baseline limits.',
                supervisor_chart(ns, 'i_chart', height = 345)),
              panel_card('Moving-range control chart',
                'Each range uses its true predecessor in the full zone/priority sequence, even when this view hides other points.',
                supervisor_chart(ns, 'mr_chart', height = 265))),
            div(class = 'flag-panel panel-card',
              div(class = 'flag-heading', h3('Signals to investigate'),
                span(class = 'flag-count', textOutput(ns('flag_count'), inline = TRUE))),
              tags$p(class = 'subhead', 'Select a signal to locate it on the charts and review the evidence.'),
              uiOutput(ns('flag_cards')),
              uiOutput(ns('investigation_drawer')))),
          uiOutput(ns('findings_summary')),
          panel_card('Completed metric records in this view',
            'The display filters change which completed metric records you see, not the baseline or control limits.',
            tableOutput(ns('completed_table')))
        ),
        tabPanel('Patterns',
          panel_card('Neighborhood distribution',
            'Selected time span and priority. Type-7 quartiles and 1.5×IQR whiskers; descriptive only.',
            supervisor_chart(ns, 'zone_boxplot', height = 380)),
          div(class = 'two-col',
            panel_card('Metric start hour × weekday',
              'Median response minutes in America/New_York; labels show median and sample count. Blank means no data.',
              supervisor_chart(ns, 'heatmap', height = 420)),
            panel_card('Weather context',
              'Synthetic assignment-time weather. Descriptive only; weather does not change QC limits or establish cause.',
              supervisor_chart(ns, 'weather_boxplot', height = 420))),
          uiOutput(ns('patterns_findings_summary'))
        ),
        tabPanel('History analysis',
          div(class = 'history-filter-bar',
            div(class = 'filter-field filter-field-wide', selectInput(ns('history_metric'), 'Response metric',
              choices = setNames(response_metric_catalog()$id, response_metric_catalog()$label),
              selected = 'call_to_scene')),
            div(class = 'filter-field', selectInput(ns('history_zone'), 'Incident zone',
              choices = qc_zones, selected = 'Far Rockaway')),
            div(class = 'filter-field', selectInput(ns('history_priority'), 'Priority',
              choices = qc_priorities, selected = 'High')),
            div(class = 'filter-field', selectInput(ns('history_period'), 'History period',
              choices = c('Latest 7 calendar days' = '7', 'Latest 14 calendar days' = '14',
                'Latest 30 calendar days' = '30', 'Latest 90 calendar days' = '90',
                'Full baseline year' = 'full'), selected = 'full')),
            div(class = 'filter-field', selectInput(ns('history_aggregation'), 'Subgroup interval',
              choices = c('Automatic' = 'auto', 'Daily' = 'day', 'Weekly' = 'week'),
              selected = 'auto')),
            div(class = 'filter-field', selectInput(ns('history_phase'), 'Records',
              choices = c('Baseline + monitoring' = 'both', 'Baseline only' = 'baseline',
                'Closed monitoring only' = 'monitoring'), selected = 'both')),
            actionButton(ns('reset_history'), 'Reset', class = 'btn-default filter-reset')),
          div(class = 'history-tour-launch',
            actionButton(ns('restart_history_tour'), 'Show page tutorial',
              icon = icon('circle-question'), class = 'btn-default btn-sm')),
          uiOutput(ns('subgroup_status')),
          uiOutput(ns('history_chart_layout')),
          div(class = 'service-goal-panel', panel_card('Selected-scope service performance',
            'One clear comparison for the metric, zone, priority and period selected above.',
            uiOutput(ns('service_goal_status')),
            plotOutput(ns('service_goal_plot'), height = 230),
            chart_help(
              reading_key(
                key_item('Observed p90', 'key-bar key-meeting'),
                key_item('Configured service goal', 'key-goal')),
              uiOutput(ns('service_goal_reading'))),
            tags$details(class = 'service-goal-settings',
              tags$summary('Configure service goals'),
              div(class = 'service-goal-controls',
                selectInput(ns('goal_priority'), 'Priority', choices = qc_priorities),
                selectInput(ns('goal_zone'), 'Scope', choices = c('All zones', qc_zones)),
                numericInput(ns('goal_minutes'), 'Goal (minutes)', value = 12, min = 1,
                  max = 120, step = .5),
                actionButton(ns('save_goal'), 'Save goal', class = 'btn-primary'),
                actionButton(ns('clear_goal'), 'Clear goal', class = 'btn-default')),
              uiOutput(ns('service_goal_summary'))))),
          div(class = 'two-col history-comparisons',
            panel_card('Weather-adjusted zone comparison',
              'Marginal model estimates hold priority, weather, hour and weekday at the synthetic reference mix.',
              plotOutput(ns('adjusted_zone_plot'), height = 360),
              chart_help(
                reading_key(
                  key_item('Adjusted mean', 'key-dot'),
                  key_item('95% model interval', 'key-interval')),
                p('Each point is an adjusted geometric mean. The horizontal line is its 95% model interval. Compare positions and interval overlap; do not treat the ranking as causal or as staff performance.'))),
            panel_card('Weather-adjusted priority comparison',
              'Marginal model estimates hold zone, weather, hour and weekday at the synthetic reference mix.',
              plotOutput(ns('adjusted_priority_plot'), height = 360),
              chart_help(
                reading_key(
                  key_item('Adjusted mean', 'key-dot'),
                  key_item('95% model interval', 'key-interval')),
                p('Each point is an adjusted geometric mean. The horizontal line is its 95% model interval. Differences are descriptive associations after adjustment for the available synthetic context—not causal effects.')))),
          uiOutput(ns('history_findings_summary'))
        )
      )
    )
  )
  )
}

supervisor_server <- function(id, production_records) {
  moduleServer(id, function(input, output, session) {
  baseline_records <- dashboard_load_fixture(repo_root, 'stable')
  state <- reactiveValues(mode = 'ordinary', developer_calls = NULL,
    daily_demo_as_of = NULL, demo_day_cases = NULL,
    baseline_records = baseline_records,
    ui_tick = 0L, completed_tick = 0L, last_step = NULL,
    service_goals = supervisor_suggested_service_goals(baseline_records),
    history_tour_seen = FALSE,
    developer_quality = data.frame(issue_id = character(), incident_id = character(),
      severity = character(), issue = character(), stringsAsFactors = FALSE))

  start_history_tour <- function() {
    session$sendCustomMessage('history-tour-start', list(
      completeInput = session$ns('history_tour_complete'),
      steps = list(
        list(selector = '.history-filter-bar', title = 'Choose a comparable scope',
          body = 'Try changing History period, metric, zone, or priority. The highlighted controls remain usable; these display choices never refit the frozen baseline.',
          action = 'Change a filter, then select Next.'),
        list(selector = '.history-chart-grid .panel-card:first-child',
          title = 'Check whether the process is stable',
          body = 'The page has scrolled to the subgroup control chart. Open How to read this chart, then inspect the grey 3σ band, center line, point sizes, and red signals.',
          action = 'Open the chart guide, then select Next.'),
        list(selector = '.service-goal-panel', title = 'Check whether service meets the goal',
          body = 'Review observed p90, the service goal, percent within goal, and completed-response count for the selected scope.',
          action = 'Explore the highlighted section, then select Next.'),
        list(selector = '.history-comparisons', title = 'Use context before taking action',
          body = 'These adjusted comparisons help form questions, but they do not prove a cause or assign staff fault.',
          action = 'Open a chart guide for interpretation, then finish the tutorial.'))))
  }

  set_period_for_mode <- function(day) {
    values <- supervisor_period_values(day)
    updateDateInput(session, 'day_date', value = values$day)
    updateSelectInput(session, 'week_start', selected = as.character(values$week))
    updateSelectInput(session, 'month_start', selected = as.character(values$month))
  }

  enter_developer_mode <- function() {
    state$daily_demo_as_of <- NULL
    state$demo_day_cases <- NULL
    state$mode <- 'developer'
    state$developer_calls <- response_metric_empty_records()
    state$last_step <- NULL
    state$developer_quality <- state$developer_quality[FALSE, , drop = FALSE]
    state$ui_tick <- state$ui_tick + 1L
    state$completed_tick <- state$completed_tick + 1L
    set_period_for_mode(supervisor_today())
  }

  leave_developer_mode <- function() {
    state$daily_demo_as_of <- NULL
    state$demo_day_cases <- NULL
    state$mode <- 'ordinary'
    state$developer_calls <- NULL
    state$last_step <- NULL
    state$developer_quality <- state$developer_quality[FALSE, , drop = FALSE]
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

  observeEvent(input$clear_filters, {
    updateSelectInput(session, 'time_span', selected = 'day')
    updateSelectInput(session, 'metric', selected = 'call_to_scene')
    updateSelectInput(session, 'zone', selected = 'Far Rockaway')
    updateSelectInput(session, 'priority', selected = 'High')
    set_period_for_mode(supervisor_today())
  })

  observeEvent(input$nav_active, updateTabsetPanel(session, 'main_tabs', selected = 'Active tracking'))
  observeEvent(input$nav_patterns, updateTabsetPanel(session, 'main_tabs', selected = 'Patterns'))
  observeEvent(input$nav_history, {
    updateTabsetPanel(session, 'main_tabs', selected = 'History analysis')
    if (!isTRUE(state$history_tour_seen)) start_history_tour()
  })
  observeEvent(input$restart_history_tour, start_history_tour())
  observeEvent(input$history_tour_complete, {
    state$history_tour_seen <- TRUE
  }, ignoreInit = TRUE)
  output$workspace_nav <- renderUI({
    selected <- input$main_tabs %||% 'Active tracking'
    nav_button <- function(id, label, icon_name, tab) actionButton(session$ns(id), label,
      icon = icon(icon_name), class = paste('rail-nav-button', if (identical(selected, tab)) 'is-active'))
    div(class = 'rail-nav',
      div(class = 'kpi-label', 'Workspace'),
      nav_button('nav_active', 'Active tracking', 'wave-square', 'Active tracking'),
      nav_button('nav_patterns', 'Patterns', 'chart-column', 'Patterns'),
      nav_button('nav_history', 'History analysis', 'clock-rotate-left', 'History analysis'))
  })

  observeEvent(input$reset_history, {
    updateSelectInput(session, 'history_metric', selected = 'call_to_scene')
    updateSelectInput(session, 'history_zone', selected = 'Far Rockaway')
    updateSelectInput(session, 'history_priority', selected = 'High')
    updateSelectInput(session, 'history_period', selected = 'full')
    updateSelectInput(session, 'history_aggregation', selected = 'auto')
    updateSelectInput(session, 'history_phase', selected = 'both')
  })

  observeEvent(input$save_goal, {
    req(input$history_metric, input$goal_priority, input$goal_zone, input$goal_minutes)
    state$service_goals <- supervisor_set_service_goal(state$service_goals,
      input$history_metric, input$goal_priority, input$goal_zone, input$goal_minutes)
  })
  observeEvent(input$clear_goal, {
    req(input$history_metric, input$goal_priority, input$goal_zone)
    state$service_goals <- supervisor_clear_service_goal(state$service_goals,
      input$history_metric, input$goal_priority, input$goal_zone)
  })

  developer_target <- reactive({
    if (identical(input$metric, 'transport_to_hospital')) input$developer_transport_target
    else input$developer_scene_target
  })

  add_developer_call <- function(event_type = 'stable') {
    req(identical(state$mode, 'developer'), input$zone, input$priority)
    calls <- state$developer_calls
    state$daily_demo_as_of <- NULL
    state$demo_day_cases <- NULL
    if (identical(event_type, 'data_quality')) {
      state$developer_quality <- rbind(state$developer_quality, data.frame(
        issue_id = sprintf('DEMO-DQ-%02d', nrow(state$developer_quality) + 1L),
        incident_id = sprintf('SIM-%04d', nrow(calls) + 1L), severity = 'Warning',
        issue = 'Demo-only missing weather context', stringsAsFactors = FALSE))
      state$ui_tick <- isolate(state$ui_tick) + 1L
      return(invisible(NULL))
    }
    start_time <- Sys.time()
    if (nrow(calls)) start_time <- max(start_time,
      max(qc_parse_time(calls$hospital_arrival_time)) + 60)
    call <- supervisor_simulate_call(state$baseline_records, input$zone, input$priority,
      index = nrow(calls) + 1L, start_time = start_time,
      responder = supervisor_developer_responders[[1L]])
    call <- supervisor_apply_demo_event(call, event_type, input$metric,
      developer_target(), nrow(calls) + 1L)
    state$developer_calls <- rbind(calls, call[, names(calls), drop = FALSE])
    event <- response_metric_completion_events(call, allow_mock = FALSE)
    event <- event[event$metric_id == input$metric, , drop = FALSE]
    call$display_minutes <- event$duration_minutes[[1L]]
    call$display_metric <- metric_info()$short_label
    call$display_event <- event_type
    state$last_step <- call
    state$completed_tick <- isolate(state$completed_tick) + 1L
    state$ui_tick <- isolate(state$ui_tick) + 1L
  }

  # Developer mode: each click adds one simulated completed call to the zone,
  # priority and responder currently selected, so it lands on the visible chart.
  observeEvent(input$add_call, {
    add_developer_call('stable')
  })
  observeEvent(input$trigger_demo_event, add_developer_call(input$demo_event_type))

  observeEvent(input$load_daily_demo, {
    req(identical(state$mode,'developer'),input$zone,input$priority,input$metric)
    state$demo_day_cases <- supervisor_simulate_day(state$baseline_records,input$zone,input$priority,
      input$metric,abnormal=identical(input$daily_demo_scenario,'abnormal'))
    state$developer_calls <- response_metric_empty_records()
    state$developer_quality <- state$developer_quality[FALSE, , drop=FALSE]
    state$last_step <- NULL
    state$daily_demo_as_of <- qc_parse_time(state$demo_day_cases$call_received_time[1])
    state$completed_tick <- isolate(state$completed_tick)+1L
    state$ui_tick <- isolate(state$ui_tick)+1L
    updateSelectInput(session,'time_span',selected='day')
    updateDateInput(session,'day_date',value=as.Date(state$daily_demo_as_of,tz='America/New_York'))
    updateTabsetPanel(session,'main_tabs',selected='Active tracking')
  })
  reveal_day_cases <- function(finish=FALSE) {
    req(identical(state$mode,'developer'))
    planned <- state$demo_day_cases
    if(is.null(planned)) { showNotification('Select Start demo day first.',type='message');return() }
    n <- nrow(state$developer_calls)
    if(n>=nrow(planned)) {showNotification('Demo day complete. Start another day to replay.',type='message');return()}
    end <- if(finish)nrow(planned) else n+1L
    state$developer_calls <- planned[seq_len(end),,drop=FALSE]
    last <- planned[end,,drop=FALSE]
    event <- response_metric_completion_events(last,allow_mock=FALSE)
    event <- event[event$metric_id==input$metric,,drop=FALSE]
    last$display_minutes <- event$duration_minutes[[1]]
    last$display_metric <- metric_info()$short_label
    last$display_event <- if(grepl('planted',last$response_system_source))'planted delay' else 'normal demo case'
    state$last_step <- last
    state$daily_demo_as_of <- qc_parse_time(last$hospital_arrival_time)
    state$completed_tick <- isolate(state$completed_tick)+1L
    state$ui_tick <- isolate(state$ui_tick)+1L
  }
  observeEvent(input$next_demo_case,reveal_day_cases())
  observeEvent(input$finish_demo_day,reveal_day_cases(TRUE))

  snapshot <- reactive({
    state$ui_tick
    records <- if (identical(state$mode, 'developer')) state$developer_calls
      else production_records()
    as_of <- Sys.time()
    if (identical(state$mode, 'developer') && nrow(records))
      as_of <- if(!is.null(state$daily_demo_as_of)) state$daily_demo_as_of else max(qc_parse_time(records$call_received_time))
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
        tags$p('Select Try demo mode above the dashboard to explore generated examples.')))
    }
    tagList(
      div(class = 'developer-mode-card',
        tags$strong('Developer mode active'),
        tags$p('Graphs show only developer cases. The connected Caller/Responder trip remains visible separately. Generated cases are discarded when you leave developer mode.')),
      div(class = 'review-box',
        checkboxInput(session$ns('demo_review'),
          'Use synthetic baseline-review fixture', value = TRUE),
        tags$small('Demonstration only. This is not approval of a real EMS baseline.')),
      div(class = 'review-box',
        tags$strong('Manual demo targets'),
        numericInput(session$ns('developer_scene_target'), 'Call → scene target (minutes)',
          value = 12, min = 1, max = 120, step = .5),
        numericInput(session$ns('developer_transport_target'), 'Departure → hospital target (minutes)',
          value = 8, min = 1, max = 120, step = .5),
        tags$small('Developer mode only. Ordinary mode uses the frozen synthetic context model.')),
      tags$p('Select Exit demo mode above the graphs to return to the shared live records.'))
  })

  # The bar itself renders only when the mode changes, so the button keeps focus
  # across clicks; the status text updates separately.
  output$developer_bar <- renderUI({
    if (!identical(state$mode, 'developer')) return(
      div(class='developer-bar',role='region',`aria-label`='Demo controls',
        div(strong('Explore a demo'),p('Generate normal and delayed daily cases. Your connected live trip stays separate.')),
        actionButton(session$ns('enter_developer'),'Try demo mode',class='btn-primary')))
    div(class='developer-bar',role='region',`aria-label`='Developer simulation',
      uiOutput(session$ns('developer_bar_text'),class='developer-bar-text'),
      div(class='developer-actions',
        selectInput(session$ns('daily_demo_scenario'),'Daily demo',
          choices=c('Normal day'='stable','Day with delays'='abnormal')),
        actionButton(session$ns('load_daily_demo'),'Start demo day',class='btn-primary'),
        actionButton(session$ns('next_demo_case'),'Next completed case',class='btn-default'),
        actionButton(session$ns('finish_demo_day'),'Complete demo day',class='btn-default'),
        actionButton(session$ns('end_developer'),'Exit demo mode',class='btn-default')),
      p('One day, 12 cases: reveal each completed case or jump to the end. The delay scenario has three 30-minute cases. Live trips stay separate.'),
      tags$details(tags$summary('Advanced: individual cases and events'),
        div(class='developer-actions',
          actionButton(session$ns('add_call'),'Add stable call',class='btn-primary',icon=icon('plus')),
          selectInput(session$ns('demo_event_type'),'Event',
            choices=c('Extreme point'='extreme','Shift'='shift','Trend'='trend','Target miss'='target_miss',
              'Data-quality warning'='data_quality','Improvement sequence'='improvement'),selected='extreme'),
          actionButton(session$ns('trigger_demo_event'),'Trigger demo event',class='btn-warning'))))
  })
  output$developer_bar_text <- renderUI({
    req(identical(state$mode, 'developer'))
    target <- sprintf('%s · %s priority', input$zone, input$priority)
    last <- state$last_step
    n <- nrow(state$developer_calls)
    day_summary <- NULL
    if(!is.null(state$demo_day_cases)) {
      daily <- qc_validate(response_metric_to_qc(state$developer_calls,input$metric,allow_mock=FALSE))$completed
      day_summary <- tags$p(if(!nrow(daily))'No completed demo cases yet.' else
        sprintf('Demo-day average: %.1f min · %s · %s / %s · n=%d',mean(daily$response_minutes),
          metric_info()$short_label,state$demo_day_cases$zone[1],state$demo_day_cases$call_priority[1],nrow(daily)))
    }
    tagList(
      div(class = 'kpi-label', 'Developer mode · synthetic simulation'),
      tags$strong(if(!is.null(state$demo_day_cases))sprintf('Demo day: %d of 12 completed cases%s',n,if(n==12)' · day complete' else '') else sprintf('%d simulated call%s', n, if (n == 1L) '' else 's')),day_summary,
      tags$span(class = 'developer-bar-detail', role = 'status',
        if (is.null(last)) paste('Next call goes to', target)
        else sprintf('Last: %s · %s / %s · %s %.1f min · %s · next call goes to %s',
          last$incident_id, last$zone, last$call_priority,
          tolower(last$display_metric), last$display_minutes, last$display_event, target)))
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

  output$metric_definition <- renderText({
    metric <- if (identical(input$main_tabs, 'History analysis')) input$history_metric else input$metric
    req(metric); response_metric_info(metric)$definition
  })
  output$dashboard_title <- renderText({
    if (identical(input$main_tabs, 'History analysis')) 'Historical response analysis'
    else paste(metric_info()$short_label, 'overview')
  })
  output$dashboard_subtitle <- renderText({
    if (identical(input$main_tabs, 'History analysis'))
      'Review completed baseline and closed monitoring periods with independent history filters.'
    else 'Completed response-system records enter the selected metric view. Historical limits stay fixed.'
  })
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
    supervisor_visible_monitoring(selected_group(), display_window(), 'all')
  })
  selected_monitor_summary <- reactive({ qc_summary(visible_monitoring()$response_minutes) })
  target_model <- reactive(supervisor_target_model(state$baseline_records, input$metric))
  visible_targets <- reactive({
    m <- visible_monitoring()
    if (identical(state$mode, 'developer')) {
      value <- developer_target()
      if (is.null(value) || !is.finite(value)) value <- NA_real_
      return(rep(value, nrow(m)))
    }
    supervisor_target_predict(target_model(), m)
  })
  signal_rules <- reactive({
    m <- visible_monitoring(); g <- selected_group()
    sigma <- (g$limits$i_ucl - g$limits$center) / 3
    supervisor_signal_rules(m, g$limits$center, sigma, visible_targets())
  })
  visible_flags <- reactive({
    if (!isTRUE(selected_group()$flags_enabled)) return(visible_monitoring()[FALSE, , drop = FALSE])
    m <- visible_monitoring(); rules <- signal_rules()
    cbind(m[rules$performance_signal, , drop = FALSE],
      rules[rules$performance_signal, c('rule_labels', 'primary_rule', 'target_miss'), drop = FALSE])
  })
  signal_groups <- reactive(supervisor_group_signals(visible_monitoring(), signal_rules(),
    input$metric, input$zone, input$priority))
  selected_flag_id <- reactiveVal(NULL)
  selected_signal_group_id <- reactiveVal(NULL)
  observeEvent(input$selected_signal_group, {
    selected_signal_group_id(input$selected_signal_group)
    group <- signal_groups()
    hit <- group[group$group_id == input$selected_signal_group, , drop = FALSE]
    if (nrow(hit)) selected_flag_id(tail(strsplit(hit$incident_ids[[1L]], ', ')[[1L]], 1L))
  }, ignoreInit = TRUE)
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
  output$kpi_target <- renderText({
    if (identical(state$mode, 'developer')) return(fmt_num(developer_target(), 1))
    x <- visible_targets()
    value <- if (any(is.finite(x))) stats::median(x, na.rm = TRUE)
      else supervisor_target_for_scope(target_model(), input$zone, input$priority)
    fmt_num(value, 1)
  })
  output$kpi_flags <- renderText({ nrow(visible_flags()) })
  output$flag_count <- renderText({ nrow(signal_groups()) })

  output$findings_summary <- renderUI({
    m <- visible_monitoring(); g <- selected_group(); targets <- visible_targets()
    misses <- ifelse(is.finite(targets),m$response_minutes > targets,NA)
    service <- supervisor_service_performance(m,state$service_goals,input$metric)
    service <- service[service$zone == input$zone & service$priority == input$priority,,drop=FALSE]
    summary <- supervisor_plain_summary(m,g$flags_enabled,g$status,misses,service)
    panel_card('What this view tells you',
      paste(metric_info()$short_label,'·',input$zone,'/',input$priority,'· selected display period'),
      p(strong('Historical variation: '),summary$history),
      p(strong('Context target: '),summary$target),
      p(strong('Configured service goal: '),summary$goal),
      tags$small(summary$action))
  })

  output$target_notice <- renderUI({
    if (identical(state$mode, 'developer'))
      return(div(class = 'target-notice', strong('Target source: '),
        sprintf('manual developer setting · %.1f minutes.', developer_target())))
    model <- target_model()
    if (!identical(model$status, 'synthetic_context_model'))
      return(div(class = 'target-notice danger', 'Context target unavailable: ', model$reason))
    div(class = 'target-notice', strong('Target source: '),
      'frozen synthetic 90th-percentile goal using zone, priority, weather, hour and weekday. Traffic is unavailable in the canonical response stream.')
  })

  output$shared_trip <- renderUI({
    if(!exists('responder_trip_snapshot',mode='function')) return(NULL)
    trip <- responder_trip_snapshot()
    if(is.null(trip)) return(NULL)
    label <- if(trip$phase=='hospital') 'At hospital' else if(trip$phase=='transport') 'Transporting to hospital' else responder_demo(trip$scene_elapsed,trip$scenario)$status
    div(class='panel-card',div(class='kpi-label','Shared live demo trip'),
      h3(paste(trip$incident_id,'·',trip$unit)),
      p(paste(trip$zone,'·',trip$priority,'·',label)),
      if(!is.null(trip$hospital)) p(strong('Confirmed hospital: '),trip$hospital$name),
      p(class='filter-note','Same trip as Responder and private Caller tracking. Live trip records stay separate from developer graphs. Synthetic event times.'))
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
    group <- signal_groups()
    hit <- group[group$group_id == selected_signal_group_id(), , drop = FALSE]
    if (!nrow(hit)) return(flags[FALSE, , drop = FALSE])
    ids <- strsplit(hit$incident_ids[[1L]], ', ')[[1L]]
    flags[flags$incident_id %in% ids, , drop = FALSE]
  })

  output$flag_cards <- renderUI({
    g <- selected_group()
    groups <- signal_groups()
    if (!isTRUE(g$flags_enabled))
      return(div(class = 'flag-empty', 'Signals are unavailable until the baseline review is documented.'))
    if (!nrow(groups))
      return(div(class = 'flag-empty', 'No control-chart signals among completed metric records in this view. This does not prove service is acceptable.'))
    current <- selected_signal_group_id()
    cards <- lapply(seq_len(nrow(groups)), function(i) {
      row <- groups[i, , drop = FALSE]
      tags$button(type = 'button',
        class = paste('flag-card', if (identical(row$group_id, current)) 'is-selected'),
        onclick = sprintf('Shiny.setInputValue(%s,%s,{priority:"event"})',
          jsonlite::toJSON(session$ns('selected_signal_group'), auto_unbox = TRUE),
          jsonlite::toJSON(as.character(row$group_id), auto_unbox = TRUE)),
        tags$strong(sprintf('%s · %d incident%s', row$primary_rule, row$count,
          if (row$count == 1L) '' else 's')),
        tags$small(sprintf('New · latest %s · max %.1f min',
          format(row$latest_at, '%b %d %H:%M', tz = 'America/New_York'), row$max_minutes)))
    })
    div(class = 'flag-list', cards)
  })

  output$investigation_drawer <- renderUI({
    rows <- selected_signal()
    if (!nrow(rows)) return(NULL)
    g <- selected_group()
    rules <- unique(unlist(strsplit(rows$rule_labels, '; ', fixed = TRUE)))
    rules <- rules[nzchar(rules)]
    targets <- visible_targets()[match(rows$incident_id, visible_monitoring()$incident_id)]
    div(class = 'investigation-drawer',
      div(class = 'kpi-label', 'Investigation drawer'),
      h4(paste(rules, collapse = ' + ')),
      p(strong('Affected records. '), paste(rows$incident_id, collapse = ', ')),
      p(sprintf('Observed %.1f–%.1f min · fixed center %.1f · I UCL %.1f min · contextual target median %s min.',
        min(rows$response_minutes), max(rows$response_minutes), g$limits$center, g$limits$i_ucl,
        fmt_num(if (any(is.finite(targets))) median(targets, na.rm = TRUE) else NA_real_, 1))),
      p(strong('Interpretation. '),
        'The selected group meets at least one supplementary SPC or contextual target rule. The fixed control limits have not been recalculated.'),
      p(strong('Supervisor check. '),
        'Verify timestamps, then compare weather, assignment hour, responder availability and access conditions before documenting an intervention.'),
      tags$small('Traffic is not yet in the canonical stream. A signal requests investigation; it is not proof of fault or causality.'),
      actionButton(session$ns('follow_live'), 'Return to latest metric records', class = 'btn-default'))
  })

  output$i_chart <- renderPlot({
    g <- selected_group(); b <- g$baseline; m <- visible_monitoring()
    focus <- selected_signal()
    latest_id <- if (nrow(m)) m$incident_id[which.max(m$on_scene_time)] else NULL
    m <- supervisor_chart_window(m, selected_flag_id())
    rule_map <- signal_rules()
    mplot <- data.frame(sequence = m$monitoring_sequence, value = m$response_minutes,
      signal = rule_map$performance_signal[match(m$incident_id, rule_map$incident_id)],
      selected = m$incident_id %in% focus$incident_id,
      new = m$incident_id %in% latest_id)
    segments <- supervisor_consecutive_segments(m, 'response_minutes')
    target_frame <- data.frame(monitoring_sequence = m$monitoring_sequence,
      target = visible_targets()[match(m$incident_id, visible_monitoring()$incident_id)])
    target_segments <- supervisor_consecutive_segments(target_frame, 'target')
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
      geom_segment(data = target_segments, aes(x = x, xend = xend, y = y, yend = yend),
        color = '#6B8F7A', linewidth = .8, linetype = 'dotdash', na.rm = TRUE) +
      geom_point(data = target_frame, aes(monitoring_sequence, target),
        color = '#6B8F7A', shape = 95, size = 3, na.rm = TRUE) +
      scale_color_manual(values = c(`FALSE` = '#5B6B7A', `TRUE` = '#D15B4A'), guide = 'none') +
      coord_cartesian(xlim = c(left, right)) +
      labs(x = 'Monitoring incident #',
        y = 'Duration (minutes)',
        title = sprintf('%s · %s priority · %s', g$zone, g$call_priority,
          metric_info()$short_label),
        subtitle = sprintf('Baseline n=%d · visible n=%d',
          nrow(b), nrow(visible_monitoring())),
        caption = 'Red: SPC or target signal · green dot-dash: contextual target · ring: latest/selected') +
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

  output$patterns_findings_summary <- renderUI({
    summary <- supervisor_patterns_summary(descriptive(),input$zone,input$priority)
    panel_card('What this view tells you',
      paste(metric_info()$short_label,'· selected display period'),
      p(strong('Included data: '),summary$scope),
      p(strong('Neighborhood medians: '),summary$zones),
      p(strong('Weather medians in the selected zone: '),summary$weather),
      p(strong('Time patterns: '),summary$time),
      tags$small(summary$action))
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
      labs(x = 'Metric start hour (America/New_York)', y = NULL,
        title = sprintf('%s / %s · %s · n=%d', input$zone, input$priority,
          metric_info()$short_label, nrow(d))) +
      theme_minimal(base_size = 10) + theme(panel.grid = element_blank())
  })

  data_quality <- reactive({
    state$ui_tick
    d <- supervisor_data_quality(active_completed(), input$metric)
    if (identical(state$mode, 'developer') && nrow(state$developer_quality))
      d <- rbind(d, state$developer_quality)
    d
  })
  output$data_quality_panel <- renderUI({
    d <- data_quality()
    if (!nrow(d)) return(NULL)
    div(class = 'data-quality-panel',
      div(class = 'flag-heading', h3('Data-quality warnings'),
        span(class = 'quality-count', nrow(d))),
      p(class = 'subhead', 'These are input-integrity issues, not service-performance signals.'),
      tags$ul(lapply(seq_len(min(nrow(d), 5L)), function(i)
        tags$li(strong(paste0(d$severity[i], ': ')), d$incident_id[i], ' · ', d$issue[i])))
    )
  })

  subgroup_open_day <- reactive({
    invalidateLater(60000, session)
    as.Date(snapshot()$simulated_time, tz = 'America/New_York')
  })
  history_analysis <- reactive({
    req(input$history_metric)
    historical <- response_metric_to_qc(state$baseline_records, input$history_metric,
      allow_mock = TRUE)
    completed <- response_metric_to_qc(active_completed(), input$history_metric,
      allow_mock = FALSE)
    supervisor_analyze(historical, completed, TRUE, supervisor_live_period())
  })
  history_group <- reactive({
    req(input$history_zone, input$history_priority)
    history_analysis()$groups[[paste(input$history_zone, input$history_priority, sep = '/')]]
  })
  history_all_records <- reactive({
    make_frame <- function(records, allow_mock, phase) {
      raw <- response_metric_to_qc(records, input$history_metric, allow_mock = allow_mock)
      start <- qc_parse_time(raw$dispatch_time)
      finish <- qc_parse_time(raw$on_scene_time)
      minutes <- as.numeric(difftime(finish, start, units = 'mins'))
      data.frame(dispatch_time = start, response_minutes = minutes, zone = raw$zone,
        call_priority = raw$call_priority, phase = rep(phase, nrow(raw)), stringsAsFactors = FALSE)
    }
    baseline <- make_frame(state$baseline_records, TRUE, '2025 baseline')
    period <- dashboard_period()
    baseline_stamp <- as.numeric(baseline$dispatch_time)
    baseline <- baseline[baseline_stamp >= as.numeric(period$baseline_start) &
      baseline_stamp < as.numeric(period$baseline_end) & is.finite(baseline$response_minutes) &
      baseline$response_minutes >= 0, , drop = FALSE]
    closed <- make_frame(active_completed(), FALSE, 'Closed monitoring day')
    closed_day <- as.Date(closed$dispatch_time, tz = 'America/New_York')
    closed <- closed[is.finite(closed$response_minutes) & closed$response_minutes >= 0 &
      !is.na(closed_day) & closed_day < subgroup_open_day(), , drop = FALSE]
    rbind(baseline, closed)
  })
  history_filtered_records <- reactive({
    d <- history_all_records()
    if (identical(input$history_phase, 'baseline')) d <- d[d$phase == '2025 baseline', , drop = FALSE]
    if (identical(input$history_phase, 'monitoring')) d <- d[d$phase == 'Closed monitoring day', , drop = FALSE]
    supervisor_history_period(d, input$history_period)
  })
  subgroup_source <- reactive({
    d <- history_filtered_records()
    d[d$zone == input$history_zone & d$call_priority == input$history_priority,
      c('dispatch_time', 'response_minutes', 'phase'), drop = FALSE]
  })
  subgroup_open_count <- reactive({
    raw <- response_metric_to_qc(active_completed(), input$history_metric, allow_mock = FALSE)
    start <- qc_parse_time(raw$dispatch_time)
    finish <- qc_parse_time(raw$on_scene_time)
    minutes <- as.numeric(difftime(finish, start, units = 'mins'))
    day <- as.Date(start, tz = 'America/New_York')
    sum(raw$zone == input$history_zone & raw$call_priority == input$history_priority &
      is.finite(minutes) & minutes >= 0 & !is.na(day) & day == subgroup_open_day())
  })
  output$subgroup_status <- renderUI({
    all_source <- history_all_records()
    source <- subgroup_source()
    baseline_n <- sum(all_source$phase == '2025 baseline' & all_source$zone == input$history_zone &
      all_source$call_priority == input$history_priority)
    baseline_days <- length(unique(as.Date(all_source$dispatch_time[all_source$phase == '2025 baseline' &
      all_source$zone == input$history_zone & all_source$call_priority == input$history_priority],
      tz = 'America/New_York')))
    closed_dates <- as.Date(all_source$dispatch_time[all_source$phase == 'Closed monitoring day'],
      tz = 'America/New_York')
    through <- if (length(closed_dates)) format(max(closed_dates), '%b %d, %Y') else 'No closed monitoring day yet'
    visible_dates <- as.Date(source$dispatch_time, tz = 'America/New_York')
    available <- if (length(visible_dates)) sprintf('%s–%s', format(min(visible_dates), '%b %d, %Y'),
      format(max(visible_dates), '%b %d, %Y')) else 'No dates for this selection'
    div(class = 'subgroup-status',
      div(strong('Available dates: '), available,
        tags$small(sprintf('%d baseline incidents across %d completed days', baseline_n, baseline_days))),
      div(strong('Monitoring available through: '), through,
        tags$small(sprintf('%d current-day record%s pending · refreshed %s ET', subgroup_open_count(),
          if (subgroup_open_count() == 1L) '' else 's', format(Sys.time(), '%H:%M')))))
  })
  history_aggregation <- reactive({
    if (!identical(input$history_aggregation, 'auto')) input$history_aggregation
    else if (identical(input$history_period, 'full')) 'week' else 'day'
  })
  history_subgroups <- reactive({
    g <- history_group(); sigma <- (g$limits$i_ucl - g$limits$center) / 3
    source <- subgroup_source()
    d <- supervisor_period_subgroups(source, g$limits$center, sigma, history_aggregation())
    if (nrow(d)) {
      closed_periods <- as.Date(source$dispatch_time[source$phase == 'Closed monitoring day'],
        tz = 'America/New_York')
      if (identical(history_aggregation(), 'week'))
        closed_periods <- closed_periods - (as.integer(format(closed_periods, '%u')) - 1L)
      d$phase <- ifelse(d$period %in% unique(closed_periods),
        'Closed monitoring', '2025 baseline')
    }
    d
  })
  history_daily_distribution <- reactive({
    g <- history_group(); sigma <- (g$limits$i_ucl - g$limits$center) / 3
    supervisor_period_subgroups(subgroup_source(), g$limits$center, sigma, 'day')
  })
  output$history_control_chart <- renderPlot({
    g <- history_group(); d <- history_subgroups()
    validate(need(nrow(d) > 0L, 'No completed subgroup is available for this selection.'))
    label <- if (identical(history_aggregation(), 'week')) 'Weekly' else 'Daily'
    interval <- if (identical(history_aggregation(), 'week')) 'week' else 'day'
    ggplot(d, aes(period, mean)) +
      geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = 'Variable 3σ control limits'),
        alpha = .28) +
      geom_point(aes(color = signal, size = n, shape = phase), alpha = .85) +
      geom_line(color = '#6F93B4', linewidth = .45, alpha = .65) +
      geom_hline(yintercept = g$limits$center, color = '#2C3E50', linewidth = .7) +
      scale_color_manual(values = c(`FALSE` = '#6B8F7A', `TRUE` = '#D15B4A'), guide = 'none') +
      scale_fill_manual(values = c('Variable 3σ control limits' = '#AFC6D9'),
        name = 'Control reference') +
      scale_shape_manual(values = c(`2025 baseline` = 16, `Closed monitoring` = 17),
        name = 'Completed period') +
      scale_size_continuous(range = c(3, 7),
        breaks = supervisor_count_breaks(d$n),
        name = sprintf('Completed responses\nper %s (n)', interval)) +
      labs(x = paste(label, 'start · America/New_York'), y = paste(label, 'mean (minutes)'),
        title = sprintf('%s · %s priority · %s subgroups', input$history_zone,
          input$history_priority, tolower(label)),
        caption = sprintf(paste0('n = completed response records in each %s; point size encodes n · ',
          'variable 3σ limits use n · current day excluded.'), interval)) +
      theme_minimal(base_size = 11)
  })

  output$history_distribution_chart <- renderPlot({
    d <- history_daily_distribution(); g <- history_group()
    validate(need(nrow(d) > 1L, 'At least two completed days are needed for a distribution.'))
    goal <- supervisor_service_goal_value(state$service_goals, input$history_metric,
      input$history_zone, input$history_priority)
    bins <- suppressWarnings(grDevices::nclass.FD(d$mean))
    if (!is.finite(bins) || bins < 1L) bins <- ceiling(sqrt(nrow(d)))
    bins <- if (nrow(d) <= 7L) max(3L, nrow(d)) else min(30L, max(7L, bins))
    p <- ggplot(d, aes(mean)) +
      geom_histogram(bins = bins, fill = '#AFC6D9', color = 'white') +
      geom_rug(color = '#6F93B4', alpha = .6, sides = 'b') +
      geom_vline(xintercept = g$limits$center, color = '#2C3E50', linewidth = .8) +
      labs(x = 'Completed daily subgroup mean (minutes)', y = 'Number of completed days',
        title = 'Distribution of completed daily subgroup means',
        subtitle = sprintf('%d completed days · %d completed responses · %d histogram bins',
          nrow(d), sum(d$n), bins),
        caption = paste0('Every tick is one completed daily mean, even when the control chart is weekly · ',
          'dark line: fixed baseline center · service-goal marker appears after configuration')) +
      theme_minimal(base_size = 11)
    if (is.finite(goal)) p <- p + geom_vline(xintercept = goal, color = '#E6A700',
      linetype = 'dashed', linewidth = .9)
    p
  })

  output$history_monthly_chart <- renderPlot({
    source <- subgroup_source(); g <- history_group()
    sigma <- (g$limits$i_ucl - g$limits$center) / 3
    d <- supervisor_period_subgroups(source, g$limits$center, sigma, 'day')
    validate(need(nrow(d) > 1L, 'Monthly distributions need at least two completed days.'))
    d$month <- factor(format(d$period, '%b'), levels = month.abb)
    ggplot(d, aes(month, mean)) +
      geom_boxplot(fill = '#E6EEF5', color = '#2C3E50', outlier.color = '#D15B4A', width = .62) +
      geom_hline(yintercept = g$limits$center, color = '#6B8F7A', linetype = 'dashed') +
      labs(x = NULL, y = 'Daily subgroup mean (minutes)',
        title = 'Month-to-month distribution',
        caption = 'Twelve readable monthly summaries of completed daily subgroup means; descriptive only.') +
      theme_minimal(base_size = 11)
  })

  output$history_control_reading <- renderUI({
    d <- history_subgroups()
    interval <- if (identical(history_aggregation(), 'week')) 'week' else 'day'
    signals <- if (nrow(d)) sum(d$signal, na.rm = TRUE) else 0L
    tagList(
      p(strong('What each mark means: '),
        sprintf('Each dot is one completed %s mean; dot size is completed-response count n. ', interval),
        'The dark line is the frozen baseline center and the grey ribbon is the variable 3σ control-limit band.'),
      p(strong('This selection: '), if (!nrow(d)) 'No completed subgroup is available.'
        else if (!signals) sprintf('None of the %d completed %ss is outside the control limits.', nrow(d), interval)
        else sprintf('%d completed %s%s outside the limits and should be reviewed.', signals,
          interval, if (signals == 1L) ' is' else 's are')),
      p(class = 'reading-caution', 'Control limits describe process variation; they are not service targets and do not establish cause.'))
  })

  output$history_distribution_reading <- renderUI({
    d <- history_daily_distribution()
    if (!nrow(d)) return(p('No completed daily subgroups are available for this selection.'))
    p(sprintf(paste0('Bar height counts completed days, and each rug tick along the bottom is one daily mean. ',
      'The selected window contains %d completed days and %d responses; the median daily mean is %.1f minutes. ',
      'Use this chart to inspect shape and spread, not sequence or causality.'),
      nrow(d), sum(d$n), stats::median(d$mean)))
  })

  output$history_monthly_reading <- renderUI({
    source <- subgroup_source(); g <- history_group()
    sigma <- (g$limits$i_ucl - g$limits$center) / 3
    d <- supervisor_period_subgroups(source, g$limits$center, sigma, 'day')
    if (!nrow(d)) return(p('No completed daily subgroups are available for monthly review.'))
    month <- format(d$period, '%b')
    medians <- tapply(d$mean, month, stats::median)
    high <- names(which.max(medians)); low <- names(which.min(medians))
    p(sprintf(paste0('Each box summarizes completed daily means within one month; the center line is the median, ',
      'the box is the middle 50%%, and points beyond the whiskers are descriptive outliers. ',
      '%s has the highest monthly median and %s the lowest in this selection; review context before interpreting the difference.'),
      high, low))
  })

  output$history_chart_layout <- renderUI({
    if (!identical(input$history_period, 'full'))
      return(div(class = 'two-col history-chart-grid',
        panel_card('Subgroup control chart',
          'Completed-period sequence; point size is the number of response records in that subgroup.',
          plotOutput(session$ns('history_control_chart'), height = 360),
          chart_help(
            reading_key(
              key_item('Subgroup mean', 'key-dot'),
              key_item('Control signal', 'key-dot key-signal'),
              key_item('Baseline center', 'key-line'),
              key_item('Variable 3σ limits', 'key-band')),
            uiOutput(session$ns('history_control_reading')))),
        panel_card('Subgroup distribution',
          'Histogram uses every completed daily subgroup in the selected calendar window.',
          plotOutput(session$ns('history_distribution_chart'), height = 360),
          chart_help(
            reading_key(
              key_item('Completed days', 'key-bar'),
              key_item('Daily mean', 'key-tick'),
              key_item('Baseline center', 'key-line'),
              key_item('Service goal', 'key-goal')),
            uiOutput(session$ns('history_distribution_reading'))))))
    tagList(
      div(class = 'two-col history-chart-grid',
        panel_card('Subgroup control chart', 'Automatic mode uses weekly subgroups for the full-year view.',
          plotOutput(session$ns('history_control_chart'), height = 360),
          chart_help(
            reading_key(
              key_item('Subgroup mean', 'key-dot'),
              key_item('Control signal', 'key-dot key-signal'),
              key_item('Baseline center', 'key-line'),
              key_item('Variable 3σ limits', 'key-band')),
            uiOutput(session$ns('history_control_reading')))),
        panel_card('Subgroup distribution',
          'Histogram retains every completed daily mean while the timeline stays weekly.',
          plotOutput(session$ns('history_distribution_chart'), height = 360),
          chart_help(
            reading_key(
              key_item('Completed days', 'key-bar'),
              key_item('Daily mean', 'key-tick'),
              key_item('Baseline center', 'key-line'),
              key_item('Service goal', 'key-goal')),
            uiOutput(session$ns('history_distribution_reading'))))),
      panel_card('Monthly distribution', 'One summary per month avoids an unreadable boxplot for every day.',
        plotOutput(session$ns('history_monthly_chart'), height = 320),
        chart_help(
          reading_key(
            key_item('Middle 50%', 'key-box'),
            key_item('Median', 'key-line'),
            key_item('Descriptive outlier', 'key-dot key-signal')),
          uiOutput(session$ns('history_monthly_reading')))))
  })

  service_scope <- reactive(supervisor_service_scope_summary(history_filtered_records(),
    state$service_goals, input$history_metric, input$history_zone, input$history_priority))

  service_goal_source <- reactive({
    goals <- state$service_goals
    exact <- goals$metric_id == input$history_metric & goals$priority == input$history_priority &
      goals$zone == input$history_zone
    fallback <- goals$metric_id == input$history_metric & goals$priority == input$history_priority &
      goals$zone == 'All zones'
    row <- if (any(exact)) tail(which(exact), 1L) else if (any(fallback)) tail(which(fallback), 1L)
      else integer()
    if (!length(row)) 'not configured' else goals$source[row]
  })

  output$history_findings_summary <- renderUI({
    g <- history_group()
    summary <- supervisor_history_summary(history_subgroups(),service_scope(),history_aggregation(),
      g$flags_enabled,g$status,subgroup_open_count())
    panel_card('What this view tells you',
      paste(response_metric_info(input$history_metric)$short_label,'·',input$history_zone,'/',input$history_priority,
        '·',switch(input$history_period,'full'='Full baseline year',paste('Latest',input$history_period,'calendar days')),
        '·',switch(input$history_phase,'baseline'='Baseline only','monitoring'='Closed monitoring only','Baseline + monitoring')),
      p(strong('Historical variation: '),summary$history),
      p(strong('Service-goal performance: '),summary$goal),
      p(strong('Included data: '),summary$coverage),
      tags$small(summary$action))
  })

  output$service_goal_status <- renderUI({
    d <- service_scope()
    status_class <- if (identical(d$status, 'Meeting goal')) 'is-meeting'
      else if (identical(d$status, 'Above goal')) 'is-missing' else 'is-unavailable'
    div(class = 'service-scope-summary',
      div(class = paste('service-status-badge', status_class), d$status),
      div(class = 'service-scope-metrics',
        div(span('Observed p90'), strong(if (is.finite(d$p90)) sprintf('%.1f min', d$p90) else '—')),
        div(span('Service goal'), strong(if (is.finite(d$goal)) sprintf('%.1f min', d$goal) else '—')),
        div(span('Within goal'), strong(if (is.finite(d$within_goal))
          sprintf('%.0f%%', 100 * d$within_goal) else '—')),
        div(span('Completed responses'), strong(d$n))))
  })

  output$service_goal_plot <- renderPlot({
    d <- service_scope()
    validate(need(d$n > 0L, 'No completed responses are available for this selection.'),
      need(is.finite(d$goal), 'Configure a service goal to display this comparison.'))
    axis_max <- max(d$p90, d$goal) * 1.28
    ggplot(d, aes(x = p90, y = 'Selected scope', fill = status)) +
      geom_col(width = .42) +
      geom_vline(xintercept = d$goal, color = '#2C3E50', linetype = 'dashed', linewidth = .9) +
      annotate('text', x = d$goal, y = 'Selected scope', label = sprintf('Goal %.1f', d$goal),
        hjust = -.08, vjust = -1.8, color = '#2C3E50', size = 3.4) +
      geom_text(aes(label = sprintf('Observed p90 %.1f', p90)), hjust = 1.05,
        color = 'white', fontface = 'bold', size = 3.7) +
      scale_fill_manual(values = c('Meeting goal' = '#6B8F7A', 'Above goal' = '#D15B4A'),
        guide = 'none') +
      scale_x_continuous(limits = c(0, axis_max), expand = expansion(mult = c(0, .02))) +
      labs(x = 'Minutes', y = NULL,
        title = sprintf('%s · %s priority · %s', input$history_zone, input$history_priority,
          response_metric_info(input$history_metric)$short_label),
        caption = sprintf('Goal source: %s · changing the goal never changes SPC limits.',
          service_goal_source())) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
  })

  output$service_goal_reading <- renderUI({
    d <- service_scope()
    if (!d$n) return(p('No completed responses are available for this scope.'))
    tagList(
      p('The colored bar is the observed 90th percentile: 90% of completed responses are at or below this duration. The dashed vertical line is the editable service goal.'),
      p(if (!is.finite(d$goal)) 'No goal is configured.' else sprintf(
        'For this selection, p90 is %.1f minutes, the goal is %.1f minutes, and %.0f%% of %d completed responses are within goal.',
        d$p90, d$goal, 100 * d$within_goal, d$n)),
      p(class = 'reading-caution', 'This management comparison is separate from statistical control limits and is not an official clinical standard.'))
  })

  output$service_goal_summary <- renderUI({
    d <- state$service_goals[state$service_goals$metric_id == input$history_metric, , drop = FALSE]
    if (!nrow(d)) return(div(class = 'flag-empty',
      'No service goal is configured for this metric. The chart remains descriptive.'))
    div(class = 'goal-chip-list', lapply(seq_len(nrow(d)), function(i)
      span(class = 'goal-chip', sprintf('%s · %s: %.1f min · %s', d$priority[i], d$zone[i],
        d$minutes[i], d$source[i]))))
  })

  adjusted_plot <- function(field, title) {
    d <- supervisor_adjusted_comparison(supervisor_target_model(state$baseline_records,
      input$history_metric), field)
    validate(need(nrow(d) > 0L, 'Adjusted comparison unavailable.'))
    d$group <- factor(d$group, levels = rev(d$group))
    ggplot(d, aes(estimate, group)) +
      geom_errorbar(aes(xmin = lower, xmax = upper), width = .18,
        orientation = 'y', color = '#5B6B7A') +
      geom_point(aes(size = n), color = '#D15B4A') +
      scale_size_continuous(range = c(3, 7), name = 'Reference n') +
      labs(x = 'Adjusted geometric mean (minutes)', y = NULL, title = title,
        caption = '95% model interval · synthetic reference model · descriptive association, not causal') +
      theme_minimal(base_size = 11)
  }
  output$adjusted_zone_plot <- renderPlot({
    adjusted_plot('zone', paste(response_metric_info(input$history_metric)$short_label, 'by zone'))
  })
  output$adjusted_priority_plot <- renderPlot({
    adjusted_plot('call_priority', paste(response_metric_info(input$history_metric)$short_label, 'by priority'))
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
