library(shiny)
library(ggplot2)

if (!requireNamespace('openssl', quietly = TRUE)) {
  stop("Install openssl first: install.packages('openssl')")
}

# Public, disposable demo accounts. They are regenerated on every app start.
demo_accounts <- lapply(c('supervisor', 'responder'), function(role) {
  password <- openssl::base64_encode(openssl::rand_bytes(12))
  salt <- openssl::rand_bytes(16)
  list(username = paste0(role, '.demo'), password = password, salt = salt,
    hash = openssl::bcrypt_pbkdf(password, salt, rounds = 16L))
})
names(demo_accounts) <- c('supervisor', 'responder')

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
source(file.path(repo_root, 'app', 'feed.R'), local = TRUE)
source(file.path(repo_root, 'app', 'workflow.R'), local = TRUE)
source(file.path(repo_root, 'app', 'responder-demo.R'), local = TRUE)
source(file.path(repo_root, 'app', 'responder-server.R'), local = TRUE)
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite.")
demo_hospitals <- jsonlite::fromJSON(file.path(repo_root, "app", "www", "demo-hospitals.json"), simplifyVector = FALSE)$hospitals
hospital_candidates <- function(requirement) {
  matches <- Filter(function(h) requirement %in% h$capabilities, demo_hospitals)
  matches[order(vapply(matches, function(h) h$distance_m, numeric(1)))]
}
brief_item <- function(symbol, label, value) div(class = "brief-item", span(class = "brief-symbol", icon(symbol), `aria-hidden` = "true"), div(span(class = "eyebrow", label), div(class = "brief-value", value)))


empty_state <- function(title, detail) div(class = "empty", h3(title), p(detail))

fmt_num <- function(x, digits = 1) {
  if (!length(x) || is.na(x)) '—' else formatC(x, digits = digits, format = 'f')
}

kpi <- function(label, value_id, unit = NULL) {
  div(class = 'kpi', div(class = 'kpi-label', label),
    div(class = 'kpi-value', textOutput(value_id, inline = TRUE)),
    if (!is.null(unit)) div(class = 'kpi-unit', unit))
}

panel_card <- function(title, subtitle, ...) {
  div(class = 'panel-card', h3(title), div(class = 'subhead', subtitle), ...)
}

ui <- fluidPage(
  tags$head(
    tags$title('DIIS Emergency Response Monitor'),
    tags$meta(name = 'viewport', content = 'width=device-width, initial-scale=1'),
    tags$meta(name = 'theme-color', content = '#68b900'),
    tags$meta(name = 'apple-mobile-web-app-capable', content = 'yes'),
    tags$meta(name = 'apple-mobile-web-app-title', content = 'DIIS EMS'),
    tags$link(rel = 'manifest', href = 'manifest.webmanifest'),
    tags$link(rel = 'icon', type = 'image/png', href = 'icons/icon-192.png'),
    tags$link(rel = 'apple-touch-icon', href = 'icons/apple-touch-icon.png'),
    tags$script(src = 'pwa.js', defer = NA),
    tags$link(rel = 'stylesheet', href = 'vendor/leaflet.css'),
    tags$link(rel = 'stylesheet', href = 'responder.css'),
    tags$script(src = 'vendor/leaflet.js'),
    tags$script(src = 'responder-map.js', defer = NA),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css')
  ),
  div(class = 'topbar',
    div(class = 'brand', 'DIIS Response Monitor',
      tags$small('Ambulance response quality · Queens demonstration')),
    div(class = 'synthetic-badge', '● Synthetic data — not for operations')
  ),
  uiOutput('role_navigation'),
  conditionalPanel("output.active_role === 'choose'",
    div(class = 'role-intro',
      div(class = 'kpi-label', 'Welcome'),
      h1('How are you joining today?'),
      p('Choose a role to open the appropriate synthetic demonstration workspace.')),
    div(class = 'roles',
      div(class = 'role-card', div(class = 'role-number', '01 / COORDINATE'),
        h3('Supervisor'),
        p('Run the synthetic dispatch replay, inspect I-MR charts, and investigate response-delay signals.'),
        actionButton('choose_supervisor', 'Continue as supervisor', class = 'btn-primary')),
      div(class = 'role-card', div(class = 'role-number', '02 / RESPOND'),
        h3('Primary responder'),
        p('Open the responder prototype for an assigned incident and response status.'),
        actionButton('choose_responder', 'Continue as primary responder', class = 'btn-primary')),
      div(class = 'role-card', div(class = 'role-number', '03 / REQUEST HELP'),
        h3('Caller'),
        p('Read emergency-request guidance. This demonstration never contacts EMS.'),
        actionButton('choose_caller', 'Continue as caller', class = 'btn-primary'))),
    p(class = 'role-note', 'Hackathon demonstration · Staff roles use disposable credentials shown at login.')),
  uiOutput('responder_workspace'),
  conditionalPanel("output.active_role === 'caller'",
    div(class = 'role-workspace panel-card', div(class = 'kpi-label', 'Caller / Prototype'),
      h2('Emergency medical help'),
      div(class = 'alert-note danger',
        'This prototype cannot contact EMS or dispatch an ambulance. For an actual emergency in the U.S., call 911.'),
      div(class = 'empty-state', h3('No request has been sent'),
        p('This screen does not collect patient information or track an ambulance.')))),
  conditionalPanel("output.active_role === 'supervisor'",
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
      h2('Replay controls'),
      selectInput('scenario', 'Bundled scenario',
        choices = c('Stable process' = 'stable', 'Planted delay' = 'planted')),
      numericInput('seed', 'Shuffle seed', value = 20261016, min = 1, step = 1),
      selectInput('speed', 'Replay speed',
        choices = c('30×' = 30, '120×' = 120, '300×' = 300, '600×' = 600),
        selected = 300),
      div(class = 'button-row',
        actionButton('start', 'Start', class = 'btn-primary'),
        actionButton('pause_resume', 'Pause / resume')),
      actionButton('reset', 'Reset replay', width = '100%'),
      hr(), h2('Analysis filters'),
      selectInput('zone', 'Neighborhood', choices = qc_zones),
      selectInput('priority', 'Call priority', choices = qc_priorities),
      dateRangeInput('date_range', 'Descriptive date range',
        start = as.Date('2025-01-01'), end = as.Date('2026-02-28'),
        min = as.Date('2025-01-01'), max = as.Date('2026-02-28')),
      div(class = 'review-box',
        checkboxInput('demo_review',
          'Enable demo-only baseline review fixture', value = FALSE),
        tags$small('Non-operational. This unlocks synthetic monitoring flags; it is not approval of a real EMS baseline.')),
      div(class = 'rail-note',
        strong('Measurement'), br(),
        'Assignment to crew-confirmed on-scene time, in minutes. Control limits describe historical synthetic behavior; they are not service targets.')
    ),
    tags$main(class = 'workspace',
      div(class = 'status-strip',
        div(class = 'status-pill', strong('Replay: '), textOutput('run_status', inline = TRUE)),
        div(class = 'status-pill', strong('Simulated time: '), textOutput('sim_time', inline = TRUE)),
        div(class = 'status-pill', strong('Events: '), textOutput('event_progress', inline = TRUE)),
        div(class = 'status-pill', strong('QC: '), textOutput('qc_status', inline = TRUE))
      ),
      tabsetPanel(id = 'main_tabs',
        tabPanel('Live response',
          div(class = 'kpi-grid',
            kpi('Active calls', 'kpi_active', 'calls now'),
            kpi('Completed replay', 'kpi_completed', 'valid incidents'),
            kpi('Median response', 'kpi_median', 'minutes · selected group'),
            kpi('90th percentile', 'kpi_p90', 'minutes · selected group')),
          uiOutput('replay_notice'),
          panel_card('Active synthetic calls',
            'Elapsed time is simulated and descriptive; pending calls never enter the control chart.',
            tableOutput('active_table')),
          panel_card('Latest completed responses',
            'Automatic on-scene events stop the response clock.',
            tableOutput('completed_table'))
        ),
        tabPanel('I‑MR control',
          uiOutput('review_notice'),
          div(class = 'two-col',
            panel_card('Individuals chart',
              'Assignment-to-confirmed-on-scene response time (minutes); fixed 12-month baseline limits.',
              plotOutput('i_chart', height = 355)),
            panel_card('Moving-range chart',
              'Absolute change in response time between adjacent incidents in the same group (minutes).',
              plotOutput('mr_chart', height = 355))),
          panel_card('Incidents requiring investigation',
            'A response strictly above the fixed Individuals UCL is a signal, not proof of cause, fault, or unacceptable service.',
            tableOutput('investigation_table'))
        ),
        tabPanel('Patterns',
          panel_card('Neighborhood distribution',
            'Type-7 quartiles and 1.5×IQR whiskers by neighborhood; dots show individual observations. Descriptive only.',
            plotOutput('zone_boxplot', height = 380)),
          div(class = 'two-col',
            panel_card('Assignment hour × weekday',
              'Median response minutes in America/New_York; labels show median and sample count. Blank means no data.',
              plotOutput('heatmap', height = 420)),
            panel_card('Weather context',
              'Synthetic assignment-time weather. Descriptive only; weather does not change QC limits or establish cause.',
              plotOutput('weather_boxplot', height = 420)))
        ),
        tabPanel('Data & method',
          div(class = 'two-col',
            panel_card('Fixture validation',
              'Validation of the selected bundled CSV before replay.',
              tableOutput('validation_table')),
            panel_card('Frozen baseline reference',
              'Selected neighborhood and priority. Eligibility is not the same as human approval.',
              tableOutput('limits_table'))),
          panel_card('How to interpret this dashboard', '',
            tags$p(strong('Signal meaning: '),
              "Above the upper control limit means unusually long relative to this group's baseline; investigate circumstances."),
            tags$p('No signal is not proof that service is acceptable. Control limits are not clinical targets. Active calls are excluded until an automatic on-scene event completes them.'),
            tags$p('The replay is a seeded shuffle of committed synthetic monitoring records. Resetting with the same seed reproduces the same event order.'))
        )
      )
    )
  )
  )
)

server <- function(input, output, session) {
  active_role <- reactiveVal('choose')
  authenticated_role <- reactiveVal(NULL)
  requested_role <- reactiveVal(NULL)
  login_error <- reactiveVal('')
  attempts <- reactiveVal(0L)
  locked_until <- reactiveVal(as.POSIXct(0, origin = '1970-01-01'))

  output$active_role <- renderText(active_role())
  outputOptions(output, 'active_role', suspendWhenHidden = FALSE)
  output$login_error <- renderText(login_error())
  output$role_navigation <- renderUI({
    if (active_role() == 'choose') return(NULL)
    div(class = 'switch-role', actionButton('change_role',
      if (is.null(authenticated_role())) '← Change role' else 'Log out / Change role'))
  })

  open_login <- function(role) {
    authenticated_role(NULL)
    active_role('choose')
    requested_role(role)
    login_error('')
    account <- demo_accounts[[role]]
    showModal(modalDialog(
      title = if (role == 'supervisor') 'Supervisor login' else 'Primary responder login',
      p('Use this disposable demo account. It changes whenever the app restarts.'),
      div(class = 'alert-note', strong('Demo username: '), tags$code(account$username), br(),
        strong('Demo password: '), tags$code(account$password)),
      textInput('login_username', 'Username'),
      passwordInput('login_password', 'Password'),
      div(role = 'alert', textOutput('login_error')),
      footer = tagList(actionButton('cancel_login', 'Cancel'),
        actionButton('submit_login', 'Log in', class = 'btn-primary')),
      easyClose = FALSE))
  }

  observeEvent(input$choose_supervisor, open_login('supervisor'))
  observeEvent(input$choose_responder, open_login('responder'))
  observeEvent(input$choose_caller, {
    authenticated_role(NULL)
    requested_role(NULL)
    active_role('caller')
  })
  observeEvent(input$cancel_login, {
    removeModal()
    requested_role(NULL)
    updateTextInput(session, 'login_password', value = '')
  })
  observeEvent(input$submit_login, {
    role <- requested_role()
    req(role %in% names(demo_accounts))
    if (Sys.time() < locked_until()) {
      login_error('Too many attempts. Wait 30 seconds before trying again.')
      return()
    }
    account <- demo_accounts[[role]]
    username <- if (is.null(input$login_username)) '' else trimws(input$login_username)
    password <- if (is.null(input$login_password)) '' else input$login_password
    candidate <- if (nzchar(password)) {
      tryCatch(openssl::bcrypt_pbkdf(password, account$salt, rounds = 16L),
        error = function(e) NULL)
    } else NULL
    valid_hash <- !is.null(candidate) && identical(as.raw(candidate), as.raw(account$hash))
    if (!identical(username, account$username) || !valid_hash) {
      attempts(attempts() + 1L)
      if (attempts() >= 5L) {
        locked_until(Sys.time() + 30)
        attempts(0L)
      }
      login_error('Username or password is incorrect for this role.')
      return()
    }
    attempts(0L)
    authenticated_role(role)
    active_role(role)
    requested_role(NULL)
    updateTextInput(session, 'login_password', value = '')
    removeModal()
  })
  observeEvent(input$change_role, session$reload())

  responder_server(input, output, session, authenticated_role, active_role)

  state <- reactiveValues(feed = NULL, records = NULL, scenario = NULL,
    seed = NULL, ui_tick = 0L, completed_tick = 0L)

  reset_feed <- function() {
    scenario <- if (is.null(input$scenario)) 'stable' else input$scenario
    seed <- if (is.null(input$seed) || is.na(input$seed)) 20261016L else as.integer(input$seed)
    records <- dashboard_load_fixture(repo_root, scenario)
    state$feed <- feed_new(dashboard_build_feed(records, seed))
    state$records <- records
    state$scenario <- scenario
    state$seed <- seed
    state$ui_tick <- state$ui_tick + 1L
    state$completed_tick <- state$completed_tick + 1L
  }
  isolate(reset_feed())

  observeEvent(input$start, {
    req(state$feed)
    feed_start(state$feed)
    state$ui_tick <- state$ui_tick + 1L
  })
  observeEvent(input$pause_resume, {
    req(state$feed)
    if (isTRUE(state$feed$running)) feed_pause(state$feed) else feed_start(state$feed)
    state$ui_tick <- state$ui_tick + 1L
  })
  observeEvent(input$reset, reset_feed())

  observe({
    invalidateLater(500, session)
    req(state$feed)
    before <- nrow(feed_completed(state$feed))
    if (isTRUE(state$feed$running)) {
      feed_advance(state$feed, as.numeric(input$speed) * 0.5)
      if (all(state$feed$processed)) feed_pause(state$feed)
    }
    after <- nrow(feed_completed(state$feed))
    if (after != before) state$completed_tick <- isolate(state$completed_tick) + 1L
    state$ui_tick <- isolate(state$ui_tick) + 1L
  })

  snapshot <- reactive({ state$ui_tick; req(state$feed); feed_snapshot(state$feed) })
  analysis <- reactive({
    state$completed_tick
    req(state$records, state$feed)
    dashboard_analyze(state$records, feed_completed(state$feed), isTRUE(input$demo_review))
  })
  selected_group <- reactive({
    key <- paste(input$zone, input$priority, sep = '/')
    analysis()$groups[[key]]
  })
  selected_monitor_summary <- reactive({ selected_group()$monitoring_summary })

  output$run_status <- renderText({
    s <- snapshot()
    if (isTRUE(s$running)) 'running' else if (all(state$feed$processed)) 'complete' else 'paused'
  })
  output$sim_time <- renderText({
    format(snapshot()$simulated_time, '%b %d, %Y %H:%M:%S %Z', tz = 'America/New_York')
  })
  output$event_progress <- renderText({
    s <- snapshot(); sprintf('%d / %d', s$processed_events, s$total_events)
  })
  output$qc_status <- renderText({ selected_group()$status })
  output$kpi_active <- renderText({ nrow(snapshot()$pending) })
  output$kpi_completed <- renderText({ nrow(snapshot()$completed) })
  output$kpi_median <- renderText({ fmt_num(selected_monitor_summary()$median, 1) })
  output$kpi_p90 <- renderText({ fmt_num(selected_monitor_summary()$p90, 1) })

  output$replay_notice <- renderUI({
    scenario <- if (state$scenario == 'planted') 'Planted delay' else 'Stable process'
    div(class = 'alert-note',
      sprintf('%s scenario · seed %d · %d completed of 405 synthetic monitoring incidents.',
        scenario, state$seed, nrow(snapshot()$completed)))
  })
  output$review_notice <- renderUI({
    group <- selected_group()
    if (isTRUE(group$flags_enabled)) {
      div(class = 'alert-note danger',
        'DEMO-ONLY REVIEW FIXTURE ACTIVE. Synthetic flags are visible for demonstration and are not operational approval.')
    } else {
      div(class = 'alert-note',
        paste('Flags disabled:', group$status,
          '— use the explicitly labeled demo-only control to preview synthetic flags.'))
    }
  })

  output$active_table <- renderTable({
    s <- snapshot(); d <- s$pending
    if (!nrow(d)) return(data.frame(Status = 'No active calls at this simulated time.'))
    elapsed <- pmax(0, as.numeric(difftime(s$simulated_time, d$dispatch_time, units = 'secs')))
    shown <- data.frame(
      Incident = d$incident_id, Neighborhood = d$zone, Priority = d$call_priority,
      Weather = d$weather_condition,
      Status = d$status,
      `Elapsed simulated time` = sprintf('%02d:%02d', floor(elapsed / 60), floor(elapsed %% 60)),
      check.names = FALSE
    )
    head(shown[order(elapsed, decreasing = TRUE), ], 18)
  }, striped = TRUE, hover = TRUE, spacing = 's', rownames = FALSE)

  output$completed_table <- renderTable({
    d <- snapshot()$completed
    if (!nrow(d)) return(data.frame(Status = 'No completed replay incidents yet.'))
    d <- d[order(d$on_scene_time, decreasing = TRUE), , drop = FALSE]
    shown <- data.frame(
      Incident = d$incident_id, Neighborhood = d$zone, Priority = d$call_priority,
      Weather = d$weather_condition,
      `Response (minutes)` = sprintf('%.1f', d$response_minutes),
      `Confirmed on scene` = format(d$on_scene_time, '%b %d %H:%M:%S', tz = 'America/New_York'),
      check.names = FALSE
    )
    head(shown, 12)
  }, striped = TRUE, hover = TRUE, spacing = 's', rownames = FALSE)

  output$i_chart <- renderPlot({
    g <- selected_group(); b <- g$baseline; m <- g$monitoring
    bplot <- data.frame(sequence = seq_len(nrow(b)), value = b$response_minutes,
      phase = 'Baseline', signal = FALSE)
    mplot <- data.frame(sequence = nrow(b) + seq_len(nrow(m)), value = m$response_minutes,
      phase = 'Replay monitoring', signal = !is.na(m$individual_signal) & m$individual_signal)
    pdat <- rbind(bplot, mplot)
    ggplot(pdat, aes(sequence, value)) +
      geom_line(color = '#718096', linewidth = .45) +
      geom_point(aes(color = signal, shape = phase), size = 2) +
      geom_hline(yintercept = g$limits$center, color = '#68b900', linewidth = .7) +
      geom_hline(yintercept = c(g$limits$i_lcl, g$limits$i_ucl),
        color = '#d34747', linetype = 'dashed', linewidth = .65) +
      geom_vline(xintercept = nrow(b) + .5, color = '#36485e', linetype = 'dotted') +
      scale_color_manual(values = c(`FALSE` = '#34383b', `TRUE` = '#d34747'), guide = 'none') +
      scale_shape_manual(values = c(Baseline = 16, `Replay monitoring` = 17)) +
      labs(x = 'Incident sequence', y = 'Response time (minutes)', shape = NULL,
        title = sprintf('%s · %s', g$zone, g$call_priority),
        subtitle = sprintf('Baseline n=%d · replay monitoring n=%d', nrow(b), nrow(m))) +
      theme_minimal(base_size = 11) + theme(legend.position = 'bottom')
  })

  output$mr_chart <- renderPlot({
    g <- selected_group(); b <- g$baseline; m <- g$monitoring
    bmr <- data.frame(sequence = 2:nrow(b), value = g$limits$moving_range,
      phase = 'Baseline', signal = FALSE)
    mmr <- data.frame(sequence = nrow(b) + seq_len(nrow(m)), value = m$moving_range,
      phase = 'Replay monitoring', signal = !is.na(m$variation_signal) & m$variation_signal)
    pdat <- rbind(bmr, mmr)
    ggplot(pdat, aes(sequence, value)) +
      geom_line(color = '#718096', linewidth = .45) +
      geom_point(aes(color = signal, shape = phase), size = 2) +
      geom_hline(yintercept = g$limits$mr_center, color = '#68b900', linewidth = .7) +
      geom_hline(yintercept = g$limits$mr_ucl, color = '#d34747', linetype = 'dashed', linewidth = .65) +
      geom_vline(xintercept = nrow(b) + .5, color = '#36485e', linetype = 'dotted') +
      scale_color_manual(values = c(`FALSE` = '#34383b', `TRUE` = '#d34747'), guide = 'none') +
      scale_shape_manual(values = c(Baseline = 16, `Replay monitoring` = 17)) +
      labs(x = 'Incident sequence', y = 'Moving range (minutes)', shape = NULL,
        title = sprintf('%s · %s', g$zone, g$call_priority),
        subtitle = sprintf('Baseline ranges n=%d · replay ranges n=%d',
          length(g$limits$moving_range), nrow(m))) +
      theme_minimal(base_size = 11) + theme(legend.position = 'bottom')
  })

  output$investigation_table <- renderTable({
    a <- analysis()
    if (!isTRUE(input$demo_review)) {
      return(data.frame(Status = 'Flags disabled until the demo-only review fixture is explicitly enabled.'))
    }
    rows <- lapply(a$groups, function(g) {
      m <- g$monitoring
      hit <- !is.na(m$individual_signal) & m$individual_signal
      if (!any(hit)) return(NULL)
      data.frame(
        Incident = m$incident_id[hit], Neighborhood = g$zone, Priority = g$call_priority,
        Weather = m$weather_condition[hit],
        `Assignment time (New York)` = format(m$dispatch_time[hit], '%Y-%m-%d %H:%M:%S', tz = 'America/New_York'),
        `Response (minutes)` = sprintf('%.2f', m$response_minutes[hit]),
        `Fixed I UCL (minutes)` = sprintf('%.2f', g$limits$i_ucl),
        `Baseline dates` = '2025-01-01 to 2025-12-31', check.names = FALSE
      )
    })
    out <- do.call(rbind, rows)
    if (is.null(out) || !nrow(out)) data.frame(Status = 'No Individuals-UCL signals among completed replay incidents.') else out
  }, striped = TRUE, hover = TRUE, spacing = 's', rownames = FALSE)

  descriptive <- reactive({
    req(input$date_range)
    dashboard_descriptive_records(analysis(), input$date_range[1], input$date_range[2])
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
    ggplot() +
      geom_jitter(data = pts, aes(x, value), width = .12, color = '#68b900', alpha = .2, size = 1.2) +
      geom_segment(data = frame, aes(x = x, xend = x, y = low, yend = high), linewidth = .7, na.rm = TRUE) +
      geom_rect(data = frame, aes(xmin = x - .27, xmax = x + .27, ymin = q1, ymax = q3),
        fill = '#dff0c7', color = '#4f730f', linewidth = .7, na.rm = TRUE) +
      geom_segment(data = frame, aes(x = x - .27, xend = x + .27, y = median, yend = median),
        color = '#d34747', linewidth = 1, na.rm = TRUE) +
      scale_x_continuous(breaks = frame$x, labels = sprintf('%s\n(n=%d)', frame$group, frame$n)) +
      labs(x = NULL, y = 'Response time (minutes)', title = title) +
      theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 18, hjust = 1))
  }

  output$zone_boxplot <- renderPlot({
    d <- descriptive(); d <- d[d$call_priority == input$priority, , drop = FALSE]
    box_plot(d, 'zone', qc_zones,
      sprintf('%s priority · %d completed incidents', input$priority, nrow(d)))
  })
  output$weather_boxplot <- renderPlot({
    d <- descriptive(); d <- d[d$zone == input$zone & d$call_priority == input$priority, , drop = FALSE]
    box_plot(d, 'weather_condition', qc_weather,
      sprintf('%s / %s · %d completed incidents', input$zone, input$priority, nrow(d)))
  })
  output$heatmap <- renderPlot({
    d <- descriptive(); d <- d[d$zone == input$zone & d$call_priority == input$priority, , drop = FALSE]
    grid <- qc_heatmap(d)
    grid$weekday <- factor(grid$weekday, levels = 7:1,
      labels = c('Sun', 'Sat', 'Fri', 'Thu', 'Wed', 'Tue', 'Mon'))
    grid$label <- ifelse(grid$n == 0, '', sprintf('%.1f\nn=%d', grid$median, grid$n))
    ggplot(grid, aes(hour, weekday, fill = median)) +
      geom_tile(color = 'white', linewidth = .25) +
      geom_text(aes(label = label), size = 2.2, color = '#10283c') +
      scale_fill_gradient(low = '#f2f4ef', high = '#89bf35', na.value = '#eceeeb',
        name = 'Median\nminutes') +
      scale_x_continuous(breaks = seq(0, 23, 3)) +
      labs(x = 'Assignment hour (America/New_York)', y = NULL,
        title = sprintf('%s / %s · n=%d', input$zone, input$priority, nrow(d))) +
      theme_minimal(base_size = 10) + theme(panel.grid = element_blank())
  })

  output$validation_table <- renderTable({
    req(state$records)
    v <- qc_validate(state$records, mode = 'completed')
    data.frame(
      Measure = c('Source rows', 'Accepted rows', 'Completed rows', 'Rejected rows',
        'Weather values normalized'),
      Count = c(nrow(state$records), unname(v$counts['accepted']),
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
}

shinyApp(ui, server)
