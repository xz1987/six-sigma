library(shiny)

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

supervisor_path <- if (file.exists('supervisor.R')) 'supervisor.R' else file.path('app', 'supervisor.R')
source(supervisor_path, local = TRUE)
source(file.path(repo_root, 'app', 'responder-demo.R'), local = TRUE)
source(file.path(repo_root, 'app', 'responder-server.R'), local = TRUE)
source(file.path(repo_root, 'app', 'caller', 'caller.R'), local = TRUE)
shiny::addResourcePath('caller-assets', normalizePath(file.path(repo_root, 'app', 'caller', 'www')))
source(file.path(repo_root, 'app', 'eta.R'), local=TRUE)
source(file.path(repo_root, 'app', 'shared-incidents.R'), local=TRUE)
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite.")
demo_hospitals <- jsonlite::fromJSON(file.path(repo_root, "app", "www", "demo-hospitals.json"), simplifyVector = FALSE)$hospitals
hospital_candidates <- function(requirement) {
  matches <- Filter(function(h) requirement %in% h$capabilities, demo_hospitals)
  matches[order(vapply(matches, function(h) h$distance_m, numeric(1)))]
}
brief_item <- function(symbol, label, value) div(class = "brief-item", span(class = "brief-symbol", icon(symbol), `aria-hidden` = "true"), div(span(class = "eyebrow", label), div(class = "brief-value", value)))


empty_state <- function(title, detail) div(class = "empty", h3(title), p(detail))



# Process-level stream shared by Caller, Responder, and Supervisor sessions.
# It is intentionally in-memory until the production response backend is connected.
production_response_records <- reactiveVal(response_metric_empty_records())
eta_bundle <- tryCatch(eta_validate(read.csv(file.path(repo_root,'data/eta/v1/weather_effect.csv'),stringsAsFactors=FALSE)),
  error=function(e)eta_unavailable('Validated synthetic ETA fixture unavailable'))
route_json <- jsonlite::fromJSON(file.path(repo_root,'app/www/demo-road-route.json'))
shared_route <- as.data.frame(route_json$geometry$coordinates)
names(shared_route) <- c('x','y')
hospital_routes <- setNames(lapply(c('a','b','c'),function(id) {
  feature <- jsonlite::fromJSON(file.path(repo_root,'app/www',paste0('hospital-route-',id,'.json')))
  points <- as.data.frame(feature$geometry$coordinates);names(points) <- c('x','y');points
}),c('a','b','c'))
incident_service <- shared_new(eta_bundle, shared_route, publish=production_response_records,hospital_routes=hospital_routes)

ui <- fluidPage(
  tags$head(
    tags$title('DIIS Emergency Response Monitor'),
    tags$meta(name = 'viewport', content = 'width=device-width, initial-scale=1'),
    tags$meta(name = 'theme-color', content = '#3B0A0A'),
    tags$meta(name = 'apple-mobile-web-app-capable', content = 'yes'),
    tags$meta(name = 'apple-mobile-web-app-title', content = 'DIIS EMS'),
    tags$link(rel = 'manifest', href = 'manifest.webmanifest'),
    tags$link(rel = 'icon', type = 'image/png', href = 'icons/icon-192.png'),
    tags$link(rel = 'apple-touch-icon', href = 'icons/apple-touch-icon.png'),
    tags$script(src = 'pwa.js', defer = NA),
    tags$script(src = 'navigation.js', defer = NA),
    tags$link(rel = 'stylesheet', href = 'vendor/leaflet.css'),
    tags$link(rel = 'stylesheet', href = 'responder.css'),
    tags$script(src = 'vendor/leaflet.js'),
    tags$script(src = 'responder-map.js', defer = NA),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'supervisor.css')
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
        p('Track a synthetic assigned incident using its private link. This demonstration never contacts EMS.'),
        actionButton('choose_caller', 'Continue as caller', class = 'btn-primary'))),
    p(class = 'role-note', 'Hackathon demonstration · Staff roles use disposable credentials shown at login.')),
  uiOutput('responder_workspace'),
  conditionalPanel("output.active_role === 'caller'",
    div(class='alert-note danger','Synthetic demo. This app cannot contact EMS. For an actual emergency in the U.S., call 911.'),
    uiOutput('dispatchPreview'), caller_ui('caller', asset_prefix='caller-assets/', include_leaflet=FALSE)),
  conditionalPanel("output.active_role === 'supervisor'", supervisor_ui('supervisor'))
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
  observeEvent(input$change_role, {
    authenticated_role(NULL)
    requested_role(NULL)
    active_role('choose')
    session$sendCustomMessage('diis-return-home', list())
  })

  responder_server(input, output, session, authenticated_role, active_role, incident_service)
  created_dispatch <- reactiveVal(NULL)
  observeEvent(input$createDispatch, {
    req(active_role() == 'caller')
    created_dispatch(shared_create(incident_service))
  })
  observeEvent(input$trackingLinkOpened, {
    if (isTRUE(input$trackingLinkOpened)) {
      authenticated_role(NULL); requested_role(NULL); active_role('caller')
    }
  })
  output$dispatchPreview <- renderUI({
    if (active_role() != 'caller' || isTRUE(input$trackingLinkOpened)) return(NULL)
    created <- created_dispatch()
    div(class='caller-demo-bar', strong('DEMO DISPATCH'),
      p('Open your private tracking link. For this demo, create a synthetic dispatch below.'),
      actionButton('createDispatch','Create demo tracking link',class='btn-primary'),
      if(!is.null(created)) tagList(tags$br(),tags$a(class='btn btn-primary',href=created$fragment,'Open caller tracking'),
        p('Private demo link expires in 30 minutes. No message or EMS request is sent.')))
  })
  caller_server('caller', lookup=function(token)shared_lookup(incident_service,token))
  supervisor_server('supervisor', production_records = production_response_records)
}

shinyApp(ui, server)
