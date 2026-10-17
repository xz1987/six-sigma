# Adapted from demos/shinyapp/app.R. Statistics will be supplied by H-03.
library(shiny)
# source() supports both repo-root checks and Shiny's app-directory execution.
source(if (file.exists("responder-demo.R")) "responder-demo.R" else "app/responder-demo.R", local = TRUE)
if (!requireNamespace("openssl", quietly = TRUE)) stop("Install the openssl package.")
# Public, disposable demo accounts. Never use these for operational data.
demo_accounts <- lapply(c("supervisor", "responder"), function(role) {
  password <- openssl::base64_encode(openssl::rand_bytes(12))
  salt <- openssl::rand_bytes(16)
  list(username = paste0(role, ".demo"), password = password, salt = salt,
       hash = openssl::bcrypt_pbkdf(password, salt, rounds = 16L))
})
names(demo_accounts) <- c("supervisor", "responder")
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite.")
demo_hospitals <- jsonlite::fromJSON(if (file.exists("www/demo-hospitals.json")) "www/demo-hospitals.json" else "app/www/demo-hospitals.json", simplifyVector = FALSE)$hospitals
hospital_candidates <- function(requirement) {
  matches <- Filter(function(h) requirement %in% h$capabilities, demo_hospitals)
  matches[order(vapply(matches, function(h) h$distance_m, numeric(1)))]
}
brief_item <- function(symbol, label, value) div(class = "brief-item", span(class = "brief-symbol", icon(symbol), `aria-hidden` = "true"), div(span(class = "eyebrow", label), div(class = "brief-value", value)))

required_columns <- c("incident_id", "zone", "call_priority", "dispatch_time", "on_scene_time")
empty_state <- function(title, detail) div(class = "empty", h3(title), p(detail))
metric <- function(label, value, detail) div(class = "metric", span(class = "eyebrow", label), h2(value), p(detail))

ui <- fluidPage(
  tags$head(tags$title("DIIS Emergency Response Monitor"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(name = "theme-color", content = "#135b63"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-title", content = "DIIS EMS"),
    tags$link(rel = "manifest", href = "manifest.webmanifest"),
    tags$link(rel = "icon", type = "image/png", href = "icons/icon-192.png"),
    tags$link(rel = "apple-touch-icon", href = "icons/apple-touch-icon.png"),
    tags$script(src = "pwa.js", defer = NA),
    tags$link(rel = "stylesheet", href = "vendor/leaflet.css"),
    tags$script(src = "vendor/leaflet.js"),
    tags$script(src = "responder-map.js", defer = NA),
    tags$style(HTML("
      .hospital-map-icon span{display:grid;place-items:center;background:white;color:#6941a5;border:2px solid #6941a5;border-radius:5px;width:26px;height:26px;font-weight:800;font-size:17px;box-shadow:0 1px 4px #0003}
      #care_requirement .radio-inline{display:inline-flex;gap:8px;align-items:center;border:1px solid #d4dfe8;border-radius:9px;padding:12px 14px 12px 34px;margin:6px 8px 6px 0;background:#f8fafc}#care_requirement .radio-inline:has(input:checked){background:#e5f1ef;border-color:#135b63;color:#135b63}#care_requirement .radio-inline input{accent-color:#135b63}#care_requirement .radio-inline svg{margin-right:5px}
      .brief-grid{display:grid;grid-template-columns:1fr 1fr;gap:16px;margin-top:16px}.brief-item{display:flex;gap:12px;align-items:flex-start;padding:12px;background:#f5f8fb;border-radius:10px}.brief-symbol{font-size:22px;color:#135b63;padding-top:5px}.brief-value{font-size:16px;font-weight:600;margin-top:4px}.assignment-priority{color:#a13c2f;background:#fff0e9;padding:7px 12px;border-radius:20px;font-weight:700}.hospital-card{padding:12px;border:1px solid #dfe6ef;border-radius:10px;margin:10px 0}.hospital-card:first-child{border-left:4px solid #135b63}.zone-chip{display:inline-flex;align-items:center;gap:6px;font-size:12px;margin:5px 12px 0 0}.zone-swatch{width:14px;height:14px;border:3px solid;display:inline-block;background:white}.zone-label{background:white;border:1px solid #64748b;border-radius:4px;font-weight:700;color:#17293e}.map-controls{display:flex;gap:10px;margin-bottom:10px}@media(max-width:700px){.brief-grid{grid-template-columns:1fr}}
      .suggestion-card{border:1px solid #cedbe4;border-left:4px solid #3b7c85;border-radius:10px;padding:14px 16px;margin:12px 0;background:#f8fbfc}.suggestion-card .form-group{margin:0}.suggestion-card .checkbox{margin:0}.suggestion-card label{line-height:1.55;color:#243c50}.suggestion-card input{accent-color:#135b63;width:17px;height:17px;margin-top:4px}.suggestion-card label span{display:inline-block;padding-left:7px}.suggestion-card:has(input:checked){background:#eaf4ef;border-color:#9bc6b0;border-left-color:#307a57}.suggestion-card:has(input:checked) label{color:#416454}
      .map-legend{background:#f3f6fa;border:1px solid #dfe6ef;border-radius:10px;padding:16px;margin-top:14px}.map-legend-items{display:flex;gap:22px;flex-wrap:wrap;margin:10px 0}.map-legend-item{display:flex;gap:9px;align-items:center;font-size:14px;font-weight:600}.legend-dot{display:inline-block;flex-shrink:0;width:15px;height:15px;border:2px solid white;border-radius:50%;box-shadow:0 0 0 1px #b8c6d4}.legend-responder{background:#135b63}.legend-incident{background:#b54736}.legend-route{display:inline-block;flex-shrink:0;width:30px;height:5px;border-radius:3px;background:#2468c4}.map-legend p{font-size:12px;margin:6px 0 0}.map-legend-title{font-size:11px;font-weight:700;letter-spacing:1.2px;color:#53677d;text-transform:uppercase}
      .responder-grid{display:grid;grid-template-columns:minmax(0,1.25fr) minmax(0,1fr);gap:22px}.call-header{display:flex;justify-content:space-between;align-items:center;gap:15px;flex-wrap:wrap}.call-status{border-radius:18px;background:#e4f0ec;padding:8px 14px;color:#135b63;font-weight:700}.simulation-controls{display:flex;gap:10px;flex-wrap:wrap;margin:16px 0}.map-key{font-size:13px;margin-top:12px}.demo-clock{font-size:32px;font-weight:700}.responder-grid li{margin:12px 0}.incident-label{color:#a13c2f;font-weight:700}.leaflet-container{font-family:inherit;border-radius:12px}.responder-full{max-width:1250px;margin:25px auto}@media(max-width:900px){.responder-grid{grid-template-columns:1fr}}
      [hidden]{display:none!important}
      body{background:#f3f6fa;color:#17293e;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}
      .container-fluid{max-width:1460px;padding:28px 32px}.topbar{display:flex;align-items:center;justify-content:space-between;gap:20px;margin-bottom:26px}
      .brand{display:flex;gap:14px;align-items:center}.logo{background:#135b63;color:white;border-radius:14px;padding:13px;font-size:23px;font-weight:700}
      h1{font-size:25px;font-weight:700;margin:0 0 5px}h2{font-size:30px;font-weight:700;margin:14px 0 8px}h3{font-size:18px;font-weight:650}
      p{color:#617185}.eyebrow{text-transform:uppercase;letter-spacing:1.2px;font-size:11px;font-weight:700;color:#617185}
      .badge-mode{background:#e2edf0;color:#24545e;border-radius:20px;padding:9px 14px;font-weight:600;white-space:nowrap}
      .well,.panel-box,.metric{background:white;border:1px solid #dfe6ef;border-radius:14px;box-shadow:none}
      .well{padding:22px}.panel-box{padding:22px;margin-bottom:20px}.metric{padding:20px;min-height:158px;margin-bottom:20px}
      .metric p{font-size:12px;margin:0}.notice{background:#edf4f7;border-left:4px solid #367482;padding:12px 16px;border-radius:6px;margin-bottom:22px;color:#345460}
      .empty{min-height:175px;display:flex;align-items:center;justify-content:center;flex-direction:column;text-align:center;padding:22px;background:#f8fafc;border:1px dashed #cbd5e1;border-radius:10px}
      .empty p{max-width:560px}.btn{border-radius:8px}.btn-primary{background:#135b63;border-color:#135b63}
      .nav-tabs{margin-bottom:22px}.nav-tabs>li.active>a{color:#135b63;font-weight:700}.form-control{border-radius:7px;box-shadow:none}
      .table-wrap{overflow-x:auto}.table{font-size:13px}.shiny-output-error-validation{color:#596b80}.footer{font-size:12px;color:#617185;margin-top:20px}
      .role-intro{max-width:760px;margin:50px auto 30px;text-align:center}.role-intro h2{font-size:36px}.roles{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:22px;margin:30px auto;max-width:1150px}.role-card{display:flex;flex-direction:column;padding:30px;background:white;border:1px solid #dfe6ef;border-radius:16px;min-height:260px}.role-card p{flex:1;line-height:1.6}.role-card .btn{padding:12px;white-space:normal}.role-number{color:#135b63;font-size:12px;font-weight:700;letter-spacing:2px}.role-workspace{max-width:850px;margin:35px auto}.switch-role{margin-bottom:18px}.role-note{text-align:center;font-size:13px;color:#617185}
      @media(max-width:767px){.roles{grid-template-columns:1fr}.role-intro{margin:20px auto}.role-intro h2{font-size:28px}.role-card{min-height:210px}}
      @media(max-width:767px){.container-fluid{padding:18px 14px}.topbar{align-items:flex-start;flex-direction:column}h1{font-size:22px}.metric{min-height:120px}.panel-box{padding:15px}}
    "))),
  div(class = "topbar", div(class = "brand", div(class = "logo", "+"),
    div(h1("Emergency Response Monitor"), span(class = "eyebrow", "DIIS / Emergency response workspace"))),
    span(class = "badge-mode", "Interface preview")),
  div(id = "connection-status", class = "notice", role = "status", hidden = NA,
    "You're offline. Reconnect to use login, incident records, and response updates."),
  div(class = "pwa-tools", style = "margin-bottom:18px;",
    tags$button(id = "install-app", type = "button", class = "btn btn-primary", hidden = NA, "Install app"),
    tags$details(tags$summary("Add this app to your device"),
      p("iPhone/iPad: use your browser's Share menu → Add to Home Screen. Safari on Mac: File → Add to Dock. Chrome/Edge: use the install option in the address bar or browser menu."),
      p("Live features require an internet connection. This remains a hackathon demo.")),
    tags$span(id = "pwa-setup-status", role = "status")),
  uiOutput("role_navigation"),
  conditionalPanel("output.active_role === 'choose'",
    div(class = "role-intro", span(class = "eyebrow", "Welcome"), h2("How are you joining today?"),
      p("Choose your role to open the right workspace.")),
    div(class = "roles",
      div(class = "role-card", span(class = "role-number", "01 / COORDINATE"), h3("Supervisor"),
        p("Review incident records, compare city zones, and investigate response delays."),
        actionButton("choose_supervisor", "Continue as supervisor", class = "btn-primary")),
      div(class = "role-card", span(class = "role-number", "02 / RESPOND"), h3("Primary responder"),
        p("Access your incident briefing and response status workspace."),
        actionButton("choose_responder", "Continue as primary responder", class = "btn-primary")),
      div(class = "role-card", span(class = "role-number", "03 / REQUEST HELP"), h3("Caller"),
        p("Get information about requesting emergency medical services. No account needed."),
        actionButton("choose_caller", "Continue as caller", class = "btn-primary"))),
    p(class = "role-note", "Hackathon demo · Staff login required. Caller access is open.")),
  uiOutput("responder_workspace"),
  conditionalPanel("output.active_role === 'caller'",
    div(class = "role-workspace panel-box", span(class = "eyebrow", "Caller / Prototype"), h2("Emergency medical help"),
      div(class = "notice", "This prototype cannot contact EMS or dispatch an ambulance. For an actual emergency in the U.S., call 911."),
      empty_state("Caller experience coming next", "No request has been sent. This screen does not collect patient information or track an ambulance."))),
  uiOutput("supervisor_workspace")
)

supervisor_ui <- function() tagList(
  div(class = "notice", "Start with incident records. Analysis and investigation signals will appear after the statistical engine is verified."),
  sidebarLayout(
    sidebarPanel(width = 3,
      h3("Your incident data"),
      fileInput("incidents", "Upload incident CSV", accept = ".csv", buttonLabel = "Choose CSV"),
      uiOutput("upload_status"), tags$hr(),
      selectInput("zone", "Incident zone", "All zones"),
      selectInput("priority", "Call priority", "All priorities"),
      tags$hr(), h3("Analysis setup"),
      dateRangeInput("baseline", "Proposed baseline", start = "2026-01-01", end = "2026-06-30"),
      helpText("Baseline settings are captured for the future analysis engine. They do not filter this record preview."),
      downloadButton("download", "Export filtered preview"),
      tags$hr(), p(class = "footer", "Uploaded records stay in this session. This interface is not connected to a live dispatch system.")
    ),
    mainPanel(width = 9,
      fluidRow(column(4, uiOutput("record_count")),
        column(4, metric("Response reliability", "Pending", "Median and P90 await verified calculations")),
        column(4, metric("Investigation status", "Not evaluated", "No flags have been calculated"))),
      tabsetPanel(id = "view",
        tabPanel("Overview",
          div(class = "panel-box", h3("Where should we investigate?"),
            empty_state("Zone monitoring is being prepared", "Zone and priority comparisons will show sample counts, fixed baseline limits, and evidence for each signal.")),
          div(class = "panel-box", h3("Response time over time"),
            empty_state("Control charts pending", "Individuals and moving-range charts will appear here after H-03 verification. No signal does not prove acceptable service."))),
        tabPanel("Incident records", div(class = "panel-box", h3("Incident record preview"),
          p("Filtered by zone and priority. Showing the first 100 matching rows; export includes all matching rows. Records have not passed full validation."),
          div(class = "table-wrap", tableOutput("records")))),
        tabPanel("Data checks", div(class = "panel-box", h3("Upload readiness"), uiOutput("checks"),
          h3("Expected columns"), tags$code(paste(required_columns, collapse = ", ")),
          p("Use ISO 8601 timestamps with a UTC offset. Duplicate IDs, invalid timestamps, missing values, and negative durations will be checked by the validation engine."))),
        tabPanel("How to read this", div(class = "panel-box", h3("A signal starts an investigation"),
          p("A response above a group's fixed upper control limit is unusual relative to that baseline. It does not establish a cause, staff fault, or clinical outcome."),
          tags$ul(tags$li("Compare incidents within the same zone and priority."),
            tags$li("Control limits describe historical behavior, not service targets."),
            tags$li("Insufficient data and unsuitable baselines must remain visible.")),
          p("This first interface contains no statistical calculations or live operational controls.")))
      ), div(class = "footer", "Emergency Response Quality Monitor · Cornell Six Sigma Hackathon · DIIS")
    )
  )
)

server <- function(input, output, session) {
  active_role <- reactiveVal("choose")
  authenticated_role <- reactiveVal(NULL)
  requested_role <- reactiveVal(NULL)
  login_error <- reactiveVal("")
  attempts <- reactiveVal(0L)
  locked_until <- reactiveVal(as.POSIXct(0, origin = "1970-01-01"))
  output$login_error <- renderText(login_error())
  open_login <- function(role) {
    authenticated_role(NULL)
    active_role("choose")
    requested_role(role)
    login_error("")
    account <- demo_accounts[[role]]
    showModal(modalDialog(
      title = if (role == "supervisor") "Supervisor login" else "Primary responder login",
      p("Use this pre-created demo account. Credentials are public for the hackathon and change when the app restarts."),
      div(class = "notice", strong("Demo username: "), tags$code(account$username), tags$br(),
          strong("Demo password: "), tags$code(account$password)),
      textInput("login_username", "Username"),
      passwordInput("login_password", "Password"),
      div(role = "alert", textOutput("login_error")),
      footer = tagList(actionButton("cancel_login", "Cancel"), actionButton("submit_login", "Log in", class = "btn-primary")),
      easyClose = FALSE))
  }
  observeEvent(input$choose_supervisor, open_login("supervisor"))
  observeEvent(input$choose_responder, open_login("responder"))
  observeEvent(input$cancel_login, {
    removeModal(); requested_role(NULL); updateTextInput(session, "login_password", value = "")
  })
  observeEvent(input$submit_login, {
    role <- requested_role()
    req(role %in% names(demo_accounts))
    if (Sys.time() < locked_until()) {
      login_error("Too many attempts. Wait 30 seconds before trying again."); return()
    }
    account <- demo_accounts[[role]]
    username <- if (is.null(input$login_username)) "" else trimws(input$login_username)
    password <- if (is.null(input$login_password)) "" else input$login_password
    candidate <- openssl::bcrypt_pbkdf(password, account$salt, rounds = 16L)
    if (!identical(username, account$username) || !identical(as.raw(candidate), as.raw(account$hash))) {
      attempts(attempts() + 1L)
      if (attempts() >= 5L) { locked_until(Sys.time() + 30); attempts(0L) }
      login_error("Username or password is incorrect for this role.")
      updateTextInput(session, "login_password", value = "")
      return()
    }
    attempts(0L); authenticated_role(role); active_role(role); requested_role(NULL)
    updateTextInput(session, "login_password", value = ""); removeModal()
  })
  observeEvent(input$choose_caller, {
    authenticated_role(NULL); requested_role(NULL); active_role("caller")
  })
  # Reload ends the authenticated session and discards uploaded data.
  observeEvent(input$change_role, session$reload())
  output$active_role <- renderText(active_role())
  outputOptions(output, "active_role", suspendWhenHidden = FALSE)
  output$role_navigation <- renderUI({
    if (active_role() == "choose") return(NULL)
    div(class = "switch-role", actionButton("change_role",
      if (is.null(authenticated_role())) "← Change role" else "Log out / Change role"))
  })
  output$supervisor_workspace <- renderUI({
    req(identical(authenticated_role(), "supervisor"), active_role() == "supervisor")
    supervisor_ui()
  })
  transport_phase <- reactiveVal("scene")
  transport_elapsed <- reactiveVal(0)
  chosen_hospital <- reactiveVal(NULL)
  hospital_arrival <- reactiveVal(NULL)
  clear_transport <- function() {
    transport_phase("scene"); transport_elapsed(0); chosen_hospital(NULL); hospital_arrival(NULL)
  }
  demo_started <- reactiveVal(FALSE)
  demo_running <- reactiveVal(FALSE)
  demo_elapsed <- reactiveVal(0)
  demo_scenario <- reactiveVal("Rain + rush hour")
  observeEvent(input$simulate_call, {
    req(identical(authenticated_role(), "responder"))
    demo_scenario(if (is.null(input$demo_conditions)) "Rain + rush hour" else input$demo_conditions)
    for (i in 1:3) updateCheckboxInput(session, paste0("delay_tip_", i), value = FALSE)
    clear_transport()
    demo_elapsed(0); demo_started(TRUE); demo_running(TRUE)
  })
  observeEvent(input$pause_call, {
    req(identical(authenticated_role(), "responder"), demo_started(), demo_elapsed() < 720)
    demo_running(!demo_running())
  })
  observeEvent(input$reset_call, {
    req(identical(authenticated_role(), "responder"))
    clear_transport()
    demo_running(FALSE); demo_started(FALSE); demo_elapsed(0)
  })
  observeEvent(input$advance_scene, {
    req(identical(authenticated_role(), "responder"), demo_started())
    demo_elapsed(720); demo_running(FALSE)
  })
  observeEvent(input$begin_transport, {
    req(identical(authenticated_role(), "responder"), demo_started(), transport_phase() == "scene")
    if (demo_elapsed() < 720) {showNotification("Reach the scene first, or use Advance to scene in this demo.", type="message"); return()}
    candidates <- hospital_candidates(input$care_requirement)
    selected <- Filter(function(h) identical(h$id,input$hospital_choice), candidates)
    if (!length(selected)) {showNotification("No matching demo hospital selected.",type="error");return()}
    chosen_hospital(selected[[1]]);transport_elapsed(0);transport_phase("transport")
  })
  observeEvent(input$arrived_hospital, {
    req(identical(authenticated_role(), "responder"), transport_phase() == "transport")
    if (transport_elapsed() < 480) {showNotification("The simulated transport has not reached the hospital yet.",type="message");return()}
    transport_phase("hospital"); hospital_arrival(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  })
  observe({
    invalidateLater(1000, session)
    if (identical(isolate(authenticated_role()), "responder") && isolate(transport_phase()) == "transport" && isolate(transport_elapsed()) < 480) {
      transport_elapsed(min(480,isolate(transport_elapsed())+30))
    }
    if (isTRUE(isolate(demo_running())) && identical(isolate(authenticated_role()), "responder")) {
      next_time <- min(720, isolate(demo_elapsed()) + 30)
      demo_elapsed(next_time)
      if (next_time >= 720) demo_running(FALSE)
    }
  })
  output$responder_workspace <- renderUI({
    req(identical(authenticated_role(), "responder"), active_role() == "responder")
    div(class = "responder-full",
      div(class = "notice", "SYNTHETIC DEMO · Fictional incident, traffic, weather, and responder positions. No live dispatch, routing, or validated time savings."),
      div(class = "panel-box", h2("Responder workspace"),
        selectInput("demo_conditions", "Choose simulated conditions", c("Rain + rush hour", "Heavy rain + rush hour", "Dry + light traffic", "Snow/ice + road closure")),
        actionButton("simulate_call", "Simulate incoming call", class = "btn-primary"),
        p(class = "footer", "Replay runs at 30 simulated seconds per real second. Conditions are fixed when the call starts.")),
      uiOutput("active_call")
    )
  })
  output$active_call <- renderUI({
    req(identical(authenticated_role(), "responder"))
    if (!demo_started()) return(empty_state("Ready for a demo call", "Choose conditions above, then simulate an incoming call."))
    # Do not rebuild map or controls on every clock tick.
    d <- responder_demo(0, demo_scenario())
    tagList(
      div(class = "panel-box", div(class = "call-header",
        div(span(class = "eyebrow", d$incident_id), h3(tagList(icon("ambulance"), " New assignment"))),
        span(class = "assignment-priority", icon("exclamation-triangle"), " High · demo"), uiOutput("call_status")),
        div(class = "brief-grid",
          brief_item("phone", "Reason for call", "Reported fall · EMS assessment requested"),
          brief_item("map-marker-alt", "Scene", "Near Astoria Park · Queens (synthetic)"),
          brief_item("ambulance", "Assigned unit", paste(d$unit, "·", d$zone)),
          brief_item("door-open", "Access", "East-side park entrance · confirm with dispatch")),
        div(class = "simulation-controls", actionButton("pause_call", "Pause / Resume", icon=icon("pause")),
          actionButton("advance_scene", "Advance to scene", icon=icon("forward")), actionButton("reset_call", "Reset", icon=icon("undo"))),
        uiOutput("call_clock")),
      uiOutput("hospital_panel"),
      div(class = "responder-grid",
        div(class = "panel-box", h3("Incident map"),
          div(class="map-controls",tags$button(id="show-all-zones",type="button",class="btn btn-default","All 5 zones"),tags$button(id="focus-route",type="button",class="btn btn-default","Focus route")),
          div(id = "responder-map", role = "region", `aria-label` = "Interactive map of synthetic responder and incident locations", style = "height:380px;width:100%;"),
          p(id = "map-status", role = "status"), p(id = "map-zones-status", role = "status"),
          div(class = "map-legend", role = "group", `aria-label` = "Map legend",
            div(class = "map-legend-title", "Map legend"),
            div(class = "map-legend-items",
              div(class = "map-legend-item", span(class = "legend-dot legend-responder", `aria-hidden` = "true"), "Responder · Q-12"),
              div(class = "map-legend-item", span(class = "legend-dot legend-incident", `aria-hidden` = "true"), "Incident destination"),
              div(class = "map-legend-item", span(class = "legend-route", `aria-hidden` = "true"), "Driving route")),
            div(lapply(seq_along(c("Astoria","Flushing","Jamaica","Forest Hills","Far Rockaway")),function(i) {
              colors <- c("#7c3aed","#b45309","#be185d","#15803d","#334155")
              div(class="zone-chip",span(class="zone-swatch",style=paste0("border-color:",colors[i]),`aria-hidden`="true"),c("Astoria","Flushing","Jamaica","Forest Hills","Far Rockaway")[i])
            })),
            p("Solid borders: app zone groupings of NYC statistical areas; not official EMS districts."),
            p("H markers: fictional demo hospitals. Drag to pan · + / − to zoom · Select markers for details."),
            p("Simulated responder position · Road-route snapshot excludes live traffic and weather."))),
        div(class = "panel-box", h3("Reduce avoidable delays"),
          span(class = "eyebrow", "Scenario-based coordination prompts"),
          h3("Traffic"), p(d$traffic), h3("Weather"), div(style = "display:flex;align-items:center;gap:12px;", weather_icon(d$weather_icon), p(d$weather)),
          p(class = "footer", "Check off coordination steps as you review them. Resets with each demo call."),
          div(class = "suggestion-list", lapply(seq_along(d$tips), function(i) {
            div(class = "suggestion-card", checkboxInput(paste0("delay_tip_", i), d$tips[[i]], value = FALSE))
          })),
          p(class = "footer", "These prompts come from the selected demo scenario. No live conditions, route optimization, or estimated minutes saved are calculated.")))
    )
  })
  output$call_status <- renderUI({
    req(identical(authenticated_role(), "responder"), demo_started())
    d <- responder_demo(demo_elapsed(), demo_scenario())
    label <- if (transport_phase() == "hospital") "At hospital" else if (transport_phase() == "transport") "Transporting to hospital" else paste(d$status, if (!demo_running() && d$elapsed < 720) "· Paused" else "")
    span(class = "call-status", label)
  })
  output$call_clock <- renderUI({
    req(identical(authenticated_role(), "responder"), demo_started())
    d <- responder_demo(demo_elapsed(), demo_scenario())
    tagList(span(class = "eyebrow", "Scene response · simulated"),
      div(class = "demo-clock", sprintf("%02d:%02d", d$elapsed %/% 60, d$elapsed %% 60)),
      p(if (d$status == "On scene") "Scene arrival recorded · clock stopped" else "30× demo replay"))
  })
  output$hospital_panel <- renderUI({
    req(identical(authenticated_role(), "responder"), demo_started())
    div(class="panel-box",h3(tagList(icon("hospital")," Hospital destination")),
      p("Fictional facilities near the synthetic scene. Capabilities are invented; crew/dispatch must confirm the destination. No clinical triage is performed."),
      if (transport_phase() == "scene") tagList(
        tags$label(`for`="care_requirement", class="control-label", tagList(icon("briefcase-medical"), " Care requirement · demo")),
        radioButtons("care_requirement",label=NULL,choiceNames=list(
          tagList(icon("bandage"), " Trauma assessment"),
          tagList(icon("briefcase-medical"), " Emergency assessment"),
          tagList(icon("heart-pulse"), " Cardiac care"),
          tagList(icon("child"), " Pediatric assessment"),
          tagList(icon("brain"), " Stroke care")),
          choiceValues=c("Trauma assessment","Emergency assessment","Cardiac care","Pediatric assessment","Stroke care"),selected="Trauma assessment",inline=TRUE),
        uiOutput("hospital_candidates_ui")) else tagList(
          h3(chosen_hospital()$name),p(paste("Demo capabilities:",paste(chosen_hospital()$capabilities,collapse=", "))),
          uiOutput("transport_progress")))
  })
  output$hospital_candidates_ui <- renderUI({
    req(identical(authenticated_role(), "responder"), demo_started())
    candidates <- hospital_candidates(input$care_requirement)
    if (!length(candidates)) return(div(class="notice","No demo facility matches this requirement. Ask dispatch to resolve; no destination is assigned."))
    labels <- vapply(candidates,function(h) sprintf("%s · %.2f miles by road",h$name,h$distance_m/1609.344),character(1))
    ids <- vapply(candidates,function(h) h$id,character(1))
    tagList(div(lapply(seq_along(candidates),function(i) {
      h<-candidates[[i]]
      div(class="hospital-card",strong(paste0(if(i==1) "Closest matching demo option: " else "Also matches: ",h$name)),
          p(sprintf("%.2f miles from scene by road · %s",h$distance_m/1609.344,paste(h$capabilities,collapse=" · "))))
    })),selectInput("hospital_choice","Confirm destination",choices=setNames(ids,labels)),
    actionButton("begin_transport","Confirm & begin trip",icon=icon("route"),class="btn-primary"),
    p(class="footer","Available after scene arrival. Distance uses the road snapshot; real capacity, diversion, travel conditions, and clinical suitability are unknown."))
  })
  output$transport_progress <- renderUI({
    req(identical(authenticated_role(), "responder"), !is.null(chosen_hospital()))
    if (transport_phase() == "hospital") return(div(class="notice",icon("check-circle")," Hospital arrival confirmed · ",hospital_arrival()," (local demo time)"))
    tagList(p(sprintf("Transport replay: %02d:%02d / 08:00 simulated",transport_elapsed()%/%60,transport_elapsed()%%60)),
      p(if(transport_elapsed()>=480) "At demo destination — confirm hospital arrival." else "Fictional 8-minute transport replay, not a predicted travel time."),
      actionButton("arrived_hospital","Arrived at hospital",icon=icon("hospital"),class="btn-primary"))
  })
  observe({
    req(identical(authenticated_role(), "responder"), demo_started())
    elapsed <- demo_elapsed(); phase <- transport_phase(); travel <- transport_elapsed(); hospital <- chosen_hospital()
    session$onFlushed(function() {
      session$sendCustomMessage("responder-map", list(elapsed = elapsed, phase = phase, travel = travel,
        hospital = if (is.null(hospital)) NULL else hospital$id))
    }, once = TRUE)
  })
  upload <- reactive({
    req(identical(authenticated_role(), "supervisor"))
    if (is.null(input$incidents)) return(list(data = NULL, error = NULL))
    tryCatch({
      d <- read.csv(input$incidents$datapath, stringsAsFactors = FALSE, check.names = FALSE,
                    colClasses = "character")
      if (anyDuplicated(names(d))) stop("Column names must be unique.")
      missing <- setdiff(required_columns, names(d))
      if (length(missing)) stop(paste("Missing columns:", paste(missing, collapse = ", ")))
      list(data = d[, required_columns, drop = FALSE], error = NULL)
    }, error = function(e) list(data = NULL, error = conditionMessage(e)))
  })
  observeEvent(upload(), {
    d <- upload()$data
    zones <- if (is.null(d)) character() else sort(unique(d$zone[!is.na(d$zone) & nzchar(d$zone)]))
    priorities <- if (is.null(d)) character() else sort(unique(d$call_priority[!is.na(d$call_priority) & nzchar(d$call_priority)]))
    updateSelectInput(session, "zone", choices = c("All zones", zones), selected = "All zones")
    updateSelectInput(session, "priority", choices = c("All priorities", priorities), selected = "All priorities")
  })
  filtered <- reactive({
    d <- upload()$data
    if (is.null(d)) return(NULL)
    if (!is.null(input$zone) && input$zone != "All zones") d <- d[!is.na(d$zone) & d$zone == input$zone, , drop = FALSE]
    if (!is.null(input$priority) && input$priority != "All priorities") d <- d[!is.na(d$call_priority) & d$call_priority == input$priority, , drop = FALSE]
    d
  })
  output$upload_status <- renderUI({
    if (!is.null(upload()$error)) return(div(role = "alert", upload()$error))
    if (is.null(upload()$data)) return(helpText("No file uploaded yet."))
    helpText("Columns recognized. Full record validation pending.")
  })
  output$record_count <- renderUI({
    d <- filtered()
    metric("Records in preview", if (is.null(d)) "—" else format(nrow(d), big.mark = ","), "Uploaded rows, not validated incidents")
  })
  output$records <- renderTable({
    validate(need(is.null(upload()$error), upload()$error), need(!is.null(filtered()), "Upload a CSV to preview incident records."))
    head(filtered(), 100)
  }, striped = TRUE, bordered = FALSE, rownames = FALSE)
  output$checks <- renderUI({
    if (!is.null(upload()$error)) return(div(role = "alert", upload()$error))
    if (is.null(upload()$data)) return(p("Upload a CSV to check its column names."))
    tagList(p("Required columns: present."), p("Record-level validity: not evaluated."), p("Baseline suitability: not evaluated."))
  })
  output$download <- downloadHandler(filename = function() "incident-preview.csv", content = function(file) {
    req(identical(authenticated_role(), "supervisor"), filtered())
    write.csv(filtered(), file, row.names = FALSE, na = "")
  })
}

shinyApp(ui = ui, server = server)
