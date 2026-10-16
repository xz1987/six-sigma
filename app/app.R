# Adapted from demos/shinyapp/app.R. Statistics will be supplied by H-03.
library(shiny)
if (!requireNamespace("openssl", quietly = TRUE)) stop("Install the openssl package.")
# Public, disposable demo accounts. Never use these for operational data.
demo_accounts <- lapply(c("supervisor", "responder"), function(role) {
  password <- openssl::base64_encode(openssl::rand_bytes(12))
  salt <- openssl::rand_bytes(16)
  list(username = paste0(role, ".demo"), password = password, salt = salt,
       hash = openssl::bcrypt_pbkdf(password, salt, rounds = 16L))
})
names(demo_accounts) <- c("supervisor", "responder")

required_columns <- c("incident_id", "zone", "call_priority", "dispatch_time", "on_scene_time")
empty_state <- function(title, detail) div(class = "empty", h3(title), p(detail))
metric <- function(label, value, detail) div(class = "metric", span(class = "eyebrow", label), h2(value), p(detail))

ui <- fluidPage(
  tags$head(tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
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
  output$responder_workspace <- renderUI({
    req(identical(authenticated_role(), "responder"), active_role() == "responder")
    div(class = "role-workspace panel-box", span(class = "eyebrow", "Primary responder / Demo account"),
      h2("Your incident workspace"), empty_state("No incident assigned", "Incident briefings and response status controls are planned. This prototype has no live dispatch connection."))
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
