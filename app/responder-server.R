responder_server <- function(input, output, session, authenticated_role, active_role, incident_service = NULL) {
  if(is.null(incident_service$ui_state)) incident_service$ui_state <- new_shared_responder_state()
  ui_state <- incident_service$ui_state
  state_value <- function(name) function(value) {
    if(missing(value)) ui_state[[name]] else ui_state[[name]] <- value
  }
  shared_id <- state_value('incident_id')
  active_hospitals <- function() {
    d <- incident_service$incidents[[shared_id()]]
    if(is.null(d$zone_fixture)) demo_hospitals else d$zone_fixture$hospitals
  }
  shared_link <- state_value('incident_link')
  caller_tracking_link <- state_value('caller_tracking_link')
  queued_ids <- reactivePoll(1000,session,
    checkFunc=function() {
      if(is.null(incident_service)) return('')
      paste(names(Filter(function(d)!isTRUE(d$claimed),incident_service$incidents)),collapse='|')
    },
    valueFunc=function() {
      if(is.null(incident_service)) return(character())
      names(Filter(function(d)!isTRUE(d$claimed),incident_service$incidents))
    })
  output$shared_queue <- renderUI({
    req(identical(authenticated_role(), 'responder'))
    if(is.null(incident_service)) return(NULL)
    ids <- queued_ids()
    previous <- isolate(input$shared_incident)
    selected <- if(length(ids)) ids[[1]] else '__new__'
    if(!is.null(previous) && previous %in% ids) selected <- previous
    labels <- vapply(ids,function(id)paste(incident_service$incidents[[id]]$zone,'· waiting caller ·',id),character(1))
    tagList(selectInput('shared_incident','Caller incident to receive',
      choices=c(setNames(ids,labels),'Create a separate synthetic incident'='__new__'),selected=selected),
      p(class='footer',if(length(ids))'A waiting caller is selected. Receive this incident to update that caller’s open window.' else
        'No waiting caller incidents. Create a new incident, then open its private caller link.'))
  })
  output$shared_tracking <- renderUI({
    req(identical(authenticated_role(), 'responder'))
    link <- shared_link()
    if(is.null(link)) return(NULL)
    tagList(p('Private caller link · expires 30 minutes after assignment'),
      tags$a(href=link$fragment,target='_blank',rel='noopener',class='btn btn-primary','Open this incident’s caller tracking'))
  })
  output$shared_eta <- renderUI({
    req(identical(authenticated_role(), 'responder'),!is.null(shared_id()))
    demo_elapsed()
    shared_eta_card(incident_service$incidents[[shared_id()]])
  })
  transport_phase <- state_value('transport_phase')
  transport_elapsed <- state_value('transport_elapsed')
  chosen_hospital <- state_value('chosen_hospital')
  hospital_arrival <- state_value('hospital_arrival')
  clear_transport <- function() {
    transport_phase("scene"); transport_elapsed(0); chosen_hospital(NULL); hospital_arrival(NULL); caller_tracking_link(NULL)
  }
  demo_started <- state_value('demo_started')
  demo_running <- state_value('demo_running')
  demo_elapsed <- state_value('demo_elapsed')
  demo_scenario <- state_value('demo_scenario')
  observeEvent(input$simulate_call, {
    req(identical(authenticated_role(), "responder"))
    if(!is.null(shared_id()) && !is.null(incident_service$incidents[[shared_id()]]) &&
       is.null(incident_service$incidents[[shared_id()]]$hospital_arrived_at)) {
      showNotification('Finish or reset the current incident before receiving another.',type='message'); return()
    }
    if(!is.null(incident_service)) {
      selected <- input$shared_incident
      # Also handle the brief period before a refreshed dropdown reaches the client.
      if(is.null(selected)||!nzchar(selected)) {
        waiting <- names(Filter(function(d)!isTRUE(d$claimed),incident_service$incidents))
        selected <- if(length(waiting)) waiting[[1]] else '__new__'
      }
      if(identical(selected,'__new__')) {
        link <- shared_create(incident_service, if(is.null(input$demo_conditions)) 'Rain + rush hour' else input$demo_conditions)
      } else {
        if(is.null(incident_service$incidents[[selected]])||isTRUE(incident_service$incidents[[selected]]$claimed)) {
          showNotification('Incident was already assigned. Select another.',type='message'); return()
        }
        grant <- Filter(function(g)identical(g$incident_id,selected),incident_service$grants)
        link <- list(incident_id=selected,fragment=if(length(grant))paste0('#incident=',grant[[1]]$token) else '')
      }
      d <- shared_claim(incident_service,link$incident_id)
      shared_id(d$incident_id);shared_link(link)
      ui_state$started_at <- d$dispatched_at
      ui_state$last_tick <- Sys.time()
    }
    demo_scenario(if (is.null(input$demo_conditions)) "Rain + rush hour" else input$demo_conditions)
    if(!is.null(incident_service)) demo_scenario(d$scenario)
    for (i in 1:3) updateCheckboxInput(session, paste0("delay_tip_", i), value = FALSE)
    clear_transport()
    demo_elapsed(0); demo_started(TRUE); demo_running(TRUE)
  })
  observeEvent(input$pause_call, {
    req(identical(authenticated_role(), "responder"), demo_started(), demo_elapsed() < 720)
    demo_running(!demo_running())
  })
  reset_trip <- function() {
    req(identical(authenticated_role(), "responder"))
    if(!is.null(incident_service)&&!is.null(shared_id())) {
      shared_revoke(incident_service,shared_id())
    }
    shared_id(NULL);shared_link(NULL)
    clear_transport()
    demo_running(FALSE); demo_started(FALSE); demo_elapsed(0)
  }
  observeEvent(input$reset_call, reset_trip())
  observeEvent(input$new_demo_call, reset_trip())
  observeEvent(input$advance_scene, {
    req(identical(authenticated_role(), "responder"), demo_started())
    demo_elapsed(720); demo_running(FALSE)
    if(!is.null(shared_id())) shared_advance(incident_service,shared_id(),720)
  })
  observeEvent(input$begin_transport, {
    req(identical(authenticated_role(), "responder"), demo_started(), transport_phase() == "scene")
    if (demo_elapsed() < 720) {showNotification("Reach the scene first, or use Advance to scene in this demo.", type="message"); return()}
    candidates <- hospital_candidates(input$care_requirement, active_hospitals())
    selected <- Filter(function(h) identical(h$id,input$hospital_choice), candidates)
    if (!length(selected)) {showNotification("No matching demo hospital selected.",type="error");return()}
    if(!is.null(shared_id())) shared_transport(incident_service,shared_id(),selected[[1]])
    chosen_hospital(selected[[1]]);transport_elapsed(0);transport_phase("transport")
    caller_tracking_link(shared_link()$fragment)
    showNotification("Destination confirmed and sent to this incident’s caller.",type="message")
  })
  observeEvent(input$arrived_hospital, {
    req(identical(authenticated_role(), "responder"), transport_phase() == "transport")
    if (transport_elapsed() < 480) {showNotification("The simulated transport has not reached the hospital yet.",type="message");return()}
    if(!is.null(shared_id())) shared_hospital_arrival(incident_service,shared_id())
    transport_phase("hospital")
    confirmed <- incident_service$incidents[[shared_id()]]$hospital_arrived_at
    hospital_arrival(format(confirmed, "%b %d, %I:%M:%S %p %Z",tz="America/New_York"))
  })
  observe({
    invalidateLater(1000, session)
    last <- isolate(ui_state$last_tick)
    if(!isTRUE(isolate(demo_started())) || (!is.null(last) && as.numeric(difftime(Sys.time(),last,units='secs')) < .95)) return()
    if(isolate(transport_phase()) == 'hospital' || (isolate(transport_phase()) == 'scene' && !isTRUE(isolate(demo_running())))) return()
    ui_state$last_tick <- Sys.time()
    if ( isolate(transport_phase()) == "transport" && isolate(transport_elapsed()) < 480) {
      transport_elapsed(min(480,isolate(transport_elapsed())+30))
      if(!is.null(isolate(shared_id()))) shared_transport_advance(incident_service,isolate(shared_id()),isolate(transport_elapsed()))
    }
    if (isTRUE(isolate(demo_running()))) {
      next_time <- min(720, isolate(demo_elapsed()) + 30)
      demo_elapsed(next_time)
      if(!is.null(shared_id())) shared_advance(incident_service,shared_id(),next_time)
      if (next_time >= 720) demo_running(FALSE)
    }
  })
  workflow_stage <- reactive({
    if (!demo_started()) 'assignment' else if (transport_phase() == 'hospital') 'complete' else if (transport_phase() == 'transport') 'transport' else if (demo_elapsed() >= 720) 'hospital' else 'respond'
  })
  output$responder_stage <- renderText(workflow_stage())
  outputOptions(output, 'responder_stage', suspendWhenHidden = FALSE)
  output$responder_steps <- renderUI({
    stages <- c('assignment','respond','hospital','transport','complete')
    labels <- c('Get assignment','Respond to scene','Choose hospital','Hospital trip','Complete')
    current <- match(workflow_stage(), stages)
    tags$ol(class='workflow-steps', `aria-label`='Response progress', lapply(seq_along(labels), function(i)
      tags$li(class=if(i==current)'is-current' else if(i<current)'is-complete' else '',
        `aria-current`=if(i==current)'step' else NULL, span(class='step-number',i),labels[i])))
  })
  output$responder_workspace <- renderUI({
    req(identical(authenticated_role(), "responder"), active_role() == "responder")
    div(class = "responder-full",
      div(class = "notice", "SYNTHETIC DEMO · Fictional incident, traffic, weather, and responder positions. No live dispatch, routing, or validated time savings."),
      uiOutput("responder_steps"),
      conditionalPanel("output.responder_stage === 'assignment'", div(class = "panel-box", h2("1 · Get an assignment"),
        selectInput("demo_conditions", "Choose simulated conditions", c("Rain + rush hour", "Heavy rain + rush hour", "Dry + light traffic", "Snow/ice + road closure")),
        uiOutput("shared_queue"),
        actionButton("simulate_call", "Receive selected incident", class = "btn-primary"),
        p(class = "footer", "New calls randomly select one of five Queens zones. Replay runs at 30 simulated seconds per real second. Conditions are fixed when the call starts."))),
      tags$details(tags$summary("Trip link & arrival estimate"), uiOutput("shared_tracking"), uiOutput("shared_eta")), uiOutput("active_call")
    )
  })
  output$active_call <- renderUI({
    req(identical(authenticated_role(), "responder"))
    if (!demo_started()) return(empty_state("Ready for a demo call", "Choose conditions above, then simulate an incoming call."))
    # Do not rebuild map or controls on every clock tick.
    d <- responder_demo(0, demo_scenario())
    if(!is.null(shared_id())) {
      d$incident_id <- shared_id(); d$zone <- incident_service$incidents[[shared_id()]]$zone
    }
    tagList(
      div(class = "panel-box", div(class = "call-header",
        div(span(class = "eyebrow", d$incident_id), h3(tagList(icon("ambulance"), " New assignment"))),
        span(class = "assignment-priority", icon("exclamation-triangle"), " High · demo"), uiOutput("call_status")),
        div(class = "brief-grid",
          brief_item("phone", "Reason for call", "Reported fall · EMS assessment requested"),
          brief_item("map-marker-alt", "Scene", paste(d$zone, "· Queens (synthetic scene)")),
          brief_item("ambulance", "Assigned unit", paste(d$unit, "·", d$zone)),
          brief_item("door-open", "Access", "Demo scene access point · confirm with dispatch")),
        tags$details(tags$summary("Demo replay controls"), div(class = "simulation-controls", actionButton("pause_call", "Pause / Resume", icon=icon("pause")),
          actionButton("advance_scene", "Advance to scene", icon=icon("forward")), actionButton("reset_call", "Start another demo call", icon=icon("undo")))),
        uiOutput("call_clock")),
      conditionalPanel("output.responder_stage === 'hospital' || output.responder_stage === 'transport' || output.responder_stage === 'complete'", uiOutput("hospital_panel")),
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
        tags$details(class = "panel-box", open=NA, tags$summary("Scene coordination checklist"),
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
          if(!is.null(caller_tracking_link())) tags$a(href=caller_tracking_link(),class="btn btn-default",icon("eye")," View caller tracking"),
          uiOutput("transport_progress")))
  })
  output$hospital_candidates_ui <- renderUI({
    req(identical(authenticated_role(), "responder"), demo_started())
    candidates <- hospital_candidates(input$care_requirement, active_hospitals())
    if (!length(candidates)) return(div(class="notice","No demo facility matches this requirement. Ask dispatch to resolve; no destination is assigned."))
    labels <- vapply(candidates,function(h) sprintf("%s · %.2f miles by road",h$name,h$distance_m/1609.344),character(1))
    ids <- vapply(candidates,function(h) h$id,character(1))
    tagList(div(lapply(seq_along(candidates),function(i) {
      h<-candidates[[i]]
      div(class="hospital-card",strong(paste0(if(i==1) "Closest matching demo option: " else "Also matches: ",h$name)),
          p(sprintf("%.2f miles from scene by road · %s",h$distance_m/1609.344,paste(h$capabilities,collapse=" · "))))
    })),selectInput("hospital_choice","Hospital to confirm",choices=setNames(ids,labels)),
    actionButton("begin_transport","Confirm & begin trip",icon=icon("route"),class="btn-primary"),
    p(class="footer","Available after scene arrival. Distance uses the road snapshot; real capacity, diversion, travel conditions, and clinical suitability are unknown."))
  })
  output$transport_progress <- renderUI({
    req(identical(authenticated_role(), "responder"), !is.null(chosen_hospital()))
    if (transport_phase() == "hospital") return(tagList(
      div(class="notice",icon("check-circle"),strong(" Trip complete"),
        p(paste("Hospital arrival confirmed · simulated time:",hospital_arrival()))),
      actionButton("new_demo_call","Start another demo call",icon=icon("undo"),class="btn-primary")))
    tagList(p(sprintf("Transport replay: %02d:%02d / 08:00 simulated",transport_elapsed()%/%60,transport_elapsed()%%60)),
      p(if(transport_elapsed()>=480) "At demo destination — confirm hospital arrival." else "Fictional 8-minute transport replay, not a predicted travel time."),
      if(transport_elapsed()>=480) actionButton("arrived_hospital","Confirm hospital arrival",icon=icon("hospital"),class="btn-primary"))
  })
  observe({
    req(identical(authenticated_role(), "responder"), demo_started())
    elapsed <- demo_elapsed(); phase <- transport_phase(); travel <- transport_elapsed(); hospital <- chosen_hospital()
    zone <- incident_service$incidents[[shared_id()]]$zone
    session$onFlushed(function() {
      session$sendCustomMessage("responder-map", list(elapsed = elapsed, phase = phase, travel = travel,
        hospital = if (is.null(hospital)) NULL else hospital$id,
        zone = zone))
    }, once = TRUE)
  })
}
