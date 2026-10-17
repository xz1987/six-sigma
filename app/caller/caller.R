# Caller module: source this file, then caller_ui('caller') / caller_server(...).
# Provider contract: lookup(token) returns ONLY the authorized incident, or NULL.
# Incident coordinates and replay are synthetic illustrations, never live GPS.
caller_demo_incident <- function(id='DEMO-CALL-001', zone='Astoria', now=Sys.time()) {
  centers<-list(Astoria=c(-73.9235,40.7645),Flushing=c(-73.829,40.759),Jamaica=c(-73.799,40.703),`Forest Hills`=c(-73.845,40.720),`Far Rockaway`=c(-73.755,40.603))
  center<-centers[[zone]];if(is.null(center))stop('Unsupported synthetic neighborhood')
  route<-data.frame(x=center[1]+c(-.008,-.006,-.004,-.002,0,.002,.004),y=center[2]+c(-.004,-.003,-.002,-.001,0,.001,.002))
  list(incident_id=id, ambulance_id='DEMO UNIT 24', zone=zone,
       dispatched_at=now, updated_at=now, status='assigned', arrived_at=NULL,
       route=route,
       progress=0, eta=NULL, confirmed_destination=NULL)
}
# Only a trusted crew/dispatch provider calls this hook; callers cannot confirm destinations.
caller_confirm_destination <- function(d, name, address, confirmed_at=Sys.time()) {
  valid_text<-function(x)is.character(x)&&length(x)==1&&!is.na(x)&&nzchar(trimws(x))
  if(!valid_text(name)||!valid_text(address)||!inherits(confirmed_at,'POSIXt')||
     length(confirmed_at)!=1||is.na(confirmed_at)||confirmed_at<d$dispatched_at)
    stop('Destination requires a name, address and valid confirmation timestamp')
  d$confirmed_destination<-list(name=trimws(name),address=trimws(address),confirmed_at=confirmed_at)
  d
}
caller_destination_panel <- function(d) {
  destination<-d$confirmed_destination
  confirmed<-!is.null(destination)&&is.character(destination$name)&&length(destination$name)==1&&
    !is.na(destination$name)&&nzchar(trimws(destination$name))&&
    is.character(destination$address)&&length(destination$address)==1&&
    !is.na(destination$address)&&nzchar(trimws(destination$address))&&
    inherits(destination$confirmed_at,'POSIXt')&&length(destination$confirmed_at)==1&&
    !is.na(destination$confirmed_at)&&destination$confirmed_at>=d$dispatched_at
  shiny::div(class='caller-card caller-destination',
    shiny::span(class='caller-eyebrow','YOUR HOSPITAL'),
    shiny::h3(if(confirmed)'Your crew confirmed the hospital' else 'Hospital update coming soon'),
    if(confirmed)shiny::tagList(shiny::p(class='caller-destination-name',shiny::strong(destination$name)),
      shiny::p(destination$address),shiny::p(class='caller-map-note',paste('Confirmed · simulated time',
        format(destination$confirmed_at,'%b %d, %I:%M:%S %p %Z',tz='America/New_York'))),
      shiny::p('Your crew will let you know if the destination changes.'))
    else shiny::p('Your hospital name and address will appear here once your crew confirms them.'))
}
caller_issue_access <- function(incident_id, now=Sys.time(), ttl=1800) {
  stopifnot(length(incident_id)==1, nzchar(incident_id), ttl>0)
  list(token=paste(format(openssl::rand_bytes(32)),collapse=''), incident_id=incident_id, expires_at=now+ttl)
}
# Called by the dispatcher when an incident is created. Store stays server-side.
caller_dispatch_store <- function() new.env(parent=emptyenv())
caller_create_dispatch <- function(store, zone=sample(c('Astoria','Flushing','Jamaica','Forest Hills','Far Rockaway'),1L), now=Sys.time()) {
  id<-paste0('DEMO-',paste(format(openssl::rand_bytes(6)),collapse=''))
  d<-caller_demo_incident(id,zone,now)
  grant<-caller_issue_access(id,now)
  assign(grant$token,list(incident=d,grant=grant),envir=store)
  list(incident_id=id,fragment=paste0('#incident=',grant$token))
}
caller_dispatch_lookup <- function(store, token, now=Sys.time()) {
  if(!is.character(token)||length(token)!=1||is.na(token)||
     !grepl('^[a-f0-9]{64}$',token)||!exists(token,envir=store,inherits=FALSE))return(NULL)
  entry<-get(token,envir=store,inherits=FALSE)
  if(!isTRUE(now<entry$grant$expires_at)) {
    rm(list=token,envir=store);return(NULL)
  }
  d<-entry$incident
  d<-caller_replay(d,as.numeric(difftime(now,d$dispatched_at,units='secs')),now)
  caller_lookup(token,list(entry$grant),setNames(list(d),d$incident_id),now)
}
caller_lookup <- function(token, grants, incidents, now=Sys.time()) {
  if(!is.character(token)||length(token)!=1||is.na(token)||!nzchar(token)) return(NULL)
  for(g in grants) if(identical(token,g$token) && isTRUE(now<g$expires_at)) {
    d<-incidents[[g$incident_id]]
    if(!is.null(d) && identical(d$incident_id,g$incident_id)) return(d)
  }
  NULL
}
caller_replay <- function(d, seconds, now=Sys.time()) {
  d$progress<-min(1,max(0,seconds/90))
  d$status<-if(seconds>=90)'arrived' else if(seconds>=65)'approaching' else 'assigned'
  d$arrived_at<-if(seconds>=90) d$dispatched_at+90 else NULL
  d$updated_at<-now
  d
}
caller_position <- function(route, progress) {
  lengths<-sqrt(diff(route$x)^2+diff(route$y)^2); distance<-min(1,max(0,progress))*sum(lengths)
  ends<-cumsum(lengths); i<-which(ends>=distance)[1]
  start<-if(i==1)0 else ends[i-1]; fraction<-if(lengths[i]==0)0 else (distance-start)/lengths[i]
  c(x=route$x[i]+fraction*(route$x[i+1]-route$x[i]),y=route$y[i]+fraction*(route$y[i+1]-route$y[i]))
}
caller_map <- function(d) {
  if(is.null(d))return(NULL)
  pos<-caller_position(d$route,d$progress);target<-tail(d$route,1)
  hospital <- NULL; route <- NULL
  transporting <- !is.null(d$confirmed_destination) && !is.null(d$transport_departed_at)
  if(transporting && !is.null(d$hospital)) {
    hospital <- d$hospital
    if(is.data.frame(d$hospital_route) && nrow(d$hospital_route)>=2) {
      route <- lapply(seq_len(nrow(d$hospital_route)),function(i)
        c(unname(d$hospital_route$y[i]),unname(d$hospital_route$x[i])))
      pos <- caller_position(d$hospital_route,min(1,d$transport_elapsed/480))
    }
  }
  list(incident_id=d$incident_id,ambulance=list(lat=unname(pos[2]),lng=unname(pos[1])),
       destination=list(lat=target$y,lng=target$x),hospital=hospital,hospital_route=route,
       route_key=paste(d$incident_id,if(is.null(hospital))'scene' else hospital$id,sep=':'),
       transporting=transporting,waiting_for_confirmation=identical(d$claimed,FALSE),
       arrived=if(transporting)!is.null(d$hospital_arrived_at) else identical(d$status,'arrived'),
       stale=as.numeric(difftime(Sys.time(),d$updated_at,units='secs'))>30)
}
caller_ui <- function(id, asset_prefix = '', include_leaflet = TRUE) {
  ns<-shiny::NS(id)
  shiny::tagList(
    if (include_leaflet) shiny::tagList(shiny::tags$link(rel='stylesheet',href=paste0(asset_prefix,'vendor/leaflet/leaflet.css')), shiny::tags$script(src=paste0(asset_prefix,'vendor/leaflet/leaflet.js'))),
    shiny::tags$link(rel='stylesheet',href=paste0(asset_prefix,'caller.css?v=role-steps-3')),
    shiny::tags$script(src=paste0(asset_prefix,'caller.js?v=shared-service-2'),defer=NA),
    shiny::div(class='caller-workspace',
      shiny::div(class='caller-heading',shiny::span(class='caller-eyebrow','HERE FOR YOU'),shiny::h2('Your EMS updates'),shiny::p('Follow your ambulance and see where you’re headed.')),
      shiny::div(class='caller-connection',role='status','Connection lost. These are your last updates. Reconnect for the latest.'),
      shiny::div(class='caller-access caller-access-compact',
        shiny::textOutput(ns('accessMessage')),
        shiny::actionButton(ns('close'),'Close updates')),
      shiny::uiOutput(ns('tracking')),
      shiny::conditionalPanel(sprintf("output['%s'] === 'yes'",ns('authorized')),
        shiny::div(class='caller-card',shiny::div(class='caller-card-top',shiny::h3('Your trip map'),shiny::tags$button(type='button',class='btn caller-recenter',`data-map`=ns('map'),'⌖ Recenter')),
          shiny::div(id=ns('map'),class='caller-map-output',role='region',`aria-label`='Queens street map for your incident destination'),
          shiny::p(class='caller-tile-status',role='status'),
          shiny::p(class='caller-route-status',role='status'),
          shiny::uiOutput(ns('legend')),
          shiny::tags$details(class='caller-trip-details',shiny::tags$summary('About this demo map'),shiny::p(class='caller-map-note','Queens street map with simulated ambulance movement. After crew confirmation, the blue line shows a stored road-route snapshot to a fictional hospital. No live GPS, navigation or emergency request.')),

          shiny::uiOutput(ns('lastUpdate')))),
      shiny::uiOutput(ns('destination'))))
}
caller_server <- function(id, lookup) {
  shiny::moduleServer(id,function(input,output,session) {
    access<-shiny::reactiveVal(NULL)
    observe<-shiny::observeEvent
    observe(input$linkToken,{token<-input$linkToken;access(if(is.character(token)&&length(token)==1&&nzchar(token))token else NULL)})
    observe(input$close,{access(NULL)})
    current<-shiny::reactive({shiny::invalidateLater(1000,session);token<-access();if(is.null(token))return(NULL);lookup(token)})
    output$accessMessage<-shiny::renderText({if(is.null(access()))return('Open the link from your dispatcher to see your updates. No sign-in needed.');if(is.null(current()))'This link is no longer available. Ask your dispatcher for a new one.' else 'You’re viewing your private updates.'})
    output$authorized<-shiny::renderText(if(is.null(current()))'no' else 'yes')
    shiny::outputOptions(output,'authorized',suspendWhenHidden=FALSE)
    output$map<-shiny::renderText({d<-current();if(is.null(d))'null' else jsonlite::toJSON(caller_map(d),auto_unbox=TRUE,digits=NA,null="null")})
    shiny::outputOptions(output,'map',suspendWhenHidden=FALSE)
    output$tracking<-shiny::renderUI({
      d<-current();if(is.null(d))return(NULL)
      status<-switch(d$status,assigned=if(identical(d$claimed,FALSE))'Waiting for ambulance confirmation' else 'Ambulance assigned',approaching='Approaching the scene',arrived='Ambulance has arrived','Status unavailable')
      if(!is.null(d$hospital_arrived_at)) status <- 'Hospital arrival confirmed'
      else if(!is.null(d$transport_departed_at)) status <- 'Transporting to confirmed hospital'
      format_time<-function(t)format(t,'%b %d, %I:%M:%S %p %Z',tz='America/New_York')
      shiny::tagList(
        shiny::div(class='caller-card',shiny::div(class='caller-card-top',shiny::span(class='caller-eyebrow','YOUR INCIDENT'),shiny::span(class='caller-demo','SYNTHETIC DEMO')),
          shiny::h3(d$zone),shiny::tags$details(class='caller-trip-details',shiny::tags$summary('Trip details'),shiny::p(class='caller-reference',paste(d$incident_id,'·',d$ambulance_id))),
          shiny::div(class=paste('caller-status',if(d$status=='arrived')'caller-arrived'),role='status',
            shiny::strong(if(!is.null(d$transport_departed_at))status else if(d$status=='arrived')'✓ Arrival confirmed' else status),
            if(d$status=='arrived')shiny::p(paste('Scene arrival · simulated time:',format_time(d$arrived_at)))),

          shiny::tags$ol(class='caller-steps workflow-steps', `aria-label`='Your trip progress', lapply(seq_len(4),function(i) {
            stage <- if(!is.null(d$hospital_arrived_at))4L else if(!is.null(d$transport_departed_at))3L else if(d$status=='arrived')2L else 1L
            shiny::tags$li(class=if(i==stage)'is-current' else if(i<stage)'is-complete' else '', `aria-current`=if(i==stage)'step' else NULL,
              shiny::span(class='step-number',i),c(if(identical(d$claimed,FALSE))'Awaiting confirmation' else 'On the way','At your location','To hospital','Arrived')[i])
          })),
          if(exists('shared_eta_card',mode='function')) shared_eta_card(d,caller_view=TRUE) else
            shiny::div(class='caller-eta',shiny::span(class='caller-eyebrow','ETA'),shiny::h2(if(d$status=='arrived')'Arrival confirmed' else 'Not available yet'))))


    })
    output$legend<-shiny::renderUI({
      d<-current();if(is.null(d))return(NULL)
      h<-caller_map(d)$hospital
      shiny::div(class='caller-legend',if(!identical(d$claimed,FALSE)) shiny::span(class='caller-legend-ambulance','🚑 Assigned ambulance'),
        if(is.null(h)) shiny::span(class='caller-legend-scene','🏠 Your incident location') else shiny::span(class='caller-legend-hospital','🏥 Confirmed hospital'))
    })
    output$destination<-shiny::renderUI({d<-current();if(is.null(d))return(NULL);if(is.null(d$confirmed_destination)) shiny::tags$details(class='caller-trip-details',shiny::tags$summary('Hospital updates'),caller_destination_panel(d)) else caller_destination_panel(d)})
    output$lastUpdate<-shiny::renderUI({
      d<-current();if(is.null(d))return(NULL)
      stale<-as.numeric(difftime(Sys.time(),d$updated_at,units='secs'))>30
      shiny::div(class=paste('caller-updated',if(stale)'caller-stale'),
        shiny::strong(if(stale)'Waiting for a location update' else 'Last updated'),
        shiny::p(format(d$updated_at,'%b %d, %I:%M:%S %p %Z',tz='America/New_York')),
        if(stale)shiny::p('The marker shows the last reported location. A fresh update is needed to confirm its current position.'),
        shiny::p(class='caller-map-note',if(identical(d$claimed,FALSE))'Ambulance tracking begins after responder confirmation.' else 'This private view shows only your assigned ambulance.'))
    })
    list(current=current,access=access)
  })
}
