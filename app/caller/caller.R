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
    shiny::span(class='caller-eyebrow','HOSPITAL DESTINATION'),
    shiny::h3(if(confirmed)'Destination confirmed by crew' else 'Destination not yet confirmed'),
    if(confirmed)shiny::tagList(shiny::p(class='caller-destination-name',shiny::strong(destination$name)),
      shiny::p(destination$address),shiny::p(class='caller-map-note',paste('Confirmed',
        format(destination$confirmed_at,'%b %d, %I:%M:%S %p %Z',tz='America/New_York'))),
      shiny::p('This is the latest destination supplied by the crew. It may change; follow their updates.'))
    else shiny::p('A hospital name and address will appear here only after the crew confirms the destination. Arrival at the scene does not confirm a hospital destination.'))
}
caller_issue_access <- function(incident_id, now=Sys.time(), ttl=1800) {
  stopifnot(length(incident_id)==1, nzchar(incident_id), ttl>0)
  list(token=paste(format(openssl::rand_bytes(32)),collapse=''), incident_id=incident_id, expires_at=now+ttl)
}
# Called by the dispatcher when an incident is created. Store stays server-side.
caller_dispatch_store <- function() new.env(parent=emptyenv())
caller_create_dispatch <- function(store, zone='Astoria', now=Sys.time()) {
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
  list(incident_id=d$incident_id,ambulance=list(lat=unname(pos[2]),lng=unname(pos[1])),
       destination=list(lat=target$y,lng=target$x),
       arrived=identical(d$status,'arrived'),
       stale=as.numeric(difftime(Sys.time(),d$updated_at,units='secs'))>30)
}
caller_ui <- function(id, asset_prefix = '', include_leaflet = TRUE) {
  ns<-shiny::NS(id)
  shiny::tagList(
    if (include_leaflet) shiny::tagList(shiny::tags$link(rel='stylesheet',href=paste0(asset_prefix,'vendor/leaflet/leaflet.css')), shiny::tags$script(src=paste0(asset_prefix,'vendor/leaflet/leaflet.js'))),
    shiny::tags$link(rel='stylesheet',href=paste0(asset_prefix,'caller.css')),
    shiny::tags$script(src=paste0(asset_prefix,'caller.js'),defer=NA),
    shiny::div(class='caller-workspace',
      shiny::div(class='caller-heading',shiny::span(class='caller-eyebrow','03 / YOUR RESPONSE'),shiny::h2('Your ambulance, at a glance'),shiny::p('Follow the response to your incident in one place.')),
      shiny::div(class='caller-connection',role='status','Connection interrupted. Showing last known information; reconnect for updates.'),
      shiny::div(class='caller-access caller-access-compact',
        shiny::textOutput(ns('accessMessage')),
        shiny::actionButton(ns('close'),'Close tracking')),
      shiny::uiOutput(ns('tracking')),
      shiny::conditionalPanel(sprintf("output['%s'] === 'yes'",ns('authorized')),
        shiny::div(class='caller-card',shiny::div(class='caller-card-top',shiny::h3('Your ambulance in Queens'),shiny::tags$button(type='button',class='btn caller-recenter',`data-map`=ns('map'),'⌖ Recenter')),
          shiny::div(id=ns('map'),class='caller-map-output',role='region',`aria-label`='Queens street map with your assigned ambulance and incident destination'),
          shiny::p(class='caller-tile-status',role='status'),
          shiny::div(class='caller-legend',shiny::span('🚑 Assigned ambulance'),shiny::span('🏠 Your incident location')),
          shiny::p(class='caller-map-note','Real Queens street map. Ambulance and house locations are synthetic; movement is scripted, not live GPS or a calculated road route.'),
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
    output$accessMessage<-shiny::renderText({if(is.null(access()))return('Open the private tracking link supplied for your incident. No account or password is needed.');if(is.null(current()))'Tracking link unavailable or expired. Request a new link from the dispatcher.' else 'Your private incident tracking is open. No sign-in needed.'})
    output$authorized<-shiny::renderText(if(is.null(current()))'no' else 'yes')
    shiny::outputOptions(output,'authorized',suspendWhenHidden=FALSE)
    output$map<-shiny::renderText({d<-current();if(is.null(d))'null' else jsonlite::toJSON(caller_map(d),auto_unbox=TRUE)})
    shiny::outputOptions(output,'map',suspendWhenHidden=FALSE)
    output$tracking<-shiny::renderUI({
      d<-current();if(is.null(d))return(NULL)
      status<-switch(d$status,assigned='Ambulance assigned',approaching='Approaching the scene',arrived='Ambulance has arrived','Status unavailable')
      format_time<-function(t)format(t,'%b %d, %I:%M:%S %p %Z',tz='America/New_York')
      shiny::tagList(
        shiny::div(class='caller-card',shiny::div(class='caller-card-top',shiny::span(class='caller-eyebrow','YOUR INCIDENT'),shiny::span(class='caller-demo','SYNTHETIC DEMO')),
          shiny::h3(d$zone),shiny::p(class='caller-reference',paste(d$incident_id,'·',d$ambulance_id)),
          shiny::div(class=paste('caller-status',if(d$status=='arrived')'caller-arrived'),role='status',
            shiny::strong(if(d$status=='arrived')'✓ Arrival confirmed' else status),
            if(d$status=='arrived')shiny::p(paste('Ambulance on scene at',format_time(d$arrived_at)))),
          shiny::div(class='caller-steps',lapply(c('Assigned','Approaching','Arrived'),function(label)shiny::span(class=if(label==switch(d$status,assigned='Assigned',approaching='Approaching',arrived='Arrived',''))'is-current' else '',label))),
          shiny::div(class='caller-eta',shiny::span(class='caller-eyebrow','ESTIMATED ARRIVAL'),shiny::h2(if(d$status=='arrived')'Arrival confirmed' else 'Estimate unavailable'),
            shiny::p(if(d$status=='arrived')paste('Confirmed on scene:',format_time(d$arrived_at)) else 'An estimate will appear after the experimental ETA model is validated.'))))
    })
    output$destination<-shiny::renderUI({d<-current();if(is.null(d))return(NULL);caller_destination_panel(d)})
    output$lastUpdate<-shiny::renderUI({
      d<-current();if(is.null(d))return(NULL)
      stale<-as.numeric(difftime(Sys.time(),d$updated_at,units='secs'))>30
      shiny::div(class=paste('caller-updated',if(stale)'caller-stale'),
        shiny::strong(if(stale)'Location update delayed · last known position' else 'Latest received location update'),
        shiny::p(format(d$updated_at,'%b %d, %I:%M:%S %p %Z',tz='America/New_York')),
        if(stale)shiny::p('The marker shows the last reported location. A fresh update is needed to confirm its current position.'),
        shiny::p(class='caller-map-note','Only your assigned ambulance and incident are shown. This demo cannot request emergency services.'))
    })
    list(current=current,access=access)
  })
}
