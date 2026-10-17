# Process-local synthetic incident service. Tokens never enter supervisor records.
shared_new <- function(bundle, route, publish = function(records) NULL, hospital_routes = list()) {
  e <- new.env(parent=emptyenv())
  e$incidents <- list(); e$grants <- list(); e$bundle <- bundle
  e$revision <- shiny::reactiveVal(0L)
  e$route <- route; e$publish <- publish; e$hospital_routes <- hospital_routes
  e
}
shared_stamp <- function(t) format(t, '%Y-%m-%dT%H:%M:%SZ', tz='UTC')
shared_records <- function(store) {
  rows <- lapply(store$incidents, function(d) {
    if(is.null(d$arrived_at)) return(NULL)
    data.frame(incident_id=d$incident_id, zone=d$zone, call_priority=d$call_priority,
      weather_condition=d$weather_condition, call_received_time=shared_stamp(d$call_received_at),
      scene_arrival_time=shared_stamp(d$arrived_at),
      transport_departure_time=if(is.null(d$transport_departed_at)) NA_character_ else shared_stamp(d$transport_departed_at),
      hospital_arrival_time=if(is.null(d$hospital_arrived_at)) NA_character_ else shared_stamp(d$hospital_arrived_at),
      responder_id=d$ambulance_id, response_system_source='Shared synthetic incident replay',
      stringsAsFactors=FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if(!length(rows)) return(response_metric_empty_records())
  do.call(rbind, rows)
}
shared_save <- function(store, d) {
  store$incidents[[d$incident_id]] <- d
  store$revision(shiny::isolate(store$revision()) + 1L)
  store$publish(shared_records(store))
  invisible(d)
}
shared_create <- function(store, scenario='Rain + rush hour', now=Sys.time(), zone='Astoria', weather_inputs=NULL) {
  id <- paste0('DEMO-',paste(format(openssl::rand_bytes(6)),collapse=''))
  d <- caller_demo_incident(id,zone,now)
  d$route <- store$route; d$ambulance_id <- 'Q-12'; d$call_priority <- 'High'
  d$scenario <- scenario; d$claimed <- FALSE; d$elapsed <- 0
  # Synthetic call receipt and assignment coincide in this demo; distinct fields
  # prevent the assignment-trained ETA being mislabeled as a call-to-scene model.
  d$call_received_at <- now
  d$transport_departed_at <- NULL; d$hospital_arrived_at <- NULL
  weather <- switch(scenario, 'Dry + light traffic'='Dry', 'Snow/ice + road closure'='Snow/Ice', 'Rain')
  intensity <- switch(scenario, 'Dry + light traffic'='None', 'Heavy rain + rush hour'='Heavy', 'Moderate')
  inputs <- data.frame(incident_id=id,dispatch_time=shared_stamp(now),zone=zone,call_priority='High',
    weather_condition=weather,precipitation_intensity=intensity,wind_speed_mph=10,
    wind_gust_mph=16,weather_observed_at=shared_stamp(now),stringsAsFactors=FALSE)
  if(!is.null(weather_inputs)) for(n in names(weather_inputs)) inputs[[n]] <- weather_inputs[[n]]
  d$eta_inputs <- inputs
  d$weather_condition <- inputs$weather_condition
  d$eta <- list(incident_id=id,assigned_at=now,prediction=eta_predict(store$bundle,inputs))
  grant <- caller_issue_access(id,now)
  store$grants[[grant$token]] <- grant
  shared_save(store,d)
  list(incident_id=id,fragment=paste0('#incident=',grant$token))
}
shared_lookup <- function(store,token,now=Sys.time()) {
  # Subscribe caller sessions to writes; polling remains only for expiry/staleness.
  if(!is.null(shiny::getDefaultReactiveDomain())) store$revision()
  if(!is.character(token)||length(token)!=1||is.na(token)||
     !grepl('^[a-f0-9]{64}$',token)) return(NULL)
  grant <- store$grants[[token]]
  if(is.null(grant)||!isTRUE(now<grant$expires_at)) return(NULL)
  caller_lookup(token,list(grant),store$incidents,now)
}
shared_claim <- function(store,id) {
  d <- store$incidents[[id]]
  if(is.null(d)||isTRUE(d$claimed)) stop('Incident is unavailable or already assigned')
  d$claimed <- TRUE; shared_save(store,d); d
}
shared_advance <- function(store,id,elapsed,now=Sys.time()) {
  d <- store$incidents[[id]]
  if(is.null(d)||!isTRUE(d$claimed)) stop('Incident is not assigned')
  if(length(elapsed)!=1||!is.finite(elapsed)||elapsed<d$elapsed||elapsed>720) stop('Invalid replay transition')
  d$elapsed <- elapsed; d$progress <- elapsed/720
  d$status <- if(elapsed>=720)'arrived' else if(elapsed>=600)'approaching' else 'assigned'
  if(elapsed>=720 && is.null(d$arrived_at)) d$arrived_at <- d$dispatched_at+720
  d$updated_at <- now
  shared_save(store,d)
}
shared_transport <- function(store,id,hospital,now=Sys.time()) {
  d <- store$incidents[[id]]
  if(is.null(d)||is.null(d$arrived_at)||!is.null(d$transport_departed_at)) stop('Transport requires confirmed scene arrival')
  # Fictional facilities have synthetic coordinates, not verified street addresses.
  address <- sprintf('Synthetic location %.6f, %.6f · Queens (fictional facility)',hospital$lat,hospital$lng)
  d <- caller_confirm_destination(d,hospital$name,address,max(now,d$arrived_at))
  d$hospital <- list(id=hospital$id,name=hospital$name,lat=hospital$lat,lng=hospital$lng)
  d$hospital_route <- store$hospital_routes[[hospital$id]]
  d$transport_elapsed <- 0
  d$transport_departed_at <- max(now,d$arrived_at)
  d$updated_at <- now
  shared_save(store,d)
}
shared_transport_advance <- function(store,id,elapsed,now=Sys.time()) {
  d <- store$incidents[[id]]
  if(is.null(d)||is.null(d$transport_departed_at)||!is.null(d$hospital_arrived_at))
    stop('Active transport required')
  if(length(elapsed)!=1||!is.finite(elapsed)||elapsed<d$transport_elapsed||elapsed>480)
    stop('Invalid transport replay transition')
  d$transport_elapsed <- elapsed; d$updated_at <- now
  shared_save(store,d)
}
shared_hospital_arrival <- function(store,id,now=Sys.time()) {
  d <- store$incidents[[id]]
  if(is.null(d)||is.null(d$transport_departed_at)) stop('Hospital arrival requires transport')
  if(d$transport_elapsed < 480) stop('Transport replay has not reached hospital')
  if(is.null(d$hospital_arrived_at)) d$hospital_arrived_at <- d$transport_departed_at+480
  d$updated_at <- now
  shared_save(store,d)
}
shared_revoke <- function(store,id) {
  store$grants <- Filter(function(g)!identical(g$incident_id,id),store$grants)
  store$revision(shiny::isolate(store$revision()) + 1L)
}
# Display uses only the frozen prediction, never refits after arrival.
shared_eta_card <- function(d) {
  p <- if(is.null(d$eta)) NULL else d$eta$prediction
  available <- !is.null(p) && identical(p$status,'experimental_synthetic')
  clock <- function(t)format(t,'%b %d, %I:%M:%S %p %Z',tz='America/New_York')
  shiny::div(class='caller-eta',shiny::span(class='caller-eyebrow','EXPERIMENTAL ARRIVAL ESTIMATE · SYNTHETIC'),
    shiny::h2(if(available)clock(p$arrival_median) else 'Not available yet'),
    if(available)shiny::p(paste('95% prediction range:',clock(p$arrival_lower),'to',clock(p$arrival_upper)))
    else shiny::p(if(is.null(p))'An arrival estimate is not available yet.' else p$reason),
    shiny::p('Fixed at assignment. This estimate is for arrival at the scene, not the hospital.'),
    shiny::tags$details(class='caller-trip-details',shiny::tags$summary('About this estimate'),
      if(available)shiny::p(sprintf('Median assignment-to-scene duration: %.1f min',p$median_minutes)),
      if(!is.null(d$elapsed))shiny::p(sprintf('Actual replay elapsed since assignment: %.1f min',d$elapsed/60)),
      if(!is.null(d$arrived_at))shiny::p(paste('Actual scene arrival:',clock(d$arrived_at))),
      if(!is.null(d$eta_inputs))shiny::p(sprintf('Synthetic assignment weather: %s · %s precipitation · sustained wind %s mph · gust %s mph',
        d$eta_inputs$weather_condition,d$eta_inputs$precipitation_intensity,
        d$eta_inputs$wind_speed_mph,d$eta_inputs$wind_gust_mph)),
      shiny::p('Separate from actual elapsed time and QC. Accelerated replay timestamps are simulated, not wall-clock arrival promises.')))
}
