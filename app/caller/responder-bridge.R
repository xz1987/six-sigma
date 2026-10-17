# Caller renders the responder's shared trip, not a separate replay.
caller_incident_from_trip <- function(trip) {
  if(is.null(trip)) return(NULL)
  transport <- trip$phase %in% c('transport','hospital')
  route_file <- if(transport) paste0('hospital-route-',trip$hospital$id,'.json') else 'demo-road-route.json'
  coords <- jsonlite::fromJSON(file.path(repo_root,'app','www',route_file))$geometry$coordinates
  scene <- jsonlite::fromJSON(file.path(repo_root,'app','www','demo-road-route.json'))$geometry$coordinates
  scene <- scene[nrow(scene),]
  d <- caller_demo_incident(trip$incident_id,trip$zone,trip$started_at)
  d$ambulance_id <- trip$unit
  d$route <- data.frame(x=coords[,1],y=coords[,2])
  d$scene <- list(lng=scene[1],lat=scene[2])
  d$progress <- if(transport) min(1,trip$transport_elapsed/480) else min(1,trip$scene_elapsed/720)
  d$status <- if(trip$phase=='hospital') 'hospital_arrived' else if(transport) 'transporting' else if(trip$scene_elapsed>=720) 'arrived' else if(trip$scene_elapsed>=600) 'approaching' else 'assigned'
  d$arrived_at <- if(trip$scene_elapsed>=720) trip$started_at+720 else NULL
  d$updated_at <- trip$updated_at
  if(!is.null(trip$hospital)) {
    h<-trip$hospital
    d<-caller_confirm_destination(d,h$name,
      sprintf('Fictional demo facility · map location %.5f, %.5f',h$lat,h$lng),
      confirmed_at=trip$started_at+720,lat=h$lat,lng=h$lng)
  }
  d
}
caller_link_responder_hospital <- function(store, trip_provider, now=Sys.time()) {
  trip <- trip_provider()
  stopifnot(!is.null(trip),!is.null(trip$hospital))
  grant <- caller_issue_access(trip$incident_id,now)
  incident_id <- trip$incident_id
  provider <- function() {
    current <- trip_provider()
    if(is.null(current)||!identical(current$incident_id,incident_id)) return(NULL)
    caller_incident_from_trip(current)
  }
  assign(grant$token,list(grant=grant,incident=provider(),trip_provider=provider),envir=store)
  paste0('#incident=',grant$token)
}
