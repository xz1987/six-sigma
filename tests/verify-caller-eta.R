source('app/app.R')
store <- shared_new(eta_bundle,shared_route,hospital_routes=hospital_routes)
link <- shared_create(store)
d <- shared_claim(store,link$incident_id)
frozen <- d$eta
markup <- function() as.character(shared_eta_card(store$incidents[[d$incident_id]],caller_view=TRUE))
stopifnot(grepl('Around',markup()),!grepl('Arrival confirmed',markup()))
shared_advance(store,d$incident_id,720)
stopifnot(grepl('SCENE ARRIVAL',markup()),grepl('Ambulance on scene',markup()))
shared_transport(store,d$incident_id,demo_hospitals[[1]])
stopifnot(grepl('HOSPITAL TRIP',markup()),grepl('On the way to hospital',markup()),
  grepl('Hospital ETA is not available',markup()),!grepl('Arrival confirmed',markup()))
shared_transport_advance(store,d$incident_id,480)
stopifnot(grepl('Awaiting arrival confirmation',markup()),!grepl('Arrival confirmed',markup()))
shared_hospital_arrival(store,d$incident_id)
stopifnot(grepl('HOSPITAL ARRIVAL',markup()),grepl('Arrival confirmed',markup()),
  identical(frozen,store$incidents[[d$incident_id]]$eta),
  grepl('95% prediction range',as.character(shared_eta_card(store$incidents[[d$incident_id]]))))
cat('PASS: caller scene ETA, on-scene status, hospital transport, pending arrival and crew-confirmed arrival\n')
