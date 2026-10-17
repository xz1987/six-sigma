source('app/app.R')
store <- shared_new(eta_bundle,shared_route,hospital_routes=hospital_routes)
store$zone_trips <- incident_service$zone_trips
for(zone in names(store$zone_trips)) {
  link <- shared_create(store,zone=zone)
  d <- shared_claim(store,link$incident_id)
  stopifnot(d$zone==zone,nrow(d$route)>2,identical(d$eta_inputs$zone,zone))
  fixture <- store$zone_trips[[zone]]
  for(h in fixture$hospitals) {
    route <- shared_points(fixture$hospital_routes[[h$id]])
    stopifnot(nrow(route)>2,h$distance_m>0,
      max(abs(unlist(tail(d$route,1))-unlist(head(route,1))))<0.0001,
      max(abs(unlist(tail(route,1))-c(h$lng,h$lat)))<0.0001)
  }
  shared_advance(store,d$incident_id,720)
  h <- hospital_candidates('Trauma assessment',fixture$hospitals)[[1]]
  shared_transport(store,d$incident_id,h)
  live <- shared_lookup(store,sub('#incident=','',link$fragment,fixed=TRUE))
  stopifnot(live$zone==zone,caller_map(live)$hospital$name==h$name)
  shared_transport_advance(store,d$incident_id,480)
  shared_hospital_arrival(store,d$incident_id)
}
set.seed(32)
random <- replicate(100,{ link <- shared_create(store); store$incidents[[link$incident_id]]$zone })
stopifnot(setequal(unique(random),names(store$zone_trips)))
records <- shared_records(store)
stopifnot(setequal(records$zone,names(store$zone_trips)))
cat('PASS: all five zone routes, matching hospital endpoints, shared records, and randomized creation\n')
