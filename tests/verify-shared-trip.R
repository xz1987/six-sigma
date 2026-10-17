# Run from the repo root: Rscript tests/verify-shared-trip.R
source('app/app.R')
shiny::testServer(server, {
  session$setInputs(choose_responder=1)
  session$setInputs(login_username=demo_accounts$responder$username,
    login_password=demo_accounts$responder$password,submit_login=1)
  session$setInputs(simulate_call=1,care_requirement='Trauma assessment',hospital_choice='a')
  trip_id<-shared_responder_state$incident_id
  stopifnot(nzchar(trip_id),nrow(production_response_records())==0)
  stopifnot(is.null(caller_map(incident_service$incidents[[trip_id]])$hospital))
  session$setInputs(advance_scene=1,begin_transport=1)
  token<-sub('^#incident=','',shared_responder_state$caller_tracking_link)
  d<-shared_lookup(incident_service,token)
  stopifnot(d$incident_id==trip_id,d$confirmed_destination$name==shared_responder_state$chosen_hospital$name,
    !is.null(d$transport_departed_at),!is.null(caller_map(d)$hospital))
  session$setInputs(`caller-linkToken`=token,trackingLinkOpened=TRUE)
  stopifnot(active_role()=='caller',is.null(authenticated_role()),
    grepl('Confirmed hospital',paste(output$`caller-legend`,collapse=' ')))
  # Verify timer advancement while Caller is active, with no staff login.
  shared_responder_state$last_tick<-Sys.time()-2
  session$elapse(1100)
  stopifnot(shared_responder_state$transport_elapsed>0)
  shared_transport_advance(incident_service,trip_id,240)
  shared_responder_state$transport_elapsed<-240
  d2<-shared_lookup(incident_service,token)
  stopifnot(d2$incident_id==trip_id,d2$transport_elapsed==240)
  map<-caller_map(d2)
  stopifnot(!identical(map$ambulance,map$destination),map$hospital$name==d2$confirmed_destination$name)
  # Returning to Responder retains the trip rather than creating another one.
  session$setInputs(choose_responder=2)
  session$setInputs(login_username=demo_accounts$responder$username,
    login_password=demo_accounts$responder$password,submit_login=2)
  shared_transport_advance(incident_service,trip_id,480)
  shared_responder_state$transport_elapsed<-480
  session$setInputs(arrived_hospital=1)
  d3<-shared_lookup(incident_service,token)
  records<-production_response_records()
  stopifnot(d3$incident_id==trip_id,!is.null(d3$hospital_arrived_at),d3$transport_elapsed==480,
    nrow(records)==1,records$incident_id==trip_id,!is.na(records$hospital_arrival_time))
  metric<-response_metric_to_qc(records,'transport_to_hospital',allow_mock=FALSE)
  stopifnot(as.numeric(difftime(qc_parse_time(metric$on_scene_time),
    qc_parse_time(metric$dispatch_time),units='mins'))==8)
  session$setInputs(reset_call=1)
  stopifnot(is.null(shared_lookup(incident_service,token)))
})
cat('PASS: shared incident identity, hospital marker/legend, Caller-active movement, crew arrival, supervisor upsert, and reset isolation\n')
