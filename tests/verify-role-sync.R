source('app/app.R')
shiny::testServer(server, {
  session$setInputs(choose_responder=1)
  session$setInputs(login_username=demo_accounts$responder$username,
    login_password=demo_accounts$responder$password,submit_login=1,simulate_call=1)
  id <- shared_responder_state$incident_id
  token <- sub('#incident=','',shared_responder_state$incident_link$fragment,fixed=TRUE)
  session$setInputs(`caller-linkToken`=token)
  shared_advance(incident_service,id,600)
  session$flushReact()
  # Reproduce a UI counter incorrectly showing scene arrival.
  shared_responder_state$demo_elapsed<-720
  session$flushReact()
  stopifnot(output$responder_stage=='respond',
    grepl('Approaching the scene',paste(as.character(output$`caller-tracking`),collapse=' ')),
    grepl('Approaching scene',paste(as.character(output$call_status),collapse=' ')))
  shared_responder_state$last_tick <- Sys.time()-2
  session$elapse(1000)
  stopifnot(incident_service$incidents[[id]]$elapsed==630,
    shared_responder_state$demo_elapsed==630)
  shared_advance(incident_service,id,720)
  session$flushReact()
  stopifnot(output$responder_stage=='hospital',
    grepl('Ambulance on scene',paste(as.character(output$`caller-tracking`),collapse=' ')),
    shared_responder_state$demo_elapsed==720)
  snapshot <- responder_trip_snapshot()
  stopifnot(snapshot$incident_id==id,snapshot$zone==incident_service$incidents[[id]]$zone)
  session$setInputs(care_requirement='Trauma assessment',hospital_choice='a',begin_transport=1)
  stopifnot(output$responder_stage=='transport',
    grepl('Transporting to confirmed hospital',paste(as.character(output$`caller-tracking`),collapse=' ')))
  shared_transport_advance(incident_service,id,480)
  session$flushReact()
  session$setInputs(arrived_hospital=1)
  stopifnot(output$responder_stage=='complete',
    grepl('Hospital arrival confirmed',paste(as.character(output$`caller-tracking`),collapse=' ')))
})
cat('PASS: shared role stages, mismatched counter recovery, canonical timer and actual Supervisor zone\n')
