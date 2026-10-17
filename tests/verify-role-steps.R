source('app/app.R')
wait_store <- shared_new(eta_bundle,shared_route,hospital_routes=hospital_routes)
wait_link <- shared_create(wait_store,zone='Astoria')
wait_token <- sub('^#incident=','',wait_link$fragment)
shiny::testServer(caller_server,args=list(lookup=function(t)shared_lookup(wait_store,t)),{
  session$setInputs(linkToken=wait_token)
  stopifnot(grepl('Waiting for responder assignment',paste(output$tracking,collapse=' ')),
    !grepl('Assigned ambulance',paste(output$legend,collapse=' ')),caller_map(current())$waiting_for_confirmation)
  shared_claim(wait_store,wait_link$incident_id)
  session$flushReact()
  shown <- paste(output$tracking,collapse=' ')
  stopifnot(grepl('Ambulance assigned',shown),!grepl('Waiting for responder assignment',shown),grepl('Around',shown))
})
shiny::testServer(server, {
  session$setInputs(choose_responder=1)
  session$setInputs(login_username=demo_accounts$responder$username,
    login_password=demo_accounts$responder$password, submit_login=1)
  stopifnot(output$responder_stage=='assignment')
  # Caller-created incidents wait until responder acceptance; frozen ETA unchanged.
  pending <- shared_create(incident_service,zone='Astoria')
  pd <- incident_service$incidents[[pending$incident_id]]
  stopifnot(grepl('Waiting for assignment',as.character(shared_eta_card(pd,caller_view=TRUE))))
  frozen <- pd$eta
  shared_claim(incident_service,pending$incident_id)
  pd <- incident_service$incidents[[pending$incident_id]]
  html <- as.character(shared_eta_card(pd,caller_view=TRUE))
  stopifnot(grepl('ESTIMATED SCENE ARRIVAL',html),grepl('Around',html),identical(frozen,pd$eta))
  shared_advance(incident_service,pending$incident_id,720)
  shared_transport(incident_service,pending$incident_id,demo_hospitals[[1]])
  stopifnot(grepl('On the way to hospital',as.character(shared_eta_card(incident_service$incidents[[pending$incident_id]],caller_view=TRUE))))
  shared_revoke(incident_service,pending$incident_id)

  session$setInputs(simulate_call=1)
  stopifnot(output$responder_stage=='respond')
  session$setInputs(begin_transport=1)
  stopifnot(output$responder_stage=='respond')
  session$setInputs(advance_scene=1)
  stopifnot(output$responder_stage=='hospital')
  session$setInputs(care_requirement='Trauma assessment',hospital_choice='a',begin_transport=2)
  stopifnot(output$responder_stage=='transport')
  session$setInputs(next_call=1,arrived_hospital=1)
  stopifnot(output$responder_stage=='transport')
  shared_responder_state$transport_elapsed<-480
  shared_transport_advance(incident_service,shared_responder_state$incident_id,480)
  session$setInputs(arrived_hospital=2)
  stopifnot(output$responder_stage=='complete',grepl('Start next demo call',paste(as.character(output$transport_progress),collapse=' ')))
  previous <- shared_responder_state$incident_id
  token <- sub('#incident=','',shared_responder_state$caller_tracking_link,fixed=TRUE)
  records <- production_response_records()
  session$setInputs(next_call=2)
  stopifnot(output$responder_stage=='respond',shared_responder_state$incident_id!=previous,
    shared_responder_state$demo_elapsed==0,is.null(shared_responder_state$chosen_hospital),
    !is.null(shared_lookup(incident_service,token)$hospital_arrived_at),
    previous %in% production_response_records()$incident_id,
    nrow(production_response_records())==nrow(records))
  session$setInputs(reset_call=1)
  stopifnot(output$responder_stage=='assignment')
})
cat('PASS: responder stages, premature action guards, arrival confirmation, next-call history retention and reset\n')
