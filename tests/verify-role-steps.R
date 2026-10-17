source('app/app.R')
wait_store <- shared_new(eta_bundle,shared_route,hospital_routes=hospital_routes)
wait_link <- shared_create(wait_store,zone='Astoria')
wait_token <- sub('^#incident=','',wait_link$fragment)
shiny::testServer(caller_server,args=list(lookup=function(t)shared_lookup(wait_store,t)),{
  session$setInputs(linkToken=wait_token)
  stopifnot(grepl('Waiting for ambulance confirmation',paste(output$tracking,collapse=' ')),
    !grepl('Assigned ambulance',paste(output$legend,collapse=' ')),caller_map(current())$waiting_for_confirmation)
  shared_claim(wait_store,wait_link$incident_id)
  session$flushReact()
  shown <- paste(output$tracking,collapse=' ')
  stopifnot(grepl('Ambulance assigned',shown),!grepl('Waiting for ambulance confirmation',shown),grepl('Around',shown))
})
shiny::testServer(server, {
  session$setInputs(choose_responder=1)
  session$setInputs(login_username=demo_accounts$responder$username,
    login_password=demo_accounts$responder$password, submit_login=1)
  stopifnot(output$responder_stage=='assignment')
  # Caller-created incidents wait until responder acceptance; frozen ETA unchanged.
  pending <- shared_create(incident_service,zone='Astoria')
  pd <- incident_service$incidents[[pending$incident_id]]
  stopifnot(grepl('after the responder confirms',as.character(shared_eta_card(pd,caller_view=TRUE))))
  frozen <- pd$eta
  shared_claim(incident_service,pending$incident_id)
  pd <- incident_service$incidents[[pending$incident_id]]
  html <- as.character(shared_eta_card(pd,caller_view=TRUE))
  stopifnot(grepl('ESTIMATED SCENE ARRIVAL',html),grepl('Around',html),identical(frozen,pd$eta))
  shared_advance(incident_service,pending$incident_id,720)
  shared_transport(incident_service,pending$incident_id,demo_hospitals[[1]])
  stopifnot(is.null(shared_eta_card(incident_service$incidents[[pending$incident_id]],caller_view=TRUE)))
  shared_revoke(incident_service,pending$incident_id)

  session$setInputs(simulate_call=1)
  stopifnot(output$responder_stage=='respond')
  session$setInputs(begin_transport=1)
  stopifnot(output$responder_stage=='respond')
  session$setInputs(advance_scene=1)
  stopifnot(output$responder_stage=='hospital')
  session$setInputs(care_requirement='Trauma assessment',hospital_choice='a',begin_transport=2)
  stopifnot(output$responder_stage=='transport')
  session$setInputs(arrived_hospital=1)
  stopifnot(output$responder_stage=='transport')
  shared_responder_state$transport_elapsed<-480
  shared_transport_advance(incident_service,shared_responder_state$incident_id,480)
  session$setInputs(arrived_hospital=2)
  stopifnot(output$responder_stage=='complete')
  complete <- paste(output$transport_progress,collapse=' ')
  stopifnot(grepl('Trip complete',complete),grepl('Start another demo call',complete),grepl('simulated time',complete))
  stamp <- format(incident_service$incidents[[shared_responder_state$incident_id]]$hospital_arrived_at,
                  '%b %d, %I:%M:%S %p %Z',tz='America/New_York')
  stopifnot(grepl(stamp,complete,fixed=TRUE))
  session$setInputs(new_demo_call=1)
  stopifnot(output$responder_stage=='assignment')
})
cat('PASS: responder stages, premature action guards, completion and reset\n')
