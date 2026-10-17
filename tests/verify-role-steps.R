source('app/app.R')
shiny::testServer(server, {
  session$setInputs(choose_responder=1)
  session$setInputs(login_username=demo_accounts$responder$username,
    login_password=demo_accounts$responder$password, submit_login=1)
  stopifnot(output$responder_stage=='assignment')
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
