source('app/app.R')
shiny::testServer(server, {
  session$setInputs(choose_responder=1)
  session$setInputs(login_username=demo_accounts$responder$username,
    login_password=demo_accounts$responder$password,submit_login=1,simulate_call=1)
  previous <- shared_responder_state$incident_id
  session$setInputs(advance_scene=1)
  stopifnot(output$responder_stage=='hospital')
  session$setInputs(choose_caller=1,createDispatch=1)
  link <- created_dispatch()
  id <- link$incident_id
  token <- sub('#incident=','',link$fragment,fixed=TRUE)
  session$setInputs(`caller-linkToken`=token)
  stopifnot(id!=previous,shared_responder_state$incident_id==id,
    output$responder_stage=='assignment',shared_responder_state$demo_elapsed==0,
    !shared_responder_state$demo_running,!shared_responder_state$demo_started,
    grepl('Waiting for responder assignment',paste(as.character(output$`caller-tracking`),collapse=' ')))
  session$elapse(2000)
  stopifnot(shared_lookup(incident_service,token)$elapsed==0,
    !isTRUE(shared_lookup(incident_service,token)$claimed),
    !is.null(incident_service$incidents[[previous]]$arrived_at))
  session$setInputs(choose_responder=2)
  session$setInputs(login_username=demo_accounts$responder$username,
    login_password=demo_accounts$responder$password,submit_login=2)
  stopifnot(output$responder_stage=='assignment',grepl(id,paste(as.character(output$shared_queue),collapse=' ')))
  # A stale dropdown input must not cause a new or previous incident to be claimed.
  session$setInputs(shared_incident='',simulate_call=2)
  stopifnot(output$responder_stage=='respond',shared_responder_state$incident_id==id,
    isTRUE(shared_lookup(incident_service,token)$claimed),shared_lookup(incident_service,token)$elapsed==0)
  session$setInputs(advance_scene=2)
  stopifnot(output$responder_stage=='hospital',shared_lookup(incident_service,token)$elapsed==720)
})
cat('PASS: caller-created demo resets old step 3 to step 1; acceptance starts the exact queued incident\n')
