# Two independent Shiny sessions sharing the same actual app incident service.
source('app/app.R')
created <- shared_create(incident_service,zone='Forest Hills')
paired_id <- created$incident_id
paired_token <- sub('^#incident=','',created$fragment)
shiny::testServer(caller_server,args=list(lookup=function(t)shared_lookup(incident_service,t)),{
  caller_session <- session
  session$setInputs(linkToken=paired_token)
  stopifnot(grepl('Waiting for responder assignment',paste(output$tracking,collapse=' ')))
  responder_session <- shiny::MockShinySession$new()
  on.exit(responder_session$close(),add=TRUE)
  shiny::withReactiveDomain(responder_session,{
    responder_server(responder_session$input,responder_session$output,responder_session,
      shiny::reactiveVal('responder'),shiny::reactiveVal('responder'),incident_service)
    session <- responder_session
    session$flushReact()
    # No opaque ID selection needed: the default action must accept waiting caller.
    session$setInputs(simulate_call=1)
    stopifnot(shared_responder_state$incident_id==paired_id,
      sub('^#incident=','',shared_responder_state$incident_link$fragment)==paired_token,
      length(incident_service$incidents)==1)
    caller_session$flushReact()
    stopifnot(grepl('Ambulance assigned',paste(output$tracking,collapse=' ')))
    session$setInputs(advance_scene=1,care_requirement='Trauma assessment',hospital_choice='a')
    session$setInputs(begin_transport=1)
    stopifnot(!is.null(shared_lookup(incident_service,paired_token)$hospital))
    shared_transport_advance(incident_service,paired_id,480)
    shared_responder_state$transport_elapsed <- 480
    session$setInputs(arrived_hospital=1)
  })
  caller_session$flushReact()
  shown <- paste(output$tracking,collapse=' ')
  stopifnot(current()$incident_id==paired_id,grepl('Hospital arrival confirmed',shown),
    !is.null(caller_map(current())$hospital),
    grepl(demo_hospitals[[1]]$name,paste(output$destination,collapse=' '),fixed=TRUE),
    nrow(shared_records(incident_service))==1)
  shared_revoke(incident_service,paired_id)
  caller_session$flushReact()
  stopifnot(is.null(current()),is.null(output$tracking),is.null(output$destination))
})
cat('PASS: independent Caller and Responder sessions share default queued incident, hospital route, completion and revocation\n')
