# Executable integration checks, also safe for testthat::test_dir('tests').
root <- if(file.exists('app/app.R')) '.' else '..'
old_wd <- getwd()
setwd(root)
local({
  on.exit(setwd(old_wd))
  library(shiny)
  app_env <- new.env(parent=globalenv())
  source('app/app.R',local=app_env)
  with(app_env, {
    now <- as.POSIXct('2026-10-16 16:00:00',tz='UTC')
    published <- NULL
    store <- shared_new(eta_bundle,shared_route,function(d)published <<- d,hospital_routes=hospital_routes)
    a <- shared_create(store,now=now);b <- shared_create(store,now=now,zone='Flushing')
    token <- sub('#incident=','',a$fragment,fixed=TRUE)
    stopifnot(shared_lookup(store,token,now)$incident_id==a$incident_id,
      is.null(shared_lookup(store,b$incident_id,now)),
      is.null(shared_lookup(store,'invalid',now)),
      is.null(shared_lookup(store,token,now+1800)),nrow(shared_records(store))==0)
    original <- shared_lookup(store,token,now)$eta
    stopifnot(original$prediction$status=='experimental_synthetic')
    d <- shared_claim(store,a$incident_id)
    stopifnot(inherits(try(shared_claim(store,a$incident_id),silent=TRUE),'try-error'))
    shared_advance(store,a$incident_id,600,now+20)
    stopifnot(shared_lookup(store,token,now+20)$status=='approaching',nrow(shared_records(store))==0)
    shared_advance(store,a$incident_id,720,now+24)
    d <- shared_lookup(store,token,now+24)
    stopifnot(d$status=='arrived',d$arrived_at==now+720,is.null(d$confirmed_destination),
      identical(original,d$eta),nrow(shared_records(store))==1,
      !any(grepl('token|fragment',names(shared_records(store)))))
    q <- response_metric_to_qc(shared_records(store),'call_to_scene',allow_mock=FALSE)
    stopifnot(qc_validate(q)$completed$response_minutes==12)
    shared_transport(store,a$incident_id,demo_hospitals[[1]],now+25)
    d <- shared_lookup(store,token,now+25)
    stopifnot(d$confirmed_destination$name==demo_hospitals[[1]]$name,
      is.null(shared_lookup(store,sub('#incident=','',b$fragment,fixed=TRUE),now)$confirmed_destination))
    shared_transport_advance(store,a$incident_id,480,now+40)
    shared_hospital_arrival(store,a$incident_id,now+41)
    q <- response_metric_to_qc(shared_records(store),'transport_to_hospital',allow_mock=FALSE)
    stopifnot(qc_validate(q)$completed$response_minutes==8,nrow(shared_records(store))==1,
      identical(original,shared_lookup(store,token,now+41)$eta))
    shared_advance(store,a$incident_id,720,now+42)
    stopifnot(nrow(shared_records(store))==1,
      inherits(try(shared_advance(store,a$incident_id,1),silent=TRUE),'try-error'))
    missing <- shared_create(store,now=now,weather_inputs=list(wind_speed_mph=NA_real_))
    stopifnot(store$incidents[[missing$incident_id]]$eta$prediction$status=='unavailable')
    testServer(caller_server,args=list(lookup=function(t)shared_lookup(store,t,now+42)),{
      session$setInputs(linkToken=token);session$flushReact()
      stopifnot(current()$incident_id==a$incident_id,
        !grepl('95% prediction range',paste(as.character(output$tracking),collapse=' ')),
        grepl('Hospital arrival confirmed',paste(as.character(output$tracking),collapse=' ')),
        grepl('95% prediction range',as.character(shared_eta_card(current()))),
        grepl('Demo Hospital A',paste(as.character(output$destination),collapse=' ')))
      shared_revoke(store,a$incident_id)
      session$setInputs(linkToken='invalid');session$flushReact()
      stopifnot(is.null(current()),is.null(output$tracking),is.null(output$destination))
    })
    stopifnot(is.null(shared_lookup(store,token,now+43)))
    for(h in demo_hospitals) {
      trip <- shared_new(eta_bundle,shared_route,hospital_routes=hospital_routes)
      link <- shared_create(trip)
      invisible(shared_claim(trip,link$incident_id))
      shared_advance(trip,link$incident_id,720)
      stopifnot(is.null(caller_map(trip$incidents[[link$incident_id]])$hospital))
      shared_transport(trip,link$incident_id,h)
      map <- caller_map(trip$incidents[[link$incident_id]])
      stopifnot(map$hospital$id==h$id,length(map$hospital_route)==nrow(hospital_routes[[h$id]]),
        abs(map$hospital$lat-h$lat)<1e-8)
    }
    no_route <- shared_new(eta_bundle,shared_route)
    no_route_link <- shared_create(no_route)
    invisible(shared_claim(no_route,no_route_link$incident_id))
    shared_advance(no_route,no_route_link$incident_id,720)
    shared_transport(no_route,no_route_link$incident_id,demo_hospitals[[1]])
    stopifnot(is.null(caller_map(no_route$incidents[[no_route_link$incident_id]])$hospital_route),
      inherits(try(shared_hospital_arrival(no_route,no_route_link$incident_id),silent=TRUE),'try-error'))
    # A caller already open before crew confirmation must update without a timer,
    # a new linkToken, a manual refresh, or reopening its private link.
    live <- shared_new(eta_bundle,shared_route,hospital_routes=hospital_routes)
    live_link <- shared_create(live)
    live_token <- sub('#incident=','',live_link$fragment,fixed=TRUE)
    invisible(shared_claim(live,live_link$incident_id))
    shared_advance(live,live_link$incident_id,720)
    testServer(caller_server,args=list(lookup=function(t)shared_lookup(live,t)),{
      session$setInputs(linkToken=live_token);session$flushReact()
      stopifnot(grepl('Hospital update coming soon',paste(as.character(output$destination),collapse=' ')),
        is.null(jsonlite::fromJSON(output$map)$hospital),
        is.null(jsonlite::fromJSON(output$map)$hospital_route))
      shared_transport(live,live_link$incident_id,demo_hospitals[[1]])
      session$flushReact()
      stopifnot(grepl('Demo Hospital A',paste(as.character(output$destination),collapse=' ')),
        grepl('Your crew confirmed the hospital',paste(as.character(output$destination),collapse=' ')))
      map <- jsonlite::fromJSON(output$map)
      stopifnot(map$hospital$id=='a',length(map$hospital_route)>2,map$transporting,
        !grepl('Demo Hospital C',output$map))
      initial <- map$ambulance
      shared_transport_advance(live,live_link$incident_id,240)
      session$flushReact()
      midway <- jsonlite::fromJSON(output$map)
      stopifnot(!identical(initial,midway$ambulance))
      shared_transport_advance(live,live_link$incident_id,480)
      shared_hospital_arrival(live,live_link$incident_id)
      session$flushReact()
      final <- jsonlite::fromJSON(output$map)
      stopifnot(final$arrived,abs(final$ambulance$lat-tail(hospital_routes$a$y,1))<1e-8,
        abs(final$ambulance$lng-tail(hospital_routes$a$x,1))<1e-8)
      shared_revoke(live,live_link$incident_id)
      session$flushReact()
      stopifnot(is.null(current()),is.null(output$destination),is.null(output$tracking))
    })
    # Real responder observers create the incident, freeze ETA, and publish arrival.
    responder_store <- shared_new(eta_bundle,shared_route,hospital_routes=hospital_routes)
    role <- reactiveVal('responder')
    testServer(function(input,output,session) {
      authenticated_role <- role; active_role <- role; incident_service <- responder_store
      eval(body(responder_server))
    },{
      session$setInputs(demo_conditions='Dry + light traffic',shared_incident='',simulate_call=1)
      session$flushReact()
      id <- shared_id();stopifnot(!is.null(id),demo_started())
      frozen <- responder_store$incidents[[id]]$eta
      session$setInputs(advance_scene=1);session$flushReact()
      stopifnot(responder_store$incidents[[id]]$status=='arrived',nrow(shared_records(responder_store))==1)
      session$setInputs(care_requirement='Trauma assessment',hospital_choice='a',begin_transport=1)
      session$flushReact()
      stopifnot(responder_store$incidents[[id]]$confirmed_destination$name=='Demo Hospital A')
      shared_transport_advance(responder_store,id,480)
      transport_elapsed(480)
      session$setInputs(arrived_hospital=1);session$flushReact()
      stopifnot(!is.null(responder_store$incidents[[id]]$hospital_arrived_at),
        identical(frozen,responder_store$incidents[[id]]$eta))
      role(NULL);session$flushReact()
      session$setInputs(simulate_call=2);session$flushReact()
      stopifnot(length(responder_store$incidents)==1)
    })
    # Caller-created incident is adopted, without replacing its assignment/model.
    queued <- shared_new(eta_bundle,shared_route,hospital_routes=hospital_routes)
    link <- shared_create(queued,now=now)
    before <- queued$incidents[[link$incident_id]]$eta
    role <- reactiveVal('responder')
    testServer(function(input,output,session) {
      authenticated_role <- role; active_role <- role; incident_service <- queued
      eval(body(responder_server))
    },{
      session$setInputs(shared_incident=link$incident_id,simulate_call=1)
      session$flushReact()
      stopifnot(shared_id()==link$incident_id,length(queued$incidents)==1,
        identical(before,queued$incidents[[link$incident_id]]$eta))
      session$setInputs(reset_call=1);session$flushReact()
      stopifnot(is.null(shared_lookup(queued,sub('#incident=','',link$fragment,fixed=TRUE),now)))
    })
    testthat::expect_true(TRUE)
    cat('SHARED INCIDENTS PASS\n')
  })
})
