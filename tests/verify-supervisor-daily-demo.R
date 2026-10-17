source('app/app.R')
live_link <- shared_create(incident_service,zone='Astoria')
live_id <- live_link$incident_id
invisible(shared_claim(incident_service,live_id))
shared_responder_state$incident_id <- live_id
shared_responder_state$demo_started <- TRUE
shared_responder_state$demo_running <- TRUE
shared_responder_state$started_at <- incident_service$incidents[[live_id]]$dispatched_at
shiny::testServer(supervisor_server,args=list(production_records=production_response_records),{
 stopifnot(grepl('Try demo mode',paste(output$developer_bar,collapse=' ')))
 session$setInputs(metric='call_to_scene',zone='Astoria',priority='High',time_span='day',
   day_date=as.character(Sys.Date()),history_metric='call_to_scene',history_zone='Astoria',
   history_priority='High',history_phase='monitoring',history_period='30',history_aggregation='day',
   main_tabs='Active tracking',developer_panel_open=TRUE,enter_developer=1,demo_review=TRUE)
 session$setInputs(daily_demo_scenario='abnormal',load_daily_demo=1)
 stopifnot(nrow(active_completed())==0,grepl(live_id,paste(output$shared_trip,collapse=' ')))
 session$setInputs(next_demo_case=1)
 stopifnot(nrow(active_completed())==1,nrow(visible_monitoring())==1)
 session$setInputs(finish_demo_day=1)
 stopifnot(nrow(active_completed())==12,nrow(visible_monitoring())==12,
   grepl('day complete',paste(output$developer_bar_text,collapse=' ')),
   grepl('Start demo day',paste(output$developer_bar,collapse=' ')),
   !is.null(output$i_chart),!is.null(output$mr_chart))
 expected_mean <- mean(qc_validate(response_metric_to_qc(state$developer_calls,'call_to_scene',allow_mock=FALSE))$completed$response_minutes)
 stopifnot(grepl(sprintf('Demo-day average: %.1f min',expected_mean),paste(output$developer_bar_text,collapse=' '),fixed=TRUE))
 stopifnot(grepl('What this view tells you',paste(output$findings_summary,collapse=' ')),
   grepl('Historical variation:',paste(output$findings_summary,collapse=' ')),
   grepl('Context target:',paste(output$findings_summary,collapse=' ')),
   grepl('Configured service goal:',paste(output$findings_summary,collapse=' ')))
 session$setInputs(history_phase='baseline',history_period='full',history_aggregation='week')
 stopifnot(grepl('What this view tells you',paste(output$history_findings_summary,collapse=' ')),
   grepl('weekly means',paste(output$history_findings_summary,collapse=' ')),
   grepl('12 current-day responses are excluded',paste(output$history_findings_summary,collapse=' ')))
 session$setInputs(history_phase='monitoring',history_period='7',history_aggregation='day')
 stopifnot(grepl('No completed subgroups',paste(output$history_findings_summary,collapse=' ')),
   grepl('Latest 7 calendar days',paste(output$history_findings_summary,collapse=' ')))
 stopifnot(grepl('What this view tells you',paste(output$patterns_findings_summary,collapse=' ')),
   grepl('12 records in Astoria / High',paste(output$patterns_findings_summary,collapse=' ')),
   grepl('Neighborhood medians:',paste(output$patterns_findings_summary,collapse=' ')))
 original <- state$developer_calls
 frozen <- lapply(history_analysis()$groups,function(g)g$limits)
 shared_advance(incident_service,live_id,720)
 session$flushReact()
 stopifnot(grepl('On scene',paste(output$shared_trip,collapse=' ')),
           nrow(production_response_records())==1,identical(original,state$developer_calls),
           nrow(active_completed())==12,
           identical(frozen,lapply(history_analysis()$groups,function(g)g$limits)))
 session$setInputs(end_developer=1)
 stopifnot(state$mode=='ordinary',is.null(state$developer_calls),nrow(active_completed())==1)
})
cat('PASS: one active demo day with progressive cases; live trip updates alongside isolated graphs; frozen limits and ordinary records preserved\n')
