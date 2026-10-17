source(file.path('..','app','supervisor.R'),local=TRUE)
test_that('active-day demo is deterministic, planted and calendar-correct',{
 raw <- dashboard_load_fixture(repo_root,'stable')
 now <- as.POSIXct('2026-10-17 12:00:00',tz='America/New_York')
 normal <- supervisor_simulate_day(raw,'Astoria','High','call_to_scene',as_of=now)
 delayed <- supervisor_simulate_day(raw,'Astoria','High','call_to_scene',TRUE,now)
 expect_identical(normal,supervisor_simulate_day(raw,'Astoria','High','call_to_scene',as_of=now))
 expect_equal(nrow(delayed),12L)
 expect_equal(anyDuplicated(delayed$incident_id),0L)
 q <- response_metric_to_qc(delayed,'call_to_scene',allow_mock=FALSE)
 d <- qc_validate(q)$completed
 expect_equal(nrow(d),12L)
 day <- as.Date(d$dispatch_time,tz='America/New_York')
 expect_equal(length(unique(day)),1L)
 expect_true(all(table(day)==12L));expect_true(all(day==as.Date(now)))
 expect_equal(tail(d$response_minutes,3),rep(30,3))
 # Independently sum the 12 last-day values, including the planted 90 minutes.
 n <- qc_validate(response_metric_to_qc(normal,'call_to_scene',allow_mock=FALSE))$completed
 expect_equal(mean(tail(d$response_minutes,12)),(sum(tail(n$response_minutes,12)[1:9])+90)/12)
 expect_identical(normal[1:9,],delayed[1:9,])
 base <- supervisor_analyze(raw,response_metric_to_qc(response_metric_empty_records(),'call_to_scene',allow_mock=FALSE),TRUE)
 after <- supervisor_analyze(raw,response_metric_to_qc(delayed,'call_to_scene',allow_mock=FALSE),TRUE)
 expect_identical(lapply(base$groups,function(g)g$limits),lapply(after$groups,function(g)g$limits))
 # The other metric is independently selectable; extreme transport is 30 min.
 transport <- supervisor_simulate_day(raw,'Astoria','High','transport_to_hospital',TRUE,now)
 t <- qc_validate(response_metric_to_qc(transport,'transport_to_hospital',allow_mock=FALSE))$completed
 expect_equal(tail(t$response_minutes,3),rep(30,3))
 empty <- supervisor_period_subgroups(data.frame(dispatch_time=as.POSIXct(character()),response_minutes=numeric()),8,1,'day')
 expect_equal(nrow(empty),0L)
 constant <- data.frame(dispatch_time=as.POSIXct(rep('2026-10-16 10:00:00',3),tz='America/New_York'),response_minutes=rep(8,3))
 cg <- supervisor_period_subgroups(constant,8,0,'day')
 expect_equal(cg$mean,8);expect_equal(cg$range,0);expect_false(cg$signal)
 subgroup <- supervisor_period_subgroups(d,base$groups[['Astoria/High']]$limits$center,
   base$groups[['Astoria/High']]$limits$sigma,'day')
 expect_equal(tail(subgroup$mean,1),mean(tail(d$response_minutes,12)))
 expect_true(tail(subgroup$signal,1))
})
