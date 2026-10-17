# Standalone runnable H-11 tests; test_file changes cwd.
eta_root <- if(file.exists('app/eta.R')) '.' else if(file.exists('../../app/eta.R')) '../..' else stop('repository root not found')
source(file.path(eta_root,'app/eta.R'))
eta_fixture_data<-function(scenario)read.csv(file.path(eta_root,'data/eta/v1',paste0(scenario,'.csv')),stringsAsFactors=FALSE)
eta_effect_data<-eta_fixture_data('weather_effect')
eta_null_data<-eta_fixture_data('no_weather_effect')
eta_effect_bundle<-eta_validate(eta_effect_data)
eta_null_bundle<-eta_validate(eta_null_data)
eta_parts<-eta_partitions(eta_effect_data)
eta_train_model<-eta_fit(eta_parts$train)
eta_supported_row<-eta_parts$test[which(eta_candidate_predict(eta_train_model,eta_parts$test)$status=='candidate_for_evaluation')[1],,drop=FALSE]

testthat::test_that('independent intercept-only prediction arithmetic is correct', {
  fit<-lm(y~1,data=data.frame(y=c(-1,0,1)))
  got<-eta_log_prediction(fit,data.frame(dummy=0))
  half<-4.302652729911275*sqrt(4/3)
  testthat::expect_equal(got$median_minutes,1,tolerance=1e-12)
  testthat::expect_equal(got$mean_minutes,exp(.5),tolerance=1e-12)
  testthat::expect_equal(got$lower_minutes,exp(-half),tolerance=1e-10)
  testthat::expect_equal(log(got$upper_minutes),half,tolerance=1e-9)
  confidence<-predict(fit,data.frame(dummy=0),interval='confidence')
  testthat::expect_gt(log(got$upper_minutes),confidence[1,'upr'])
})

testthat::test_that('non-intercept fit uses independent coefficient and leverage arithmetic', {
  # X centered, Sxx=2; beta intercept=2/3, slope=1, SSE=2/3, df=1.
  fit<-lm(y~x,data=data.frame(x=c(-1,0,1),y=c(0,0,2)))
  got<-eta_log_prediction(fit,data.frame(x=.5))
  mu<-2/3+.5;h<-1/3+.5^2/2;s<-sqrt(2/3)
  half<-12.7062047364321*s*sqrt(1+h)
  testthat::expect_equal(log(got$median_minutes),mu,tolerance=1e-12)
  testthat::expect_equal(log(got$mean_minutes),mu+1/3,tolerance=1e-12)
  testthat::expect_equal(log(got$upper_minutes),mu+half,tolerance=1e-9)
  testthat::expect_equal(log(got$lower_minutes),mu-half,tolerance=1e-9)
  testthat::expect_error(eta_log_prediction(lm(y~1,data=data.frame(y=c(1,1,1))),data.frame(dummy=0)),'degenerate')
})

testthat::test_that('positive effect passes fixed gates and negative control is unavailable', {
  testthat::expect_identical(eta_effect_bundle$status,'validated_synthetic')
  testthat::expect_true(all(eta_effect_bundle$validation$gates))
  testthat::expect_true(all(eta_effect_bundle$test$gates))
  testthat::expect_true(eta_effect_bundle$diagnostics_pass)
  testthat::expect_identical(eta_null_bundle$status,'unavailable')
  testthat::expect_false(eta_null_bundle$validation$gates[['weather_gain']])
  testthat::expect_false(eta_null_bundle$test$gates[['weather_gain']])
  testthat::expect_null(eta_null_bundle$model)
  testthat::expect_true(all(is.na(eta_predict(eta_null_bundle,eta_supported_row)$median_minutes)))
})

testthat::test_that('median and arrival ranges use assignment inputs, independent of eventual arrival', {
  p<-eta_predict(eta_effect_bundle,eta_supported_row)
  testthat::expect_identical(p$status,'experimental_synthetic')
  testthat::expect_gt(p$mean_minutes,p$median_minutes)
  testthat::expect_lt(p$lower_minutes,p$median_minutes)
  testthat::expect_gt(p$upper_minutes,p$median_minutes)
  testthat::expect_equal(as.numeric(difftime(p$arrival_median,eta_time(eta_supported_row$dispatch_time),units='secs')),p$median_minutes*60)
  poisoned<-eta_supported_row;poisoned$on_scene_time<-'not-an-arrival'
  poisoned$scenario<-'poison';poisoned$split<-'poison';poisoned$incident_id<-'different'
  poisoned$base_log_minutes<-1e9;poisoned$response_minutes<-1e9
  testthat::expect_equal(eta_predict(eta_effect_bundle,poisoned),p)
  assignment_only<-eta_supported_row;assignment_only$on_scene_time<-NULL
  testthat::expect_equal(eta_predict(eta_effect_bundle,assignment_only),p)
  testthat::expect_true(all(!grepl('arrival|on_scene|incident|event|truth|scenario|split|response_minutes',names(coef(eta_train_model$fit)))))
})

testthat::test_that('missing invalid unseen and incompatible inputs return unavailable row by row', {
  cases<-list(zone='Unknown Zone',call_priority='Critical',weather_condition='Unknown',
    precipitation_intensity='Unknown',wind_speed_mph=NA,wind_gust_mph=-1,
    dispatch_time='2026-02-01 12:00:00',weather_observed_at='not-time')
  for(field in names(cases)) {
    d<-eta_supported_row;d[[field]]<-cases[[field]]
    p<-eta_predict(eta_effect_bundle,d)
    testthat::expect_identical(p$status,'unavailable',info=field)
    testthat::expect_true(is.na(p$median_minutes),info=field)
    testthat::expect_true(nzchar(p$reason),info=field)
  }
  for(field in c('weather_condition','precipitation_intensity','wind_speed_mph','weather_observed_at')) {
    d<-eta_supported_row;d[[field]]<-NULL
    testthat::expect_identical(eta_predict(eta_effect_bundle,d)$status,'unavailable')
  }
  for(value in c(-1,Inf,NaN,1e4)){
    d<-eta_supported_row;d$wind_speed_mph<-value
    testthat::expect_identical(eta_predict(eta_effect_bundle,d)$status,'unavailable')
  }
  d<-eta_supported_row;d$weather_condition<-'Dry';d$precipitation_intensity<-'Heavy'
  testthat::expect_match(eta_predict(eta_effect_bundle,d)$reason,'incompatible')
  d<-eta_supported_row;d$weather_observed_at<-format(eta_time(d$dispatch_time)+1,'%Y-%m-%dT%H:%M:%S%z',tz='UTC')
  testthat::expect_match(eta_predict(eta_effect_bundle,d)$reason,'future')
  d$weather_observed_at<-format(eta_time(d$dispatch_time)-3600,'%Y-%m-%dT%H:%M:%S%z',tz='UTC')
  testthat::expect_match(eta_predict(eta_effect_bundle,d)$reason,'stale')
  mixed<-rbind(eta_supported_row,eta_supported_row);mixed$zone[2]<-'Elsewhere'
  testthat::expect_identical(eta_predict(eta_effect_bundle,mixed)$status,c('experimental_synthetic','unavailable'))
  testthat::expect_equal(nrow(eta_predict(eta_effect_bundle,eta_supported_row[FALSE,])),0)
})

testthat::test_that('sparse combination and joint/numeric support guards are enforced', {
  p<-eta_prepare(eta_supported_row)
  sparse<-eta_effect_bundle;sparse$model$keys[p$key]<-4
  testthat::expect_match(eta_predict(sparse,eta_supported_row)$reason,'sparse')
  hour<-eta_effect_bundle;hour$model$hours[as.character(p$hour)]<-19
  testthat::expect_match(eta_predict(hour,eta_supported_row)$reason,'hour')
  leverage<-eta_effect_bundle;leverage$model$max_leverage<-0
  testthat::expect_match(eta_predict(leverage,eta_supported_row)$reason,'joint')
  wind<-eta_supported_row;wind$wind_speed_mph<-100;wind$wind_gust_mph<-110
  testthat::expect_match(eta_predict(eta_effect_bundle,wind)$reason,'outside training')
})

testthat::test_that('chronological partitions reject labels duplicates and shared-event leakage', {
  wrong<-eta_effect_data;wrong$split[1]<-'test'
  testthat::expect_error(eta_partitions(wrong),'split labels')
  dup<-rbind(eta_effect_data,eta_effect_data[1,])
  testthat::expect_error(eta_partitions(dup),'duplicate')
  mixed<-eta_effect_data;mixed$scenario[1]<-'no_weather_effect'
  testthat::expect_error(eta_partitions(mixed),'mixed scenarios')
  leak<-eta_effect_data;leak$weather_event_id[leak$split=='test'][1]<-leak$weather_event_id[1]
  testthat::expect_error(eta_partitions(leak),'crosses partitions')
  block<-eta_effect_data;block$weather_block_id[block$split=='test'][1]<-block$weather_block_id[1]
  testthat::expect_error(eta_partitions(block),'crosses partitions')
  dates<-eta_effect_data;dates$dispatch_time[1]<-'2024-01-01T00:00:00+0000'
  testthat::expect_error(eta_partitions(dates),'outside frozen')
  testthat::expect_equal(eta_effect_bundle$late_completions_excluded,2)
  testthat::expect_equal(unname(eta_effect_bundle$split_counts),c(3660L,1858L,1680L))
  testthat::expect_true(all(eta_effect_bundle$trained_on%in%eta_parts$train$incident_id))
  testthat::expect_false(any(eta_effect_bundle$trained_on%in%c(eta_parts$validation$incident_id,eta_parts$test$incident_id)))
})

testthat::test_that('training is unchanged by validation or final-test outcomes and unrelated columns', {
  altered<-eta_effect_data
  changed<-which(altered$split!='train')
  altered$on_scene_time[changed]<-format(eta_time(altered$dispatch_time[changed])+6000,'%Y-%m-%dT%H:%M:%S%z',tz='UTC')
  changed_model<-eta_fit(eta_partitions(altered)$train)
  testthat::expect_equal(coef(changed_model$fit),coef(eta_train_model$fit))
  extra<-eta_parts$train;extra$truth<-seq_len(nrow(extra));extra$actual_duration<-10000
  testthat::expect_equal(coef(eta_fit(extra)$fit),coef(eta_train_model$fit))
  testthat::expect_identical(eta_predict(eta_train_model,eta_supported_row)$status,'unavailable')
})

testthat::test_that('constant empty tiny rank-deficient and sparse-category fits are unavailable', {
  testthat::expect_identical(eta_fit(eta_parts$train[FALSE,])$status,'unavailable')
  testthat::expect_identical(eta_fit(eta_parts$train[1,,drop=FALSE])$status,'unavailable')
  d<-eta_parts$train;d$on_scene_time<-format(eta_time(d$dispatch_time)+600,'%Y-%m-%dT%H:%M:%S%z',tz='UTC')
  testthat::expect_match(eta_fit(d)$reason,'constant')
  d<-eta_parts$train;d$zone<-'Astoria'
  testthat::expect_match(eta_fit(d)$reason,'categorical')
  d<-eta_parts$train;d$wind_speed_mph<-10;d$wind_gust_mph<-15
  testthat::expect_match(eta_fit(d)$reason,'rank-deficient')
})

testthat::test_that('assignment time transforms handle UTC offsets and daylight saving', {
  d<-eta_supported_row[rep(1,4),]
  d$dispatch_time<-c('2025-11-02T05:30:00+0000','2025-11-02T06:30:00+0000','2026-01-05T12:30:00+0000','2026-01-05T17:30:00+0000')
  p<-eta_prepare(d)
  testthat::expect_equal(p$hour,c(1L,1L,7L,12L))
  testthat::expect_equal(p$features$peak,c(0L,0L,1L,0L))
  testthat::expect_equal(p$features$weekday,c(0L,0L,1L,1L))
  testthat::expect_equal(eta_time('2026-01-05T07:30:00-05:00'),eta_time('2026-01-05T12:30:00Z'))
})


testthat::test_that('neither evaluation stage can rescue failure in the other', {
  changed<-eta_effect_data
  rows<-changed$split=='validation'
  changed$on_scene_time[rows]<-format(eta_time(changed$dispatch_time[rows])+6000,'%Y-%m-%dT%H:%M:%S%z',tz='UTC')
  failed_validation<-eta_validate(changed)
  testthat::expect_false(failed_validation$validation$pass)
  testthat::expect_true(failed_validation$test$pass)
  testthat::expect_identical(failed_validation$status,'unavailable')
  testthat::expect_null(failed_validation$model)
  changed<-eta_effect_data;rows<-changed$split=='test'
  changed$on_scene_time[rows]<-format(eta_time(changed$dispatch_time[rows])+6000,'%Y-%m-%dT%H:%M:%S%z',tz='UTC')
  failed_test<-eta_validate(changed)
  testthat::expect_true(failed_test$validation$pass)
  testthat::expect_false(failed_test$test$pass)
  testthat::expect_identical(failed_test$status,'unavailable')
  testthat::expect_null(failed_test$model)
})

testthat::test_that('impossible calendar dates times and UTC offsets are quietly unavailable', {
  testthat::expect_warning(p<-eta_time(c('2026-02-30T12:00:00+0000','2026-01-01T25:00:00+0000','2026-01-01T12:00:00+2500')),NA)
  testthat::expect_true(all(is.na(p)))
})
