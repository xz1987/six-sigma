# H-12 reproducible verification of the ETA view already wired under H-23.
# Does not refit after completion, alter UI styling or create baseline approval.
# From the repository root: Rscript app/eta-view.R
eta_view_verify <- function() {
  app <- new.env(parent=globalenv())
  invisible(source('app/app.R',local=app))
  with(app, {
    now <- as.POSIXct('2026-10-16T16:00:00',format='%Y-%m-%dT%H:%M:%S',tz='UTC')
    store <- shared_new(eta_bundle,shared_route,hospital_routes=hospital_routes)
    example <- shared_create(store,now=now)
    id <- example$incident_id
    original <- store$incidents[[id]]$eta
    p <- original$prediction
    stopifnot(p$status=='experimental_synthetic')

    # Independently reconstruct the existing fitted model's log-scale prediction
    # and individual prediction interval; do not use eta_log_prediction here.
    fit <- eta_bundle$model$fit
    features <- eta_prepare(store$incidents[[id]]$eta_inputs)$features
    x <- model.matrix(delete.response(terms(fit)),features,contrasts.arg=fit$contrasts)
    log_center <- as.numeric(x %*% coef(fit))
    sigma <- sqrt(sum(residuals(fit)^2)/df.residual(fit))
    se <- sqrt(sigma^2 + as.numeric(x %*% vcov(fit) %*% t(x)))
    expected <- exp(log_center + c(0,-1,1)*qt(.975,df.residual(fit))*se)
    stopifnot(isTRUE(all.equal(unname(unlist(p[c('median_minutes','lower_minutes','upper_minutes')])),expected,tolerance=1e-10)))
    for (pair in list(c('arrival_median','median_minutes'),c('arrival_lower','lower_minutes'),c('arrival_upper','upper_minutes'))) {
      seconds <- as.numeric(difftime(p[[pair[1]]],now,units='secs'))
      stopifnot(abs(seconds-60*p[[pair[2]]])<1e-6)
    }
    cat('EXPERIMENTAL ETA — SYNTHETIC DATA ONLY\n')
    cat('Assignment: 2026-10-16 16:00:00 UTC; Astoria/High; Rain/Moderate; wind 10 mph; gust 16 mph\n')
    cat(sprintf('Median duration: %.9f min; 95%% individual prediction interval: [%.9f, %.9f] min\n',
                p$median_minutes,p$lower_minutes,p$upper_minutes))
    clock <- function(t)format(t,'%Y-%m-%d %H:%M:%S UTC',tz='UTC')
    cat('Predicted scene arrival:',clock(p$arrival_median),'\n')
    cat('Arrival range:',clock(p$arrival_lower),'to',clock(p$arrival_upper),'\n')
    cat('PASS: independent model-matrix prediction and interval agree.\n')

    raw <- read.csv('data/stable.csv',stringsAsFactors=FALSE)
    period <- qc_period('2025-01-01T00:00:00-0500','2026-01-01T00:00:00-0500',
                        '2026-01-01T00:00:00-0500','2027-01-01T00:00:00-0500')
    before <- qc_analyze(raw,period)
    invisible(shared_claim(store,id))
    # Advance the synthetic clock automatically; no manual arrival input.
    for (elapsed in seq(0,720,by=30)) shared_advance(store,id,elapsed,now+elapsed)
    done <- store$incidents[[id]]
    stopifnot(done$status=='arrived',identical(original,done$eta),done$elapsed==720,
              as.numeric(difftime(done$arrived_at,now,units='mins'))==12)
    card <- paste(as.character(shared_eta_card(done)),collapse=' ')
    stopifnot(grepl('95% prediction range',card,fixed=TRUE),
              grepl('Actual replay elapsed since assignment: 12.0 min',card,fixed=TRUE),
              grepl('EXPERIMENTAL ARRIVAL ESTIMATE',card,fixed=TRUE))
    q <- response_metric_to_qc(shared_records(store),'call_to_scene',allow_mock=FALSE)
    fields <- c('incident_id','zone','call_priority','weather_condition','dispatch_time','on_scene_time')
    # Canonical adapter returns offset timestamp strings, matching raw CSV fields.
    after <- qc_analyze(rbind(raw[,fields],q[,fields]),period)
    stopifnot(identical(lapply(before$groups,function(g)g$limits),lapply(after$groups,function(g)g$limits)))
    cat('PASS: automatic replay ends at 12.0 min; original assignment prediction unchanged.\n')
    cat('PASS: adding the completed record leaves all 15 core QC baseline limits unchanged.\n')

    missing <- shared_create(store,now=now,weather_inputs=list(wind_speed_mph=NA_real_))
    unavailable <- store$incidents[[missing$incident_id]]
    stopifnot(unavailable$eta$prediction$status=='unavailable',
              is.na(unavailable$eta$prediction$median_minutes),
              grepl('Not available yet',as.character(shared_eta_card(unavailable)),fixed=TRUE))
    frozen <- unavailable$eta
    invisible(shared_claim(store,missing$incident_id))
    for (elapsed in seq(0,720,by=30)) shared_advance(store,missing$incident_id,elapsed,now+elapsed)
    stopifnot(identical(frozen,store$incidents[[missing$incident_id]]$eta),
              store$incidents[[missing$incident_id]]$status=='arrived')
    cat('Unavailable-input reason:',unavailable$eta$prediction$reason,'\n')
    cat('PASS: missing wind shows Not available yet; automatic arrival still completes; no zero estimate.\n')
    cat('H-12 ETA VIEW PASS\n')
  })
  invisible(TRUE)
}
if (sys.nframe()==0L) eta_view_verify()
