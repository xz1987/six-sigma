test_that('I-MR arithmetic uses baseline only and strict upper rules', {
  f <- imr_fit(c(4,5,6,5,4))
  expect_equal(f$center, 4.8); expect_equal(f$mr_center, 1)
  expect_equal(f$sigma, 1/1.128); expect_equal(f$i_ucl, 4.8+3/1.128)
  expect_equal(f$i_lcl, 4.8-3/1.128); expect_equal(f$mr_ucl, 3.267)
  s <- imr_monitor(f, c(8, f$i_ucl))
  expect_equal(s$moving_range[1], 4); expect_true(s$individual_signal[1])
  expect_false(s$individual_signal[2]); expect_true(s$variation_signal[1])
  expect_equal(f$center, 4.8)
  expect_equal(imr_fit(rep(10,5))$status, 'degenerate baseline')
  expect_equal(imr_fit(1)$status, 'insufficient baseline data')
  expect_error(imr_fit(c(1, NA)))
})

test_that('timestamp parsing is offset aware, strict and calendar valid', {
  expect_equal(as.numeric(qc_parse_time('2025-01-01T00:00:00-0500')),
               as.numeric(qc_parse_time('2025-01-01T05:00:00Z')))
  expect_true(is.na(qc_parse_time('2025-02-30T00:00:00+00:00')))
  expect_true(is.na(qc_parse_time('2025-01-01T24:00:00Z')))
  expect_true(is.na(qc_parse_time('2025-01-01T00:00:00')))
  expect_true(is.na(qc_parse_time('2025-01-01T00:00:00+2500')))
})

test_that('descriptive chart calculations use type7, counts and local time', {
  b <- qc_box(c(1,2,3,4,100))
  expect_equal(b$q1,2); expect_equal(b$median,3); expect_equal(b$q3,4)
  expect_equal(b$upper_whisker,4); expect_equal(b$outliers,100)
  expect_equal(qc_summary(c(8,9,10,8,9))$p90,9.6)
  d <- data.frame(dispatch_time=qc_parse_time(c('2025-03-09T06:30:00Z','2025-03-09T07:30:00Z')), response_minutes=c(2,4))
  h <- qc_heatmap(d)
  expect_equal(nrow(h),168); expect_equal(sum(h$n),2)
  expect_equal(h$median[h$weekday==7 & h$hour==1],2)
  expect_equal(h$median[h$weekday==7 & h$hour==3],4)
  expect_true(is.na(h$median[h$weekday==7 & h$hour==2]))
})

test_that('descriptive degenerate cases and both DST transitions are explicit', {
  expect_true(is.na(imr_fit(rep(10,5))$sigma))
  expect_true(is.na(imr_fit(rep(10,5))$i_ucl))
  expect_true(all(is.na(imr_monitor(imr_fit(rep(10,5)),20)$individual_signal)))
  expect_equal(qc_summary(numeric())$n,0)
  expect_true(is.na(qc_summary(numeric())$median))
  expect_true(is.na(qc_summary(numeric())$p90))
  expect_equal(qc_summary(rep(0,4))$median,0)
  expect_equal(qc_summary(rep(0,4))$p90,0)
  expect_equal(qc_box(1:4)$q1,1.75); expect_equal(qc_box(1:4)$q3,3.25)
  expect_equal(qc_box(9)$upper_whisker,9)
  expect_true(is.na(qc_box(numeric())$q1))
  d <- data.frame(dispatch_time=qc_parse_time(c('2025-11-02T01:30:00-0400','2025-11-02T01:30:00-0500')),response_minutes=c(2,6))
  h <- qc_heatmap(d)
  expect_equal(h$n[h$weekday==7 & h$hour==1],2)
  expect_equal(h$median[h$weekday==7 & h$hour==1],4)
})

period_fixture <- function() qc_period('2025-01-01T00:00:00-0500','2026-01-01T00:00:00-0500',monitoring_end='2026-02-01T00:00:00-0500')
fixture_csv <- function(name) read.csv(file.path('..','data',name),stringsAsFactors=FALSE)
# Test-only review records exercise the gate. They are not operational approval
# of the synthetic fixture baselines, which still require genuine human review.
test_reviews <- function(a) lapply(a$groups,function(g) qc_review(g,'test reviewer','test-only approval gate fixture','approved','2026-02-01T00:00:00Z'))

test_that('all CSVs validate; unknown weather retained; active arrivals pending', {
  e <- qc_validate(fixture_csv('edge_cases.csv'))
  expect_equal(unname(e$counts['accepted']),261)
  expect_equal(unname(e$counts['rejected']),9)
  expect_equal(unname(e$counts['weather_normalized']),2)
  expect_equal(sum(e$completed$weather_condition=='Unknown' & grepl('^E-M-',e$completed$incident_id)),3)
  expect_equal(sum(e$rejected$incident_id=='E-BAD-DUP',na.rm=TRUE),2)
  raw <- fixture_csv('edge_cases.csv')
  v <- qc_validate(raw,mode='active')
  expect_equal(v$pending$incident_id,'E-BAD-ARRIVAL')
  expect_false('E-BAD-ARRIVAL' %in% v$completed$incident_id)
  for(n in c('stable.csv','planted_delay.csv')) {
    x <- qc_validate(fixture_csv(n)); expect_equal(nrow(x$completed),3465)
    expect_equal(nrow(x$rejected),0)
  }
})

test_that('eligibility and genuine review are separate and baseline bound', {
  raw <- fixture_csv('stable.csv'); p <- period_fixture()
  a <- qc_analyze(raw,p); expect_length(a$groups,15)
  expect_true(all(vapply(a$groups,function(g) g$status=='baseline requires review',logical(1))))
  expect_true(all(is.na(a$groups[[1]]$monitoring$individual_signal)))
  approved <- test_reviews(a)
  b <- qc_analyze(raw,p,approved)
  expect_true(all(vapply(b$groups,function(g) g$flags_enabled,logical(1))))
  expect_equal(b$groups[[1]]$limits$center,8.159166666666667)
  expect_equal(b$groups[[1]]$limits$i_ucl,11.456815512545445,tolerance=1e-8)
  expect_equal(sum(vapply(b$groups,function(g) sum(g$monitoring$individual_signal),integer(1))),1)
  expect_equal(sum(vapply(b$groups,function(g) sum(g$monitoring$variation_signal),integer(1))),5)
  # Input row order does not change approval binding or chronology.
  reversed <- qc_analyze(raw[nrow(raw):1,],p,approved)
  expect_equal(reversed$groups[[1]]$monitoring$incident_id,b$groups[[1]]$monitoring$incident_id)
  expect_true(reversed$groups[[1]]$flags_enabled)
  # Weather is context only; it must not alter limits or the monitoring sequence.
  wet <- raw; wet$weather_condition <- 'Rain'
  c <- qc_analyze(wet,p,approved)
  expect_equal(c$groups[[1]]$limits,b$groups[[1]]$limits)
  expect_equal(c$groups[[1]]$monitoring$individual_signal,b$groups[[1]]$monitoring$individual_signal)
  # Any reference observation change invalidates that group's approval.
  changed <- raw; ix <- which(raw$zone=='Astoria' & raw$call_priority=='High' & grepl('^B-',raw$incident_id))[1]
  changed$on_scene_time[ix] <- '2025-01-01T00:00:00-0500'
  stale <- qc_analyze(changed,p,approved)
  expect_false(stale$groups[[1]]$flags_enabled)
  rejected_review <- qc_review(a$groups[[1]],'second reviewer','unresolved structure','unsuitable','2026-02-01T00:00:00Z')
  expect_equal(qc_analyze(raw,p,list(rejected_review))$groups[[1]]$status,'baseline unsuitable')
  expect_error(qc_review(a$groups[[1]],'','reason','approved','2026-02-01T00:00:00Z'))
  expect_error(qc_period('2025-02-01T00:00:00-0500','2026-01-01T00:00:00-0500',monitoring_end='2026-02-01T00:00:00-0500'))
})

test_that('planted delays share baseline and add exactly documented signals', {
  p <- period_fixture(); a <- qc_analyze(fixture_csv('stable.csv'),p)
  reviews <- test_reviews(a)
  b <- qc_analyze(fixture_csv('planted_delay.csv'),p,reviews)
  expect_equal(lapply(b$groups, function(g) g$limits),lapply(a$groups,function(g) g$limits))
  expect_equal(sum(vapply(b$groups,function(g) sum(g$monitoring$individual_signal),integer(1))),4)
  expect_equal(sum(vapply(b$groups,function(g) sum(g$monitoring$variation_signal),integer(1))),6)
  m <- b$groups[[1]]$monitoring
  planted <- c('M-G01-0011','M-G01-0007','M-G01-0013')
  expect_true(all(m$individual_signal[m$incident_id %in% planted]))
  expect_equal(m$response_minutes[m$incident_id %in% planted],rep(30,3))
})

test_that('edge groups cannot be enabled by approval and ties remain deterministic', {
  raw <- fixture_csv('edge_cases.csv'); p <- period_fixture()
  a <- qc_analyze(raw,p); b <- qc_analyze(raw,p,test_reviews(a))
  expect_equal(b$groups[['Flushing/Medium']]$status,'degenerate baseline')
  expect_equal(b$groups[['Jamaica/Low']]$status,'insufficient baseline data')
  expect_equal(b$groups[['Forest Hills/High']]$status,'insufficient baseline data')
  expect_true(all(is.na(b$groups[['Flushing/Medium']]$monitoring$individual_signal)))
  m <- b$groups[['Astoria/High']]$monitoring
  expect_equal(m$response_minutes,c(8,9,10,8,9))
  expect_equal(m$incident_id,c('E-M-01','E-M-02','E-M-03','E-M-04','E-M-05'))
  expect_equal(b$groups[['Astoria/High']]$monitoring_summary$p90,9.6)
  expect_equal(m$moving_range[1],abs(8-tail(b$groups[['Astoria/High']]$baseline$response_minutes,1)))
  boxes <- qc_boxes(m,'weather_condition')
  expect_equal(boxes$Unknown$n,3)
  expect_equal(sum(vapply(boxes,function(x) x$n,integer(1))),5)
  # When an earlier assignment completes later, recomputing restores the sequence.
  partial <- raw[raw$incident_id!='E-M-03' | is.na(raw$incident_id),]
  earlier <- qc_analyze(partial,p,test_reviews(a))$groups[['Astoria/High']]$monitoring
  expect_equal(earlier$moving_range[earlier$incident_id=='E-M-04'],1)
  expect_equal(m$moving_range[m$incident_id=='E-M-04'],2)
})
