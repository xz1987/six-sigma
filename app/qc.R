# Framework-independent QC. Units: response minutes. Assignment order determines
# adjacent ranges; limits are reference-only. NIST I-MR constants: d2=1.128,
# D4=3.267. No automatic approval, weather adjustment, or clinical targets.
qc_zones <- c('Astoria','Flushing','Jamaica','Forest Hills','Far Rockaway')
qc_priorities <- c('High','Medium','Low')
qc_weather <- c('Dry','Rain','Snow/Ice','Unknown')

# Strict second-resolution ISO 8601, mandatory numeric offset or Z. Explicit
# roundtrip catches R platforms which normalize impossible calendar dates.
qc_parse_time <- function(x) {
  if (inherits(x, 'POSIXt')) return(as.POSIXct(x, tz='UTC'))
  x <- as.character(x)
  out <- rep(NA_real_, length(x))
  pattern <- '^([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2})(Z|[+-][0-9]{2}:?[0-9]{2})$'
  for (i in seq_along(x)) {
    if (is.na(x[i]) || !grepl(pattern,x[i])) next
    wall <- substr(x[i],1,19); suffix <- substring(x[i],20)
    parts <- as.integer(c(substr(wall,12,13),substr(wall,15,16),substr(wall,18,19)))
    if (any(parts > c(23,59,59))) next
    t <- as.POSIXct(strptime(wall, '%Y-%m-%dT%H:%M:%S',tz='UTC'))
    if (is.na(t) || format(t,'%Y-%m-%dT%H:%M:%S',tz='UTC') != wall) next
    offset <- 0
    if (suffix != 'Z') {
      s <- gsub(':','',suffix,fixed=TRUE)
      hh <- as.integer(substr(s,2,3)); mm <- as.integer(substr(s,4,5))
      if (hh>14 || mm>59 || (hh==14 && mm!=0)) next
      offset <- (hh*3600+mm*60) * ifelse(substr(s,1,1)=='+',1,-1)
    }
    out[i] <- as.numeric(t)-offset
  }
  as.POSIXct(out,origin='1970-01-01',tz='UTC')
}

# Completed CSV mode rejects absent arrival; active feed mode retains it pending.
# Every issue is reported by original row, with excluded distinguishing warnings.
qc_validate <- function(records, mode=c('completed','active')) {
  mode <- match.arg(mode)
  if (!is.data.frame(records)) stop('records must be a data frame')
  d <- records; n <- nrow(d)
  required <- c('incident_id','zone','call_priority','dispatch_time','on_scene_time')
  for (k in setdiff(c(required,'weather_condition'), names(d))) d[[k]] <- rep(NA_character_,n)
  d$row_id <- seq_len(n)
  issues <- data.frame(row_id=integer(), incident_id=character(), reason=character(), excluded=logical())
  excluded <- rep(FALSE,n)
  add <- function(which,reason,reject=TRUE) {
    ix <- which(which)
    if(length(ix)) {
      issues <<- rbind(issues,data.frame(row_id=ix,incident_id=as.character(d$incident_id[ix]),reason=reason,excluded=reject))
      if(reject) excluded[ix] <<- TRUE
    }
  }
  missing <- function(x) is.na(x) | trimws(as.character(x))==''
  for(k in c('incident_id','zone','call_priority')) add(missing(d[[k]]),paste('missing',k))
  add(!missing(d$zone) & !d$zone %in% qc_zones,'invalid zone')
  add(!missing(d$call_priority) & !d$call_priority %in% qc_priorities,'invalid call_priority')
  id <- as.character(d$incident_id)
  add(!missing(id) & (duplicated(id)|duplicated(id,fromLast=TRUE)),'duplicate incident_id')
  arrival_missing <- missing(d$on_scene_time)
  start <- qc_parse_time(d$dispatch_time); end <- qc_parse_time(d$on_scene_time)
  add(is.na(start),'missing or invalid dispatch_time')
  add(is.na(end) & (!arrival_missing | mode=='completed'),'missing or invalid on_scene_time')
  add(!is.na(start)&!is.na(end)&end<start,'negative response duration')
  bad_weather <- missing(d$weather_condition) | !d$weather_condition %in% qc_weather
  add(bad_weather,'weather normalized to Unknown',FALSE)
  d$weather_condition <- as.character(d$weather_condition); d$weather_condition[bad_weather] <- 'Unknown'
  d$dispatch_time <- start; d$on_scene_time <- end
  d$response_minutes <- as.numeric(difftime(end,start,units='mins'))
  d$record_status <- ifelse(arrival_missing & mode=='active','pending','completed')
  list(completed=d[!excluded & d$record_status=='completed',,drop=FALSE],
       pending=d[!excluded & d$record_status=='pending',,drop=FALSE],
       rejected=d[excluded,,drop=FALSE],issues=issues,
       counts=c(accepted=sum(!excluded),completed=sum(!excluded & d$record_status=='completed'),pending=sum(!excluded & d$record_status=='pending'),rejected=sum(excluded),weather_normalized=sum(bad_weather)))
}

# Arithmetic primitive: status is computability only, never human suitability.
# Returns n, center, MR vector/center, sigma estimate, I/MR limits and final
# reference value. Constant/short baselines have unavailable usable limits.
# Direct imr_monitor is for numerical verification; applications must use the
# approval-gated qc_analyze, not infer eligibility from computable arithmetic.
imr_fit <- function(baseline) {
  if(!is.numeric(baseline)||any(!is.finite(baseline))||any(baseline<0)) stop('baseline must contain finite nonnegative minutes')
  n <- length(baseline); center <- if(n) mean(baseline) else NA_real_
  mr <- abs(diff(baseline)); mrbar <- if(length(mr)) mean(mr) else NA_real_
  sigma <- if(n>=2 && mrbar>0) mrbar/1.128 else NA_real_
  list(n=n,center=center,moving_range=mr,mr_center=mrbar,sigma=sigma,
       i_lcl=center-3*sigma,i_ucl=center+3*sigma,mr_lcl=if(n>=2 && mrbar>0) 0 else NA_real_,mr_ucl=if(n>=2 && mrbar>0) 3.267*mrbar else NA_real_,
       last=if(n) tail(baseline,1) else NA_real_,
       status=if(n<2) 'insufficient baseline data' else if(mrbar==0) 'degenerate baseline' else 'computable')
}
imr_monitor <- function(fit,monitoring,enabled=TRUE) {
  if(!is.numeric(monitoring)||any(!is.finite(monitoring))||any(monitoring<0)) stop('monitoring must contain finite nonnegative minutes')
  mr <- if(length(monitoring)) abs(diff(c(fit$last,monitoring))) else numeric()
  ok <- isTRUE(enabled) && identical(fit$status,'computable')
  data.frame(response_minutes=monitoring,moving_range=mr,
    individual_signal=if(ok) monitoring>fit$i_ucl else rep(NA,length(monitoring)),
    variation_signal=if(ok) mr>fit$mr_ucl else rep(NA,length(monitoring)))
}
# Descriptive summaries accept empty groups (n=0, undefined statistics=NA).
qc_summary <- function(x) {
  if(any(!is.finite(x))) stop('summary requires finite completed durations')
  list(n=length(x),median=if(length(x)) median(x) else NA_real_,
       p90=if(length(x)) unname(quantile(x,.9,type=7)) else NA_real_)
}
qc_box <- function(x) {
  if(any(!is.finite(x))) stop('boxplot requires finite completed durations')
  if(!length(x)) return(list(n=0L,q1=NA_real_,median=NA_real_,q3=NA_real_,lower_whisker=NA_real_,upper_whisker=NA_real_,outliers=numeric(),points=numeric()))
  q <- unname(quantile(x,c(.25,.5,.75),type=7)); spread <- q[3]-q[1]
  inside <- x>=q[1]-1.5*spread & x<=q[3]+1.5*spread
  list(n=length(x),q1=q[1],median=q[2],q3=q[3],lower_whisker=min(x[inside]),upper_whisker=max(x[inside]),outliers=x[!inside],points=x)
}
# Call on filtered validated completed data; no weather filtering of QC sequences.
# Input is completed valid data already filtered by priority/date (and by
# neighborhood for weather comparisons). Returns named boxes including empty
# known categories; points retain sparse observations for the rendering layer.
qc_boxes <- function(completed,group_col) {
  if(!group_col %in% names(completed)) stop('unknown grouping column')
  groups <- if(group_col=='zone') qc_zones else if(group_col=='weather_condition') qc_weather else unique(as.character(completed[[group_col]]))
  setNames(lapply(groups,function(g) qc_box(completed$response_minutes[completed[[group_col]]==g])),groups)
}
qc_heatmap <- function(completed,tz='America/New_York') {
  grid <- expand.grid(hour=0:23,weekday=1:7); grid$n <- 0L; grid$median <- NA_real_
  local <- as.POSIXlt(completed$dispatch_time,tz=tz)
  weekday <- ((local$wday+6)%%7)+1L
  for(i in seq_len(nrow(grid))) {
    x <- completed$response_minutes[local$hour==grid$hour[i] & weekday==grid$weekday[i]]
    grid$n[i] <- length(x); if(length(x)) grid$median[i] <- median(x)
  }
  grid # weekday 1=Monday ... 7=Sunday
}

# Half-open boundaries: baseline_start inclusive, baseline_end exclusive;
# monitoring_start inclusive, monitoring_end exclusive. Calendar months use NYC.
qc_period <- function(baseline_start,baseline_end,monitoring_start=baseline_end,monitoring_end) {
  p <- lapply(list(baseline_start,baseline_end,monitoring_start,monitoring_end),qc_parse_time)
  if(any(vapply(p,function(x) length(x)!=1||is.na(x),logical(1)))) stop('period requires four valid offset timestamps')
  if(p[[1]]>=p[[2]]||p[[2]]>p[[3]]||p[[3]]>=p[[4]]) stop('periods must be ordered and nonoverlapping')
  local <- lapply(p[1:2],function(x) format(x,'%Y-%m-%dT%H:%M:%S',tz='America/New_York'))
  if(any(!grepl('-01T00:00:00$',unlist(local)))) stop('baseline must span complete local calendar months')
  months <- seq(as.Date(substr(local[[1]],1,10)),as.Date(substr(local[[2]],1,10)),by='month')
  if(length(months)!=13L) stop('baseline must span exactly 12 calendar months')
  list(baseline_start=p[[1]],baseline_end=p[[2]],monitoring_start=p[[3]],monitoring_end=p[[4]],months=format(months[-13],'%Y-%m'))
}
qc_order <- function(d) d[order(d$dispatch_time,d$incident_id),,drop=FALSE]

# Serialized exact reference contents (not a security credential) bind human review
# to group, assignment/arrival timestamps, IDs and period; no monitoring data.
qc_baseline_key <- function(baseline,period,zone,priority) {
  b <- qc_order(baseline)
  as.character(serialize(list(zone=zone,priority=priority,start=as.numeric(period$baseline_start),
    end=as.numeric(period$baseline_end),id=as.character(b$incident_id),
    dispatch=as.numeric(b$dispatch_time),arrival=as.numeric(b$on_scene_time)),NULL,version=2))
}
qc_review <- function(group,reviewer,rationale,decision=c('approved','unsuitable'),reviewed_at) {
  decision <- match.arg(decision)
  if(length(reviewer)!=1 || is.na(reviewer)||!nzchar(trimws(reviewer)) ||
     length(rationale)!=1||is.na(rationale)||!nzchar(trimws(rationale))) stop('reviewer and rationale are required')
  t <- qc_parse_time(reviewed_at)
  if(length(t)!=1||is.na(t)) stop('review timestamp must have an offset')
  list(zone=group$zone,call_priority=group$call_priority,baseline_key=group$baseline_key,
       reviewer=reviewer,rationale=rationale,decision=decision,reviewed_at=t)
}
qc_diagnostics <- function(b,fit,months) {
  m <- format(b$dispatch_time,'%Y-%m',tz='America/New_York')
  monthly <- data.frame(month=months,n=integer(length(months)),mean=NA_real_,median=NA_real_)
  for(i in seq_along(months)) {
    x <- b$response_minutes[m==months[i]]
    monthly$n[i] <- length(x)
    if(length(x)) { monthly$mean[i] <- mean(x); monthly$median[i] <- median(x) }
  }
  x <- b$response_minutes
  lag1 <- if(length(x)>2 && sd(head(x,-1))>0 && sd(tail(x,-1))>0) cor(head(x,-1),tail(x,-1)) else NA_real_
  prelim <- b
  prelim$moving_range <- c(NA_real_,fit$moving_range)[seq_along(x)]
  prelim$above_i_ucl <- if(fit$status=='computable') x>fit$i_ucl else rep(NA,length(x))
  prelim$below_i_lcl <- if(fit$status=='computable') x<fit$i_lcl else rep(NA,length(x))
  prelim$above_mr_ucl <- if(fit$status=='computable') prelim$moving_range>fit$mr_ucl else rep(NA,length(x))
  list(preliminary=prelim,monthly=monthly,distribution=qc_box(x),lag1_correlation=lag1,
       lag_pairs=data.frame(previous=head(x,-1),next_value=tail(x,-1)),
       note='Diagnostics support human review; no automatic suitability decision.')
}

# Public grouped API consumes raw completed records, validates all 15 groups and
# returns diagnostics even when monitoring flags are disabled. Use qc_review on
# a returned group and rerun with reviews=list(review) after genuine human review.
qc_analyze <- function(records,period,reviews=list(),mode=c('completed','active')) {
  mode <- match.arg(mode)
  validation <- qc_validate(records,mode)
  d <- validation$completed
  groups <- list()
  for(z in qc_zones) for(pr in qc_priorities) {
    g <- d[d$zone==z & d$call_priority==pr,,drop=FALSE]
    b <- qc_order(g[g$dispatch_time>=period$baseline_start & g$dispatch_time<period$baseline_end,,drop=FALSE])
    m <- qc_order(g[g$dispatch_time>=period$monitoring_start & g$dispatch_time<period$monitoring_end,,drop=FALSE])
    fit <- imr_fit(b$response_minutes); diag <- qc_diagnostics(b,fit,period$months)
    key <- qc_baseline_key(b,period,z,pr)
    eligible <- nrow(b)>=100 && all(diag$monthly$n>0)
    matches <- Filter(function(r) identical(r$zone,z)&&identical(r$call_priority,pr)&&identical(r$baseline_key,key),reviews)
    review <- if(length(matches)) tail(matches,1)[[1]] else NULL
    valid_review <- !is.null(review) && length(review$reviewer)==1 && !is.na(review$reviewer) && nzchar(trimws(review$reviewer)) &&
      length(review$rationale)==1 && !is.na(review$rationale) && nzchar(trimws(review$rationale)) &&
      length(review$reviewed_at)==1 && inherits(review$reviewed_at,'POSIXt') && !is.na(review$reviewed_at) && review$decision %in% c('approved','unsuitable')
    status <- if(!eligible) 'insufficient baseline data' else if(fit$status=='degenerate baseline') 'degenerate baseline' else if(!valid_review) 'baseline requires review' else if(review$decision=='unsuitable') 'baseline unsuitable' else 'approved'
    enabled <- status=='approved'
    observations <- imr_monitor(fit,m$response_minutes,enabled)
    m$moving_range <- observations$moving_range
    m$individual_signal <- observations$individual_signal
    m$variation_signal <- observations$variation_signal
    m$qc_status <- if(!enabled) rep(status,nrow(m)) else ifelse(m$individual_signal,'signal detected','no signal detected')
    groups[[paste(z,pr,sep='/')]] <- list(zone=z,call_priority=pr,baseline_key=key,
      status=status,flags_enabled=enabled,baseline=b,monitoring=m,limits=fit,
      baseline_start=period$baseline_start,baseline_end=period$baseline_end,
      review=if(valid_review) review else NULL,diagnostics=diag,
      baseline_summary=qc_summary(b$response_minutes),monitoring_summary=qc_summary(m$response_minutes))
  }
  list(groups=groups,validation=validation,period=period)
}
