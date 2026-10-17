# H-10 synthetic fixture utilities only; no model fitting or QC changes.
eta_beaufort <- function(wind_speed_mph) {
  x <- suppressWarnings(as.numeric(as.character(wind_speed_mph)))
  # NWS Boise whole-mph lower bounds; continuous inputs use these left-closed bins.
  out <- findInterval(x, c(0,1,5,8,13,19,25,32,39,47,55,64,73)) - 1L
  out[!is.finite(x) | x < 0] <- NA_integer_
  out
}
eta_clean_weather <- function(d) {
  issues <- data.frame(row=integer(), field=character(), reason=character())
  report <- function(rows, field, reason) {
    if(length(rows)) issues <<- rbind(issues,data.frame(row=rows,field=field,reason=reason))
  }
  for(field in c('wind_speed_mph','wind_gust_mph')) {
    raw <- if(field %in% names(d)) as.character(d[[field]]) else rep(NA_character_,nrow(d))
    x <- suppressWarnings(as.numeric(raw))
    missing <- is.na(raw) | trimws(raw)==''
    invalid <- !missing & (!is.finite(x) | x<0)
    report(which(missing),field,'missing')
    report(which(invalid),field,'invalid')
    x[missing | invalid] <- NA_real_
    d[[field]] <- x
  }
  bad_gust <- which(!is.na(d$wind_speed_mph) & !is.na(d$wind_gust_mph) & d$wind_gust_mph<d$wind_speed_mph)
  report(bad_gust,'wind_gust_mph','below_sustained'); d$wind_gust_mph[bad_gust] <- NA_real_
  for(field in c('weather_condition','precipitation_intensity')) {
    allowed <- if(field=='weather_condition') c('Dry','Rain','Snow/Ice','Unknown') else c('None','Light','Moderate','Heavy','Unknown')
    raw <- if(field %in% names(d)) as.character(d[[field]]) else rep(NA_character_,nrow(d))
    missing <- is.na(raw) | trimws(raw)==''
    invalid <- !missing & !raw %in% allowed
    report(which(missing),field,'missing');report(which(invalid),field,'invalid')
    raw[missing | invalid] <- 'Unknown';d[[field]] <- raw
  }
  d$beaufort_force <- eta_beaufort(d$wind_speed_mph)
  list(data=d,issues=issues)
}
eta_fixture <- function(seed=5310L) {
  set.seed(seed)
  days <- seq(as.Date('2025-11-01'),as.Date('2026-02-28'),by='day')
  groups <- expand.grid(zone=c('Astoria','Flushing','Jamaica','Forest Hills','Far Rockaway'),
    call_priority=c('High','Medium','Low'),stringsAsFactors=FALSE)
  # Hourly blocks are observations; variable-length episodes define related weather.
  observation_time <- seq(as.POSIXct('2025-11-01',tz='UTC'),
    as.POSIXct('2026-02-28 23:00:00',tz='UTC'),by='hour')
  n_hours <- length(observation_time); n <- length(days)*60L
  weather <- intensity <- character(n_hours)
  speed <- gust <- numeric(n_hours); episode <- integer(n_hours)
  partition_end <- c(1464L,2208L,2880L) # exclusive date boundaries expressed as hour counts
  cursor <- 1L;event <- 0L
  while(cursor<=n_hours) {
    event <- event+1L
    end <- min(cursor+sample(3:12,1)-1L,partition_end[which(partition_end>=cursor)[1]])
    type <- sample(c('Dry','Rain','Snow/Ice'),1,prob=c(.5,.3,.2))
    for(i in cursor:end) {
      episode[i] <- event;weather[i] <- type
      intensity[i] <- if(type=='Dry')'None' else
        if(i>cursor && runif(1)<.7) intensity[i-1] else sample(c('Light','Moderate','Heavy'),1)
      speed[i] <- round(if(i==cursor)runif(1,0,30) else
        min(30,max(0,speed[i-1]+runif(1,-4,4))),3)
      gust[i] <- round(speed[i]+runif(1,0,15),3)
    }
    cursor <- end+1L
  }
  # Sixty calls/day: two per UTC hour plus twelve randomly chosen extra hours.
  # Every neighborhood/priority occurs four times/day; group-hour assignment is shuffled.
  j <- g <- integer(n)
  for(day in seq_along(days)) {
    rows <- ((day-1)*60+1):(day*60)
    j[rows] <- (day-1)*24+sample(c(rep(0:23,each=2),sample(0:23,12)))+1L
    g[rows] <- sample(rep(seq_len(15),4))
  }
  assignment <- observation_time[j]+sample(0:3599,n,replace=TRUE)
  observed_at <- observation_time[j]
  local_hour <- as.integer(format(assignment,'%H',tz='America/New_York'))
  local_weekday <- as.integer(format(assignment,'%u',tz='America/New_York'))
  base_log <- log(c(High=8,Medium=11,Low=15)[groups$call_priority[g]])+
    c(Astoria=0,Flushing=.06,Jamaica=.1,`Forest Hills`=-.04,`Far Rockaway`=.15)[groups$zone[g]]+
    .08*(local_hour %in% c(7:9,16:18))+.03*(local_weekday<=5)
  weather_log <- .12*(weather[j]=='Rain')+.28*(weather[j]=='Snow/Ice')+
    c(None=0,Light=.02,Moderate=.08,Heavy=.16)[intensity[j]]+.006*speed[j]+.003*(gust[j]-speed[j])
  noise <- rnorm(n,0,.12)
  stamp <- function(x)format(x,'%Y-%m-%dT%H:%M:%S%z',tz='UTC')
  common <- data.frame(schema_version='eta-v1',synthetic=TRUE,
    incident_id=sprintf('ETA-V1-%05d',seq_len(n)),weather_event_id=sprintf('WX-V1-%04d',episode[j]),
    weather_block_id=sprintf('HOUR-V1-%04d',j),
    zone=groups$zone[g],call_priority=groups$call_priority[g],weather_condition=weather[j],
    wind_speed_mph=as.character(speed[j]),wind_gust_mph=as.character(gust[j]),
    precipitation_intensity=intensity[j],weather_observed_at=stamp(observed_at),
    dispatch_time=stamp(assignment),
    split=ifelse(assignment<as.POSIXct('2026-01-01',tz='UTC'),'train',
      ifelse(assignment<as.POSIXct('2026-02-01',tz='UTC'),'validation','test')),stringsAsFactors=FALSE)
  # Assignment-time reporting defects; do not drop the completed incident.
  common$wind_speed_mph[seq(97,n,by=97)] <- ''
  common$wind_gust_mph[seq(101,n,by=101)] <- '-1'
  common$precipitation_intensity[seq(103,n,by=103)] <- 'unrated'
  common$weather_condition[seq(107,n,by=107)] <- ''
  make <- function(effect) {
    d <- common;d$scenario <- if(effect)'weather_effect' else 'no_weather_effect'
    seconds <- round(60*exp(base_log+(if(effect)weather_log else 0)+noise))
    d$on_scene_time <- stamp(assignment+seconds)
    d[order(assignment,d$incident_id),]
  }
  truth <- data.frame(incident_id=common$incident_id,base_log_minutes=unname(base_log),
    weather_log_effect=unname(weather_log),noise_log_minutes=noise,
    true_wind_speed_mph=speed[j],true_wind_gust_mph=gust[j],
    true_weather_condition=weather[j],true_precipitation_intensity=intensity[j])
  observations <- data.frame(weather_event_id=sprintf('WX-V1-%04d',episode),
    weather_block_id=sprintf('HOUR-V1-%04d',seq_len(n_hours)),
    weather_observed_at=stamp(observation_time),
    weather_condition=weather,precipitation_intensity=intensity,
    wind_speed_mph=speed,wind_gust_mph=gust,stringsAsFactors=FALSE)
  list(weather_effect=make(TRUE),no_weather_effect=make(FALSE),truth=truth,
    weather_observations=observations)
}
eta_write_fixture <- function(folder) {
  dir.create(folder,recursive=TRUE,showWarnings=FALSE)
  x <- eta_fixture()
  for(name in names(x))write.csv(x[[name]],file.path(folder,paste0(name,'.csv')),row.names=FALSE,na='')
  invisible(x)
}
