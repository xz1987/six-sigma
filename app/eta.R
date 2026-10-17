# H-11: framework-independent synthetic ETA experiments. No QC/UI dependencies.
eta_zones <- c('Astoria','Flushing','Jamaica','Forest Hills','Far Rockaway')
eta_priorities <- c('High','Medium','Low')
eta_weather <- c('Dry','Rain','Snow/Ice')
eta_time <- function(x) {
  x <- as.character(x)
  valid <- !is.na(x) & grepl('^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:?\\d{2})$',x)
  out <- rep(as.POSIXct(NA,tz='UTC'),length(x))
  value <- sub('Z$','+0000',x[valid]);value<-sub('([+-][0-9]{2}):([0-9]{2})$','\\1\\2',value)
  out[valid] <- suppressWarnings(as.POSIXct(value,format='%Y-%m-%dT%H:%M:%S%z',tz='UTC'))
  out
}
eta_prepare <- function(d) {
  stopifnot(is.data.frame(d));n<-nrow(d)
  column<-function(name)if(name%in%names(d))as.character(d[[name]]) else rep(NA_character_,n)
  time<-eta_time(column('dispatch_time'));observed<-eta_time(column('weather_observed_at'))
  hour<-as.integer(format(time,'%H',tz='America/New_York'))
  weekday<-as.integer(format(time,'%u',tz='America/New_York'))<=5
  speed<-suppressWarnings(as.numeric(column('wind_speed_mph')))
  gust<-suppressWarnings(as.numeric(column('wind_gust_mph')))
  zone<-column('zone');priority<-column('call_priority');weather<-column('weather_condition');intensity<-column('precipitation_intensity')
  reason<-rep('',n)
  reject<-function(bad,message){bad[is.na(bad)]<-TRUE;reason[bad & reason=='']<<-message}
  reject(!zone%in%eta_zones,'unsupported neighborhood')
  reject(!priority%in%eta_priorities,'unsupported priority')
  reject(is.na(time),'invalid assignment timestamp')
  reject(!weather%in%eta_weather,'missing or unsupported weather')
  reject(!intensity%in%c('None','Light','Moderate','Heavy'),'missing or unsupported intensity')
  reject((weather=='Dry' & intensity!='None') | (weather%in%c('Rain','Snow/Ice') & intensity=='None'),'incompatible weather/intensity')
  reject(!is.finite(speed)|speed<0|!is.finite(gust)|gust<speed,'missing or invalid wind')
  age<-as.numeric(difftime(time,observed,units='secs'))
  reject(is.na(age)|age<0|age>=3600,'missing, future or stale weather observation')
  features<-data.frame(zone=factor(zone,levels=eta_zones),call_priority=factor(priority,levels=eta_priorities),
    peak=as.integer(hour%in%c(7:9,16:18)),weekday=as.integer(weekday),
    weather_condition=factor(weather,levels=eta_weather),moderate=as.integer(intensity=='Moderate'),
    heavy=as.integer(intensity=='Heavy'),wind_speed_mph=speed,gust_excess=gust-speed)
  target<-as.numeric(difftime(eta_time(column('on_scene_time')),time,units='mins'))
  list(features=features,time=time,hour=hour,target=target,reason=reason,
    key=paste(zone,priority,weather,intensity,sep='|'))
}
eta_unavailable <- function(reason) list(status='unavailable',reason=reason)
eta_fit <- function(d, weather=TRUE) {
  p<-eta_prepare(d)
  valid<-p$reason=='' & is.finite(p$target) & p$target>0
  x<-p$features[valid,,drop=FALSE];x$log_minutes<-log(p$target[valid])
  if(nrow(x)<500)return(eta_unavailable('insufficient eligible training data'))
  if(diff(range(x$log_minutes))<1e-10)return(eta_unavailable('degenerate constant target'))
  if(!all(eta_zones%in%as.character(x$zone))||!all(eta_priorities%in%as.character(x$call_priority))||
     !all(eta_weather%in%as.character(x$weather_condition)))
    return(eta_unavailable('insufficient categorical training support'))
  f<-if(weather)log_minutes~zone+call_priority+peak+weekday+weather_condition+moderate+heavy+wind_speed_mph+gust_excess else log_minutes~zone+call_priority+peak+weekday
  fit<-stats::lm(f,data=x,na.action=na.fail)
  sigma<-sqrt(sum(residuals(fit)^2)/fit$df.residual)
  if(fit$rank!=ncol(stats::model.matrix(fit)) || fit$df.residual<30 || !is.finite(sigma) || sigma<1e-10)
    return(eta_unavailable('rank-deficient or degenerate regression'))
  residual<-residuals(fit)
  tie<-if('incident_id'%in%names(d))as.character(d$incident_id[valid]) else as.character(which(valid))
  order_time<-order(p$time[valid],tie)
  variance_ratio<-function(group) {
    v<-tapply(residual,group,var);counts<-table(group)
    if(any(counts<30)||any(!is.finite(v))||any(v<=0))Inf else max(v)/min(v)
  }
  diagnostic<-c(skew=mean((residual-mean(residual))^3)/mean((residual-mean(residual))^2)^1.5,
    lag1=cor(head(residual[order_time],-1),tail(residual[order_time],-1)),
    priority_variance_ratio=variance_ratio(x$call_priority),weather_variance_ratio=variance_ratio(x$weather_condition))
  list(status='candidate',fit=fit,weather=weather,n=nrow(x),excluded=sum(!valid),
    keys=table(p$key[valid]),hours=table(factor(p$hour[valid],levels=0:23)),
    peak=unique(x$peak),weekday=unique(x$weekday),
    speed_range=range(x$wind_speed_mph),gust_range=range(x$gust_excess),
    max_leverage=max(hatvalues(fit)),diagnostics=diagnostic,
    train_ids=if('incident_id'%in%names(d))d$incident_id[valid] else character())
}
# Low-level arithmetic for independent known-answer tests, not permission to publish a model.
eta_log_prediction <- function(fit,newdata) {
  sigma<-sqrt(sum(residuals(fit)^2)/fit$df.residual)
  if(fit$rank!=ncol(model.matrix(fit))||fit$df.residual<1||!is.finite(sigma)||sigma<1e-10)
    stop('prediction unavailable: degenerate regression')
  pi<-stats::predict(fit,newdata=newdata,interval='prediction',level=.95)
  data.frame(median_minutes=exp(pi[,'fit']),mean_minutes=exp(pi[,'fit']+sigma^2/2),
    lower_minutes=exp(pi[,'lwr']),upper_minutes=exp(pi[,'upr']))
}
eta_candidate_predict <- function(model,d) {
  p<-eta_prepare(d);n<-nrow(d)
  out<-data.frame(status=rep('unavailable',n),reason=p$reason,median_minutes=rep(NA_real_,n),
    mean_minutes=rep(NA_real_,n),lower_minutes=rep(NA_real_,n),upper_minutes=rep(NA_real_,n))
  if(!identical(model$status,'candidate')){out$reason<-rep(model$reason,n);return(out)}
  reject<-function(bad,message){bad[is.na(bad)]<-TRUE;out$reason[bad & out$reason=='']<<-message}
  counts<-as.integer(model$keys[p$key]);counts[is.na(counts)]<-0
  reject(counts<5,'unsupported or sparse predictor combination')
  hour_counts<-as.integer(model$hours[as.character(p$hour)])
  reject(is.na(hour_counts)|hour_counts<20,'unsupported assignment hour')
  reject(!p$features$peak%in%model$peak|!p$features$weekday%in%model$weekday,'unsupported time category')
  reject(p$features$wind_speed_mph<model$speed_range[1] | p$features$wind_speed_mph>model$speed_range[2] |
    p$features$gust_excess<model$gust_range[1] | p$features$gust_excess>model$gust_range[2],'wind outside training support')
  rows<-which(out$reason=='')
  if(length(rows)) {
    x<-p$features[rows,,drop=FALSE]
    mm<-model.matrix(delete.response(terms(model$fit)),x,contrasts.arg=model$fit$contrasts)
    inverse<-summary(model$fit)$cov.unscaled
    leverage<-rowSums((mm%*%inverse)*mm)
    supported<-is.finite(leverage)&leverage<=1.5*model$max_leverage
    out$reason[rows[!supported]]<-'joint predictors outside training support'
    rows<-rows[supported]
    if(length(rows)) {
      value<-eta_log_prediction(model$fit,p$features[rows,,drop=FALSE])
      finite<-apply(value,1,function(x)all(is.finite(x))&&all(x>0))
      out$reason[rows[!finite]]<-'nonfinite prediction'
      out[rows[finite],names(value)]<-value[finite,,drop=FALSE]
      out$status[rows[finite]]<-'candidate_for_evaluation'
    }
  }
  out
}
eta_metrics <- function(actual,prediction) {
  if(!length(actual))return(data.frame(n=0L,mae=NA_real_,coverage=NA_real_,median_width=NA_real_))
  data.frame(n=length(actual),mae=mean(abs(actual-prediction$median_minutes)),
    coverage=mean(actual>=prediction$lower_minutes & actual<=prediction$upper_minutes),
    median_width=median(prediction$upper_minutes-prediction$lower_minutes))
}
eta_score <- function(weather_model,base_model,d) {
  actual<-eta_prepare(d)$target
  w<-eta_candidate_predict(weather_model,d);b<-eta_candidate_predict(base_model,d)
  rows<-which(w$status=='candidate_for_evaluation' & b$status=='candidate_for_evaluation' & is.finite(actual)&actual>0)
  metric<-eta_metrics(actual[rows],w[rows,,drop=FALSE]);base<-eta_metrics(actual[rows],b[rows,,drop=FALSE])
  groups<-do.call(rbind,lapply(c('call_priority','weather_condition'),function(field) {
    levels<-if(field=='call_priority')eta_priorities else eta_weather
    do.call(rbind,lapply(levels,function(level){r<-rows[as.character(d[[field]][rows])==level]
      cbind(field=field,category=level,eta_metrics(actual[r],w[r,,drop=FALSE]))}))
  }))
  gain<-if(is.finite(base$mae)&&base$mae>0)1-metric$mae/base$mae else NA_real_
  valid_targets<-sum(is.finite(actual)&actual>0)
  gates<-c(sample=metric$n>=500,availability=metric$n/max(1,valid_targets)>=.90,
    mae=is.finite(metric$mae)&&metric$mae<=5,weather_gain=is.finite(gain)&&gain>=.10,
    coverage=is.finite(metric$coverage)&&metric$coverage>=.90&&metric$coverage<=.98,
    width=is.finite(metric$median_width)&&metric$median_width<=15,
    subgroups=all(groups$n>=30 & is.finite(groups$coverage) & groups$coverage>=.85 & groups$coverage<=.995 &
      is.finite(groups$median_width)&groups$median_width<=20 & is.finite(groups$mae)&groups$mae<=7))
  list(weather=metric,base=base,relative_mae_improvement=gain,eligible_targets=valid_targets,
    unavailable=nrow(d)-length(rows),groups=groups,gates=gates,pass=all(gates),
    unavailable_reasons=table(w$reason[w$status=='unavailable']))
}
eta_partitions <- function(d) {
  required<-c('incident_id','weather_event_id','weather_block_id','dispatch_time','on_scene_time')
  if(!all(required%in%names(d)))stop('missing partition identifiers/timestamps')
  for(field in required[1:3])if(anyNA(d[[field]])||any(!nzchar(trimws(d[[field]]))))stop('missing partition identifier')
  if(anyDuplicated(d$incident_id))stop('duplicate incident IDs; never pool scenarios')
  if('scenario'%in%names(d)&&length(unique(d$scenario))!=1)stop('mixed scenarios prohibited')
  time<-eta_time(d$dispatch_time);arrival<-eta_time(d$on_scene_time)
  if(anyNA(time)||anyNA(arrival)||any(arrival<=time))stop('invalid completed incident timestamps')
  start<-as.POSIXct('2025-11-01',tz='UTC');val<-as.POSIXct('2026-01-01',tz='UTC')
  test<-as.POSIXct('2026-02-01',tz='UTC');end<-as.POSIXct('2026-03-01',tz='UTC')
  if(any(time<start|time>=end))stop('assignment outside frozen periods')
  label<-ifelse(time<val,'train',ifelse(time<test,'validation','test'))
  if('split'%in%names(d)&&!identical(as.character(d$split),label))stop('split labels disagree with assignment dates')
  for(field in c('weather_event_id','weather_block_id'))
    if(any(vapply(split(label,d[[field]]),function(x)length(unique(x))!=1,logical(1))))stop('weather episode/block crosses partitions')
  available<-!(label=='train' & arrival>=val | label=='validation' & arrival>=test)
  out<-lapply(c('train','validation','test'),function(part)d[label==part & available,,drop=FALSE])
  names(out)<-c('train','validation','test');attr(out,'late_completions_excluded')<-sum(!available)
  out
}
eta_validate <- function(d) {
  parts<-eta_partitions(d)
  weather_model<-eta_fit(parts$train,TRUE);base_model<-eta_fit(parts$train,FALSE)
  if(weather_model$status!='candidate'||base_model$status!='candidate')
    return(eta_unavailable('training model unavailable'))
  diagnostic<-weather_model$diagnostics
  diagnostics_pass<-all(is.finite(diagnostic)) && abs(diagnostic['skew'])<=.5 && abs(diagnostic['lag1'])<=.15 &&
    diagnostic['priority_variance_ratio']<=4 && diagnostic['weather_variance_ratio']<=4
  validation<-eta_score(weather_model,base_model,parts$validation)
  # Fixed model; test diagnostics cannot rescue a failed validation decision.
  final_test<-eta_score(weather_model,base_model,parts$test)
  passed<-isTRUE(diagnostics_pass)&&validation$pass&&final_test$pass
  list(status=if(passed)'validated_synthetic' else 'unavailable',
    reason=if(passed)'Experimental — synthetic data' else 'experimental ETA failed frozen acceptance gates',
    model=if(passed)weather_model else NULL,validation=validation,test=final_test,
    diagnostics=diagnostic,diagnostics_pass=diagnostics_pass,
    split_counts=vapply(parts,nrow,integer(1)),late_completions_excluded=attr(parts,'late_completions_excluded'),
    trained_on=weather_model$train_ids,formula_weather=formula(weather_model$fit),formula_base=formula(base_model$fit))
}
eta_predict <- function(bundle,d) {
  if(!identical(bundle$status,'validated_synthetic')) {
    out<-data.frame(status=rep('unavailable',nrow(d)),reason=rep('experimental ETA unavailable: model not validated',nrow(d)),
      median_minutes=rep(NA_real_,nrow(d)),mean_minutes=rep(NA_real_,nrow(d)),lower_minutes=rep(NA_real_,nrow(d)),upper_minutes=rep(NA_real_,nrow(d)))
  } else {
    out<-eta_candidate_predict(bundle$model,d)
    out$status[out$status=='candidate_for_evaluation']<-'experimental_synthetic'
  }
  time<-eta_prepare(d)$time
  out$arrival_median<-time+60*out$median_minutes
  out$arrival_lower<-time+60*out$lower_minutes
  out$arrival_upper<-time+60*out$upper_minutes
  out
}
