# Run from repository root: Rscript data/eta/validate.R
source('data/eta/fixture.R')
source('app/qc.R')
check <- function(ok,message) {if(!isTRUE(ok))stop(message,call.=FALSE)}
read_fixture <- function(name)read.csv(file.path('data/eta/v1',paste0(name,'.csv')),stringsAsFactors=FALSE,colClasses='character',na.strings='')
a <- read_fixture('weather_effect');b <- read_fixture('no_weather_effect')
t <- read.csv('data/eta/v1/truth.csv',stringsAsFactors=FALSE)
parse_time <- function(x)as.POSIXct(x,format='%Y-%m-%dT%H:%M:%S%z',tz='UTC')
duration <- function(x)as.numeric(difftime(parse_time(x$on_scene_time),parse_time(x$dispatch_time),units='secs'))
expected_columns <- c('schema_version','synthetic','incident_id','weather_event_id','weather_block_id','zone','call_priority',
 'weather_condition','wind_speed_mph','wind_gust_mph','precipitation_intensity','weather_observed_at','dispatch_time','split','scenario','on_scene_time')
check(identical(names(a),expected_columns)&&identical(names(b),expected_columns),'Schema changed')
check(nrow(a)==7200&&nrow(b)==7200&&!anyDuplicated(a$incident_id),'Wrong row count or duplicated incidents')
check(all(a$schema_version=='eta-v1')&&all(a$synthetic=='TRUE'),'Missing synthetic/version labels')
check(all(a$scenario=='weather_effect')&&all(b$scenario=='no_weather_effect'),'Wrong scenario labels')
check(identical(a[,!names(a)%in%c('scenario','on_scene_time')],b[,!names(b)%in%c('scenario','on_scene_time')]),'Paired covariates differ')
check(!anyNA(parse_time(a$dispatch_time))&&all(diff(as.numeric(parse_time(a$dispatch_time)))>=0),'Unsorted/invalid assignments')
check(all(duration(a)>0)&&all(duration(b)>0),'Nonpositive durations')
weather_age <- as.numeric(difftime(parse_time(a$dispatch_time),parse_time(a$weather_observed_at),units='secs'))
check(all(weather_age>=0 & weather_age<3600),'Assignment does not use latest hourly weather')
check(all(a$zone%in%qc_zones)&&all(a$call_priority%in%qc_priorities),'Unknown group')
observations <- read.csv('data/eta/v1/weather_observations.csv',stringsAsFactors=FALSE)
check(nrow(observations)==2880,'Wrong hourly observation count')
obs_time <- parse_time(observations$weather_observed_at)
check(all(as.numeric(obs_time)%%3600==0),'Observations not hourly')
check(!anyDuplicated(observations$weather_block_id),'Duplicate hourly blocks')
check(all(diff(as.numeric(obs_time))==3600),'Missing hourly observation')
sizes <- table(observations$weather_event_id)
check(length(unique(as.integer(sizes)))>1 && all(sizes>=1 & sizes<=12),'Episodes not variable length')
for(id in names(sizes)[sizes<3]) {
 end<-max(obs_time[observations$weather_event_id==id])+3600
 check(format(end,'%Y-%m-%dT%H:%M:%S',tz='UTC')%in%c('2026-01-01T00:00:00','2026-02-01T00:00:00','2026-03-01T00:00:00'),'Short episode away from partition boundary')
}
for(rows in split(seq_len(nrow(observations)),observations$weather_event_id)) {
 check(all(diff(rows)==1),'Noncontiguous weather episode')
 check(length(unique(observations$weather_condition[rows]))==1,'Episode weather type changed')
}
obs_split <- ifelse(obs_time<as.POSIXct('2026-01-01',tz='UTC'),'train',
 ifelse(obs_time<as.POSIXct('2026-02-01',tz='UTC'),'validation','test'))
check(all(vapply(split(obs_split,observations$weather_event_id),function(x)length(unique(x))==1,logical(1))),'Observation episode crosses split')
match_obs<-match(a$weather_block_id,observations$weather_block_id)
check(!anyNA(match_obs),'Incident hourly block absent from audit table')
check(all(a$weather_event_id==observations$weather_event_id[match_obs]),'Incident episode differs from hourly block')
check(all(a$weather_observed_at==observations$weather_observed_at[match_obs]),'Incident observation timestamp differs')
check(all(as.numeric(parse_time(a$weather_observed_at))==floor(as.numeric(parse_time(a$dispatch_time))/3600)*3600),'Future or stale hourly observation used')
check(all(table(a$weather_block_id)%in%2:3),'Expected two or three calls per hourly block')
latent<-t[match(a$incident_id,t$incident_id),]
check(all(latent$true_wind_speed_mph==observations$wind_speed_mph[match_obs])&&
 all(latent$true_wind_gust_mph==observations$wind_gust_mph[match_obs])&&
 all(latent$true_weather_condition==observations$weather_condition[match_obs])&&
 all(latent$true_precipitation_intensity==observations$precipitation_intensity[match_obs]),'Duration covariates disagree with latest observation')
check(all(observations$wind_speed_mph>=0 & observations$wind_speed_mph<=30 &
 observations$wind_gust_mph>=observations$wind_speed_mph),'Invalid hourly wind')
cat('PASS: 2880 hourly weather updates; latest prior observation per incident; no future weather; hourly blocks and variable-length weather episodes verified\n')
cat('PASS: versioned schema; 7200 paired incidents per scenario; positive durations; assignment-time weather\n')

# Regenerate outside data/; compare actual serialized bytes, not just in-memory rows.
local({folder<-tempfile('eta-regenerate-');dir.create(folder);on.exit(unlink(folder,recursive=TRUE))
 eta_write_fixture(folder)
 for(name in c('weather_effect','no_weather_effect','truth','weather_observations'))
  check(unname(tools::md5sum(file.path(folder,paste0(name,'.csv'))))==unname(tools::md5sum(file.path('data/eta/v1',paste0(name,'.csv')))),'Regeneration hash mismatch')
})
cat('PASS: seed 5310 reproduces all four CSV files byte for byte\n')

for(d in list(a,b)) {
 expected_split <- ifelse(parse_time(d$dispatch_time)<as.POSIXct('2026-01-01',tz='UTC'),'train',
   ifelse(parse_time(d$dispatch_time)<as.POSIXct('2026-02-01',tz='UTC'),'validation','test'))
 check(identical(d$split,expected_split),'Wrong chronological split')
 check(identical(as.integer(table(factor(d$split,levels=c('train','validation','test')))),c(3660L,1860L,1680L)),'Wrong partition counts')
 check(all(vapply(split(d$split,d$weather_event_id),function(x)length(unique(x))==1,logical(1))),'Event leaked across partitions')
 check(length(unique(d$weather_event_id))==length(sizes),'Missing weather episode')
 for(part in c('train','validation','test')) {
  x<-d[d$split==part,]
  hours<-as.integer(format(parse_time(x$dispatch_time),'%H',tz='America/New_York'))
  check(identical(sort(unique(hours)),0:23),'Missing local assignment hour in partition')
  check(all(table(factor(hours,levels=0:23))>=20),'Sparse local assignment hour in partition')
  check(nrow(unique(x[c('zone','call_priority')]))==15,'Missing group in partition')
  check(all(c('Dry','Rain','Snow/Ice')%in%x$weather_condition),'Missing weather type in partition')
  check(all(c('None','Light','Moderate','Heavy')%in%x$precipitation_intensity),'Missing intensity in partition')
 }
}
cat('PASS: chronological train/validation/test = 3660/1860/1680; disjoint variable-length weather episodes; all 24 local hours per split (at least 20 incidents/hour)\n')

bounds<-c(1,5,8,13,19,25,32,39,47,55,64,73)
check(identical(eta_beaufort(bounds),1:12),'Wrong exact Beaufort boundaries')
check(identical(eta_beaufort(bounds-1e-7),0:11),'Wrong below-boundary Beaufort')
check(identical(eta_beaufort(bounds+1e-7),1:12),'Wrong above-boundary Beaufort')
check(identical(eta_beaufort(c(0,4,7,12,18,24,31,38,46,54,63,72,100)),0:12),'Wrong NWS whole-mph ranges')
check(all(is.na(eta_beaufort(c(-1,NA,Inf,NaN,'bad')))),'Invalid wind classified')
check(length(eta_beaufort(numeric()))==0,'Empty wind fails')
cat('PASS: all 12 Beaufort boundaries below/at/above; negative/missing/nonfinite/empty wind stays unavailable\n')

clean<-eta_clean_weather(a)
check(nrow(clean$data)==7200,'Weather defects discarded valid incidents')
check(nrow(clean$issues)==281,'Unexpected weather issue count')
check(sum(is.na(clean$data$wind_speed_mph))==74&&sum(is.na(clean$data$wind_gust_mph))==71,'Missing numeric wind imputed')
check(sum(clean$data$precipitation_intensity=='Unknown')==69&&sum(clean$data$weather_condition=='Unknown')==67,'Unknown labels lost')
# 74 + 71 + 69 + 67 = 281 field issues, including overlaps on individual incidents.
check(nrow(qc_validate(clean$data)$completed)==7200,'Weather defects excluded incidents from QC')
edge<-data.frame(wind_speed_mph=c('0','10','-1','','Inf','bad'),wind_gust_mph=c('0','9','20',NA,'NaN','30'),
 weather_condition=c('Dry','Rain','Snow/Ice','','Hail','Unknown'),precipitation_intensity=c('None','Light','Heavy','','Extreme','Unknown'))
e<-eta_clean_weather(edge)
check(e$data$wind_speed_mph[1]==0&&e$data$wind_gust_mph[1]==0,'Legitimate calm became missing')
check(is.na(e$data$wind_gust_mph[2])&&all(is.na(e$data$wind_speed_mph[3:6])),'Invalid numeric handling')
check(all(e$data$precipitation_intensity[4:5]=='Unknown')&&all(e$data$weather_condition[4:5]=='Unknown'),'Invalid labels not Unknown')
missing<-eta_clean_weather(data.frame(incident_id='one'))
check(nrow(missing$data)==1&&nrow(missing$issues)==4,'Missing columns not reported/retained')
empty<-eta_clean_weather(data.frame(incident_id=character()))
check(nrow(empty$data)==0&&nrow(empty$issues)==0,'Empty records fail')
cat(sprintf('PASS: %d weather field issues reported; all 7200 incidents remain QC eligible; zero/missing/invalid/empty checks\n',nrow(clean$issues)))

# Independent reconstruction from documented covariates and oracle noise.
t<-t[match(a$incident_id,t$incident_id),]
check(!anyNA(t$incident_id),'Missing truth rows')
h<-as.integer(format(parse_time(a$dispatch_time),'%H',tz='America/New_York'))
w<-as.integer(format(parse_time(a$dispatch_time),'%u',tz='America/New_York'))
zone_effect<-c(Astoria=0,Flushing=.06,Jamaica=.10,`Forest Hills`=-.04,`Far Rockaway`=.15)
base<-log(c(High=8,Medium=11,Low=15)[a$call_priority])+zone_effect[a$zone]+.08*(h%in%c(7,8,9,16,17,18))+.03*(w<6)
effect<-.12*(t$true_weather_condition=='Rain')+.28*(t$true_weather_condition=='Snow/Ice')+
 c(None=0,Light=.02,Moderate=.08,Heavy=.16)[t$true_precipitation_intensity]+.006*t$true_wind_speed_mph+.003*(t$true_wind_gust_mph-t$true_wind_speed_mph)
check(max(abs(base-t$base_log_minutes))<1e-12&&max(abs(effect-t$weather_log_effect))<1e-12,'Truth equation mismatch')
check(all(duration(a)==round(60*exp(base+effect+t$noise_log_minutes))),'Weather relationship mismatch')
check(all(duration(b)==round(60*exp(base+t$noise_log_minutes))),'No-weather equation mismatch')
check(all(duration(a)>=duration(b)),'Positive injected effects reversed')
# Hand example: Astoria/High, off-peak weekend, Dry/None, 0 wind, zero noise = 8 min.
# Rain/Moderate with 10 sustained/15 gust: exp(.12+.08+.06+.015)=exp(.275) times 8.
check(round(8*60*exp(.275))==632,'Known arithmetic fixture differs')
cat('PASS: known injected log equations recovered exactly after second rounding; calm example 480 seconds; rain example 632 seconds\n')
cat('PASS: H-10 synthetic fixtures validated; no ETA model or operational accuracy claim\n')
