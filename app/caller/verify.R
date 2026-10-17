library(shiny)
source('app/caller/caller.R')
now<-as.POSIXct('2026-10-16 12:00:00',tz='UTC')
a<-caller_demo_incident('A',now=now);b<-caller_demo_incident('B',zone='Flushing',now=now)
ga<-caller_issue_access('A',now);gb<-caller_issue_access('B',now)
incidents<-list(A=a,B=b)
stopifnot(nchar(ga$token)==64,!identical(ga$token,gb$token))
stopifnot(identical(caller_lookup(ga$token,list(ga,gb),incidents,now)$incident_id,'A'))
stopifnot(is.null(caller_lookup('B',list(ga,gb),incidents,now)),is.null(caller_lookup('bad',list(ga),incidents,now)))
stopifnot(is.null(caller_lookup(ga$token,list(ga),incidents,ga$expires_at)))
stopifnot(caller_replay(a,64,now)$status=='assigned',caller_replay(a,65,now)$status=='approaching')
stopifnot(is.null(caller_replay(a,89,now)$arrived_at),caller_replay(a,90,now)$arrived_at==now+90)
stopifnot(caller_replay(a,90,now)$status=='arrived',is.null(a$eta))
stopifnot(all(caller_position(a$route,0)==unlist(a$route[1,])),all(caller_position(a$route,1)==unlist(tail(a$route,1))))
stopifnot(!grepl('Flushing|DEMO UNIT',as.character(caller_map(a))))
lookup<-function(token)caller_lookup(token,list(ga,gb),incidents,now)
shiny::testServer(caller_server,args=list(lookup=lookup),{
 session$setInputs(linkToken=ga$token);session$flushReact()
 stopifnot(current()$incident_id=='A',grepl('Not available yet',paste(as.character(output$tracking),collapse=' ')),!grepl('Flushing',paste(as.character(output$tracking),collapse=' ')))
 session$setInputs(linkToken='bad');session$flushReact();stopifnot(is.null(current()),is.null(output$tracking))
 session$setInputs(linkToken=gb$token);session$flushReact();stopifnot(current()$incident_id=='B')
 session$setInputs(close=1);session$flushReact();stopifnot(is.null(current()),is.null(output$tracking))
})
# Separate sessions do not inherit authorized tokens.
shiny::testServer(caller_server,args=list(lookup=lookup),{stopifnot(is.null(access()),is.null(current()))})
cat('PASS: random expiring access codes; incident isolation; invalid/expired access denied; close and session isolation\n')
cat('PASS: assigned/approaching/confirmed arrival; map endpoints; unavailable ETA; real module output checks\n')

local({
 previous<-getwd();on.exit(setwd(previous));setwd('app/caller')
 e<-new.env(parent=globalenv());sys.source('app.R',e)
 shiny::testServer(e$server,{
   session$setInputs(clientReady=TRUE,trackingLinkOpened=FALSE,createDispatch=1);session$flushReact()
   created_dispatch<-created();stopifnot(!is.null(created_dispatch),grepl('Open caller tracking',paste(as.character(output$dispatchPreview),collapse=' ')))
   token<-sub('^#incident=','',created_dispatch$fragment)
   session$setInputs(`caller-linkToken`=token,trackingLinkOpened=TRUE)
   session$flushReact();stopifnot(grepl('Ambulance assigned',paste(as.character(output$`caller-tracking`),collapse=' ')),is.null(output$dispatchPreview))
   entry<-get(token,e$dispatch_store);entry$incident$dispatched_at<-Sys.time()-70;assign(token,entry,e$dispatch_store)
   session$elapse(1100);session$flushReact();stopifnot(grepl('Approaching the scene',paste(as.character(output$`caller-tracking`),collapse=' ')))
   entry$incident$dispatched_at<-Sys.time()-95;assign(token,entry,e$dispatch_store)
   session$elapse(1100);session$flushReact();stopifnot(grepl('Arrival confirmed',paste(as.character(output$`caller-tracking`),collapse=' ')))
 })
})
cat('PASS: standalone PWA server initializes; dispatch-generated link; caller assigned, approaching and arrival outputs
')
# A successful access decision must not survive expiry or revocation.
clock<-now;grants<-list(ga)
shiny::testServer(caller_server,args=list(lookup=function(token)caller_lookup(token,grants,incidents,clock)),{
 session$setInputs(linkToken=ga$token);session$flushReact();stopifnot(!is.null(current()))
 clock<<-ga$expires_at;session$elapse(1100);session$flushReact();stopifnot(is.null(current()),is.null(output$tracking))
})
clock<-now
shiny::testServer(caller_server,args=list(lookup=function(token)caller_lookup(token,grants,incidents,clock)),{
 session$setInputs(linkToken=ga$token);session$flushReact();stopifnot(!is.null(current()))
 grants<<-list();session$elapse(1100);session$flushReact();stopifnot(is.null(current()),is.null(output$tracking))
})
cat('PASS: expiry and revocation remove already-open incident outputs\n')

store<-caller_dispatch_store()
d1<-caller_create_dispatch(store,now=now);d2<-caller_create_dispatch(store,zone='Flushing',now=now)
t1<-sub('^#incident=','',d1$fragment);t2<-sub('^#incident=','',d2$fragment)
stopifnot(!identical(d1$incident_id,d2$incident_id),!identical(t1,t2))
stopifnot(caller_dispatch_lookup(store,t1,now)$incident_id==d1$incident_id)
stopifnot(caller_dispatch_lookup(store,t2,now)$zone=='Flushing')
stopifnot(is.null(caller_dispatch_lookup(store,d1$incident_id,now)))
stopifnot(is.null(caller_dispatch_lookup(store,t1,now+1800)))
# The same link opens the same incident in a separate caller session.
shiny::testServer(caller_server,args=list(lookup=function(token)caller_dispatch_lookup(store,token,now)),{
 session$setInputs(linkToken=t2);session$flushReact();stopifnot(current()$incident_id==d2$incident_id)
})
stopifnot(!grepl('type="password"',paste(as.character(caller_ui('caller')),collapse=' ')))
cat('PASS: dispatch creates distinct private links; cross-session access; IDs denied; expired grants removed; no password field\n')

for(zone in c('Astoria','Flushing','Jamaica','Forest Hills','Far Rockaway')) {
 d<-caller_demo_incident(zone=zone,now=now)
 m<-jsonlite::fromJSON(jsonlite::toJSON(caller_map(d),auto_unbox=TRUE))
 stopifnot(m$ambulance$lat>40.55,m$ambulance$lat<40.81,m$ambulance$lng> -73.97,m$ambulance$lng< -73.69)
 end<-caller_map(caller_replay(d,90,now))
 stopifnot(end$arrived,identical(end$ambulance$lat,end$destination$lat),identical(end$ambulance$lng,end$destination$lng))
}
shiny::testServer(caller_server,args=list(lookup=lookup),{
 session$setInputs(linkToken=ga$token);session$flushReact()
 stopifnot(output$authorized=='yes',jsonlite::fromJSON(output$map)$incident_id=='A')
 session$setInputs(linkToken='invalid');session$flushReact()
 stopifnot(output$authorized=='no',output$map=='null')
})
cat('PASS: geographic Queens coordinates; JSON latitude/longitude; ambulance reaches house; unauthorized map cleared\n')
old<-a;old$updated_at<-Sys.time()-60
shiny::testServer(caller_server,args=list(lookup=function(token)if(identical(token,'test-only'))old else NULL),{
 session$setInputs(linkToken='test-only');session$flushReact()
 stopifnot(grepl('Waiting for a location update',paste(as.character(output$lastUpdate),collapse=' ')),jsonlite::fromJSON(output$map)$stale)
})
markup<-paste(as.character(caller_ui('caller')),collapse=' ')
stopifnot(grepl('Recenter',markup),grepl('incident destination',markup))
cat('PASS: delayed-location warning and stale marker state; Recenter and incident-location labels\n')

# Destination is explicit crew data, independent of arrival and scoped like tracking.
unconfirmed<-caller_replay(a,90,now)
stopifnot(grepl('Hospital update coming soon',as.character(caller_destination_panel(unconfirmed))))
confirmed<-caller_confirm_destination(a,'DEMO Hospital — fictional facility','100 Example Avenue — fictional address',now+100)
stopifnot(inherits(try(caller_confirm_destination(a,'','address',now),silent=TRUE),'try-error'))
shiny::testServer(caller_server,args=list(lookup=function(token)if(identical(token,'A-access'))confirmed else if(identical(token,'B-access'))b else NULL),{
 session$setInputs(linkToken='A-access');session$flushReact()
 stopifnot(grepl('Your crew confirmed the hospital',paste(as.character(output$destination),collapse=' ')),
   grepl('DEMO Hospital',paste(as.character(output$destination),collapse=' ')))
 session$setInputs(linkToken='B-access');session$flushReact()
 stopifnot(!grepl('DEMO Hospital',paste(as.character(output$destination),collapse=' ')))
 session$setInputs(linkToken='invalid');session$flushReact()
 stopifnot(is.null(output$destination))
})
cat('PASS: explicit crew destination; arrival does not imply hospital; cross-incident isolation; unauthorized panels cleared\n')

# An already-open caller automatically sees a destination saved by its EMS provider.
shared_incident<-a
shiny::testServer(caller_server,args=list(lookup=function(token)if(identical(token,'shared-access'))shared_incident else NULL),{
 session$setInputs(linkToken='shared-access');session$flushReact()
 stopifnot(grepl('Hospital update coming soon',paste(as.character(output$destination),collapse=' ')))
 shared_incident<<-caller_confirm_destination(shared_incident,'Synthetic EMS-selected hospital','Fictional address',now+100)
 session$elapse(1100);session$flushReact()
 stopifnot(grepl('Synthetic EMS-selected hospital',paste(as.character(output$destination),collapse=' ')))
})
cat('PASS: open caller automatically receives provider-confirmed destination without reopening link\n')
