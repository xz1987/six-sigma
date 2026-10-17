source('app/app.R')
directory <- jsonlite::fromJSON('app/www/hospital-directory.json',simplifyVector=FALSE)$hospitals
stopifnot(length(directory)==9, length(unique(vapply(directory,`[[`,character(1),'id')))==9)
for(h in directory) stopifnot(nzchar(h$address),length(h$sources)>=2,
  h$lat>40 && h$lat<41,h$lng > -74 && h$lng < -73,
  all(vapply(h$sources,function(s)grepl('^https://',s$url),logical(1))))
for(zone in names(hospital_trips)) {
  facilities <- hospital_trips[[zone]]$hospitals
  trauma <- hospital_candidates('Trauma assessment',facilities)
  pediatric <- hospital_candidates('Pediatric trauma',facilities)
  general <- hospital_candidates('Emergency assessment',facilities)
  stopifnot(setequal(vapply(trauma,`[[`,character(1),'id'),c('a','b','c')),
    length(pediatric)==1,pediatric[[1]]$id=='d',
    all(diff(vapply(general,`[[`,numeric(1),'distance_m'))>=0),
    length(hospital_candidates('Undocumented service',facilities))==0,
    setequal(vapply(facilities,`[[`,character(1),'id'),vapply(directory,`[[`,character(1),'id')))
  # No zone boundary restriction: Astoria trauma must include Elmhurst.
  if(zone=='Astoria') stopifnot(any(vapply(trauma,function(h)h$neighborhood=='Elmhurst',logical(1))))
}
stopifnot(hospital_candidates('Emergency assessment',hospital_trips$`Far Rockaway`$hospitals)[[1]]$id=='g',
  hospital_candidates('Emergency assessment',hospital_trips$Astoria$hospitals)[[1]]$id=='e',
  hospital_candidates('Emergency assessment',hospital_trips$`Forest Hills`$hospitals)[[1]]$id=='f')
cat('PASS: nine sourced hospitals, age/trauma matching, no-match handling, distance ordering and cross-zone eligibility\n')
