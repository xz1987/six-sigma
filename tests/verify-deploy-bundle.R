# Verify the published app can run with no parent repository or external data.
repo <- normalizePath('.')
stopifnot(file.exists('app/data/stable.csv'),file.exists('app/data/eta/v1/weather_effect.csv'))
for(name in c('stable.csv','planted_delay.csv','eta/v1/weather_effect.csv')) {
  stopifnot(unname(tools::md5sum(file.path('data',name)))==unname(tools::md5sum(file.path('app/data',name))))
}
standalone <- tempfile('diis-standalone-')
dir.create(standalone)
files <- list.files('app',all.files=TRUE,no..=TRUE,full.names=TRUE)
stopifnot(all(file.copy(files,standalone,recursive=TRUE)))
setwd(standalone)
on.exit(setwd(repo),add=TRUE)
env <- new.env(parent=globalenv())
result <- source('app.R',local=env)$value
stopifnot(inherits(result,'shiny.appobj'),env$repo_root==normalizePath(standalone),
  length(env$hospital_trips)==5,length(env$demo_hospitals)==9,
  env$eta_bundle$status!='unavailable')
cat('DEPLOY BUNDLE PASS: standalone startup, synthetic data checksums, five zones and nine hospitals\n')
