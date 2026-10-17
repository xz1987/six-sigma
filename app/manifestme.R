# Run from the repository root or app directory before publishing app/.
if (!requireNamespace('rsconnect', quietly=TRUE)) stop('Install rsconnect before publishing.')
app_dir <- normalizePath(if(file.exists('app/qc.R')) 'app' else '.')
repo_dir <- dirname(app_dir)
data_files <- c('stable.csv','planted_delay.csv','eta/v1/weather_effect.csv')
for(name in data_files) {
  destination <- file.path(app_dir,'data',name)
  dir.create(dirname(destination),recursive=TRUE,showWarnings=FALSE)
  if(!file.copy(file.path(repo_dir,'data',name),destination,overwrite=TRUE)) stop('Cannot bundle data: ',name)
}
runtime <- c('app.R','supervisor.R','supervisor-analytics.R','responder-demo.R','responder-server.R',
  'caller/caller.R','eta.R','shared-incidents.R','shared-trip.R','qc.R',
  'response-metrics.R','feed.R','workflow.R')
assets <- c(file.path('www',list.files(file.path(app_dir,'www'),recursive=TRUE)),
  file.path('caller/www',list.files(file.path(app_dir,'caller/www'),recursive=TRUE)))
files <- c(runtime,assets,file.path('data',data_files))
rsconnect::writeManifest(appDir=app_dir,appFiles=files,appPrimaryDoc='app.R',appMode='shiny',quiet=FALSE)
cat('DEPLOY MANIFEST PASS: self-contained app sources, assets and synthetic fixtures\n')
