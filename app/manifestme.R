if (!requireNamespace('rsconnect', quietly = TRUE)) {
  stop('Install rsconnect before writing a Posit Connect manifest.')
}
rsconnect::writeManifest(appDir = 'app')
