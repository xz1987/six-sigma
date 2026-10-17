if (!requireNamespace('rsconnect', quietly = TRUE)) {
  stop('Install rsconnect before deploying to Posit Connect.')
}
server <- Sys.getenv('CONNECT_SERVER')
key <- Sys.getenv('CONNECT_API_KEY')
if (!nzchar(server) || !nzchar(key)) {
  stop('Set CONNECT_SERVER and CONNECT_API_KEY in the environment; never save them in files.')
}
rsconnect::addConnectServer(server, name = 'course-connect')
rsconnect::connectApiUser(account = 'hackathon', server = 'course-connect', apiKey = key)
rsconnect::deployApp(appDir = 'app', server = 'course-connect')
