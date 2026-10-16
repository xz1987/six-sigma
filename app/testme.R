if (!requireNamespace('shiny', quietly = TRUE)) stop('Install the shiny package first.')
port <- as.integer(Sys.getenv('PORT', '3838'))
host <- Sys.getenv('HOST', '127.0.0.1')
shiny::runApp('app', host = host, port = port, launch.browser = FALSE)
