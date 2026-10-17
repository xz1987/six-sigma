if (!requireNamespace('shiny', quietly = TRUE)) {
  stop("Install shiny first: install.packages('shiny')")
}
if (!requireNamespace('openssl', quietly = TRUE)) {
  stop("Install openssl first: install.packages('openssl')")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first: install.packages('jsonlite')")
args <- commandArgs(trailingOnly = FALSE)
script <- sub('^--file=', '', args[grepl('^--file=', args)])
app_dir <- dirname(normalizePath(script[[1]]))
port <- as.integer(Sys.getenv('PORT', '3838'))
host <- Sys.getenv('HOST', '127.0.0.1')
shiny::runApp(app_dir, host = host, port = port, launch.browser = FALSE)
