if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install shiny first: install.packages('shiny')")
}
if (!requireNamespace("openssl", quietly = TRUE)) stop("Install openssl first: install.packages('openssl')")
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
app_dir <- dirname(normalizePath(script[[1]]))
shiny::runApp(app_dir, host = "127.0.0.1", port = 5300, launch.browser = FALSE)
