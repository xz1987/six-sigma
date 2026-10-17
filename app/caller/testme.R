args<-commandArgs(trailingOnly=FALSE)
script<-sub('^--file=','',args[grepl('^--file=',args)])
folder<-dirname(normalizePath(script))
shiny::runApp(folder,host='127.0.0.1',port=as.integer(Sys.getenv('DIIS_CALLER_PORT','5770')),launch.browser=FALSE)
