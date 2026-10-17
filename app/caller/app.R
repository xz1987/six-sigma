library(shiny)
source('caller.R',local=TRUE)
# Shared by browser sessions in this single R process, never sent as a list.
# Replace with the team's shared dispatcher store when merging interfaces.
dispatch_store<-caller_dispatch_store()
ui<-fluidPage(tags$head(tags$title('DIIS | Your ambulance'),tags$meta(name='viewport',content='width=device-width, initial-scale=1'),tags$meta(name='theme-color',content='#135b63'),tags$meta(name='referrer',content='strict-origin-when-cross-origin'),tags$link(rel='manifest',href='manifest.webmanifest'),tags$link(rel='apple-touch-icon',href='icons/apple-touch-icon.png'),tags$script(src='pwa.js',defer=NA)),
  div(class='caller-topbar',div(class='caller-logo','+'),div(h1('Emergency Response Monitor'),p('DIIS / CALLER WORKSPACE'))),
  uiOutput('dispatchPreview'),
  div(class='caller-install',tags$button(id='install-app',type='button',class='btn btn-default',hidden=NA,'Install app'),tags$span(id='pwa-setup-status',role='status')),
  caller_ui('caller'))
server<-function(input,output,session){
  created<-reactiveVal(NULL)
  observeEvent(input$createDispatch,{
    created(caller_create_dispatch(dispatch_store))
  })
  output$dispatchPreview<-renderUI({
    if(!isTRUE(input$clientReady)||isTRUE(input$trackingLinkOpened))return(NULL)
    d<-created()
    div(class='caller-demo-bar',strong('SIMULATED DISPATCH · PREVIEW ONLY'),
      p('Create a synthetic dispatch to generate its private caller tracking link automatically. The caller opens that link without entering a password or code.'),
      actionButton('createDispatch','Create simulated dispatch',class='btn-primary'),
      if(!is.null(d))tagList(p(paste('Incident created:',d$incident_id)),
        tags$a(class='btn btn-primary',href=d$fragment,'Open caller tracking'),
        p('This private link can also be opened in another tab on this running preview. It expires after 30 minutes. No message is sent.')),
      p(class='caller-map-note','The dispatch controls above are a demo harness, not the staff dispatch interface. Movement uses a scripted 90-second simulation.'))
  })
  caller_server('caller',lookup=function(token)caller_dispatch_lookup(dispatch_store,token))
}
shinyApp(ui,server)
