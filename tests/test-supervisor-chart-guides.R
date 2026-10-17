source(file.path('..','app','supervisor.R'),local=TRUE)
test_that('every plotted Supervisor chart has an accessible guide and visible legend', {
  code <- paste(readLines(file.path('..','app','supervisor.R')),collapse='\n')
  matches <- regmatches(code,gregexpr('output\\$[a-z_]+ <- renderPlot',code))[[1]]
  ids <- sub(' <- renderPlot','',sub('output\\$','',matches))
  guides <- supervisor_chart_guides()
  expect_setequal(names(guides),ids)
  for (id in ids) {
    html <- as.character(supervisor_chart(shiny::NS('test'),id))
    expect_match(html,'<details',fixed=TRUE,info=id)
    expect_match(html,'How to read this chart',fixed=TRUE,info=id)
    expect_match(html,'aria-label="Chart legend"',fixed=TRUE,info=id)
    expect_match(html,paste0('id="test-',id,'"'),fixed=TRUE,info=id)
    expect_true(nchar(guides[[id]]$text)>80,info=id)
    expect_true(length(guides[[id]]$legend)>=3,info=id)
  }
})
