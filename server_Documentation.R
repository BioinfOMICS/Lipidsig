
require(shinydashboard)
observeEvent(input$link_to_Profiling, {
  newvalue <- 'Profiling'
  updateTabItems(session, 'narbarpage', newvalue)
})
observeEvent(input$link_to_DE, {
  newvalue <- "Differential expression"
  updateTabItems(session, 'narbarpage', newvalue)
})
observeEvent(input$link_to_ML, {
  newvalue <- "ML"
  updateTabItems(session, 'narbarpage', newvalue)
})
observeEvent(input$link_to_Correlation, {
  newvalue <- "Correlation"
  updateTabItems(session, 'narbarpage', newvalue)
})
observeEvent(input$link_to_Network, {
  newvalue <- "Network"
  updateTabItems(session, 'narbarpage', newvalue)
})