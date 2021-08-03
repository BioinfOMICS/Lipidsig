
observeEvent(input$jump_to_Profiling, {
  newvalue <- 'Profiling'
  updateTabsetPanel(session, 'narbarpage', newvalue)
})
observeEvent(input$jump_to_DE, {
  newvalue <- "Differential expression"
  updateTabsetPanel(session, 'narbarpage', newvalue)
})
observeEvent(input$jump_to_ML, {
  newvalue <- "ML"
  updateTabsetPanel(session, 'narbarpage', newvalue)
})
observeEvent(input$jump_to_Correlation, {
  newvalue <- "Correlation"
  updateTabsetPanel(session, 'narbarpage', newvalue)
})
observeEvent(input$jump_to_Network, {
  newvalue <- "Network"
  updateTabsetPanel(session, 'narbarpage', newvalue)
})

observeEvent(input$link_to_help_Profiling, {
  updateTabsetPanel(session, 'narbarpage', 'Help')
  updateNavlistPanel(session, "navlistPanel_FAQ", selected =   'Help_Profiling')
})
observeEvent(input$link_to_help_DE, {
  updateTabsetPanel(session, 'narbarpage', 'Help')
  updateNavlistPanel(session, "navlistPanel_FAQ", selected =   "Help_Differential expression")
})
observeEvent(input$link_to_help_ML, {
  updateTabsetPanel(session, 'narbarpage', 'Help')
  updateNavlistPanel(session, "navlistPanel_FAQ", selected =   "Help_Machine learning")
})
observeEvent(input$link_to_help_Correlation, {
  updateTabsetPanel(session, 'narbarpage', 'Help')
  updateNavlistPanel(session, "navlistPanel_FAQ", selected =   "Help_Correlation")
})
observeEvent(input$link_to_help_Network, {
  updateTabsetPanel(session, 'narbarpage', 'Help')
  updateNavlistPanel(session, "navlistPanel_FAQ", selected =   "Help_Network")
})