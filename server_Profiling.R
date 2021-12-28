##test5.v27

################################
################################
######                    ######
######   Profiling Page   ######
######                    ######
################################
################################


############################
####  Assign variables  ####
############################

#### PRO_exp_data() ####
PRO_exp_data <- reactive({
  if(input$PRO_data_source == 'PRO_demo_data'){
    variables$PRO.exp.data.demo
  }else if(input$PRO_data_source == 'PRO_user_data'){
    variables$PRO.exp.data.user
  }
})

#### PRO_exp_transform_data() ####
PRO_exp_transform_data <- reactive({
  if(input$PRO_data_source == 'PRO_demo_data'){
    data_process(PRO_exp_data(), 
                 exclude_var_missing=T, 
                 missing_pct_limit=50,
                 replace_zero=T, zero2what='NA', xmin=0.5,
                 replace_NA=T, NA2what='min', ymin=0.5,
                 pct_transform=T,
                 data_transform=F,
                 centering=F,
                 scaling=F)
  }else if(input$PRO_data_source == 'PRO_user_data'){
    PRO_Check_exp_data_table <- reactive({variables$PRO.check.exp.data.user})
    if(data_check(exp_data=PRO_Check_exp_data_table(),data_type="exp",page="Profiling",file_path=input$PRO_user_exp$datapath,
                  remove_na=input$PRO_rm_NA,remove_na_pct=input$PRO_rm_NA_pct)[[2]]){
      data_process(PRO_exp_data(), 
                   exclude_var_missing=input$PRO_rm_NA, 
                   missing_pct_limit=input$PRO_rm_NA_pct,
                   replace_zero=T, zero2what='NA', xmin=0.5,
                   replace_NA=input$PRO_rp_NA, NA2what=input$PRO_fill_NA, ymin=input$PRO_fill_min,
                   pct_transform=input$PRO_pct_trans,
                   data_transform=input$PRO_log_trans,trans_type='log',
                   centering=F,
                   scaling=F)
    }else{
      NULL
    }
  }
  
})

#### PRO_lipid_char_table() ####
PRO_lipid_char_table <- reactive({
  if(input$PRO_data_source == 'PRO_demo_data'){
    variables$PRO.lipid.char.tab.demo
  }else if(input$PRO_data_source == 'PRO_user_data'){
    if(!is.null(variables$PRO.lipid.char.tab.user)){
      variables$PRO.lipid.char.tab.user <- variables$PRO.lipid.char.tab.user %>% 
        filter(feature %in% PRO_exp_transform_data()$feature)
    }else{
      variables$PRO.lipid.char.tab.user <- NULL
    }
  }
})

#### PRO_sample_count() ####
PRO_sample_count <- reactive({
  if(input$PRO_data_source == 'PRO_demo_data'){
    23
  }else if(input$PRO_data_source == 'PRO_user_data'){
    variables$PRO.sample.count.user <- (ncol(variables$PRO.exp.data.user)-1)
  }
})

shinyjs::hide('PRO_tabPanel_div')

###########################
####  PRO Data Source  ####
###########################

#### Output: PRO.demo.download ####
output$PRO.demo.download <- downloadHandler(
  filename = function() {
    "Profiling_example_dataset.zip"
  },
  content = function(file) {
    file.copy("www/download_demo_dataset/Profiling.zip", file)
  },
  contentType = "application/zip"
) 


#### PRO demo dataset ####
observeEvent(input$PRO_demo_upload, {
  
  if(input$PRO_demo_upload > 0){
    progressSweetAlert(
      session = session, id = "PRO_demo_progress",
      title = "Work in progress",
      display_pct = TRUE, value = 0
    )
    #### shinyjs show/hide main panel ####
    shinyjs::show('PRO_demo_mainPanel_div')
    
    #### import demo dataset ####
    variables$PRO.exp.data.demo <- readRDS('www/demo_dataset/Profiling/exp_data.rds')
    variables$PRO.lipid.char.tab.demo <- readRDS('www/demo_dataset/Profiling/lipid_char_table.rds')
    #### Output: PRO.demo.exp.raw ####
    output$PRO.demo.exp.raw <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(variables$PRO.exp.data.demo, 
                      #caption = 'Lipid expression data',
                      #colnames = c('feature', ML_group_info()$label_name),
                      escape = FALSE, selection = 'none', rownames = FALSE, 
                      class = "nowrap row-border",
                      extensions = c('Buttons', 'Scroller'),
                      options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                     deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                     dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                     columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      })
    })
    updateProgressBar(
      session = session,
      id = "PRO_demo_progress",
      value = 30
    )
    #### Output: PRO.demo.exp ####
    output$PRO.demo.exp <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(PRO_exp_data() %>% mutate_if(is.numeric, ~round(., 5)), 
                      #caption = 'Lipid expression data',
                      #colnames = c('feature', ML_group_info()$label_name),
                      escape = FALSE, selection = 'none', rownames = FALSE, 
                      class = "nowrap row-border",
                      extensions = c('Buttons', 'Scroller'),
                      options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                     deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                     dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                     columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      })
    })
    updateProgressBar(
      session = session,
      id = "PRO_demo_progress",
      value = 60
    )
    #### Output: PRO.demo.lipid.char ####
    output$PRO.demo.lipid.char <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(PRO_lipid_char_table(), 
                      #caption = 'Lipid expression data',
                      #colnames = c('feature', ML_group_info()$label_name),
                      escape = FALSE, selection = 'none', rownames = FALSE, 
                      class = "nowrap row-border",
                      extensions = c('Buttons', 'Scroller'),
                      options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                     deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                     dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                     columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      })
    })
    updateProgressBar(
      session = session,
      id = "PRO_demo_progress",
      value = 90
    )
    updateProgressBar(
      session = session,
      id = "PRO_demo_progress",
      value = 100
    )
    closeSweetAlert(session = session)
  }
  
}) #observeEvent(input$PRO_demo_upload

#### PRO user dataset ####
observeEvent(input$PRO_user_upload, {
  
  #### shinyjs show/hide main panel ####
  shinyjs::show('PRO_user_mainPanel_div')
  
  #isolate({
  progressSweetAlert(
    session = session, id = "PRO_user_progress",
    title = "Work in progress",
    display_pct = TRUE, value = 0
  )
    showNotification("Start uploading file...", type = "message")
    tryCatch(
      {
        #### import user dataset ####
        ## exp_data
        variables$PRO.exp.data.user <- data.table::fread(input$PRO_user_exp$datapath, header = T,
                                                         stringsAsFactors = F, check.names = F, 
                                                         data.table = F, na.strings = c('', 'NA'))
        ## lipid_char_table
        if(!is.null(input$PRO_user_char)){
          variables$PRO.lipid.char.tab.user <- data.table::fread(input$PRO_user_char$datapath, header = T, 
                                                                 stringsAsFactors = F, check.names = F, 
                                                                 data.table = F, na.strings = c('', 'NA'))
        }else{
          variables$PRO.lipid.char.tab.user <- NULL
        }
        
        showNotification("Received uploaded file.", type = "message")
      },
      error = function(e) {
        sendSweetAlert(
          session = session,
          title = "Input data error!",
          text = as.character(message(e)),
          type = "error"
        )
        return()
      },
      warning = function(w) {
        sendSweetAlert(
          session = session,
          title = "Input data warning!",
          text = "Some error is in your dataset, it maybe cause some problem we cannot expected.",
          type = "warning"
        )
        return()
      }
    )
    
    ### data check ####
    ## exp_data
    variables$PRO.check.exp.data.user <- data.table::fread(input$PRO_user_exp$datapath, header = T,
                                                     stringsAsFactors = F, check.names = F, 
                                                     data.table = F, na.strings = c('', 'NA'))
    PRO_Check_exp_data_table <- reactive({variables$PRO.check.exp.data.user})
    output$PRO_Check_Exp_Data <- renderUI({
      isolate({
        data_check(exp_data=PRO_Check_exp_data_table(),data_type="exp",
                   page="Profiling",file_path=input$PRO_user_exp$datapath,
                   remove_na=input$PRO_rm_NA,remove_na_pct=input$PRO_rm_NA_pct)[[1]]
      })
    })
    updateProgressBar(
      session = session,
      id = "PRO_user_progress",
      value = 30
    )
    ## lipid_char_table
    if(!is.null(input$PRO_user_char)){
      variables$PRO.check.lipid.char.tab.user <- data.table::fread(input$PRO_user_char$datapath, header = T, 
                                                                   stringsAsFactors = F, check.names = F,
                                                                   data.table = F, na.strings = c('', 'NA'))
      PRO_Check_lipid_char_data_table <- reactive({variables$PRO.check.lipid.char.tab.user})
      output$PRO_Check_lipid_char <- renderUI({
        isolate({
          data_check(data=PRO_Check_lipid_char_data_table(),exp_data=PRO_Check_exp_data_table(),
                     data_type="lipid_char",file_path=input$PRO_user_char$datapath)[[1]]
        })
      })
    }else{
      variables$PRO.check.lipid.char.tab.user <- NULL
      PRO_Check_lipid_char_data_table <- reactive({variables$PRO.check.lipid.char.tab.user})
      output$PRO_Check_lipid_char <- renderUI({
        isolate({
          data_check(data=NULL,exp_data = NULL,data_type="optional_lipid_char")[[1]]
        })
      })
    }
    #### rename column name ####
    ## exp_data
    variables$PRO.exp.user.col1 <- colnames(variables$PRO.exp.data.user)[1]
    colnames(variables$PRO.exp.data.user)[1] <- 'feature'
    ## lipid_char_table
    if(!is.null(variables$PRO.lipid.char.tab.user)){
      variables$PRO.lipid.char.user.col1 <- colnames(variables$PRO.lipid.char.tab.user)[1]
      colnames(variables$PRO.lipid.char.tab.user)[1] <- 'feature'
    }
    updateProgressBar(
      session = session,
      id = "PRO_user_progress",
      value = 60
    )
    # #### Output: PRO.user.exp ####
    # output$PRO.user.exp <- renderDataTable(server = FALSE,{
    #   
    #   validate(need(!is.null(PRO_exp_transform_data()), "Some error is in your expression data, please check your data and re-upload it."))
    #   DT::datatable(PRO_exp_transform_data(), 
    #                 #caption = 'Lipid expression data',
    #                 colnames = c(variables$PRO.exp.user.col1, colnames(variables$PRO.exp.data.user)[-1]),
    #                 escape = FALSE, selection = 'none', rownames = FALSE, 
    #                 class = "nowrap row-border",
    #                 extensions = c('Buttons', 'Scroller'),
    #                 options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
    #                                deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
    #                                dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
    #                                columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    # })
    # #### Output: PRO.user.lipid.char ####
    # output$PRO.user.lipid.char <- renderDataTable(server = FALSE,{
    #   
    #   validate(need(!is.null(PRO_lipid_char_table()), "Without lipid_char_table"))
    #   DT::datatable(PRO_lipid_char_table(), 
    #                 #caption = 'Lipid expression data',
    #                 colnames = c(variables$PRO.lipid.char.user.col1, colnames(variables$PRO.lipid.char.tab.user)[-1]),
    #                 escape = FALSE, selection = 'none', rownames = FALSE, 
    #                 class = "nowrap row-border",
    #                 extensions = c('Buttons', 'Scroller'),
    #                 options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
    #                                deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
    #                                dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
    #                                columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    # })
    
    observe({
      if(is.null(PRO_Check_lipid_char_data_table())){
        if(data_check(data=NULL,exp_data = NULL,data_type="optional_lipid_char")[[2]] & 
           data_check(exp_data=PRO_Check_exp_data_table(),data_type="exp",page="Profiling",file_path=input$PRO_user_exp$datapath,
                      remove_na=input$PRO_rm_NA,remove_na_pct=input$PRO_rm_NA_pct)[[2]]){
          shinyjs::show('PRO_user_input_table_div')
          shinyjs::enable("PRO_user_start")
          output$PRO_Data_summary <-  renderUI({
            isolate({
              data_summary(exp_data = PRO_Check_exp_data_table(),remove_na=input$PRO_rm_NA,remove_na_pct=input$PRO_rm_NA_pct,
                           fill_na = input$PRO_rp_NA,fill_na_method = input$PRO_fill_NA,
                           fill_na_Multiply = input$PRO_fill_min,
                           PCT_tr = input$PRO_pct_trans,log_tr = input$PRO_log_trans)
            })
          })
          
          #### Output: PRO.user.exp.raw ####
          output$PRO.user.exp.raw <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(PRO_exp_data()), "Some error is in your expression data, please check your data and re-upload it."))
              DT::datatable(PRO_exp_data(), 
                            #caption = 'Lipid expression data',
                            #colnames = c(variables$PRO.exp.user.col1, colnames(variables$PRO.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          #### Output: PRO.user.exp ####
          output$PRO.user.exp <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(PRO_exp_transform_data()), "Some error is in your expression data, please check your data and re-upload it."))
              DT::datatable(PRO_exp_transform_data(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$PRO.exp.user.col1, colnames(variables$PRO.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          #### Output: PRO.user.lipid.char ####
          output$PRO.user.lipid.char <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(PRO_lipid_char_table()), "Not uploaded"))
              DT::datatable(PRO_lipid_char_table(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$PRO.lipid.char.user.col1, colnames(variables$PRO.lipid.char.tab.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
        }else{
          shinyjs::hide('PRO_user_input_table_div')
          shinyjs::disable("PRO_user_start")
          output$PRO_Data_summary <-  renderUI({
            NULL
          })
        }
      }else{
        if(data_check(data=PRO_Check_lipid_char_data_table(),exp_data=PRO_Check_exp_data_table(),data_type="lipid_char",file_path=input$PRO_user_char$datapath)[[2]] & 
           data_check(exp_data=PRO_Check_exp_data_table(),data_type="exp",page="Profiling",file_path=input$PRO_user_exp$datapath,
                      remove_na=input$PRO_rm_NA,remove_na_pct=input$PRO_rm_NA_pct)[[2]]){
          shinyjs::show('PRO_user_input_table_div')
          shinyjs::enable("PRO_user_start")
          output$PRO_Data_summary <-  renderUI({
            isolate({
              data_summary(exp_data = PRO_Check_exp_data_table(),remove_na=input$PRO_rm_NA,remove_na_pct=input$PRO_rm_NA_pct,
                           fill_na = input$PRO_rp_NA,fill_na_method = input$PRO_fill_NA,
                           fill_na_Multiply = input$PRO_fill_min,
                           PCT_tr = input$PRO_pct_trans,log_tr = input$PRO_log_trans)
            })
          })
          
          #### Output: PRO.user.exp.raw ####
          output$PRO.user.exp.raw <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(PRO_exp_data()), "Some error is in your expression data, please check your data and re-upload it."))
              DT::datatable(PRO_exp_data(), 
                            #caption = 'Lipid expression data',
                            #colnames = c(variables$PRO.exp.user.col1, colnames(variables$PRO.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          #### Output: PRO.user.exp ####
          output$PRO.user.exp <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(PRO_exp_transform_data()), "Some error is in your expression data, please check your data and re-upload it."))
              DT::datatable(PRO_exp_transform_data() %>% mutate_if(is.numeric, ~round(., 5)), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$PRO.exp.user.col1, colnames(variables$PRO.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          #### Output: PRO.user.lipid.char ####
          output$PRO.user.lipid.char <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(PRO_lipid_char_table()), "Not uploaded"))
              DT::datatable(PRO_lipid_char_table(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$PRO.lipid.char.user.col1, colnames(variables$PRO.lipid.char.tab.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
        }else{
          shinyjs::hide('PRO_user_input_table_div')
          shinyjs::disable("PRO_user_start")
          output$PRO_Data_summary <-  renderUI({
            NULL
          })
        }
      }
      
    }) #observe
    updateProgressBar(
      session = session,
      id = "PRO_user_progress",
      value = 100
    )
    closeSweetAlert(session = session)
  #}) #isolate
  
}) #observeEvent(input$PRO_user_upload

#### control user reset button ####
observeEvent(input$PRO_user_reset, {
  
  #### shinyjs show/hide main panel ####
  shinyjs::hide('PRO_user_mainPanel_div')
  
  #### shiny show/hide tab ####
  hideTab(inputId = 'PRO_analysis_tab', target = 'Cross-sample variability')
  hideTab(inputId = 'PRO_analysis_tab', target = 'Dimensionality reduction')
  hideTab(inputId = 'PRO_analysis_tab', target = 'Correlation heatmap')
  hideTab(inputId = 'PRO_analysis_tab', target = 'Lipid characteristics profiling')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('PRO_user_reset_div')
  
  #### clear variables ####
  variables$PRO.exp.data.user = NULL
  variables$PRO.lipid.char.tab.user = NULL
  
}) #observeEvent(input$PRO_user_reset

#### control user upload button ####
observe({
  
  if(is.null(input$PRO_user_exp)){
    shinyjs::disable("PRO_user_upload")
  }else{
    shinyjs::enable("PRO_user_upload")
  }
  
}) #observe


##################################
####  Profiling analysis tab  ####
##################################

#### control hide/show tabpanel: input$PRO_demo_start ####
observeEvent(input$PRO_demo_start, {
  
  shinyjs::show('PRO_tabPanel_div')
  showTab(inputId = 'PRO_analysis_tab', target = 'Cross-sample variability')
  showTab(inputId = 'PRO_analysis_tab', target = 'Dimensionality reduction')
  showTab(inputId = 'PRO_analysis_tab', target = 'Correlation heatmap')
  showTab(inputId = 'PRO_analysis_tab', target = 'Lipid characteristics profiling')
  
  #### shinyjs show/hide results ####
  shinyjs::hide('PRO_dim_redu_result_div')
  shinyjs::hide('PRO.corr.heatmap.lipid')
  shinyjs::hide('PRO.corr.heatmap.sample')
  
  
}) #observeEvent(input$PRO_demo_start

#### control hide/show tabpanel: input$PRO_user_start ####
observeEvent(input$PRO_user_start, {
  
  shinyjs::show('PRO_tabPanel_div')
  showTab(inputId = 'PRO_analysis_tab', target = 'Cross-sample variability')
  showTab(inputId = 'PRO_analysis_tab', target = 'Dimensionality reduction')
  showTab(inputId = 'PRO_analysis_tab', target = 'Correlation heatmap')
  
  if(is.null(input$PRO_user_char) | is.null(variables$PRO.lipid.char.tab.user)){
    hideTab(inputId = 'PRO_analysis_tab', target = 'Lipid characteristics profiling')
  }else{
    showTab(inputId = 'PRO_analysis_tab', target = 'Lipid characteristics profiling')
  }
  
  #### shinyjs show/hide results ####
  shinyjs::hide('PRO_dim_redu_result_div')
  shinyjs::hide('PRO.corr.heatmap.lipid')
  shinyjs::hide('PRO.corr.heatmap.sample')
  
}) #observeEvent(input$PRO_user_start


############################################
############################################
#####  Tab1: Cross-sample variability  #####
############################################
############################################

#### Function: exp_profilling ####
exp_pro_result <- reactive({
  if(!is.null(PRO_exp_data())){
    exp_profilling(PRO_exp_data()) 
  }else{
    NULL
  }
})

#### Output: PRO.num.of.expressed.lipid ####
output$PRO.num.of.expressed.lipid <- renderPlotly({
  exp_pro_result()$i.expr.lip
})

#### Output: PRO.lipid.amount ####
output$PRO.lipid.amount <- renderPlotly({
  exp_pro_result()$i.p.amount
})

#### Output: PRO.expr.distribution ####
output$PRO.expr.distribution <- renderPlotly({
  exp_pro_result()$p.hist.value
})



############################################
############################################
#####  Tab2: Dimensionality reduction  #####
############################################
############################################

#######################
#### Control panel ####
#######################

#### dbscan ####
#### update numeric input minPts ####
observe({
  updateNumericInput(session, 
                     inputId = 'PRO_dbscan_minPts', 
                     max = (PRO_sample_count()-1)
  ) #updateNumericInput
})

#### control reset button ####
observeEvent(input$PRO_dim_redu_reset,{
  
  #### shinyjs show/hide results ####
  shinyjs::hide('PRO_dim_redu_result_div')
  
  #### shinyjs reset control panel ####
  shinyjs::reset("PRO_dim_redu_reset_div")
  
}) #observeEvent(input$PRO_dim_redu_reset

#### control start button ####
observeEvent(input$PRO_dim_redu_start,{
  observeEvent(input$PRO_dim_redu_method,{
    if(input$PRO_dim_redu_method == 'pca'){
      feature_num_check=2
      sample_num_check=6
      shinyjs::hide('PRO_dim_redu_result_div')
    }else if(input$PRO_dim_redu_method == 'tsne'){
      feature_num_check=4
      sample_num_check=2
      shinyjs::hide('PRO_dim_redu_result_div')
    }else{
      feature_num_check=2
      sample_num_check=2
      shinyjs::hide('PRO_dim_redu_result_div')
    }
    
    
    if(submit_check(transform_data=PRO_exp_transform_data(),check_NA=T,feature_num=feature_num_check,sample_num=sample_num_check)[[1]]!=TRUE){
      showModal(modalDialog(
        title = "Important message",
        submit_check(transform_data=PRO_exp_transform_data(),check_NA=T,feature_num=feature_num_check,sample_num=sample_num_check)[[2]],
        easyClose = TRUE
      ))
      shinyjs::hide('PRO_dim_redu_result_div')
    }else{
      #### shinyjs show/hide results ####
      shinyjs::show('PRO_dim_redu_result_div')
    }
    #### Function: PCA ####
    isolate({
      if(input$PRO_dim_redu_method == 'pca'){
        
        if(input$PRO_cluster_method == 'kmeans'){
          isolate({
            variables$PRO.pca.result <- PCA(PRO_exp_transform_data(), group_info = NULL, sig_feature = NULL,
                                            scaling=input$PRO_pca_scale, 
                                            centering=input$PRO_pca_center, 
                                            cluster_method=input$PRO_cluster_method, 
                                            group_num = input$PRO_kmeans_group)
          })
        }else if(input$PRO_cluster_method == 'kmedoids'){
          isolate({
            variables$PRO.pca.result <- PCA(PRO_exp_transform_data(), group_info = NULL, sig_feature = NULL,
                                            scaling=input$PRO_pca_scale, 
                                            centering=input$PRO_pca_center, 
                                            cluster_method=input$PRO_cluster_method, 
                                            group_num = input$PRO_pam_group, 
                                            var1 = input$PRO_pam_metric)
          })
        }else if(input$PRO_cluster_method == 'hclustering'){
          isolate({
            variables$PRO.pca.result <- PCA(PRO_exp_transform_data(), group_info = NULL, sig_feature = NULL,
                                            scaling=input$PRO_pca_scale, 
                                            centering=input$PRO_pca_center, 
                                            cluster_method=input$PRO_cluster_method, 
                                            group_num = input$PRO_hclust_group, 
                                            var1 = input$PRO_hclust_dist, 
                                            var2 = input$PRO_hclust_hclust)
          })
        }else if(input$PRO_cluster_method == 'dbscan'){
          isolate({
            variables$PRO.pca.result <- PCA(PRO_exp_transform_data(), group_info = NULL, sig_feature = NULL,
                                            scaling=input$PRO_pca_scale, 
                                            centering=input$PRO_pca_center, 
                                            cluster_method=input$PRO_cluster_method, 
                                            var1 = input$PRO_dbscan_eps, 
                                            var2 = input$PRO_dbscan_minPts)
          })
        }
        
        #### Output: PRO.pca.biplot ####
        output$PRO.pca.biplot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$PRO.pca.result[[4]]), "Plot not showing. Missing value imputation is recommended."))
            variables$PRO.pca.result[[4]]
          })
        })
        
        #### Output: PRO.pca.screeplot ####
        output$PRO.pca.screeplot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$PRO.pca.result[[5]]), "Plot not showing. Missing value imputation is recommended."))
            variables$PRO.pca.result[[5]]
          })
        })
        
        #### Output: PRO.pca.rotated.data ####
        output$PRO.pca.rotated.data <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$PRO.pca.result[[2]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$PRO.pca.result[[2]] %>% mutate_if(is.numeric, ~round(., 5)),
                          #caption = 'Lipid expression data',
                          #colnames = c('feature', ML_group_info()$label_name),
                          escape = FALSE, selection = 'none', rownames = TRUE, 
                          class = "nowrap row-border",
                          extensions = c('Buttons', 'Scroller'),
                          options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                         # rowCallback = JS(
                                         #   "function(row, data) {",
                                         #   "for (i = 1; i < data.length; i++) {",
                                         #   "if (data[i]>1 | data[i]<1){",
                                         #   "$('td:eq('+i+')', row).html(data[i].toExponential(2));",
                                         #   "}",
                                         #   "}",
                                         #   "}"),
                                         deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                         dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                         columnDefs = list(list(className = 'dt-center', targets = "_all"))))
          })
        }) #output$PRO.pca.rotated.data <- renderDataTable
        
        #### Output: PRO.pca.contrib.table ####
        output$PRO.pca.contrib.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$PRO.pca.result[[3]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$PRO.pca.result[[3]] %>% mutate_if(is.numeric, ~round(., 5)),
                          #caption = 'Lipid expression data',
                          #colnames = c('feature', ML_group_info()$label_name),
                          escape = FALSE, selection = 'none', rownames = TRUE, 
                          class = "nowrap row-border",
                          extensions = c('Buttons', 'Scroller'),
                          options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                         # rowCallback = JS(
                                         #   "function(row, data) {",
                                         #   "for (i = 1; i < data.length; i++) {",
                                         #   "if (data[i]>1 | data[i]<1){",
                                         #   "$('td:eq('+i+')', row).html(data[i].toExponential(2));",
                                         #   "}",
                                         #   "}",
                                         #   "}"),
                                         deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                         dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                         columnDefs = list(list(className = 'dt-center', targets = "_all"))))
          })
        }) #output$PRO.pca.contrib.table <- renderDataTable
        
      }else if(input$PRO_dim_redu_method == 'tsne'){
        
        if(input$PRO_cluster_method == 'kmeans'){
          isolate({
            variables$PRO.tsne.result <- tsne(PRO_exp_transform_data(), group_info = NULL, sig_feature = NULL,
                                              pca=input$PRO_tsne_pca, 
                                              perplexity=input$PRO_tsne_perplexity,
                                              max_iter=input$PRO_tsne_max_iter, 
                                              cluster_method=input$PRO_cluster_method, 
                                              group_num = input$PRO_kmeans_group)
          })
        }else if(input$PRO_cluster_method == 'kmedoids'){
          isolate({
            variables$PRO.tsne.result <- tsne(PRO_exp_transform_data(), group_info = NULL, sig_feature = NULL,
                                              pca=input$PRO_tsne_pca, 
                                              perplexity=input$PRO_tsne_perplexity,
                                              max_iter=input$PRO_tsne_max_iter, 
                                              cluster_method=input$PRO_cluster_method, 
                                              group_num = input$PRO_pam_group, 
                                              var1 = input$PRO_pam_metric)      
          })
        }else if(input$PRO_cluster_method == 'hclustering'){
          isolate({
            variables$PRO.tsne.result <- tsne(PRO_exp_transform_data(), group_info = NULL, sig_feature = NULL,
                                              pca=input$PRO_tsne_pca, 
                                              perplexity=input$PRO_tsne_perplexity,
                                              max_iter=input$PRO_tsne_max_iter, 
                                              cluster_method=input$PRO_cluster_method, 
                                              group_num = input$PRO_hclust_group, 
                                              var1 = input$PRO_hclust_dist, 
                                              var2 = input$PRO_hclust_hclust)       
          })
        }else if(input$PRO_cluster_method == 'dbscan'){
          isolate({
            variables$PRO.tsne.result <- tsne(PRO_exp_transform_data(), group_info = NULL, sig_feature = NULL,
                                              pca=input$PRO_tsne_pca, 
                                              perplexity=input$PRO_tsne_perplexity,
                                              max_iter=input$PRO_tsne_max_iter, 
                                              cluster_method=input$PRO_cluster_method, 
                                              var1 = input$PRO_dbscan_eps, 
                                              var2 = input$PRO_dbscan_minPts)       
          })
        }
        
        #### Output: PRO.tsne.plot ####
        output$PRO.tsne.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$PRO.tsne.result[[2]]), "Plot not showing. Missing value imputation is recommended."))
            variables$PRO.tsne.result[[2]]
          })
        })
        
        #### Output: PRO.tsne.table ####
        output$PRO.tsne.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$PRO.tsne.result[[1]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$PRO.tsne.result[[1]],
                          #caption = 'Lipid expression data',
                          #colnames = c('feature', ML_group_info()$label_name),
                          escape = FALSE, selection = 'none', rownames = TRUE, 
                          class = "nowrap row-border",
                          extensions = c('Buttons', 'Scroller'),
                          options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                         # rowCallback = JS(
                                         #   "function(row, data) {",
                                         #   "for (i = 1; i < data.length; i++) {",
                                         #   "if (data[i]>1 | data[i]<1){",
                                         #   "$('td:eq('+i+')', row).html(data[i].toExponential(2));",
                                         #   "}",
                                         #   "}",
                                         #   "}"),
                                         deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                         dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                         columnDefs = list(list(className = 'dt-center', targets = "_all"))))
          })
        }) #output$PRO.tsne.table <- renderDataTable
        
        
      }else if(input$PRO_dim_redu_method == 'umap'){
        
        if(input$PRO_cluster_method == 'kmeans'){
          isolate({
            variables$PRO.umap.result <- UMAP(PRO_exp_transform_data(), group_info = NULL, sig_feature = NULL,
                                              n_neighbors=input$PRO_umap_n_neighbors, 
                                              scale=input$PRO_umap_scale,
                                              metric=input$PRO_umap_metric,
                                              cluster_method=input$PRO_cluster_method, 
                                              group_num = input$PRO_kmeans_group)       
          })
        }else if(input$PRO_cluster_method == 'kmedoids'){
          isolate({
            variables$PRO.umap.result <- UMAP(PRO_exp_transform_data(), group_info = NULL, sig_feature = NULL,
                                              n_neighbors=input$PRO_umap_n_neighbors, 
                                              scale=input$PRO_umap_scale,
                                              metric=input$PRO_umap_metric,
                                              cluster_method=input$PRO_cluster_method, 
                                              group_num = input$PRO_pam_group, 
                                              var1 = input$PRO_pam_metric)       
          })
        }else if(input$PRO_cluster_method == 'hclustering'){
          isolate({
            variables$PRO.umap.result <- UMAP(PRO_exp_transform_data(), group_info = NULL, sig_feature = NULL,
                                              n_neighbors=input$PRO_umap_n_neighbors, 
                                              scale=input$PRO_umap_scale,
                                              metric=input$PRO_umap_metric,
                                              cluster_method=input$PRO_cluster_method, 
                                              group_num = input$PRO_hclust_group, 
                                              var1 = input$PRO_hclust_dist, 
                                              var2 = input$PRO_hclust_hclust)       
          })
        }else if(input$PRO_cluster_method == 'dbscan'){
          isolate({
            variables$PRO.umap.result <- UMAP(PRO_exp_transform_data(), group_info = NULL, sig_feature = NULL,
                                              n_neighbors=input$PRO_umap_n_neighbors, 
                                              scale=input$PRO_umap_scale,
                                              metric=input$PRO_umap_metric, 
                                              cluster_method=input$PRO_cluster_method, 
                                              var1 = input$PRO_dbscan_eps, 
                                              var2 = input$PRO_dbscan_minPts)       
          })
        }
        
        #### Output: PRO.umap.plot ####
        output$PRO.umap.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$PRO.umap.result[[2]]), "Plot not showing. Missing value imputation is recommended."))
            variables$PRO.umap.result[[2]]
          })
        })
        
        #### Output: PRO.umap.table ####
        output$PRO.umap.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$PRO.umap.result[[1]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$PRO.umap.result[[1]],
                          #caption = 'Lipid expression data',
                          #colnames = c('feature', ML_group_info()$label_name),
                          escape = FALSE, selection = 'none', rownames = TRUE, 
                          class = "nowrap row-border",
                          extensions = c('Buttons', 'Scroller'),
                          options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                         # rowCallback = JS(
                                         #   "function(row, data) {",
                                         #   "for (i = 1; i < data.length; i++) {",
                                         #   "if (data[i]>1 | data[i]<1){",
                                         #   "$('td:eq('+i+')', row).html(data[i].toExponential(2));",
                                         #   "}",
                                         #   "}",
                                         #   "}"),
                                         deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                         dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                         columnDefs = list(list(className = 'dt-center', targets = "_all"))))
          })
        }) #output$PRO.umap.table <- renderDataTable
        
      }
    })
  }) #observeEvent(input$PRO_dim_redu_method,

})



##### PCA #####
#### Function: PCA_variable ####
PRO.pca.variable <- reactive({
  if(!is.null(variables$PRO.pca.result[[1]])){
    PCA_variable(variables$PRO.pca.result[[1]],
                 as.numeric(input$PRO_pca_variable_topN))
  }else{
    NULL
  }
  
})

#### Function: PCA_contrib ####
PRO.pca.contrib <- reactive({
  if(!is.null(variables$PRO.pca.result[[1]])){
    if(input$PRO_pca_contrib_PC == '1_2'){
      PCA_contrib(variables$PRO.pca.result[[1]],
                  n_PC = c(1, 2),
                  as.numeric(input$PRO_pca_contrib_topN))
    }else{
      PCA_contrib(variables$PRO.pca.result[[1]],
                  n_PC = as.numeric(input$PRO_pca_contrib_PC),
                  as.numeric(input$PRO_pca_contrib_topN))
    }
  }else{
    NULL
  }
  
})

#### Output: PRO.pca.variable ####
output$PRO.pca.variable <- renderPlotly({
  validate(need(!is.null(PRO.pca.variable()[[1]]), "Plot not showing. Missing value imputation is recommended."))
  PRO.pca.variable()[[1]]
})

#### Output: PRO.pca.contrib ####
output$PRO.pca.contrib <- renderPlotly({
  validate(need(!is.null(PRO.pca.contrib()[[1]]), "Plot not showing. Missing value imputation is recommended."))
  PRO.pca.contrib()[[1]]
})

#### Update sliderInput ####
observe({
  VALUE <- ifelse(nrow(variables$PRO.pca.result[[3]]) < 10, nrow(variables$PRO.pca.result[[3]]), 10)
  updateSliderInput(session, 
                    inputId = 'PRO_pca_variable_topN', 
                    value = VALUE,
                    max = nrow(variables$PRO.pca.result[[3]]))
  updateSliderInput(session, 
                    inputId = 'PRO_pca_contrib_topN', 
                    value = VALUE,
                    max = nrow(variables$PRO.pca.result[[3]]))
})

##### tSNE #####
observe({
  VALUE1 <- ifelse(PRO_sample_count()%%3 == 0, floor(PRO_sample_count()/3)-1, floor(PRO_sample_count()/3))
  VALUE2 <- ifelse(VALUE1 < 5, VALUE1, 5)
  updateNumericInput(session, 
                     inputId = 'PRO_tsne_perplexity', 
                     value = VALUE2,
                     max = VALUE1)
})

##### UMAP ######
observe({
  VALUE <- ifelse(PRO_sample_count() < 15, PRO_sample_count(), 15)
  updateNumericInput(session, 
                     inputId = 'PRO_umap_n_neighbors', 
                     value = VALUE,
                     max = PRO_sample_count())
})

#######################################
#######################################
#####  Tab3: Correlation heatmap  #####
#######################################
#######################################

#### control start button ####
observeEvent(input$PRO_corr_heatmap_start, {
  
  #### shinyjs show/hide results ####
  shinyjs::show('PRO.corr.heatmap.lipid')
  shinyjs::show('PRO.corr.heatmap.sample')
  
  #### Function: Hclustering ####
  isolate({
    
    #### Function: corr_heatmap ####
    variables$PRO.corr.heatmap.result <- corr_heatmap(PRO_exp_transform_data(),
                                                      corr_method = input$PRO_corr_heatmap_method,
                                                      distfun = input$PRO_corr_heatmap_dist, 
                                                      hclustfun = input$PRO_corr_heatmap_hclust)
    
    #### Output: PRO.corr.heatmap.lipid ####
    output$PRO.corr.heatmap.lipid <- renderIheatmap({
      
      validate(need(!is.null(variables$PRO.corr.heatmap.result$lipids_hm), "Plot not showing. Missing value imputation is recommended."))
      variables$PRO.corr.heatmap.result$lipids_hm
      
    }) #output$PRO.corr.heatmap.lipid <- renderIheatmap
    
    #### Output: PRO.corr.heatmap.sample ####
    output$PRO.corr.heatmap.sample <- renderIheatmap({
      
      validate(need(!is.null(variables$PRO.corr.heatmap.result$sample_hm), "Plot not showing. Missing value imputation is recommended."))
      variables$PRO.corr.heatmap.result$sample_hm
      
    }) #output$PRO.corr.heatmap.sample <- renderIheatmap
    
    #### Output: PRO.corr.heatmap.lipid.matrix ####
    output$PRO.corr.heatmap.lipid.matrix <- downloadHandler(
      filename = function() {
        paste0(input$PRO_corr_heatmap_method, '_correlation_', input$PRO_corr_heatmap_dist, '_', 
               input$PRO_corr_heatmap_hclust, "_lipid_matrix", ".csv")
      },
      content = function(file) {
        write.csv(variables$PRO.corr.heatmap.result$reorder_lipids_corr_coef, file)
      }
    ) #output$PRO.corr.heatmap.lipid.matrix
    
    #### Output: PRO.corr.heatmap.sample.matrix ####
    output$PRO.corr.heatmap.sample.matrix <- downloadHandler(
      filename = function() {
        paste0(input$PRO_corr_heatmap_method, '_correlation_', input$PRO_corr_heatmap_dist, '_', 
               input$PRO_corr_heatmap_hclust, "_sample_matrix", ".csv")
      },
      content = function(file) {
        write.csv(variables$PRO.corr.heatmap.result$reorder_sample_corr_coef, file)
      }
    ) #output$PRO.corr.heatmap.sample.matrix
    
  }) #isolate
  
}) #observeEvent(input$PRO_corr_heatmap_start



##################################################
##################################################
#####  Tab4: Lipid characteristics analysis  #####
##################################################
##################################################

#### update PRO lipid characteristic select input ####
observe({
  if(!is.null(PRO_lipid_char_table())){
    lipid.char <- colnames(PRO_lipid_char_table())[-1]
    updateSelectInput(session, "PRO_lipid_char",
                      choices =  lipid.char)
  }
})

#### Function: exp_compo_by_lipidinfo ####
exp_compo_result <- reactive({
  
  exp_compo_by_lipidinfo(PRO_exp_data(), PRO_lipid_char_table(), input$PRO_lipid_char)
  
})

#### Output: PRO.lipid.char.barplot ####
output$PRO.lipid.char.barplot <- renderPlotly({
  exp_compo_result()$p.barplot.p
})

#### Output: PRO.lipid.char.composition ####
output$PRO.lipid.char.composition <- renderPlotly({
  exp_compo_result()$p.compos
})








