##test5.v22

#######################################
#######################################
######                           ######
######   Machine Learning Page   ######
######                           ######
#######################################
#######################################


############################
####  Assign variables  ####
############################

#### ML_exp_data() ####
ML_exp_data <- reactive({
  if(input$ML_data_source == 'ML_demo_data'){
    variables$ML.exp.data.demo
  }else if(input$ML_data_source == 'ML_user_data'){
    variables$ML.exp.data.user
  }
})

#### ML_exp_transform_data() ####
ML_exp_transform_data <- reactive({
  
  if(input$ML_data_source == 'ML_demo_data'){
    data_process(ML_exp_data(), 
                 exclude_var_missing=F, 
                 missing_pct_limit=50,
                 replace_zero=F, zero2what='NA', xmin=0.5,
                 replace_NA=F, NA2what='min', ymin=0.5,
                 pct_transform=T,
                 data_transform=F, trans_type='log',
                 centering=F,
                 scaling=F)
  }else if(input$ML_data_source == 'ML_user_data'){
    ML_Check_exp_data_table <- reactive({variables$ML.check.exp.data.user})
    if(data_check(exp_data=ML_Check_exp_data_table(),data_type="exp",page="ML",file_path=input$ML_user_exp$datapath,remove_na=input$ML_rm_NA,remove_na_pct=input$ML_rm_NA_pct)[[2]]){
      data_process(ML_exp_data(), 
                   exclude_var_missing=input$ML_rm_NA, 
                   missing_pct_limit=input$ML_rm_NA_pct,
                   replace_zero=T, zero2what='NA', xmin=0.5,
                   replace_NA=input$ML_rp_NA, NA2what=input$ML_fill_NA, ymin=input$ML_fill_min,
                   pct_transform=input$ML_pct_trans,
                   data_transform=input$ML_log_trans, trans_type='log',
                   centering=F,
                   scaling=input$ML_scaling)
    }else{
      NULL
    }
  }
  
})

#### ML_cond_tab() ####
ML_cond_tab <- reactive({
  if(input$ML_data_source == 'ML_demo_data'){
    variables$ML.cond.tab.demo
  }else if(input$ML_data_source == 'ML_user_data'){
    variables$ML.cond.tab.user
  }
})

#### ML_lipid_char_table() ####
ML_lipid_char_table <- reactive({
  if(input$ML_data_source == 'ML_demo_data'){
    variables$ML.lipid.char.tab.demo
  }else if(input$ML_data_source == 'ML_user_data'){
    if(!is.null(variables$ML.lipid.char.tab.user)){
      variables$ML.lipid.char.tab.user <- variables$ML.lipid.char.tab.user %>% 
        filter(feature %in% ML_exp_transform_data()$feature)
    }else{
      variables$ML.lipid.char.tab.user <- NULL
    }
  }
})

shinyjs::hide('ML_result_div')

##########################
####  ML Data Source  ####
##########################

#### Output: ML.demo.download ####
output$ML.demo.download <- downloadHandler(
  filename = function() {
    "Machine_learning_example_dataset.zip"
  },
  content = function(file) {
    file.copy("www/download_demo_dataset/ML.zip", file)
  },
  contentType = "application/zip"
) 

#### ML demo dataset ####
observeEvent(input$ML_demo_upload, {
  shinyjs::hide('ML_user_mainPanel_div')
  if(input$ML_demo_upload > 0){
    progressSweetAlert(
      session = session, id = "ML_demo_upload_progress",
      title = "Work in progress",
      display_pct = TRUE, value = 0
    )
    
    #### shinyjs show/hide main panel ####
    shinyjs::show('ML_demo_mainPanel_div')
    
    #### import demo dataset ####
    variables$ML.exp.data.demo <- readRDS('www/demo_dataset/ML/exp_data.rds')
    variables$ML.cond.tab.demo <- readRDS('www/demo_dataset/ML/condition_table.rds')
    variables$ML.lipid.char.tab.demo <- readRDS('www/demo_dataset/ML/lipid_char_table.rds')
    
    #### Output: ML.demo.exp ####
    output$ML.demo.exp.raw <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(variables$ML.exp.data.demo, 
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
      id = "ML_demo_upload_progress",
      value = 25
    )
    
    #### Output: ML.demo.exp ####
    output$ML.demo.exp <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(ML_exp_transform_data() %>% mutate_if(is.numeric, ~round(., 5)), 
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
      id = "ML_demo_upload_progress",
      value = 50
    )
    
    #### Output: ML.demo.cond ####
    output$ML.demo.cond <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(ML_cond_tab() %>% mutate_if(is.numeric, ~round(., 5)), 
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
      id = "ML_demo_upload_progress",
      value = 75
    )
    
    #### Output: ML.demo.lipid.char ####
    output$ML.demo.lipid.char <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(ML_lipid_char_table() %>% mutate_if(is.numeric, ~round(., 5)), 
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
      id = "ML_demo_upload_progress",
      value = 100
    )
    closeSweetAlert(session = session)
  }
  
}) #observeEvent(input$ML_demo_upload

#### ML user dataset ####
observeEvent(input$ML_user_upload, {
  shinyjs::hide('ML_demo_mainPanel_div')
  progressSweetAlert(
    session = session, id = "ML_user_upload_progress",
    title = "Work in progress",
    display_pct = TRUE, value = 0
  )
  
  #### shinyjs show/hide main panel ####
  shinyjs::show('ML_user_mainPanel_div')
  
  #isolate({
    
    showNotification("Start uploading file...", type = "message")
    tryCatch(
      {
        #### import demo dataset ####
        ## exp_data
        variables$ML.exp.data.user <- data.table::fread(input$ML_user_exp$datapath, header = T,
                                                        stringsAsFactors = F, check.names = F, 
                                                        data.table = F, na.strings = c('', 'NA'))
        ## condition_table
        variables$ML.cond.tab.user <- data.table::fread(input$ML_user_cond$datapath, header = T,
                                                        stringsAsFactors = F, check.names = F, 
                                                        data.table = F, na.strings = c('', 'NA'))
        ## lipid_char_table
        if(!is.null(input$ML_user_char)){
          variables$ML.lipid.char.tab.user <- data.table::fread(input$ML_user_char$datapath, header = T, 
                                                                stringsAsFactors = F, check.names = F, 
                                                                data.table = F, na.strings = c('', 'NA'))
        }else{
          variables$ML.lipid.char.tab.user <- NULL
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
    
    #### data check ####
    ## exp_data
    variables$ML.check.exp.data.user <- data.table::fread(input$ML_user_exp$datapath, header = T,
                                                          stringsAsFactors = F, check.names = F, 
                                                          data.table = F, na.strings = c('', 'NA'))
    ML_Check_exp_data_table <- reactive({variables$ML.check.exp.data.user})
    output$ML_Check_Exp_Data <- renderUI({
      isolate({
        data_check(exp_data=ML_Check_exp_data_table(),data_type="exp",
                   page="ML",file_path=input$ML_user_exp$datapath,
                   remove_na=input$ML_rm_NA,remove_na_pct=input$ML_rm_NA_pct)[[1]]
      })
    })
    updateProgressBar(
      session = session,
      id = "ML_user_upload_progress",
      value = 25
    )
    ## condition_table
    variables$ML.check.cond.tab.user <- data.table::fread(input$ML_user_cond$datapath, header = T,
                                                          stringsAsFactors = F, check.names = F, 
                                                          data.table = F, na.strings = c('', 'NA'))
    ML_Check_cond_tab <- reactive({variables$ML.check.cond.tab.user})
    output$ML_Check_condition <- renderUI({
      isolate({
        data_check(data=ML_Check_cond_tab(),exp_data=ML_Check_exp_data_table(),
                   file_path=input$ML_user_cond$datapath,data_type="ML_condition")[[1]]
      })
    })
    ## lipid_char_table
    if(!is.null(input$ML_user_char)){
      variables$ML.check.lipid.char.tab.user <- data.table::fread(input$ML_user_char$datapath, header = T, 
                                                                  stringsAsFactors = F, check.names = F, 
                                                                  data.table = F, na.strings = c('', 'NA'))
      ML_Check_lipid_char_data_table <- reactive({variables$ML.check.lipid.char.tab.user})
      output$ML_Check_lipid_char <- renderUI({
        isolate({
          data_check(data=ML_Check_lipid_char_data_table(),exp_data=ML_Check_exp_data_table(),
                     data_type="lipid_char",file_path=input$ML_user_char$datapath)[[1]]
        })
      })
    }else{
      variables$ML.check.lipid.char.tab.user <- NULL
      ML_Check_lipid_char_data_table <- reactive({variables$ML.check.lipid.char.tab.user})
      output$ML_Check_lipid_char <- renderUI({
        isolate({
          data_check(data=NULL,exp_data = NULL,data_type="optional_lipid_char")[[1]]
        })
      })
    }
    updateProgressBar(
      session = session,
      id = "ML_user_upload_progress",
      value = 50
    )
    #### rename column name ####
    ## exp_data
    variables$ML.exp.user.col1 <- colnames(variables$ML.exp.data.user)[1]
    colnames(variables$ML.exp.data.user)[1] <- 'feature'
    ## condition_table
    variables$ML.cond.user.col1 <- colnames(variables$ML.cond.tab.user)[1]
    colnames(variables$ML.cond.tab.user)[1] <- 'sample_name'
    ## lipid_char_table
    if(!is.null(variables$ML.lipid.char.tab.user)){
      variables$ML.lipid.char.user.col1 <- colnames(variables$ML.lipid.char.tab.user)[1]
      colnames(variables$ML.lipid.char.tab.user)[1] <- 'feature'
    }
    updateProgressBar(
      session = session,
      id = "ML_user_upload_progress",
      value = 70
    )
    # #### Output: ML.user.exp ####
    # output$ML.user.exp <- renderDataTable(server = FALSE,{
    #   DT::datatable(ML_exp_transform_data(), 
    #                 #caption = 'Lipid expression data',
    #                 colnames = c(variables$ML.exp.user.col1, colnames(variables$ML.exp.data.user)[-1]),
    #                 escape = FALSE, selection = 'none', rownames = FALSE, 
    #                 class = "nowrap row-border",
    #                 extensions = c('Buttons', 'Scroller'),
    #                 options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
    #                                deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
    #                                dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
    #                                columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    # })
    # #### Output: ML.user.cond ####
    # output$ML.user.cond <- renderDataTable(server = FALSE,{
    #   DT::datatable(ML_cond_tab(), 
    #                 #caption = 'Lipid expression data',
    #                 colnames = c(variables$ML.cond.user.col1, colnames(variables$ML.cond.tab.user)[-1]),
    #                 escape = FALSE, selection = 'none', rownames = FALSE, 
    #                 class = "nowrap row-border",
    #                 extensions = c('Buttons', 'Scroller'),
    #                 options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
    #                                deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
    #                                dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
    #                                columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    # })
    # #### Output: ML.user.lipid.char ####
    # output$ML.user.lipid.char <- renderDataTable(server = FALSE,{
    #   
    #   validate(need(!is.null(ML_lipid_char_table()), "Without lipid_char_table"))
    #   
    #   DT::datatable(ML_lipid_char_table(), 
    #                 #caption = 'Lipid expression data',
    #                 colnames = c(variables$ML.lipid.char.user.col1, colnames(variables$ML.lipid.char.tab.user)[-1]),
    #                 escape = FALSE, selection = 'none', rownames = FALSE, 
    #                 class = "nowrap row-border",
    #                 extensions = c('Buttons', 'Scroller'),
    #                 options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
    #                                deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
    #                                dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
    #                                columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    # })
    observe({
      if(is.null(ML_Check_lipid_char_data_table())){
        if(data_check(data=NULL,exp_data = NULL,data_type="optional_lipid_char")[[2]] & 
           data_check(exp_data=ML_Check_exp_data_table(),data_type="exp",page="ML",file_path=input$ML_user_exp$datapath,remove_na=input$ML_rm_NA,remove_na_pct=input$ML_rm_NA_pct)[[2]] & 
           data_check(data=ML_Check_cond_tab(),exp_data=ML_Check_exp_data_table(),file_path=input$ML_user_cond$datapath,data_type="ML_condition")[[2]]){
          shinyjs::show('ML_user_input_table_div')
          shinyjs::enable("ML_user_start")
          output$ML_Data_summary <-  renderUI({
            isolate({
              data_summary(exp_data = ML_Check_exp_data_table(),remove_na=input$ML_rm_NA,remove_na_pct=input$ML_rm_NA_pct,
                           fill_na = input$ML_rp_NA,fill_na_method = input$ML_fill_NA,
                           fill_na_Multiply = input$ML_fill_min,
                           PCT_tr = input$ML_pct_trans,log_tr = input$ML_log_trans,scaling=input$ML_scaling)
            })
          })
          
          #### Output: ML.user.exp.raw ####
          output$ML.user.exp.raw <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(variables$ML.exp.data.user, 
                            #caption = 'Lipid expression data',
                            #colnames = c(variables$ML.exp.user.col1, colnames(variables$ML.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          #### Output: ML.user.exp ####
          output$ML.user.exp <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(ML_exp_transform_data() %>% mutate_if(is.numeric, ~round(., 5)), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$ML.exp.user.col1, colnames(variables$ML.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          #### Output: ML.user.cond ####
          output$ML.user.cond <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(ML_cond_tab() %>% mutate_if(is.numeric, ~round(., 5)), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$ML.cond.user.col1, colnames(variables$ML.cond.tab.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          #### Output: ML.user.lipid.char ####
          output$ML.user.lipid.char <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(ML_lipid_char_table()), "Not uploaded"))
              
              DT::datatable(ML_lipid_char_table() %>% mutate_if(is.numeric, ~round(., 5)), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$ML.lipid.char.user.col1, colnames(variables$ML.lipid.char.tab.user)[-1]),
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
          shinyjs::hide('ML_user_input_table_div')
          shinyjs::disable("ML_user_start")
          output$ML_Data_summary <-  renderUI({
            NULL
          })
        }
      }else{
        if(data_check(data=ML_Check_lipid_char_data_table(),exp_data=ML_Check_exp_data_table(),data_type="lipid_char",file_path=input$ML_user_char$datapath)[[2]] & 
           data_check(exp_data=ML_Check_exp_data_table(),data_type="exp",page="ML",file_path=input$ML_user_exp$datapath,
                      remove_na=input$ML_rm_NA,remove_na_pct=input$ML_rm_NA_pct)[[2]] & 
           data_check(data=ML_Check_cond_tab(),exp_data=ML_Check_exp_data_table(),file_path=input$ML_user_cond$datapath,data_type="ML_condition")[[2]]){
          shinyjs::show('ML_user_input_table_div')
          shinyjs::enable("ML_user_start")
          output$ML_Data_summary <-  renderUI({
            isolate({
              data_summary(exp_data = ML_Check_exp_data_table(),remove_na=input$ML_rm_NA,remove_na_pct=input$ML_rm_NA_pct,
                           fill_na = input$ML_rp_NA,fill_na_method = input$ML_fill_NA,
                           fill_na_Multiply = input$ML_fill_min,
                           PCT_tr = input$ML_pct_trans,log_tr = input$ML_log_trans,scaling=input$ML_scaling)
            })
          })
          
          #### Output: ML.user.exp.raw ####
          output$ML.user.exp.raw <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(variables$ML.exp.data.user, 
                            #caption = 'Lipid expression data',
                            #colnames = c(variables$ML.exp.user.col1, colnames(variables$ML.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          #### Output: ML.user.exp ####
          output$ML.user.exp <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(ML_exp_transform_data() %>% mutate_if(is.numeric, ~round(., 5)), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$ML.exp.user.col1, colnames(variables$ML.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          #### Output: ML.user.cond ####
          output$ML.user.cond <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(ML_cond_tab() %>% mutate_if(is.numeric, ~round(., 5)), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$ML.cond.user.col1, colnames(variables$ML.cond.tab.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          #### Output: ML.user.lipid.char ####
          output$ML.user.lipid.char <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(ML_lipid_char_table()), "Not uploaded"))
              
              DT::datatable(ML_lipid_char_table() %>% mutate_if(is.numeric, ~round(., 5)), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$ML.lipid.char.user.col1, colnames(variables$ML.lipid.char.tab.user)[-1]),
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
          shinyjs::hide('ML_user_input_table_div')
          shinyjs::disable("ML_user_start")
          output$ML_Data_summary <-  renderUI({
            NULL
          })
        }
      }
      
    }) #observe
    
    updateProgressBar(
      session = session,
      id = "ML_user_upload_progress",
      value = 100
    )
    closeSweetAlert(session = session)
  #}) #isolate
  
}) #observeEvent(input$ML_user_upload

#### control user reset button ####
observeEvent(input$ML_user_reset, {
  
  #### shinyjs show/hide main panel ####
  shinyjs::hide('ML_user_mainPanel_div')
  
  #### shiny show/hide tab ####
  hideTab(inputId = 'ML_analysis_tab', target = 'ROC/PR curve')
  hideTab(inputId = 'ML_analysis_tab', target = 'Model predictivity')
  hideTab(inputId = 'ML_analysis_tab', target = 'Sample probability')
  hideTab(inputId = 'ML_analysis_tab', target = 'Feature importance')
  hideTab(inputId = 'ML_analysis_tab', target = 'Network')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('ML_user_reset_div')
  
  #### clear variables ####
  variables$ML.exp.data.user = NULL
  variables$ML.cond.tab.user = NULL
  variables$ML.lipid.char.tab.user = NULL
  
}) #observeEvent(input$ML_user_reset

#### control user upload button ####
observe({
  
  if(is.null(input$ML_user_exp) & is.null(input$ML_user_cond)){
    shinyjs::disable("ML_user_upload")
  }else{
    shinyjs::enable("ML_user_upload")
  }
  
}) #observe

#### Update selectInput ####
observe({
  demo_var <- colnames(variables$ML.lipid.char.tab.demo)[-1]
  updateSelectInput(session,
                    inputId = 'ML_demo_add_var',
                    choices = demo_var, 
                    selected = NULL)
  
  if(!is.null(variables$ML.lipid.char.tab.user)){
    shinyjs::enable('ML_user_add_var')
    user_var <- colnames(variables$ML.lipid.char.tab.user)[-1]
    updateSelectInput(session,
                      inputId = 'ML_user_add_var',
                      choices = user_var, 
                      selected = NULL)
  }else{
    shinyjs::disable('ML_user_add_var')
  }
  
})

#### control demo reset2 button ####
observeEvent(input$ML_demo_reset2, {
  
  #### shiny show/hide tab ####
  hideTab(inputId = 'ML_analysis_tab', target = 'ROC/PR curve')
  hideTab(inputId = 'ML_analysis_tab', target = 'Model predictivity')
  hideTab(inputId = 'ML_analysis_tab', target = 'Sample probability')
  hideTab(inputId = 'ML_analysis_tab', target = 'Feature importance')
  hideTab(inputId = 'ML_analysis_tab', target = 'Network')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('ML_demo_reset2_div')
  
}) #observeEvent(input$ML_demo_reset2

#### control user reset2 button ####
observeEvent(input$ML_user_reset2, {
  
  #### shiny show/hide tab ####
  hideTab(inputId = 'ML_analysis_tab', target = 'ROC/PR curve')
  hideTab(inputId = 'ML_analysis_tab', target = 'Model predictivity')
  hideTab(inputId = 'ML_analysis_tab', target = 'Sample probability')
  hideTab(inputId = 'ML_analysis_tab', target = 'Feature importance')
  hideTab(inputId = 'ML_analysis_tab', target = 'Network')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('ML_user_reset2_div')
  
}) #observeEvent(input$ML_user_reset2


###########################
####  ML analysis tab  ####
###########################

#### control hide/show tabpanel: input$ML_demo_start ####
observeEvent(input$ML_demo_start, {
  progressSweetAlert(
    session = session, id = "ML_demo_progress",
    title = "Work in progress",
    display_pct = TRUE, value = 0
  )
  #### shiny show/hide tab ####
  shinyjs::show('ML_result_div')
  showTab(inputId = 'ML_analysis_tab', target = 'ROC/PR curve')
  showTab(inputId = 'ML_analysis_tab', target = 'Model predictivity')
  showTab(inputId = 'ML_analysis_tab', target = 'Sample probability')
  showTab(inputId = 'ML_analysis_tab', target = 'Feature importance')
  showTab(inputId = 'ML_analysis_tab', target = 'Network')
  
  #### shinyjs show/hide results ####
  # shinyjs::hide('PRO_dim_redu_result_div')
  # shinyjs::hide('PRO.corr.heatmap.lipid')
  # shinyjs::hide('PRO.corr.heatmap.sample')
  
  isolate({
    
    TEST <- as.numeric(str_sub(input$ML_demo_split_for_test, start = 3, end = -1))
    
    #### Function: ML_data_process_1 ####
    variables$ML.add.var <- ML_data_process_1(ML_exp_data(), 
                                              ML_cond_tab(), 
                                              ML_lipid_char_table(), 
                                              input$ML_demo_add_var)
    updateProgressBar(
      session = session,
      id = "ML_demo_progress",
      title = "Work in data process1",
      value = 10
    )
    
    #### Function: ML_data_process_2 ####
    variables$ML.data.process <- ML_data_process_2(variables$ML.add.var, 
                                                   exclude_var_missing=F, 
                                                   missing_pct_limit=50,
                                                   replace_zero=F, zero2what='NA', xmin=0.5,
                                                   replace_NA=F, NA2what='min', ymin=0.5,
                                                   pct_transform=T,
                                                   data_transform=F, trans_type='log',
                                                   centering=F,
                                                   scaling=F)
    print(variables$ML.data.process)
    updateProgressBar(
      session = session,
      id = "ML_demo_progress",
      title = "Work in data process2",
      value = 20
    )
    
    if(input$ML_demo_feature_rank_method == "ElasticNet" | 
       input$ML_demo_classification_method == "ElasticNet"){
      
      variables$ML.results <- ML_final(variables$ML.data.process[[2]],
                                       input$ML_demo_feature_rank_method, 
                                       input$ML_demo_classification_method, 
                                       split_prop=1/TEST, 
                                       as.numeric(input$ML_demo_cross_vali_time), 
                                       input$ML_demo_alpha,
                                       session = session,
                                       id = "ML_demo_progress")
      updateProgressBar(
        session = session,
        id = "ML_demo_progress",
        title = "Data process done....",
        value = 90
      )
      
    }else{
      
      variables$ML.results <- ML_final(variables$ML.data.process[[2]],
                                       input$ML_demo_feature_rank_method, 
                                       input$ML_demo_classification_method, 
                                       split_prop=1/TEST, 
                                       as.numeric(input$ML_demo_cross_vali_time), 
                                       alpha = NULL,
                                       session = session,
                                       id = "ML_demo_progress")
      updateProgressBar(
        session = session,
        id = "ML_demo_progress",
        title = "Data process done....",
        value = 90
      )
      
    }
    
    updateProgressBar(
      session = session,
      id = "ML_demo_progress",
      value = 100
    )
    closeSweetAlert(session = session)
    
    
    
  }) #isolate
  
  
  
}) #observeEvent(input$ML_demo_start

#### control hide/show tabpanel: input$ML_user_start ####
observeEvent(input$ML_user_start, {
  
  progressSweetAlert(
    session = session, id = "ML_user_progress",
    title = "Work in progress",
    display_pct = TRUE, value = 0
  )
  
  #### shiny show/hide tab ####
  shinyjs::show('ML_result_div')
  showTab(inputId = 'ML_analysis_tab', target = 'ROC/PR curve')
  showTab(inputId = 'ML_analysis_tab', target = 'Model predictivity')
  showTab(inputId = 'ML_analysis_tab', target = 'Sample probability')
  showTab(inputId = 'ML_analysis_tab', target = 'Feature importance')
  showTab(inputId = 'ML_analysis_tab', target = 'Network')
  
  #### shinyjs show/hide results ####
  # shinyjs::hide('PRO_dim_redu_result_div')
  # shinyjs::hide('PRO.corr.heatmap.lipid')
  # shinyjs::hide('PRO.corr.heatmap.sample')
  
  isolate({
    
    TEST <- as.numeric(str_sub(input$ML_user_split_for_test, start = 3, end = -1))
    
    #### Function: ML_data_process_1 ####
    variables$ML.add.var <- ML_data_process_1(ML_exp_data(), 
                                              ML_cond_tab(), 
                                              ML_lipid_char_table(), 
                                              input$ML_user_add_var)
    updateProgressBar(
      session = session,
      title = "Work in data process1",
      id = "ML_user_progress",
      value = 10
    )
    
    #### Function: ML_data_process_2 ####
    variables$ML.data.process <- ML_data_process_2(variables$ML.add.var, 
                                                   exclude_var_missing=input$ML_rm_NA, 
                                                   missing_pct_limit=input$ML_rm_NA_pct,
                                                   replace_zero=T, zero2what='NA', xmin=0.5,
                                                   replace_NA=input$ML_rp_NA, NA2what=input$ML_fill_NA, ymin=input$ML_fill_min,
                                                   pct_transform=input$ML_pct_trans,
                                                   data_transform=input$ML_log_trans, trans_type='log',
                                                   centering=F,
                                                   scaling=input$ML_scaling)
    updateProgressBar(
      session = session,
      id = "ML_user_progress",
      title = "Work in data process2",
      value = 20
    )
    
    if(input$ML_user_feature_rank_method == "ElasticNet" | 
       input$ML_user_classification_method == "ElasticNet"){
      
      variables$ML.results <- ML_final(variables$ML.data.process[[2]],
                                       input$ML_user_feature_rank_method, 
                                       input$ML_user_classification_method, 
                                       split_prop=1/TEST, 
                                       as.numeric(input$ML_user_cross_vali_time), 
                                       input$ML_user_alpha,
                                       session = session,
                                       id = "ML_user_progress")
      updateProgressBar(
        session = session,
        id = "ML_user_progress",
        title = "Data process done....",
        value = 90
      )
      
    }else{
      
      variables$ML.results <- ML_final(variables$ML.data.process[[2]],
                                       input$ML_user_feature_rank_method, 
                                       input$ML_user_classification_method, 
                                       split_prop=1/TEST, 
                                       as.numeric(input$ML_user_cross_vali_time), 
                                       alpha = NULL,
                                       session = session,
                                       id = "ML_user_progress")
      updateProgressBar(
        session = session,
        id = "ML_user_progress",
        title = "Data process done....",
        value = 90
      )
      
    }
    
    updateProgressBar(
      session = session,
      id = "ML_user_progress",
      value = 100
    )
    closeSweetAlert(session = session)
    
    
    
  }) #isolate
  
}) #observeEvent(input$ML_user_start

#### Update select input ####
num.fea <- reactive({
  nrow(variables$ML.data.process[[1]])
})
observe({
  if(!is.null(num.fea())){
    num.fea <- num.fea()
    fea.cond <- c(2, 3, 5, 10, 20, 50, 100)
    fea.cond2 <- c(2, 3, 5, 10, 20, 50, 100,num.fea)
    select.fea <- fea.cond[which(fea.cond < num.fea)]
    select.fea2 <- sort(unique(fea.cond2[which(fea.cond2 <= 50)]))
    if(num.fea < 10){
      updateSelectInput(session,
                        inputId = 'ML_ROC_PR_feature_number',
                        choices = c(select.fea, num.fea),
                        selected = num.fea)
      updateSelectInput(session,
                        inputId = 'ML_sam_prob_feature_number',
                        choices = c(select.fea, num.fea),
                        selected = num.fea)
      updateSelectInput(session, 
                        inputId = 'ML_SHAP_feature_number', 
                        choices = c(select.fea2), 
                        selected = num.fea)
      updateSelectInput(session,
                        inputId = 'ML_algorithm_feature_number',
                        choices = c(select.fea, num.fea), 
                        selected = num.fea)
      updateSelectInput(session,
                        inputId = 'ML_network_feature_number', 
                        choices = c(select.fea, num.fea),
                        selected = num.fea)
    }else{
      updateSelectInput(session, 
                        inputId = 'ML_ROC_PR_feature_number', 
                        choices = c(select.fea, num.fea), 
                        selected = 10)
      updateSelectInput(session,
                        inputId = 'ML_sam_prob_feature_number',
                        choices = c(select.fea, num.fea),
                        selected = 10)
      updateSelectInput(session, 
                        inputId = 'ML_SHAP_feature_number', 
                        choices = c(select.fea2), 
                        selected = 10)
      updateSelectInput(session,
                        inputId = 'ML_network_feature_number', 
                        choices = c(select.fea, num.fea),
                        selected = 10)
      updateSelectInput(session,
                        inputId = 'ML_algorithm_feature_number',
                        choices = c(select.fea, num.fea), 
                        selected = 10)
    }
  }
})

################################
################################
#####  Tab1: ROC/PR curve  #####
################################
################################

#### Function: ROC_plot_all ####
ROC_plot_all_result <- reactive({
  
  ROC_plot_all(variables$ML.results[[3]], variables$ML.results[[5]])
  
})

#### Function: PR_plot_all ####
PR_plot_all_result <- reactive({
  
  PR_plot_all(variables$ML.results[[4]], variables$ML.results[[5]])
  
})

#### Function: ROC_plot ####
ROC_plot_result <- reactive({
  
  tryCatch({
    ROC_plot(variables$ML.results[[3]], variables$ML.results[[5]],
             feature_n = input$ML_ROC_PR_feature_number)
  },
  error = function(e) {
    NULL
  },
  warning = function(w) {
    NULL
  }
  )

})

#### Function: PR_plot ####
PR_plot_result <- reactive({
  tryCatch({
    PR_plot(variables$ML.results[[4]], variables$ML.results[[5]],
            feature_n = input$ML_ROC_PR_feature_number)
  },
  error = function(e) {
   NULL
  },
  warning = function(w) {
    NULL
  }
  )
  
})

#### Output: ML.ROC.all ####
output$ML.ROC.all <- renderPlotly({
  
  validate(need(!is.null(ROC_plot_all_result()[[2]]), "Without ROC plot"))
  ROC_plot_all_result()[[2]]
  
})

#### Output: ML.ROC.all ####
output$ML.PR.all <- renderPlotly({
  
  validate(need(!is.null(PR_plot_all_result()[[2]]), "Without PR plot"))
  PR_plot_all_result()[[2]]
  
})

#### Output: ML.ROC ####
output$ML.ROC <- renderPlotly({
  
  validate(need(!is.null(ROC_plot_result()[[2]]), "Feature number less than selected"))
  ROC_plot_result()[[2]]
  
})

#### Output: ML.ROC ####
output$ML.PR <- renderPlotly({
  
  validate(need(!is.null(PR_plot_result()[[2]]), "Feature number less than selected"))
  PR_plot_result()[[2]]
  
})

#### Update select input ####
# observe({
#   #num.fea <- nrow(variables$ML.data.process[[1]])
#   fea.cond <- c(2, 3, 5, 10, 20, 50, 100)
#   select.fea <- fea.cond[which(fea.cond < variables$num.fea)]
#   # if(num.fea < 10){
#   #   updateSelectInput(session,
#   #                     inputId = 'ML_ROC_PR_feature_number',
#   #                     choices = c(select.fea, num.fea),
#   #                     selected = num.fea)
#   # }else{
#     updateSelectInput(session, 
#                       inputId = 'ML_ROC_PR_feature_number', 
#                       choices = c(select.fea, variables$num.fea), 
#                       selected = 10)
#   #}
#   
# })



######################################
######################################
#####  Tab2: Model predictivity  #####
######################################
######################################

#### Function: evalution_plot ####
evalution_plot_result <- reactive({
  
  evalution_plot(variables$ML.results[[2]], 
                 input$ML_evaluation_method)
  
})

#### Output: ML.evalution.plot ####
output$ML.evalution.plot <- renderPlotly({
  
  validate(need(!is.null(evalution_plot_result()[[2]]), "Without evalution plot"))
  evalution_plot_result()[[2]]

})

#### Output: ML.evalution.table ####
output$ML.evalution.table <- renderDataTable(server = FALSE,{
  
  validate(need(!is.null(evalution_plot_result()[[1]]), "Without evalution table"))
  DT::datatable(evalution_plot_result()[[1]] %>% mutate_if(is.numeric, ~round(., 5)), 
                #caption = 'Lipid expression data',
                colnames = c('Selection method', 'Classifier', 'CV run', 'Feature number', 'Evaluation index', 'Value', 'Mean value over all CVs'),
                escape = FALSE, selection = 'none', rownames = TRUE, 
                class = "nowrap row-border",
                extensions = c('Buttons', 'Scroller'),
                options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                               deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                               dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
})



######################################
######################################
#####  Tab3: Sample probability  #####
######################################
######################################

#### Function: evalution_plot ####
probability_plot_result <- reactive({
  tryCatch({
    probability_plot(variables$ML.results[[1]], 
                     as.numeric(input$ML_sam_prob_feature_number))
  },
  error = function(e) {
    NULL
  },
  warning = function(w) {
    NULL
  }
  )
  
})

#### Output: ML.probability.plot ####
output$ML.probability.plot <- renderPlotly({
  
  validate(need(!is.null(probability_plot_result()[[2]]), "Feature number less than selected"))
  probability_plot_result()[[2]]
  
})

#### Output: ML.cm.plot ####
output$ML.cm.plot <- renderPlotly({
  
  validate(need(!is.null(probability_plot_result()[[3]]), "Feature number less than selected"))
  probability_plot_result()[[3]]
  
})

#### Output: ML.probability.table ####
output$ML.probability.table <- renderDataTable(server = FALSE,{
  
  validate(need(!is.null(probability_plot_result()[[1]]), "Without probability table"))
  DT::datatable(probability_plot_result()[[1]] %>% mutate_if(is.numeric, ~round(., 5)), 
                #caption = 'Lipid expression data',
                colnames = c('Feature selection', 'Classifier', 'Feature number', 'Sample ID', 'Actual group', 'Predicted probability', 'Predicted group'),
                escape = FALSE, selection = 'none', rownames = TRUE, 
                class = "nowrap row-border",
                extensions = c('Buttons', 'Scroller'),
                options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                               deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                               dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  
})

#### Update select input ####
# observe({
#   #num.fea <- nrow(variables$ML.data.process[[1]])
#   fea.cond <- c(2, 3, 5, 10, 20, 50, 100)
#   select.fea <- fea.cond[which(fea.cond < variables$num.fea)]
#   # if(num.fea < 10){
#   #   updateSelectInput(session,
#   #                     inputId = 'ML_sam_prob_feature_number',
#   #                     choices = c(select.fea, num.fea),
#   #                     selected = num.fea)
#   # }else{
#   updateSelectInput(session, 
#                     inputId = 'ML_sam_prob_feature_number', 
#                     choices = c(select.fea, variables$num.fea), 
#                     selected = 10)
#   #}
#   
# })



######################################
######################################
#####  Tab4: Feature importance  #####
######################################
######################################

##################################
####  List1: Algorithm-based  ####
##################################

#### Function: feature_plot ####
feature_plot_result <- reactive({
  tryCatch({
    if(input$ML_data_source == 'ML_demo_data'){
      feature_plot(variables$ML.results[[6]], variables$ML.results[[7]],
                   as.numeric(input$ML_algorithm_feature_number), 
                   as.numeric(input$ML_demo_cross_vali_time))
    }else if(input$ML_data_source == 'ML_user_data'){
      feature_plot(variables$ML.results[[6]], variables$ML.results[[7]],
                   as.numeric(input$ML_algorithm_feature_number), 
                   as.numeric(input$ML_user_cross_vali_time))
    }
  },
  error = function(e) {
    NULL
  },
  warning = function(w) {
    NULL
  }
  )
  
})

#### Output: ML.feature.freq.plot ####
output$ML.fea.freq.plot <- renderPlotly({
  
  validate(need(!is.null(feature_plot_result()[[2]]), "Feature number less than selected"))
  feature_plot_result()[[2]]
  
})

#### Output: ML.fea.impo.plot ####
output$ML.fea.impo.plot <- renderPlotly({
  
  validate(need(!is.null(feature_plot_result()[[4]]), "Feature number less than selected"))
  feature_plot_result()[[4]]
  
})

#### Output: ML.fea.freq.table ####
output$ML.fea.freq.table <- renderDataTable(server = FALSE,{
  
  validate(need(!is.null(feature_plot_result()[[1]]), "Without feature frequency table"))
  DT::datatable(feature_plot_result()[[1]] %>% mutate_if(is.numeric, ~round(., 5)), 
                #caption = 'Lipid expression data',
                colnames = c('Feature selection', 'Classifier', 'Feature number', 'Feature', 'Selected frequency'),
                escape = FALSE, selection = 'none', rownames = TRUE, 
                class = "nowrap row-border",
                extensions = c('Buttons', 'Scroller'),
                options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                               deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                               dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  
})

#### Output: ML.fea.impo.table ####
output$ML.fea.impo.table <- renderDataTable(server = FALSE,{
  
  validate(need(!is.null(feature_plot_result()[[3]]), "Without feature importance table"))
  DT::datatable(feature_plot_result()[[3]] %>% mutate_if(is.numeric, ~round(., 5)), 
                #caption = 'Lipid expression data',
                colnames = c('Feature selection', 'Classifier', 'Feature number', 'Feature', 'Average importance'),
                escape = FALSE, selection = 'none', rownames = TRUE, 
                class = "nowrap row-border",
                extensions = c('Buttons', 'Scroller'),
                options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                               deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                               dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  
})

#### Update select input ####
# observe({
#   #num.fea <- nrow(variables$ML.data.process[[1]])
#   fea.cond <- c(2, 3, 5, 10, 20, 50, 100)
#   select.fea <- fea.cond[which(fea.cond < variables$num.fea)]
#   # if(num.fea < 10){
#   #   updateSelectInput(session,
#   #                     inputId = 'ML_sam_prob_feature_number',
#   #                     choices = c(select.fea, num.fea),
#   #                     selected = num.fea)
#   # }else{
#   updateSelectInput(session, 
#                     inputId = 'ML_algorithm_feature_number', 
#                     choices = c(select.fea, variables$num.fea), 
#                     selected = 10)
#   #}
#   
# })


################################
####  List2: SHAP analysis  ####
################################

#### control hide/show tabpanel: input$ML_SHAP_start ####
observeEvent(input$ML_SHAP_start, {
  
  #### shinyjs show/hide results ####
  shinyjs::show('ML.mean.shapley.plot')
  shinyjs::show('ML.all.shapley.plot')
  shinyjs::show('ML.shap.long.table')
  shinyjs::show('ML_SHAP_forceplot_control_panel_div')
  shinyjs::show('ML_SHAP_dependence_control_panel_div')
  shinyjs::hide('ML.SHAP.forceplot')
  shinyjs::hide('ML.SHAP.forceplot.table')
  shinyjs::hide('ML.SHAP.dependence.plot')
  
  isolate({
    
    
    #### Function: SHAP ####
    if(input$ML_data_source == 'ML_demo_data'){
     
      
      variables$ML.SHAP.result <- SHAP(variables$ML.data.process[[2]], variables$ML.results[[8]], variables$ML.results[[9]], 
                                       input$ML_demo_classification_method,
                                       as.numeric(input$ML_SHAP_feature_number), 
                                       input$ML_SHAP_n_sim)
      
    }else if(input$ML_data_source == 'ML_user_data'){
      if(nrow(ML_exp_transform_data())>as.numeric(input$ML_SHAP_feature_number)){
        variables$ML.SHAP.result <- SHAP(variables$ML.data.process[[2]], variables$ML.results[[8]], variables$ML.results[[9]], 
                                         input$ML_user_classification_method,
                                         as.numeric(input$ML_SHAP_feature_number), 
                                         input$ML_SHAP_n_sim)
      }else{
        variables$ML.SHAP.result <- NULL
        shinyjs::hide('ML_SHAP_forceplot_control_panel_div')
        shinyjs::hide('ML_SHAP_dependence_control_panel_div')
      }
      
    }
    
    
    # SHAP_result <- reactive({
    #   
    #   if(input$ML_data_source == 'ML_demo_data'){
    #     SHAP(variables$ML.data.process[[2]], variables$ML.results[[8]], variables$ML.results[[9]], 
    #          input$ML_demo_classification_method,
    #          as.numeric(input$ML_SHAP_feature_number), 
    #          input$ML_SHAP_n_sim)
    #   }else if(input$ML_data_source == 'ML_user_data'){
    #     SHAP(variables$ML.data.process[[2]], variables$ML.results[[8]], variables$ML.results[[9]], 
    #          input$ML_user_classification_method,
    #          as.numeric(input$ML_SHAP_feature_number), 
    #          input$ML_SHAP_n_sim)
    #   }
    #   
    # })
    
    #### Output: ML.mean.shapley.plot ####
    output$ML.mean.shapley.plot <- renderPlotly({
      
      validate(need(!is.null(variables$ML.SHAP.result[[3]]), "Feature number less than selected"))
      variables$ML.SHAP.result[[3]]
      
    })
    
    #### Output: ML.all.shapley.plot ####
    output$ML.all.shapley.plot <- renderPlotly({
      
      validate(need(!is.null(variables$ML.SHAP.result[[4]]), "Feature number less than selected"))
      variables$ML.SHAP.result[[4]]
      
    })
    
    #### Output: ML.shap.long.table ####
    output$ML.shap.long.table <- renderDataTable(server = FALSE,{
      
      validate(need(!is.null(variables$ML.SHAP.result[[2]]), "Without shap long table"))
      DT::datatable(variables$ML.SHAP.result[[2]] %>% mutate_if(is.numeric, ~round(., 5)), 
                    #caption = 'Lipid expression data',
                    colnames = c('Sample ID', 'Feature', 'Shapley value', 'Feature value', 'Normalized feature value', 'Mean shapley value'),
                    escape = FALSE, selection = 'none', rownames = TRUE, 
                    class = "nowrap row-border",
                    extensions = c('Buttons', 'Scroller'),
                    options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                   deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                   dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                   columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      
    })
    
    
  }) #isolate
  
  
  
  #### control hide/show tabpanel: input$ML_SHAP_forceplot_start ####
  observeEvent(input$ML_SHAP_forceplot_start, {
    
    #### shinyjs show/hide results ####
    shinyjs::show('ML.SHAP.forceplot')
    shinyjs::show('ML.SHAP.forceplot.table')
    #shinyjs::hide('ML.SHAP.dependence.plot')
    
    isolate({
      
      #### Function: SHAP_forceplot ####
      SHAP_forceplot_result <- SHAP_forceplot(variables$ML.SHAP.result[[1]], 
                                              as.numeric(input$ML_SHAP_forceplot_top_n_feature), 
                                              cluster_method = "ward.D", 
                                              as.numeric(input$ML_SHAP_forceplot_group_number), 
                                              zoom_in_loc=NULL, zoom_in_group=NULL)
      
      #### Output: ML.SHAP.forceplot ####
      output$ML.SHAP.forceplot <- renderPlotly({
        
        validate(need(!is.null(SHAP_forceplot_result[[2]]), "Without SHAP forceplot"))
        SHAP_forceplot_result[[2]]
        
      })
      
      #### Output: ML.SHAP.forceplot.table ####
      output$ML.SHAP.forceplot.table <- renderDataTable(server = FALSE,{
        
        validate(need(!is.null(SHAP_forceplot_result[[1]]), "Without SHAP forceplot table"))
        DT::datatable(SHAP_forceplot_result[[1]] %>% mutate_if(is.numeric, ~round(., 5)), 
                      #caption = 'Lipid expression data',
                      #colnames = c('feature', ML_group_info()$label_name),
                      escape = FALSE, selection = 'none', rownames = TRUE, 
                      class = "nowrap row-border",
                      extensions = c('Buttons', 'Scroller'),
                      options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                     deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                     dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                     columnDefs = list(list(className = 'dt-center', targets = "_all"))))
        
      })
      
    }) #isolate
    
  }) #observeEvent(input$ML_SHAP_forceplot_start
  
  
  
  #### control hide/show tabpanel: input$ML_SHAP_dependence_start ####
  observeEvent(input$ML_SHAP_dependence_start, {
    
    #### shinyjs show/hide results ####
    #shinyjs::hide('ML.SHAP.forceplot')
    #shinyjs::hide('ML.SHAP.forceplot.table')
    shinyjs::show('ML.SHAP.dependence.plot')
    
    isolate({
      
      #### Function: SHAP_dependence_plot ####
      SHAP_dependence_plot_result <- SHAP_dependence_plot(variables$ML.SHAP.result[[2]],
                                                          input$ML_SHAP_dependence_x,
                                                          input$ML_SHAP_dependence_y,
                                                          input$ML_SHAP_dependence_color)
      
      #### Output: ML.SHAP.dependence.plot ####
      output$ML.SHAP.dependence.plot <- renderPlotly({
        
        validate(need(!is.null(SHAP_dependence_plot_result), "Without SHAP dependence plot"))
        SHAP_dependence_plot_result
        
      })
      
    }) #isolate
    
  }) #observeEvent(input$ML_SHAP_dependence_start
  
  #### Update select input ####
  observe({
    Variables <- variables$ML.SHAP.result[[2]]$variable
    VALUE <- max(variables$ML.SHAP.result[[2]]$mean_shapley_value)
    MAX <- variables$ML.SHAP.result[[2]]$variable[which(variables$ML.SHAP.result[[2]]$mean_shapley_value == VALUE)]
    updateSelectInput(session, 
                      inputId = 'ML_SHAP_dependence_x', 
                      choices = Variables, 
                      selected = MAX[1])
    updateSelectInput(session, 
                      inputId = 'ML_SHAP_dependence_y', 
                      choices = Variables, 
                      selected = MAX[1])
    updateSelectInput(session, 
                      inputId = 'ML_SHAP_dependence_color', 
                      choices = Variables, 
                      selected = MAX[1])
  })
  
  observe({
    VALUE <- ifelse(input$ML_SHAP_feature_number < 10, input$ML_SHAP_feature_number, 10)
    SELECT <- ifelse(input$ML_SHAP_feature_number < 5, input$ML_SHAP_feature_number, 5)
    updateSelectInput(session,
                      inputId = 'ML_SHAP_forceplot_top_n_feature', 
                      choices = 1:VALUE, 
                      selected = SELECT)
  })
  
  #### control user reset button ####
  observeEvent(input$ML_SHAP_forceplot_reset, {
    
    #### shinyjs show/hide main panel ####
    shinyjs::hide('ML.SHAP.forceplot')
    shinyjs::hide('ML.SHAP.forceplot.table')
    #shinyjs::hide('ML.SHAP.dependence.plot')
    
    #### shinyjs reset control panel ####
    shinyjs::reset('ML_SHAP_forceplot_reset_div')
    
  }) #observeEvent(input$ML_SHAP_forceplot_reset
  
  #### control user reset button ####
  observeEvent(input$ML_SHAP_dependence_reset, {
    
    #### shinyjs show/hide main panel ####
    #shinyjs::hide('ML.SHAP.forceplot')
    #shinyjs::hide('ML.SHAP.forceplot.table')
    shinyjs::hide('ML.SHAP.dependence.plot')
    
    #### shinyjs reset control panel ####
    shinyjs::reset('ML_SHAP_dependence_reset_div')
    
  }) #observeEvent(input$ML_SHAP_dependence_reset
  
}) #observeEvent(input$ML_SHAP_start

#### Update select input ####
# observe({
#   # #num.fea <- nrow(ML_exp_data())
#   # fea.cond <- c(2, 3, 5, 10, 20, 50, 100)
#   # select.fea <- fea.cond[which(fea.cond < nrow(variables$ML.data.process[[1]]))]
#   # VALUE <- ifelse(nrow(variables$ML.data.process[[1]]) < 10, nrow(variables$ML.data.process[[1]]), 10)
#   # # if(num.fea < 10){
#   # #   updateSelectInput(session,
#   # #                     inputId = 'ML_sam_prob_feature_number',
#   # #                     choices = c(select.fea, num.fea),
#   # #                     selected = num.fea)
#   # # }else{
#   # updateSelectInput(session, 
#   #                   inputId = 'ML_SHAP_feature_number', 
#   #                   choices = c(select.fea, nrow(variables$ML.data.process[[1]])), 
#   #                   selected = VALUE)
#   # #}
#   
#   fea.cond <- c(2, 3, 5, 10, 20, 50, 100)
#   select.fea <- fea.cond[which(fea.cond < variables$num.fea)]
#   # if(num.fea < 10){
#   #   updateSelectInput(session,
#   #                     inputId = 'ML_sam_prob_feature_number',
#   #                     choices = c(select.fea, num.fea),
#   #                     selected = num.fea)
#   # }else{
#   updateSelectInput(session, 
#                     inputId = 'ML_SHAP_feature_number', 
#                     choices = c(select.fea, variables$num.fea), 
#                     selected = 10)
#   
#   
# })

#### control user reset button ####
observeEvent(input$ML_SHAP_reset, {
  
  #### shinyjs show/hide main panel ####
  shinyjs::hide('ML.mean.shapley.plot')
  shinyjs::hide('ML.all.shapley.plot')
  shinyjs::hide('ML.shap.long.table')
  shinyjs::hide('ML_SHAP_forceplot_control_panel_div')
  shinyjs::hide('ML_SHAP_dependence_control_panel_div')
  shinyjs::hide('ML.SHAP.forceplot')
  shinyjs::hide('ML.SHAP.forceplot.table')
  shinyjs::hide('ML.SHAP.dependence.plot')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('ML_SHAP_reset_div')
  
}) #observeEvent(input$ML_SHAP_reset

###########################
###########################
#####  Tab5: Network  #####
###########################
###########################

#### control start button ####
observeEvent(input$ML_network_start, {
  
  shinyjs::show('ML_network_result_div')
  
  isolate({
    #print(input$ML_user_classification_method)
    #### Function: model_for_net ####
    model_for_net_result <- reactive({
      tryCatch({
        if(input$ML_data_source == 'ML_demo_data'){
          if(input$ML_network_fea_impo_method == 'Algorithm-based'){
            model_for_net(variables$ML.data.process[[2]], 
                          input$ML_demo_classification_method, 
                          input$ML_network_fea_impo_method, 
                          variables$ML.results[[8]], 
                          variables$ML.results[[9]],
                          as.numeric(input$ML_network_feature_number), 
                          nsim = NULL)
          }else if(input$ML_network_fea_impo_method == 'SHAP analysis'){
            model_for_net(variables$ML.data.process[[2]], 
                          input$ML_demo_classification_method, 
                          input$ML_network_fea_impo_method, 
                          variables$ML.results[[8]], 
                          variables$ML.results[[9]],
                          as.numeric(input$ML_network_feature_number), 
                          input$ML_network_n_sim)
          }
        }else if(input$ML_data_source == 'ML_user_data'){
          if(input$ML_network_fea_impo_method == 'Algorithm-based'){
            model_for_net(variables$ML.data.process[[2]], 
                          input$ML_user_classification_method, 
                          input$ML_network_fea_impo_method, 
                          variables$ML.results[[8]], 
                          variables$ML.results[[9]],
                          as.numeric(input$ML_network_feature_number), 
                          nsim = NULL)
          }else if(input$ML_network_fea_impo_method == 'SHAP analysis'){
            model_for_net(variables$ML.data.process[[2]], 
                          input$ML_user_classification_method, 
                          input$ML_network_fea_impo_method, 
                          variables$ML.results[[8]], 
                          variables$ML.results[[9]],
                          as.numeric(input$ML_network_feature_number), 
                          input$ML_network_n_sim)
          }
        }
      },
      error = function(e) {
        NULL
      },
      warning = function(w) {
        NULL
      }
      )
      
    })
    
    #print(input$ML_demo_classification_method)
    #print(variables$ML.data.process[[1]])
    #print(model_for_net_result())
    # #### Function: cor_network ####
    # cor_network_result <- reactive({
    #   
    #   cor_network(variables$ML.data.process[[1]], 
    #               ML_lipid_char_table(),
    #               model_for_net_result()$feature, 
    #               model_for_net_result()$importance,
    #               input$ML_network_corr_method, 
    #               input$ML_network_corr_coef)
    #   
    # })
    
    #### Function: cor_network ####
    cor_network_result <- reactive({
      if(!is.null(model_for_net_result())){
        #colnames(variables$ML.data.process[[1]])[1] <- 'feature'
        cor_network(variables$ML.data.process[[1]], 
                    ML_lipid_char_table(),
                    model_for_net_result()$feature, 
                    model_for_net_result()$importance,
                    input$ML_network_corr_method, 
                    input$ML_network_corr_coef)
      }else{
        NULL
      }
      
    })
    
    #### Function: corr_network ####
    variables$ML.visnetwork <- if(!is.null(cor_network_result())){
      corr_network(cor_network_result()[[1]], cor_network_result()[[2]])
    }else{
      NULL
    }
    
    #### Output: ML.network ####
    output$ML.network <- renderVisNetwork({
      validate(need(!is.null(variables$ML.visnetwork), "Feature number less than selected"))
      variables$ML.visnetwork
      
    }) #output$ML.network <- renderVisNetwork
    
    
    observeEvent(input$ML_network_refresh, {
      
      output$ML.network <- renderVisNetwork({
        
        variables$ML.visnetwork
        
      }) #output$ML.network <- renderVisNetwork
      
    })
    
  }) #isolate
  
}) #observeEvent(input$ML_network_start

#### control user reset button ####
observeEvent(input$ML_network_reset, {
  
  #### shinyjs show/hide main panel ####
  shinyjs::hide('ML_network_result_div')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('ML_network_reset_div')
  
}) #observeEvent(input$ML_network_reset


#### Update select input ####
# observe({
#   #num.fea <- nrow(variables$ML.data.process[[1]])
#   #if(num.fea > 100) num.fea <- 100
#   fea.cond <- c(2, 3, 5, 10, 20, 50, 100)
#   select.fea <- fea.cond[which(fea.cond < variables$num.fea)]
#   # if(num.fea < 10){
#   #   updateSelectInput(session,
#   #                     inputId = 'ML_sam_prob_feature_number',
#   #                     choices = c(select.fea, num.fea),
#   #                     selected = num.fea)
#   # }else{
#   updateSelectInput(session, 
#                     inputId = 'ML_network_feature_number', 
#                     choices = c(select.fea, variables$num.fea), 
#                     selected = 10)
#   #}
#   
# })







