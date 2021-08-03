

##################################
##################################
######                      ######
######   Correlation Page   ######
######                      ######
##################################
##################################

observeEvent(input$CORR_link_to_FAQ4, {
  updateTabsetPanel(session, 'narbarpage', 'FAQ')
  updateNavlistPanel(session, "navlistPanel_FAQ2", selected = 'FAQ4')
})

############################
####  Assign variables  ####
############################

#### CORR_exp_data() ####
CORR_exp_data <- reactive({
  if(input$CORR_data_source == 'CORR_demo_data'){
    variables$CORR.exp.data.demo.cont
  }else if(input$CORR_data_source == 'CORR_user_data'){
    variables$CORR.exp.data.user
  }
})

#### CORR_exp_transform_data() ####
CORR_exp_transform_data <- reactive({
  if(input$CORR_data_source == 'CORR_demo_data'){
    data_process(CORR_exp_data(), 
                 exclude_var_missing=F, 
                 missing_pct_limit=50,
                 replace_zero=F, zero2what='NA', xmin=0.5,
                 replace_NA=F, NA2what='min', ymin=0.5,
                 pct_transform=F,
                 data_transform=F, trans_type='log',
                 centering=F,
                 scaling=F)
  }else if(input$CORR_data_source == 'CORR_user_data'){
    CORR_Check_exp_data_table <- reactive({variables$CORR.check.exp.data.user})
    if(data_check(exp_data=CORR_Check_exp_data_table(),data_type="exp",page="Correlation",file_path=input$CORR_user_exp$datapath,
                  remove_na=input$CORR_rm_NA,remove_na_pct=input$CORR_rm_NA_pct)[[2]]){
      data_process(CORR_exp_data(), 
                   exclude_var_missing=input$CORR_rm_NA, 
                   missing_pct_limit=input$CORR_rm_NA_pct,
                   replace_zero=T, zero2what='NA', xmin=0.5,
                   replace_NA=input$CORR_rp_NA, NA2what=input$CORR_fill_NA, ymin=input$CORR_fill_min,
                   pct_transform=input$CORR_pct_trans,
                   data_transform=input$CORR_log_trans, trans_type='log',
                   centering=F,
                   scaling=F)
    }else{
      NULL
    }
  }
})

#### CORR_cond_tab() ####
CORR_cond_tab <- reactive({
  if(input$CORR_data_source == 'CORR_demo_data'){
    variables$CORR.cond.tab.demo.cont
  }else if(input$CORR_data_source == 'CORR_user_data'){
    variables$CORR.cond.tab.user
  }
})

#### CORR_adj_tab() ####
CORR_adj_tab <- reactive({
  if(input$CORR_data_source == 'CORR_demo_data'){
    variables$CORR.adj.tab.demo.cont
  }else if(input$CORR_data_source == 'CORR_user_data'){
    variables$CORR.adj.tab.user
  }
})

#### CORR_lipid_char() ####
CORR_lipid_char <- reactive({
  if(input$CORR_data_source == 'CORR_demo_data'){
    variables$CORR.lipid.char.demo.cont
  }else if(input$CORR_data_source == 'CORR_user_data'){
    if(!is.null(variables$CORR.lipid.char.user)){
      variables$CORR.lipid.char.user <- variables$CORR.lipid.char.user %>% 
        filter(feature %in% CORR_exp_transform_data()$feature)
    }else{
      variables$CORR.lipid.char.user <- NULL
    }
  }
})


#### CORR_class_exp_data() ####
CORR_class_exp_data <- reactive({
  
  Species2Char(CORR_exp_data(), CORR_lipid_char(), input$CORR_class_corr_lipid_char)

})

#### CORR_class_exp_transform_data() ####
CORR_class_exp_transform_data <- reactive({
  if(input$CORR_data_source == 'CORR_demo_data'){
    data_process(CORR_class_exp_data(), 
                 exclude_var_missing=F, 
                 missing_pct_limit=50,
                 replace_zero=F, zero2what='NA', xmin=0.5,
                 replace_NA=F, NA2what='min', ymin=0.5,
                 pct_transform=F,
                 data_transform=F, trans_type='log',
                 centering=F,
                 scaling=F)
  }else if(input$CORR_data_source == 'CORR_user_data'){
    CORR_Check_exp_data_table <- reactive({variables$CORR.check.exp.data.user})
    if(data_check(exp_data=CORR_Check_exp_data_table(),data_type="exp",page="Correlation",file_path=input$CORR_user_exp$datapath,
                  remove_na=input$CORR_rm_NA,remove_na_pct=input$CORR_rm_NA_pct)[[2]]){
      data_process(CORR_class_exp_data(), 
                   exclude_var_missing=input$CORR_rm_NA, 
                   missing_pct_limit=input$CORR_rm_NA_pct,
                   replace_zero=T, zero2what='NA', xmin=0.5,
                   replace_NA=input$CORR_rp_NA, NA2what=input$CORR_fill_NA, ymin=input$CORR_fill_min,
                   pct_transform=input$CORR_pct_trans,
                   data_transform=input$CORR_log_trans, trans_type='log',
                   centering=F,
                   scaling=F)
    }else{
      NULL
    }
  }
  
})

shinyjs::hide('CORR_result_div')

############################
####  CORR Data Source  ####
############################

# #### control demo cate reset button ####
# observeEvent(input$CORR_demo_cate_reset, {
#   
#   #### shinyjs show/hide main panel ####
#   shinyjs::hide('CORR_demo_cate_mainPanel_div')
#   
#   #### shiny show/hide tab ####
#   hideTab(inputId = 'CORR_analysis_tab', target = 'Lipid species analysis')
#   hideTab(inputId = 'CORR_analysis_tab', target = 'Lipid category analysis')
#   
#   #### shinyjs reset control panel ####
#   shinyjs::reset('CORR_demo_reset_div')
#   
# }) #observeEvent(input$CORR_demo_cate_reset
# 
# #### CORR demo cate dataset ####
# observeEvent(input$CORR_demo_cate_upload, {
#   
#   #### shinyjs show/hide main panel ####
#   shinyjs::show('CORR_demo_cate_mainPanel_div')
#   shinyjs::hide('CORR_demo_cont_mainPanel_div')
#   
#   isolate({
#     
#     #### import demo dataset ####
#     variables$CORR.exp.data.demo.cate <- readRDS('www/demo_dataset/Correlation/cate_exp_data.rds')
#     variables$CORR.cond.tab.demo.cate <- readRDS('www/demo_dataset/Correlation/cate_condition_table.rds')
#     variables$CORR.adj.tab.demo.cate <- readRDS('www/demo_dataset/Correlation/cate_adjusted_table.rds')
#     variables$CORR.lipid.char.demo.cate <- readRDS('www/demo_dataset/Correlation/cate_lipid_char_table.rds')
#     
#     #### Output: CORR.demo.cate.exp ####
#     output$CORR.demo.cate.exp <- renderDataTable(server = FALSE,{
#       DT::datatable(CORR_exp_transform_data(), 
#                     #caption = 'Lipid expression data',
#                     #colnames = c('feature', ML_group_info()$label_name),
#                     escape = FALSE, selection = 'none', rownames = FALSE, 
#                     class = "nowrap row-border",
#                     extensions = c('Buttons', 'Scroller'),
#                     options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
#                                    deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
#                                    dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
#                                    columnDefs = list(list(className = 'dt-center', targets = "_all"))))
#     })
#     
#     #### Output: CORR.demo.cate.cond ####
#     output$CORR.demo.cate.cond <- renderDataTable(server = FALSE,{
#       DT::datatable(CORR_cond_tab(), 
#                     #caption = 'Lipid expression data',
#                     #colnames = c('feature', ML_group_info()$label_name),
#                     escape = FALSE, selection = 'none', rownames = FALSE, 
#                     class = "nowrap row-border",
#                     extensions = c('Buttons', 'Scroller'),
#                     options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
#                                    deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
#                                    dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
#                                    columnDefs = list(list(className = 'dt-center', targets = "_all"))))
#     })
#     
#     #### Output: CORR.demo.cate.adj ####
#     output$CORR.demo.cate.adj <- renderDataTable(server = FALSE,{
#       DT::datatable(CORR_adj_tab(), 
#                     #caption = 'Lipid expression data',
#                     #colnames = c('feature', ML_group_info()$label_name),
#                     escape = FALSE, selection = 'none', rownames = FALSE, 
#                     class = "nowrap row-border",
#                     extensions = c('Buttons', 'Scroller'),
#                     options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
#                                    deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
#                                    dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
#                                    columnDefs = list(list(className = 'dt-center', targets = "_all"))))
#     })
#     
#     #### Output: CORR.demo.cate.char ####
#     output$CORR.demo.cate.char <- renderDataTable(server = FALSE,{
#       DT::datatable(CORR_lipid_char(), 
#                     #caption = 'Lipid expression data',
#                     #colnames = c('feature', ML_group_info()$label_name),
#                     escape = FALSE, selection = 'none', rownames = FALSE, 
#                     class = "nowrap row-border",
#                     extensions = c('Buttons', 'Scroller'),
#                     options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
#                                    deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
#                                    dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
#                                    columnDefs = list(list(className = 'dt-center', targets = "_all"))))
#     })
#     
#   }) #isolate
#   
# }) #observeEvent(input$CORR_demo_cate_upload


#### control demo cont reset button ####
observeEvent(input$CORR_demo_cont_reset, {
  
  #### shinyjs show/hide main panel ####
  shinyjs::hide('CORR_demo_cate_mainPanel_div')
  shinyjs::hide('CORR_demo_cont_mainPanel_div')
  
  #### shiny show/hide tab ####
  hideTab(inputId = 'CORR_analysis_tab', target = 'Lipid species analysis')
  hideTab(inputId = 'CORR_analysis_tab', target = 'Lipid characteristics analysis')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('CORR_demo_reset_div')
  
}) #observeEvent(input$CORR_demo_cont_reset

#### Output: CORR.demo.download ####
output$CORR.demo.download <- downloadHandler(
  filename = function() {
    "Correlation_example_dataset.zip"
  },
  content = function(file) {
    file.copy("www/download_demo_dataset/Corr.zip", file)
  },
  contentType = "application/zip"
) 


#### CORR demo cont dataset ####
observeEvent(input$CORR_demo_cont_upload, {
  
  #### shinyjs show/hide main panel ####
  shinyjs::show('CORR_demo_cont_mainPanel_div')
  
  #isolate({
    
    #### import demo dataset ####
    variables$CORR.exp.data.demo.cont <- readRDS('www/demo_dataset/Correlation/cont_exp_data.rds')
    variables$CORR.cond.tab.demo.cont <- readRDS('www/demo_dataset/Correlation/cont_condition_table.rds')
    variables$CORR.adj.tab.demo.cont <- readRDS('www/demo_dataset/Correlation/cont_adjusted_table.rds')
    variables$CORR.lipid.char.demo.cont <- readRDS('www/demo_dataset/Correlation/cont_lipid_char_table.rds')
    
    #### Output: CORR.demo.cont.exp.raw ####
    output$CORR.demo.cont.exp.raw <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(variables$CORR.exp.data.demo.cont, 
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
    
    #### Output: CORR.demo.cont.exp ####
    output$CORR.demo.cont.exp <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(CORR_exp_transform_data(), 
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
    
    #### Output: CORR.demo.cont.cond ####
    output$CORR.demo.cont.cond <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(CORR_cond_tab(), 
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
    
    #### Output: CORR.demo.cont.adj ####
    output$CORR.demo.cont.adj <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(CORR_adj_tab(), 
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
    
    #### Output: CORR.demo.cont.char ####
    output$CORR.demo.cont.char <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(CORR_lipid_char(), 
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
    
  #}) #isolate
  
}) #observeEvent(input$CORR_demo_cont_upload


#### control user reset button ####
observeEvent(input$CORR_user_reset, {
  
  #### shinyjs show/hide main panel ####
  shinyjs::hide('CORR_user_mainPanel_div')
  
  #### shiny show/hide tab ####
  hideTab(inputId = 'CORR_analysis_tab', target = 'Lipid species analysis')
  hideTab(inputId = 'CORR_analysis_tab', target = 'Lipid characteristics analysis')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('CORR_user_reset_div')
  
  variables$CORR.exp.data.user <- NULL
  variables$CORR.cond.tab.user <- NULL
  variables$CORR.adj.tab.user <- NULL
  variables$CORR.lipid.char.user <- NULL
  
}) #observeEvent(input$CORR_user_reset


#### CORR user dataset ####
observeEvent(input$CORR_user_upload, {
  
  #### shinyjs show/hide main panel ####
  shinyjs::show('CORR_user_mainPanel_div')
  
  #isolate({
    
    showNotification("Start uploading file...", type = "message")
    tryCatch(
      {
        #### import user dataset ####
        ## exp_data
        variables$CORR.exp.data.user <- data.table::fread(input$CORR_user_exp$datapath, header = T, 
                                                          stringsAsFactors = F, check.names = F, 
                                                          data.table = F, na.strings = c('', 'NA'))
        ## condition_table
        variables$CORR.cond.tab.user <- data.table::fread(input$CORR_user_cond$datapath, header = T, 
                                                          stringsAsFactors = F, check.names = F, 
                                                          data.table = F, na.strings = c('', 'NA'))
        ## adjust_table
        if(!is.null(input$CORR_user_adj)){
          variables$CORR.adj.tab.user <- data.table::fread(input$CORR_user_adj$datapath, header = T, 
                                                           stringsAsFactors = F, check.names = F, 
                                                           data.table = F, na.strings = c('', 'NA'))
        }else{
          variables$CORR.adj.tab.user <- NULL
        }
        ## lipid_char_table
        if(!is.null(input$CORR_user_char)){
          variables$CORR.lipid.char.user <- data.table::fread(input$CORR_user_char$datapath, header = T, 
                                                              stringsAsFactors = F, check.names = F, 
                                                              data.table = F, na.strings = c('', 'NA'))
        }else{
          variables$CORR.lipid.char.user <- NULL
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
    variables$CORR.check.exp.data.user <- data.table::fread(input$CORR_user_exp$datapath, header = T, 
                                                            stringsAsFactors = F, check.names = F, 
                                                            data.table = F, na.strings = c('', 'NA'))
    CORR_Check_exp_data_table <- reactive({variables$CORR.check.exp.data.user})
    output$CORR_Check_Exp_Data <- renderUI({
      isolate({
        data_check(exp_data=CORR_Check_exp_data_table(),data_type="exp",
                   page="Correlation",file_path=input$CORR_user_exp$datapath,
                   remove_na=input$CORR_rm_NA,remove_na_pct=input$CORR_rm_NA_pct)[[1]]
      })
    })
    ## condition_table
    variables$CORR.check.cond.tab.user <- data.table::fread(input$CORR_user_cond$datapath, header = T, 
                                                            stringsAsFactors = F, check.names = F, 
                                                            data.table = F, na.strings = c('', 'NA'))
    CORR_Check_cond_tab <- reactive({variables$CORR.check.cond.tab.user})
    output$CORR_Check_condition <- renderUI({
      isolate({
        data_check(data=CORR_Check_cond_tab(),exp_data=CORR_Check_exp_data_table(),
                   file_path=input$CORR_user_cond$datapath,data_type="Continuous_condition")[[1]]
      })
    })
    ## adjust_table
    if(!is.null(input$CORR_user_adj)){
      variables$CORR.check.adj.tab.user <- data.table::fread(input$CORR_user_adj$datapath, header = T, 
                                                             stringsAsFactors = F, check.names = F, 
                                                             data.table = F, na.strings = c('', 'NA'))
      CORR_Check_adj_tab <- reactive({variables$CORR.check.adj.tab.user})
      output$CORR_Check_adjusted_table <- renderUI({
        isolate({
          data_check(data=CORR_Check_adj_tab(),exp_data=CORR_Check_exp_data_table(),
                     file_path=input$CORR_user_adj$datapath,data_type="adjusted")[[1]]
        })
      })
    }else{
      variables$CORR.check.adj.tab.user <- NULL
      CORR_Check_adj_tab <- reactive({variables$CORR.check.adj.tab.user})
      output$CORR_Check_adjusted_table <- renderUI({
        isolate({
          data_check(data=NULL,exp_data = NULL,data_type="optional_adjusted")[[1]]
        })
      })
    }
    ## lipid_char_table
    if(!is.null(input$CORR_user_char)){
      variables$CORR.check.lipid.char.user <- data.table::fread(input$CORR_user_char$datapath, header = T, 
                                                                stringsAsFactors = F, check.names = F, 
                                                                data.table = F, na.strings = c('', 'NA'))
      CORR_Check_lipid_char_data_table <- reactive({variables$CORR.check.lipid.char.user})
      output$CORR_Check_lipid_char <- renderUI({
        isolate({
          data_check(data=CORR_Check_lipid_char_data_table(),exp_data = CORR_Check_exp_data_table(),
                     file_path=input$CORR_user_char$datapath,data_type="lipid_char")[[1]]
        })
      })
    }else{
      variables$CORR.check.lipid.char.user <- NULL
      CORR_Check_lipid_char_data_table <- reactive({variables$CORR.check.lipid.char.user})
      output$CORR_Check_lipid_char <- renderUI({
        isolate({
          data_check(data=NULL,exp_data = NULL,data_type="optional_lipid_char")[[1]]
        })
      })
    }
    
    
    #### rename column name ####
    ## exp_data
    variables$CORR.exp.user.col1 <- colnames(variables$CORR.exp.data.user)[1]
    colnames(variables$CORR.exp.data.user)[1] <- 'feature'
    ## condition table
    variables$CORR.cond.user.col1 <- colnames(variables$CORR.cond.tab.user)[1]
    colnames(variables$CORR.cond.tab.user)[1] <- 'sample_name'
    ## adjust table
    if(!is.null(variables$CORR.adj.tab.user)){
      variables$CORR.adj.user.col1 <- colnames(variables$CORR.adj.tab.user)[1]
      colnames(variables$CORR.adj.tab.user)[1] <- 'sample_name'
    }
    ## lipid_char_table
    if(!is.null(variables$CORR.lipid.char.user)){
      variables$CORR.lipid.char.user.col1 <- colnames(variables$CORR.lipid.char.user)[1]
      colnames(variables$CORR.lipid.char.user)[1] <- 'feature'
    }
    
    
    # #### Output: CORR.user.exp ####
    # output$CORR.user.exp <- renderDataTable(server = FALSE,{
    #   DT::datatable(CORR_exp_transform_data(), 
    #                 #caption = 'Lipid expression data',
    #                 colnames = c(variables$CORR.exp.user.col1, colnames(variables$CORR.exp.data.user)[-1]),
    #                 escape = FALSE, selection = 'none', rownames = FALSE, 
    #                 class = "nowrap row-border",
    #                 extensions = c('Buttons', 'Scroller'),
    #                 options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
    #                                deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
    #                                dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
    #                                columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    # })
    # 
    # #### Output: CORR.user.cond ####
    # output$CORR.user.cond <- renderDataTable(server = FALSE,{
    #   DT::datatable(CORR_cond_tab(), 
    #                 #caption = 'Lipid expression data',
    #                 colnames = c(variables$CORR.cond.user.col1, colnames(variables$CORR.cond.tab.user)[-1]),
    #                 escape = FALSE, selection = 'none', rownames = FALSE, 
    #                 class = "nowrap row-border",
    #                 extensions = c('Buttons', 'Scroller'),
    #                 options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
    #                                deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
    #                                dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
    #                                columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    # })
    # 
    # #### Output: CORR.user.adj ####
    # output$CORR.user.adj <- renderDataTable(server = FALSE,{
    #   
    #   validate(need(!is.null(CORR_adj_tab()), "Not uploaded"))
    #   
    #   DT::datatable(CORR_adj_tab(), 
    #                 #caption = 'Lipid expression data',
    #                 colnames = c(variables$CORR.adj.user.col1, colnames(variables$CORR.adj.tab.user)[-1]),
    #                 escape = FALSE, selection = 'none', rownames = FALSE, 
    #                 class = "nowrap row-border",
    #                 extensions = c('Buttons', 'Scroller'),
    #                 options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
    #                                deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
    #                                dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
    #                                columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    # })
    # 
    # #### Output: CORR.user.char ####
    # output$CORR.user.char <- renderDataTable(server = FALSE,{
    #   
    #   validate(need(!is.null(CORR_lipid_char()), "Not uploaded"))
    #   
    #   DT::datatable(CORR_lipid_char(), 
    #                 #caption = 'Lipid expression data',
    #                 colnames = c(variables$CORR.lipid.char.user.col1, colnames(variables$CORR.lipid.char.user)[-1]),
    #                 escape = FALSE, selection = 'none', rownames = FALSE, 
    #                 class = "nowrap row-border",
    #                 extensions = c('Buttons', 'Scroller'),
    #                 options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
    #                                deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
    #                                dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
    #                                columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    # })
    observe({
      if(is.null(CORR_Check_lipid_char_data_table())&is.null(CORR_Check_adj_tab())){
        if(data_check(exp_data=CORR_Check_exp_data_table(),data_type="exp",page="Correlation",file_path=input$CORR_user_exp$datapath,
                      remove_na=input$CORR_rm_NA,remove_na_pct=input$CORR_rm_NA_pct)[[2]] &
           data_check(data=CORR_Check_cond_tab(),exp_data=CORR_Check_exp_data_table(),file_path=input$CORR_user_cond$datapath,data_type="Continuous_condition")[[2]]){
          shinyjs::show('CORR_user_input_table_div')
          shinyjs::enable("CORR_user_start")
          output$CORR_Data_summary <-  renderUI({
            isolate({
              data_summary(exp_data = CORR_Check_exp_data_table(),remove_na=input$CORR_rm_NA,remove_na_pct=input$CORR_rm_NA_pct,
                           fill_na = input$CORR_rp_NA,fill_na_method = input$CORR_fill_NA,
                           fill_na_Multiply = input$CORR_fill_min,
                           PCT_tr = input$CORR_pct_trans,log_tr = input$CORR_log_trans)
            })
          })
          
          #### Output: CORR.user.exp.raw ####
          output$CORR.user.exp.raw <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(variables$CORR.exp.data.user, 
                            #caption = 'Lipid expression data',
                            #colnames = c(variables$CORR.exp.user.col1, colnames(variables$CORR.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.exp ####
          output$CORR.user.exp <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(CORR_exp_transform_data(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.exp.user.col1, colnames(variables$CORR.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.cond ####
          output$CORR.user.cond <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(CORR_cond_tab(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.cond.user.col1, colnames(variables$CORR.cond.tab.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.adj ####
          output$CORR.user.adj <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(CORR_adj_tab()), "Not uploaded"))
              
              DT::datatable(CORR_adj_tab(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.adj.user.col1, colnames(variables$CORR.adj.tab.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.char ####
          output$CORR.user.char <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(CORR_lipid_char()), "Not uploaded"))
              
              DT::datatable(CORR_lipid_char(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.lipid.char.user.col1, colnames(variables$CORR.lipid.char.user)[-1]),
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
          shinyjs::hide('CORR_user_input_table_div')
          shinyjs::disable("CORR_user_start")
          output$CORR_Data_summary <-  renderUI({
            NULL
          })
        }
      }else if(is.null(CORR_Check_lipid_char_data_table())&!is.null(CORR_Check_adj_tab())){
        if(data_check(exp_data=CORR_Check_exp_data_table(),data_type="exp",page="Correlation",file_path=input$CORR_user_exp$datapath,
                      remove_na=input$CORR_rm_NA,remove_na_pct=input$CORR_rm_NA_pct)[[2]] &
           data_check(data=CORR_Check_cond_tab(),exp_data=CORR_Check_exp_data_table(),file_path=input$CORR_user_cond$datapath,data_type="Continuous_condition")[[2]] &
           data_check(data=CORR_Check_adj_tab(),exp_data=CORR_Check_exp_data_table(),file_path=input$CORR_user_adj$datapath,data_type="adjusted")[[2]]){
          shinyjs::show('CORR_user_input_table_div')
          shinyjs::enable("CORR_user_start")
          output$CORR_Data_summary <-  renderUI({
            isolate({
              data_summary(exp_data = CORR_Check_exp_data_table(),remove_na=input$CORR_rm_NA,remove_na_pct=input$CORR_rm_NA_pct,
                           fill_na = input$CORR_rp_NA,fill_na_method = input$CORR_fill_NA,
                           fill_na_Multiply = input$CORR_fill_min,
                           PCT_tr = input$CORR_pct_trans,log_tr = input$CORR_log_trans)
            })
          })
          
          #### Output: CORR.user.exp.raw ####
          output$CORR.user.exp.raw <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(variables$CORR.exp.data.user, 
                            #caption = 'Lipid expression data',
                            #colnames = c(variables$CORR.exp.user.col1, colnames(variables$CORR.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.exp ####
          output$CORR.user.exp <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(CORR_exp_transform_data(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.exp.user.col1, colnames(variables$CORR.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.cond ####
          output$CORR.user.cond <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(CORR_cond_tab(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.cond.user.col1, colnames(variables$CORR.cond.tab.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.adj ####
          output$CORR.user.adj <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(CORR_adj_tab()), "Not uploaded"))
              
              DT::datatable(CORR_adj_tab(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.adj.user.col1, colnames(variables$CORR.adj.tab.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.char ####
          output$CORR.user.char <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(CORR_lipid_char()), "Not uploaded"))
              
              DT::datatable(CORR_lipid_char(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.lipid.char.user.col1, colnames(variables$CORR.lipid.char.user)[-1]),
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
          shinyjs::hide('CORR_user_input_table_div')
          shinyjs::disable("CORR_user_start")
          output$CORR_Data_summary <-  renderUI({
            NULL
          })
        }
      }else if(!is.null(CORR_Check_lipid_char_data_table())&is.null(CORR_Check_adj_tab())){
        if(data_check(exp_data=CORR_Check_exp_data_table(),data_type="exp",page="Correlation",file_path=input$CORR_user_exp$datapath,
                      remove_na=input$CORR_rm_NA,remove_na_pct=input$CORR_rm_NA_pct)[[2]] &
           data_check(data=CORR_Check_cond_tab(),exp_data=CORR_Check_exp_data_table(),file_path=input$CORR_user_cond$datapath,data_type="Continuous_condition")[[2]] &
           data_check(data=CORR_Check_lipid_char_data_table(),exp_data = CORR_Check_exp_data_table(),file_path=input$CORR_user_char$datapath,data_type="lipid_char")[[2]]){
          shinyjs::show('CORR_user_input_table_div')
          shinyjs::enable("CORR_user_start")
          output$CORR_Data_summary <-  renderUI({
            isolate({
              data_summary(exp_data = CORR_Check_exp_data_table(),remove_na=input$CORR_rm_NA,remove_na_pct=input$CORR_rm_NA_pct,
                           fill_na = input$CORR_rp_NA,fill_na_method = input$CORR_fill_NA,
                           fill_na_Multiply = input$CORR_fill_min,
                           PCT_tr = input$CORR_pct_trans,log_tr = input$CORR_log_trans)
            })
          })
          
          #### Output: CORR.user.exp.raw ####
          output$CORR.user.exp.raw <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(variables$CORR.exp.data.user, 
                            #caption = 'Lipid expression data',
                            #colnames = c(variables$CORR.exp.user.col1, colnames(variables$CORR.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.exp ####
          output$CORR.user.exp <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(CORR_exp_transform_data(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.exp.user.col1, colnames(variables$CORR.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.cond ####
          output$CORR.user.cond <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(CORR_cond_tab(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.cond.user.col1, colnames(variables$CORR.cond.tab.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.adj ####
          output$CORR.user.adj <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(CORR_adj_tab()), "Not uploaded"))
              
              DT::datatable(CORR_adj_tab(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.adj.user.col1, colnames(variables$CORR.adj.tab.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.char ####
          output$CORR.user.char <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(CORR_lipid_char()), "Not uploaded"))
              
              DT::datatable(CORR_lipid_char(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.lipid.char.user.col1, colnames(variables$CORR.lipid.char.user)[-1]),
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
          shinyjs::hide('CORR_user_input_table_div')
          shinyjs::disable("CORR_user_start")
          output$CORR_Data_summary <-  renderUI({
            NULL
          })
        }
      }else{
        if(data_check(exp_data=CORR_Check_exp_data_table(),data_type="exp",page="Correlation",file_path=input$CORR_user_exp$datapath,
                      remove_na=input$CORR_rm_NA,remove_na_pct=input$CORR_rm_NA_pct)[[2]] &
           data_check(data=CORR_Check_cond_tab(),exp_data=CORR_Check_exp_data_table(),file_path=input$CORR_user_cond$datapath,data_type="Continuous_condition")[[2]] &
           data_check(data=CORR_Check_adj_tab(),exp_data=CORR_Check_exp_data_table(),file_path=input$CORR_user_adj$datapath,data_type="adjusted")[[2]] &
           data_check(data=CORR_Check_lipid_char_data_table(),exp_data = CORR_Check_exp_data_table(),file_path=input$CORR_user_char$datapath,data_type="lipid_char")[[2]]){
          shinyjs::show('CORR_user_input_table_div')
          shinyjs::enable("CORR_user_start")
          output$CORR_Data_summary <-  renderUI({
            isolate({
              data_summary(exp_data = CORR_Check_exp_data_table(),remove_na=input$CORR_rm_NA,remove_na_pct=input$CORR_rm_NA_pct,
                           fill_na = input$CORR_rp_NA,fill_na_method = input$CORR_fill_NA,
                           fill_na_Multiply = input$CORR_fill_min,
                           PCT_tr = input$CORR_pct_trans,log_tr = input$CORR_log_trans)
            })
          })
          
          #### Output: CORR.user.exp.raw ####
          output$CORR.user.exp.raw <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(variables$CORR.exp.data.user, 
                            #caption = 'Lipid expression data',
                            #colnames = c(variables$CORR.exp.user.col1, colnames(variables$CORR.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.exp ####
          output$CORR.user.exp <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(CORR_exp_transform_data(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.exp.user.col1, colnames(variables$CORR.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.cond ####
          output$CORR.user.cond <- renderDataTable(server = FALSE,{
            isolate({
              DT::datatable(CORR_cond_tab(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.cond.user.col1, colnames(variables$CORR.cond.tab.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.adj ####
          output$CORR.user.adj <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(CORR_adj_tab()), "Not uploaded"))
              
              DT::datatable(CORR_adj_tab(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.adj.user.col1, colnames(variables$CORR.adj.tab.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          
          #### Output: CORR.user.char ####
          output$CORR.user.char <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(CORR_lipid_char()), "Not uploaded"))
              
              DT::datatable(CORR_lipid_char(), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$CORR.lipid.char.user.col1, colnames(variables$CORR.lipid.char.user)[-1]),
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
          shinyjs::hide('CORR_user_input_table_div')
          shinyjs::disable("CORR_user_start")
          output$CORR_Data_summary <-  renderUI({
            NULL
          })
        }
      }
      
    }) #observe
    
  #}) #isolate
  
}) #observeEvent(input$CORR_user_upload


#### control user upload button ####
observe({
  
  if(is.null(input$CORR_user_exp) || is.null(input$CORR_user_cond)){
    shinyjs::disable("CORR_user_upload")
  }else{
    shinyjs::enable("CORR_user_upload")
  }
  
}) #observe



####################################
####  Correlation analysis tab  ####
####################################

# #### control hide/show tabpanel: input$CORR_demo_cate_start ####
# observeEvent(input$CORR_demo_cate_start, {
#   
#   shinyjs::show('CORR_result_div')
#   showTab(inputId = 'CORR_analysis_tab', target = 'Lipid species analysis')
#   showTab(inputId = 'CORR_analysis_tab', target = 'Lipid category analysis')
#   
#   #### tabPanel: Lipid species analysis ####
#   showTab(inputId = 'CORR_species_list', target = 'Differential expression')
#   showTab(inputId = 'CORR_species_list', target = 'ROC')
#   showTab(inputId = 'CORR_species_list', target = 'Logistic regression')
#   
#   hideTab(inputId = 'CORR_species_list', target = 'Correlation')
#   hideTab(inputId = 'CORR_species_list', target = 'Linear regression')
#   
#   #### tabPanel: Lipid category analysis ####
#   showTab(inputId = 'CORR_class_list', target = 'Differential expression')
#   showTab(inputId = 'CORR_class_list', target = 'ROC')
#   showTab(inputId = 'CORR_class_list', target = 'Logistic regression')
#   
#   hideTab(inputId = 'CORR_class_list', target = 'Correlation')
#   hideTab(inputId = 'CORR_class_list', target = 'Linear regression')
#   
#   # #### shinyjs show/hide results ####
#   # shinyjs::hide('DE_analysis_result_div')
#   # shinyjs::hide('DE_class_analysis_result_div')
#   # shinyjs::hide('DE_class_split_result_div')
#   
# }) #observeEvent(input$CORR_demo_cate_start


#### control hide/show tabpanel: input$CORR_demo_cont_start ####
observeEvent(input$CORR_demo_cont_start, {
  
  shinyjs::show('CORR_result_div')
  showTab(inputId = 'CORR_analysis_tab', target = 'Lipid species analysis')
  showTab(inputId = 'CORR_analysis_tab', target = 'Lipid characteristics analysis')
  
  #### tabPanel: Lipid species analysis ####
  # hideTab(inputId = 'CORR_species_list', target = 'Differential expression')
  # hideTab(inputId = 'CORR_species_list', target = 'ROC')
  # hideTab(inputId = 'CORR_species_list', target = 'Logistic regression')
  
  showTab(inputId = 'CORR_species_list', target = 'Correlation')
  showTab(inputId = 'CORR_species_list', target = 'Linear regression')
  
  #### tabPanel: Lipid category analysis ####
  # hideTab(inputId = 'CORR_class_list', target = 'Differential expression')
  # hideTab(inputId = 'CORR_class_list', target = 'ROC')
  # hideTab(inputId = 'CORR_class_list', target = 'Logistic regression')
  
  showTab(inputId = 'CORR_class_list', target = 'Correlation')
  showTab(inputId = 'CORR_class_list', target = 'Linear regression')
  
  # #### shinyjs show/hide results ####
  # shinyjs::hide('DE_analysis_result_div')
  # shinyjs::hide('DE_class_analysis_result_div')
  # shinyjs::hide('DE_class_split_result_div')
  
}) #observeEvent(input$CORR_demo_cont_start


#### control hide/show tabpanel: input$CORR_user_start ####
observeEvent(input$CORR_user_start, {
  
  shinyjs::show('CORR_result_div')
  
  showTab(inputId = 'CORR_analysis_tab', target = 'Lipid species analysis')
  #### tabPanel: Lipid species analysis ####
  showTab(inputId = 'CORR_species_list', target = 'Correlation')
  showTab(inputId = 'CORR_species_list', target = 'Linear regression')
  
  if(is.null(input$CORR_user_char) | is.null(variables$CORR.lipid.char.user)){
    hideTab(inputId = 'CORR_analysis_tab', target = 'Lipid characteristics analysis')
    #### tabPanel: Lipid category analysis ####
    hideTab(inputId = 'CORR_class_list', target = 'Correlation')
    hideTab(inputId = 'CORR_class_list', target = 'Linear regression')
  }else{
    showTab(inputId = 'CORR_analysis_tab', target = 'Lipid characteristics analysis')
    #### tabPanel: Lipid category analysis ####
    showTab(inputId = 'CORR_class_list', target = 'Correlation')
    showTab(inputId = 'CORR_class_list', target = 'Linear regression')
  }
  
  # if(length(unique(variables$CORR.cond.tab.user[,2])) >= 3){
  #   
  #   #### tabPanel: Lipid species analysis ####
  #   showTab(inputId = 'CORR_species_list', target = 'Differential expression')
  #   showTab(inputId = 'CORR_species_list', target = 'ROC')
  #   showTab(inputId = 'CORR_species_list', target = 'Logistic regression')
  #   
  #   hideTab(inputId = 'CORR_species_list', target = 'Correlation')
  #   hideTab(inputId = 'CORR_species_list', target = 'Linear regression')
  #   
  #   #### tabPanel: Lipid category analysis ####
  #   showTab(inputId = 'CORR_class_list', target = 'Differential expression')
  #   showTab(inputId = 'CORR_class_list', target = 'ROC')
  #   showTab(inputId = 'CORR_class_list', target = 'Logistic regression')
  #   
  #   hideTab(inputId = 'CORR_class_list', target = 'Correlation')
  #   hideTab(inputId = 'CORR_class_list', target = 'Linear regression')
  #   
  # }else{
  #   
  #   #### tabPanel: Lipid species analysis ####
  #   hideTab(inputId = 'CORR_species_list', target = 'Differential expression')
  #   hideTab(inputId = 'CORR_species_list', target = 'ROC')
  #   hideTab(inputId = 'CORR_species_list', target = 'Logistic regression')
  #   
  #   showTab(inputId = 'CORR_species_list', target = 'Correlation')
  #   showTab(inputId = 'CORR_species_list', target = 'Linear regression')
  #   
  #   #### tabPanel: Lipid category analysis ####
  #   hideTab(inputId = 'CORR_class_list', target = 'Differential expression')
  #   hideTab(inputId = 'CORR_class_list', target = 'ROC')
  #   hideTab(inputId = 'CORR_class_list', target = 'Logistic regression')
  #   
  #   showTab(inputId = 'CORR_class_list', target = 'Correlation')
  #   showTab(inputId = 'CORR_class_list', target = 'Linear regression')
  #   
  # }
  
  # #### shinyjs show/hide results ####
  # shinyjs::hide('DE_analysis_result_div')
  # shinyjs::hide('DE_class_analysis_result_div')
  # shinyjs::hide('DE_class_split_result_div')
  
}) #observeEvent(input$CORR_user_start


##########################################
##########################################
#####  Tab1: Lipid species analysis  #####
##########################################
##########################################


# ###########################################################
# ####  Lipid species analysis: Differential expression  ####
# ###########################################################
# 
# #### control reset button ####
# observeEvent(input$CORR_species_DE_reset, {
#   
#   #### shinyjs show/hide results ####
#   shinyjs::hide('CORR_species_DE_result_div')
#   
#   #### shinyjs reset ####
#   shinyjs::reset("CORR_species_DE_reset_div")
#   
# }) #observeEvent(input$CORR_species_DE_reset
# 
# #### control start button ####
# observeEvent(input$CORR_species_DE_start, {
#   
#   #### shinyjs show/hide results ####
#   shinyjs::show('CORR_species_DE_result_div')
#   
#   #### Function: Clin_DE_2_heatmap ####
#   isolate({
#     
#     variables$CORR.species.DE.result <- Clin_DE_2_heatmap(CORR_exp_data(), data_transform = T, 
#                                                           lipid_char_table=NULL, char_var=NULL, 
#                                                           CORR_cond_tab(), 
#                                                           paired = F, 
#                                                           test = input$CORR_species_DE_stat_method, 
#                                                           adjust_p_method = input$CORR_species_DE_adj_stat_method, 
#                                                           sig_stat = input$CORR_species_DE_sig_p, 
#                                                           sig_pvalue = input$CORR_species_DE_pval, 
#                                                           sig_FC = input$CORR_species_DE_fc,
#                                                           heatmap_col = input$CORR_species_DE_color,
#                                                           distfun = input$CORR_species_DE_dist, 
#                                                           hclustfun = input$CORR_species_DE_hclust)
#     
#     
#     #### Output: CORR.species.DE.heatmap ####
#     output$CORR.species.DE.heatmap <- renderIheatmap({
#       
#       validate(need(!is.null(variables$CORR.species.DE.result$Clin_DE_table_plot), "Without clustering result!"))
#       variables$CORR.species.DE.result$Clin_DE_table_plot
#       
#     }) #output$CORR.species.DE.heatmap <- renderIheatmap
#     
#     #### Output: CORR.species.DE.heatmap.matrix ####
#     output$CORR.species.DE.heatmap.matrix <- downloadHandler(
#       filename = function() {
#         paste0(input$CORR_species_DE_dist, '_', input$CORR_species_DE_hclust, '_', input$CORR_species_DE_color,"_matrix", ".csv")
#       },
#       content = function(file) {
#         write.csv(variables$CORR.species.DE.result$Clin_DE_reorder_mat, file)
#       }
#     ) #output$CORR.species.DE.heatmap.matrix
#     
#   }) #isolate
#   
# }) #observeEvent(input$CORR_species_DE_start
# 
# 
# #######################################
# ####  Lipid species analysis: ROC  ####
# #######################################
# 
# #### control reset button ####
# observeEvent(input$CORR_species_ROC_reset, {
#   
#   #### shinyjs show/hide results ####
#   shinyjs::hide('CORR_species_ROC_result_div')
#   
#   #### shinyjs reset ####
#   shinyjs::reset("CORR_species_ROC_reset_div")
#   
# }) #observeEvent(input$CORR_species_ROC_reset
# 
# #### control start button ####
# observeEvent(input$CORR_species_ROC_start, {
#   
#   #### shinyjs show/hide results ####
#   shinyjs::show('CORR_species_ROC_result_div')
#   
#   #### Function: Clin_ROC_heatmap ####
#   isolate({
#     
#     variables$CORR.species.ROC.result <- Clin_ROC_heatmap(CORR_exp_data(), data_transform = T, 
#                                                           lipid_char_table=NULL, char_var=NULL, 
#                                                           CORR_cond_tab(), 
#                                                           sig_auc = input$CORR_species_ROC_sig_auc,
#                                                           distfun = input$CORR_species_ROC_dist, 
#                                                           hclustfun = input$CORR_species_ROC_hclust)
#     
#     #### Output: CORR.species.ROC.heatmap ####
#     output$CORR.species.ROC.heatmap <- renderIheatmap({
#       
#       validate(need(!is.null(variables$CORR.species.ROC.result$Clin_ROC_table_plot), "Without clustering result!"))
#       variables$CORR.species.ROC.result$Clin_ROC_table_plot
#       
#     }) #output$CORR.species.ROC.heatmap <- renderIheatmap
#     
#     #### Output: CORR.species.ROC.heatmap.matrix ####
#     output$CORR.species.ROC.heatmap.matrix <- downloadHandler(
#       filename = function() {
#         paste0(input$CORR_species_ROC_dist, '_', input$CORR_species_ROC_hclust, '_', input$CORR_species_ROC_color,"_matrix", ".csv")
#       },
#       content = function(file) {
#         write.csv(variables$CORR.species.ROC.result$Clin_ROC_reorder_mat, file)
#       }
#     ) #output$CORR.species.ROC.heatmap.matrix
#     
#   }) #isolate
#   
# }) #observeEvent(input$CORR_species_ROC_start
# 
# 
# #######################################################
# ####  Lipid species analysis: Logistic regression  ####
# #######################################################
# 
# #### control reset button ####
# observeEvent(input$CORR_species_logistic_reset, {
#   
#   #### shinyjs show/hide results ####
#   shinyjs::hide('CORR_species_logistic_result_div')
#   
#   #### shinyjs reset ####
#   shinyjs::reset("CORR_species_logistic_reset_div")
#   
# }) #observeEvent(input$CORR_species_logistic_reset
# 
# #### control start button ####
# observeEvent(input$CORR_species_logistic_start, {
#   
#   #### shinyjs show/hide results ####
#   shinyjs::show('CORR_species_logistic_result_div')
#   
#   #### Function: Clin_LogR_heatmap ####
#   isolate({
#     
#     variables$CORR.species.logistic.result <- Clin_LogR_heatmap(CORR_exp_data(), data_transform = T, 
#                                                                 lipid_char_table=NULL, char_var=NULL, 
#                                                                 CORR_cond_tab(), #CORR_adj_tab(),
#                                                                 adjusted_table = NULL,
#                                                                 adjust_p_method = input$CORR_species_logistic_adj_stat_method, 
#                                                                 sig_stat = input$CORR_species_logistic_sig_p, 
#                                                                 sig_pvalue = input$CORR_species_logistic_pval, 
#                                                                 heatmap_col = input$CORR_species_logistic_color,
#                                                                 distfun = input$CORR_species_logistic_dist, 
#                                                                 hclustfun = input$CORR_species_logistic_hclust)
#     
#     #### Output: CORR.species.logistic.heatmap ####
#     output$CORR.species.logistic.heatmap <- renderIheatmap({
#       
#       validate(need(!is.null(variables$CORR.species.logistic.result$Clin_LogR_table_plot), "Without clustering result!"))
#       variables$CORR.species.logistic.result$Clin_LogR_table_plot
#       
#     }) #output$CORR.species.logistic.heatmap <- renderIheatmap
#     
#     #### Output: CORR.species.logistic.heatmap.matrix ####
#     output$CORR.species.logistic.heatmap.matrix <- downloadHandler(
#       filename = function() {
#         paste0(input$CORR_species_logistic_dist, '_', input$CORR_species_logistic_hclust, '_', input$CORR_species_logistic_color,"_matrix", ".csv")
#       },
#       content = function(file) {
#         write.csv(variables$CORR.species.logistic.result$Clin_LogR_reorder_mat, file)
#       }
#     ) #output$CORR.species.logistic.heatmap.matrix
#     
#   }) #isolate
#   
# }) #observeEvent(input$CORR_species_logistic_start


###############################################
####  Lipid species analysis: Correlation  ####
###############################################

#### control reset button ####
observeEvent(input$CORR_species_corr_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('CORR_species_corr_result_div')
  
  #### shinyjs reset ####
  shinyjs::reset("CORR_species_corr_reset_div")
  
}) #observeEvent(input$CORR_species_corr_reset

#### control start button ####
observeEvent(input$CORR_species_corr_start, {
  
  #### shinyjs show/hide results ####
  shinyjs::show('CORR_species_corr_result_div')
  
  #### Function: Clin_Cor_heatmap ####
  isolate({
    
    variables$CORR.species.corr.result <- Clin_Cor_heatmap(CORR_exp_transform_data(), #data_transform = T, 
                                                           #lipid_char_table=NULL, char_var=NULL, 
                                                           CORR_cond_tab(), 
                                                           test = input$CORR_species_corr_method, 
                                                           adjust_p_method = input$CORR_species_corr_adj_stat_method, 
                                                           sig_stat = input$CORR_species_corr_sig_p, 
                                                           sig_pvalue = input$CORR_species_corr_pval,
                                                           sig_cor_coef = input$CORR_species_corr_coef,
                                                           heatmap_col = input$CORR_species_corr_color, 
                                                           distfun = input$CORR_species_corr_dist, 
                                                           hclustfun = input$CORR_species_corr_hclust)
    
    #### Output: CORR.species.corr.heatmap ####
    output$CORR.species.corr.heatmap <- renderIheatmap({
      
      validate(need(!is.null(variables$CORR.species.corr.result$Clin_Cor_table_plot), "Plot not showing. Please adjust the cutoffs."))
      variables$CORR.species.corr.result$Clin_Cor_table_plot
      
    }) #output$CORR.species.corr.heatmap <- renderIheatmap
    
    #### Output: CORR.species.corr.heatmap.matrix ####
    output$CORR.species.corr.heatmap.matrix <- downloadHandler(
      filename = function() {
        paste0(input$CORR_species_corr_dist, '_', input$CORR_species_corr_hclust, '_', input$CORR_species_corr_color,"_matrix", ".csv")
      },
      content = function(file) {
        write.csv(variables$CORR.species.corr.result$Clin_Cor_reorder_mat, file)
      }
    ) #output$CORR.species.corr.heatmap.matrix
    
  }) #isolate
  
}) #observeEvent(input$CORR_species_corr_start


#####################################################
####  Lipid species analysis: Linear regression  ####
#####################################################

#### control reset button ####
observeEvent(input$CORR_species_linear_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('CORR_species_linear_result_div')
  
  #### shinyjs reset ####
  shinyjs::reset("CORR_species_linear_reset_div")
  
}) #observeEvent(input$CORR_species_linear_reset

#### control start button ####
observeEvent(input$CORR_species_linear_start, {
  
  #### shinyjs show/hide results ####
  shinyjs::show('CORR_species_linear_result_div')
  
  #### Function: Clin_LR_heatmap ####
  isolate({
    
    variables$CORR.species.linear.result <- Clin_LR_heatmap(CORR_exp_transform_data(), #data_transform = T, 
                                                            #lipid_char_table=NULL, char_var=NULL, 
                                                            CORR_cond_tab(), CORR_adj_tab(),
                                                            adjust_p_method = input$CORR_species_linear_adj_stat_method, 
                                                            sig_stat = input$CORR_species_linear_sig_p, 
                                                            sig_pvalue = input$CORR_species_linear_pval,
                                                            distfun = input$CORR_species_linear_dist, 
                                                            hclustfun = input$CORR_species_linear_hclust,
                                                            heatmap_col = input$CORR_species_linear_color)
    
    #### Output: CORR.species.linear.heatmap ####
    output$CORR.species.linear.heatmap <- renderIheatmap({
      
      validate(need(!is.null(variables$CORR.species.linear.result$Clin_LR_table_plot), "Plot not showing. Please adjust the cutoffs."))
      variables$CORR.species.linear.result$Clin_LR_table_plot
      
    }) #output$CORR.species.linear.heatmap <- renderIheatmap
    
    #### Output: CORR.species.linear.heatmap.matrix ####
    output$CORR.species.linear.heatmap.matrix <- downloadHandler(
      filename = function() {
        paste0(input$CORR_species_linear_dist, '_', input$CORR_species_linear_hclust, '_', input$CORR_species_linear_color,"_matrix", ".csv")
      },
      content = function(file) {
        write.csv(variables$CORR.species.linear.result$Clin_LR_reorder_mat, file)
      }
    ) #output$CORR.species.linear.heatmap.matrix
    
  }) #isolate
  
}) #observeEvent(input$CORR_species_linear_start



###########################################
###########################################
#####  Tab2: Lipid category analysis  #####
###########################################
###########################################


# #########################################################
# ####  Lipid class analysis: Differential expression  ####
# #########################################################
# 
# #### control reset button ####
# observeEvent(input$CORR_class_DE_reset, {
#   
#   #### shinyjs show/hide results ####
#   shinyjs::hide('CORR_class_DE_result_div')
#   
#   #### shinyjs reset ####
#   shinyjs::reset("CORR_class_DE_reset_div")
#   
# }) #observeEvent(input$CORR_class_DE_reset
# 
# #### control start button ####
# observeEvent(input$CORR_class_DE_start, {
#   
#   #### shinyjs show/hide results ####
#   shinyjs::show('CORR_class_DE_result_div')
#   
#   #### Function: Clin_DE_2_heatmap ####
#   isolate({
#     
#     variables$CORR.class.DE.result <- Clin_DE_2_heatmap(CORR_exp_data(), data_transform = T, 
#                                                         CORR_lipid_char(), 
#                                                         char_var = input$CORR_class_DE_lipid_char, 
#                                                         CORR_cond_tab(), 
#                                                         paired = F, 
#                                                         test = input$CORR_class_DE_stat_method, 
#                                                         adjust_p_method = input$CORR_class_DE_adj_stat_method, 
#                                                         sig_stat = input$CORR_class_DE_sig_p, 
#                                                         sig_pvalue = input$CORR_class_DE_pval, 
#                                                         sig_FC = input$CORR_class_DE_fc,
#                                                         heatmap_col = input$CORR_class_DE_color,
#                                                         distfun = input$CORR_class_DE_dist, 
#                                                         hclustfun = input$CORR_class_DE_hclust)
#     
#     #### Output: CORR.class.DE.heatmap ####
#     output$CORR.class.DE.heatmap <- renderIheatmap({
#       
#       validate(need(!is.null(variables$CORR.class.DE.result$Clin_DE_table_plot), "Without clustering result!"))
#       variables$CORR.class.DE.result$Clin_DE_table_plot
#       
#     }) #output$CORR.class.DE.heatmap <- renderIheatmap
#     
#     #### Output: CORR.class.DE.heatmap.matrix ####
#     output$CORR.class.DE.heatmap.matrix <- downloadHandler(
#       filename = function() {
#         paste0(input$CORR_class_DE_dist, '_', input$CORR_class_DE_hclust, '_', input$CORR_class_DE_color,"_matrix", ".csv")
#       },
#       content = function(file) {
#         write.csv(variables$CORR.class.DE.result$Clin_DE_reorder_mat, file)
#       }
#     ) #output$CORR.class.DE.heatmap.matrix
#     
#   }) #isolate
#   
# }) #observeEvent(input$CORR_class_DE_start
# 
# #### update CORR class DE lipid characteristic select input ####
# observe({
#   if(!is.null(CORR_lipid_char())){
#     lipid.char <- colnames(CORR_lipid_char())[-1]
#     updateSelectInput(session, "CORR_class_DE_lipid_char",
#                       choices =  lipid.char)
#   }
# })
# 
# 
# #####################################
# ####  Lipid class analysis: ROC  ####
# #####################################
# 
# #### control reset button ####
# observeEvent(input$CORR_class_ROC_reset, {
#   
#   #### shinyjs show/hide results ####
#   shinyjs::hide('CORR_class_ROC_result_div')
#   
#   #### shinyjs reset ####
#   shinyjs::reset("CORR_class_ROC_reset_div")
#   
# }) #observeEvent(input$CORR_class_ROC_reset
# 
# #### control start button ####
# observeEvent(input$CORR_class_ROC_start, {
#   
#   #### shinyjs show/hide results ####
#   shinyjs::show('CORR_class_ROC_result_div')
#   
#   #### Function: Clin_ROC_heatmap ####
#   isolate({
#     
#     variables$CORR.class.ROC.result <- Clin_ROC_heatmap(CORR_exp_data(), data_transform = T, 
#                                                         CORR_lipid_char(), 
#                                                         char_var = input$CORR_class_ROC_lipid_char, 
#                                                         CORR_cond_tab(), 
#                                                         sig_auc = input$CORR_class_ROC_sig_auc,
#                                                         distfun = input$CORR_class_ROC_dist, 
#                                                         hclustfun = input$CORR_class_ROC_hclust)
#     
#     #### Output: CORR.class.ROC.heatmap ####
#     output$CORR.class.ROC.heatmap <- renderIheatmap({
#       
#       validate(need(!is.null(variables$CORR.class.ROC.result$Clin_ROC_table_plot), "Without clustering result!"))
#       variables$CORR.class.ROC.result$Clin_ROC_table_plot
#       
#     }) #output$CORR.class.ROC.heatmap <- renderIheatmap
#     
#     #### Output: CORR.class.ROC.heatmap.matrix ####
#     output$CORR.class.ROC.heatmap.matrix <- downloadHandler(
#       filename = function() {
#         paste0(input$CORR_class_ROC_dist, '_', input$CORR_class_ROC_hclust, '_', input$CORR_class_ROC_color,"_matrix", ".csv")
#       },
#       content = function(file) {
#         write.csv(variables$CORR.class.ROC.result$Clin_ROC_reorder_mat, file)
#       }
#     ) #output$CORR.class.ROC.heatmap.matrix
#     
#   }) #isolate
#   
# }) #observeEvent(input$CORR_class_ROC_start
# 
# #### update CORR class ROC lipid characteristic select input ####
# observe({
#   if(!is.null(CORR_lipid_char())){
#     lipid.char <- colnames(CORR_lipid_char())[-1]
#     updateSelectInput(session, "CORR_class_ROC_lipid_char",
#                       choices =  lipid.char)
#   }
# })
# 
# 
# #####################################################
# ####  Lipid class analysis: Logistic regression  ####
# #####################################################
# 
# #### control reset button ####
# observeEvent(input$CORR_class_logistic_reset, {
#   
#   #### shinyjs show/hide results ####
#   shinyjs::hide('CORR_class_logistic_result_div')
#   
#   #### shinyjs reset ####
#   shinyjs::reset("CORR_class_logistic_reset_div")
#   
# }) #observeEvent(input$CORR_class_logistic_reset
# 
# #### control start button ####
# observeEvent(input$CORR_class_logistic_start, {
#   
#   #### shinyjs show/hide results ####
#   shinyjs::show('CORR_class_logistic_result_div')
#   
#   #### Function: Clin_LogR_heatmap ####
#   isolate({
#     
#     variables$CORR.class.logistic.result <- Clin_LogR_heatmap(CORR_exp_data(), data_transform = T, 
#                                                               CORR_lipid_char(), 
#                                                               char_var = input$CORR_class_logistic_lipid_char, 
#                                                               CORR_cond_tab(), CORR_adj_tab(),
#                                                               #adjusted_table = NULL,
#                                                               adjust_p_method = input$CORR_class_logistic_adj_stat_method, 
#                                                               sig_stat = input$CORR_class_logistic_sig_p, 
#                                                               sig_pvalue = input$CORR_class_logistic_pval, 
#                                                               heatmap_col = input$CORR_class_logistic_color,
#                                                               distfun = input$CORR_class_logistic_dist, 
#                                                               hclustfun = input$CORR_class_logistic_hclust)
#     
#     #### Output: CORR.class.logistic.heatmap ####
#     output$CORR.class.logistic.heatmap <- renderIheatmap({
#       
#       validate(need(!is.null(variables$CORR.class.logistic.result$Clin_LogR_table_plot), "Without clustering result!"))
#       variables$CORR.class.logistic.result$Clin_LogR_table_plot
#       
#     }) #output$CORR.class.logistic.heatmap <- renderIheatmap
#     
#     #### Output: CORR.class.logistic.heatmap.matrix ####
#     output$CORR.class.logistic.heatmap.matrix <- downloadHandler(
#       filename = function() {
#         paste0(input$CORR_class_logistic_dist, '_', input$CORR_class_logistic_hclust, '_', input$CORR_class_logistic_color,"_matrix", ".csv")
#       },
#       content = function(file) {
#         write.csv(variables$CORR.class.logistic.result$Clin_LogR_reorder_mat, file)
#       }
#     ) #output$CORR.class.logistic.heatmap.matrix
#     
#   }) #isolate
#   
# }) #observeEvent(input$CORR_class_logistic_start
# 
# #### update CORR class logistic lipid characteristic select input ####
# observe({
#   if(!is.null(CORR_lipid_char())){
#     lipid.char <- colnames(CORR_lipid_char())[-1]
#     updateSelectInput(session, "CORR_class_logistic_lipid_char",
#                       choices =  lipid.char)
#   }
# })


#############################################
####  Lipid class analysis: Correlation  ####
#############################################

#### control reset button ####
observeEvent(input$CORR_class_corr_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('CORR_class_corr_result_div')
  
  #### shinyjs reset ####
  shinyjs::reset("CORR_class_corr_reset_div")
  
}) #observeEvent(input$CORR_class_corr_reset

#### control start button ####
observeEvent(input$CORR_class_corr_start, {
  
  #### shinyjs show/hide results ####
  shinyjs::show('CORR_class_corr_result_div')
  
  #### Function: Clin_Cor_heatmap ####
  isolate({
    
    variables$CORR.class.corr.result <- Clin_Cor_heatmap(CORR_class_exp_transform_data(), #data_transform = T, 
                                                         #CORR_lipid_char(), 
                                                         #char_var = input$CORR_class_corr_lipid_char, 
                                                         CORR_cond_tab(), 
                                                         test = input$CORR_class_corr_method, 
                                                         adjust_p_method = input$CORR_class_corr_adj_stat_method, 
                                                         sig_stat = input$CORR_class_corr_sig_p, 
                                                         sig_pvalue = input$CORR_class_corr_pval,
                                                         sig_cor_coef = input$CORR_class_corr_coef,
                                                         heatmap_col = input$CORR_class_corr_color, 
                                                         distfun = input$CORR_class_corr_dist, 
                                                         hclustfun = input$CORR_class_corr_hclust)
    
    #### Output: CORR.class.corr.heatmap ####
    output$CORR.class.corr.heatmap <- renderIheatmap({
      
      validate(need(!is.null(variables$CORR.class.corr.result$Clin_Cor_table_plot), "Plot not showing. Please adjust the cutoffs."))
      variables$CORR.class.corr.result$Clin_Cor_table_plot
      
    }) #output$CORR.class.corr.heatmap <- renderIheatmap
    
    #### Output: CORR.class.corr.heatmap.matrix ####
    output$CORR.class.corr.heatmap.matrix <- downloadHandler(
      filename = function() {
        paste0(input$CORR_class_corr_dist, '_', input$CORR_class_corr_hclust, '_', input$CORR_class_corr_color,"_matrix", ".csv")
      },
      content = function(file) {
        write.csv(variables$CORR.class.corr.result$Clin_Cor_reorder_mat, file)
      }
    ) #output$CORR.class.corr.heatmap.matrix
    
  }) #isolate
  
}) #observeEvent(input$CORR_class_corr_start

#### update CORR class corr lipid characteristic select input ####
observe({
  if(!is.null(CORR_lipid_char())){
    lipid.char <- colnames(CORR_lipid_char())[-1]
    updateSelectInput(session, "CORR_class_corr_lipid_char",
                      choices =  lipid.char)
  }
})


###################################################
####  Lipid class analysis: Linear regression  ####
###################################################

#### control reset button ####
observeEvent(input$CORR_class_linear_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('CORR_class_linear_result_div')
  
  #### shinyjs reset ####
  shinyjs::reset("CORR_class_linear_reset_div")
  
}) #observeEvent(input$CORR_class_linear_reset

#### control start button ####
observeEvent(input$CORR_class_linear_start, {
  
  #### shinyjs show/hide results ####
  shinyjs::show('CORR_class_linear_result_div')
  
  #### Function: Clin_LR_heatmap ####
  isolate({
    
    variables$CORR.class.linear.result <- Clin_LR_heatmap(CORR_class_exp_transform_data(), #data_transform = T, 
                                                          #CORR_lipid_char(), 
                                                          #char_var = input$CORR_class_linear_lipid_char, 
                                                          CORR_cond_tab(), CORR_adj_tab(),
                                                          adjust_p_method = input$CORR_class_linear_adj_stat_method, 
                                                          sig_stat = input$CORR_class_linear_sig_p, 
                                                          sig_pvalue = input$CORR_class_linear_pval,
                                                          distfun = input$CORR_class_linear_dist, 
                                                          hclustfun = input$CORR_class_linear_hclust,
                                                          heatmap_col = input$CORR_class_linear_color)
    
    #### Output: CORR.class.linear.heatmap ####
    output$CORR.class.linear.heatmap <- renderIheatmap({
      
      validate(need(!is.null(variables$CORR.class.linear.result$Clin_LR_table_plot), "Plot not showing. Please adjust the cutoffs."))
      variables$CORR.class.linear.result$Clin_LR_table_plot
      
    }) #output$CORR.class.linear.heatmap <- renderIheatmap
    
    #### Output: CORR.class.linear.heatmap.matrix ####
    output$CORR.class.linear.heatmap.matrix <- downloadHandler(
      filename = function() {
        paste0(input$CORR_class_linear_dist, '_', input$CORR_class_linear_hclust, '_', input$CORR_class_linear_color,"_matrix", ".csv")
      },
      content = function(file) {
        write.csv(variables$CORR.class.linear.result$Clin_LR_reorder_mat, file)
      }
    ) #output$CORR.class.linear.heatmap.matrix
    
  }) #isolate
  
}) #observeEvent(input$CORR_class_linear_start

#### update CORR class linear lipid characteristic select input ####
observe({
  if(!is.null(CORR_lipid_char())){
    lipid.char <- colnames(CORR_lipid_char())[-1]
    updateSelectInput(session, "CORR_class_linear_lipid_char",
                      choices =  lipid.char)
  }
})






