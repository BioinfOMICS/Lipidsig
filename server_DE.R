##test5.v27

#############################################
#############################################
######                                 ######
######   Differential Expressed Page   ######
######                                 ######
#############################################
#############################################



############################
####  Assign variables  ####
############################

#### DE_exp_data() ####
DE_exp_data <- reactive({
  if(input$DE_data_source == 'DE_demo_data'){
    variables$DE.exp.data.demo
  }else if(input$DE_data_source == 'DE_user_data'){
    variables$DE.exp.data.user
  }
})

#### DE_exp_transform_data() ####
DE_exp_transform_data <- reactive({
  if(input$DE_data_source == 'DE_demo_data'){
    data_process(DE_exp_data(), 
                 exclude_var_missing=F, 
                 missing_pct_limit=50,
                 replace_zero=T, zero2what='NA', xmin=0.5,
                 replace_NA=T, NA2what='min', ymin=0.5,
                 pct_transform=T,
                 data_transform=F, trans_type='log',
                 centering=F,
                 scaling=F)
  }else if(input$DE_data_source == 'DE_user_data'){
    DE_Check_exp_data_table <- reactive({variables$DE.check.exp.data.user})
    if(data_check(exp_data=DE_Check_exp_data_table(),data_type="exp",page="DE",file_path=input$DE_user_exp$datapath,remove_na=input$DE_rm_NA,remove_na_pct=input$DE_rm_NA_pct)[[2]]){
      data_process(DE_exp_data(), 
                   exclude_var_missing=input$DE_rm_NA, 
                   missing_pct_limit=input$DE_rm_NA_pct,
                   replace_zero=T, zero2what='NA', xmin=0.5,
                   replace_NA=input$DE_rp_NA, NA2what=input$DE_fill_NA, ymin=input$DE_fill_min,
                   pct_transform=input$DE_pct_trans,
                   data_transform=input$DE_log_trans, trans_type='log',
                   centering=F,
                   scaling=F)
    }else{
      NULL
    }
  }
})

#### DE_exp_non_log_data() ####
DE_exp_non_log_data <- reactive({
  
  if(input$DE_data_source == 'DE_demo_data'){
    data_process(DE_exp_data(), 
                 exclude_var_missing=F, 
                 missing_pct_limit=50,
                 replace_zero=T, zero2what='NA', xmin=0.5,
                 replace_NA=T, NA2what='min', ymin=0.5,
                 pct_transform=T,
                 data_transform=F, trans_type='log',
                 centering=F,
                 scaling=F)
  }else if(input$DE_data_source == 'DE_user_data'){
    DE_Check_exp_data_table <- reactive({variables$DE.check.exp.data.user})
    if(data_check(exp_data=DE_Check_exp_data_table(),data_type="exp",page="DE",file_path=input$DE_user_exp$datapath,remove_na=input$DE_rm_NA,remove_na_pct=input$DE_rm_NA_pct)[[2]]){
      data_process(DE_exp_data(), 
                   exclude_var_missing=input$DE_rm_NA, 
                   missing_pct_limit=input$DE_rm_NA_pct,
                   replace_zero=T, zero2what='NA', xmin=0.5,
                   replace_NA=input$DE_rp_NA, NA2what=input$DE_fill_NA, ymin=input$DE_fill_min,
                   pct_transform=input$DE_pct_trans,
                   data_transform=F,trans_type='log',
                   centering=F,
                   scaling=F)
    }else{
      NULL
    }
  }
  
  
})

#### DE_group_info() ####
DE_group_info <- reactive({
  if(input$DE_data_source == 'DE_demo_data'){
    variables$DE.group.info.demo
  }else if(input$DE_data_source == 'DE_user_data'){
    variables$DE.group.info.user %>% 
      mutate(group = ifelse(group == input$DE_user_ref_group, 'ctrl', 'exp'))
  }
})

#### DE_lipid_char_table() ####
DE_lipid_char_table <- reactive({
  if(input$DE_data_source == 'DE_demo_data'){
    variables$DE.lipid.char.tab.demo
  }else if(input$DE_data_source == 'DE_user_data'){
    if(!is.null(variables$DE.lipid.char.tab.user)){
      variables$DE.lipid.char.tab.user <- variables$DE.lipid.char.tab.user %>% 
        filter(feature %in% DE_exp_transform_data()$feature)
    }else{
      variables$DE.lipid.char.tab.user <- NULL
    }
    
  }
})

#### DE_sample_count() ####
DE_sample_count <- reactive({
  if(input$DE_data_source == 'DE_demo_data'){
    23
  }else if(input$DE_data_source == 'DE_user_data'){
    variables$DE.sample.count.user <- nrow(variables$DE.group.info.user)
  }
})

#### DE_group_count() ####
DE_group_count <- reactive({
  if(input$DE_data_source == 'DE_demo_data'){
    2
  }else if(input$DE_data_source == 'DE_user_data'){
    variables$DE.group.count.user <- length(unique(DE_group_info()$group))
  }
})

#### DE_species_all() ####
DE_species_all <- reactive({
  variables$DE.species.2$DE_species_table_all
})

#### DE_species_sig() ####
DE_species_sig <- reactive({
  variables$DE.species.2$DE_species_table_sig
})

#### DE_enrich_char() ####
DE_enrich_char <- reactive({
  variables$DE.enrichment$enrich_char_table
})

#### DE_class_exp_data() ####
DE_class_exp_data <- reactive({
  #### Function: Species2Char ####
  Species2Char(DE_exp_data(), DE_lipid_char_table(), input$DE_class_analysis_char)
})

#### DE_class_exp_transform_data() ####
DE_class_exp_transform_data <- reactive({
  #### Function: data_process ####
  if(input$DE_data_source == 'DE_demo_data'){
    data_process(DE_class_exp_data(), 
                 exclude_var_missing=T, 
                 missing_pct_limit=50,
                 replace_zero=T, zero2what='NA', xmin=0.5,
                 replace_NA=T, NA2what='min', ymin=0.5,
                 pct_transform=T,
                 data_transform=F, trans_type='log',
                 centering=F,
                 scaling=F)
  }else if(input$DE_data_source == 'DE_user_data'){
    data_process(DE_class_exp_data(), 
                 exclude_var_missing=input$DE_rm_NA, 
                 missing_pct_limit=input$DE_rm_NA_pct,
                 replace_zero=T, zero2what='NA', xmin=0.5,
                 replace_NA=input$DE_rp_NA, NA2what=input$DE_fill_NA, ymin=input$DE_fill_min,
                 pct_transform=input$DE_pct_trans,
                 data_transform=input$DE_log_trans, trans_type='log',
                 centering=F,
                 scaling=F)
  }
})

#### DE_class_exp_non_log_data() ####
DE_class_exp_non_log_data <- reactive({
  #### Function: data_process ####
  if(input$DE_data_source == 'DE_demo_data'){
    data_process(DE_class_exp_data(), 
                 exclude_var_missing=T, 
                 missing_pct_limit=50,
                 replace_zero=T, zero2what='NA', xmin=0.5,
                 replace_NA=T, NA2what='min', ymin=0.5,
                 pct_transform=T,
                 data_transform=F, trans_type='log',
                 centering=F,
                 scaling=F)
  }else if(input$DE_data_source == 'DE_user_data'){
    data_process(DE_class_exp_data(), 
                 exclude_var_missing=input$DE_rm_NA, 
                 missing_pct_limit=input$DE_rm_NA_pct,
                 replace_zero=T, zero2what='NA', xmin=0.5,
                 replace_NA=input$DE_rp_NA, NA2what=input$DE_fill_NA, ymin=input$DE_fill_min,
                 pct_transform=input$DE_pct_trans,
                 data_transform=F, trans_type='log',
                 centering=F,
                 scaling=F)
  }
})

#### DE_class_all() ####
DE_class_all <- reactive({
  variables$DE.char.2$DE_char_table_all
})

#### DE_class_sig() ####
DE_class_sig <- reactive({
  variables$DE.char.2$DE_char_table_all %>% 
    filter(sig == 'yes')
})

#### plot_MA_Vol() ####
plot_MA_Vol <- reactive({
  if(sum(!is.na(DE_species_all()[,'p_value']))==0){
    DE_species_all() %>% 
      dplyr::select(feature, mean_exp, mean_ctrl, p_value, p_adj, 'M' = 'log2FC') %>% 
      mutate(A = (log2(mean_exp) + log2(mean_ctrl))/2, 
             m.log.p = NA,
             sig_log2fc.pvalue_colors = if (input$DE_sig_p == "p"){
               ifelse( M>log2(input$DE_fc) ,'up-regulated',
                       ifelse(-M>log2(input$DE_fc),'down-regulated','none'))
             }else{
               ifelse( M > log2(input$DE_fc), 'up-regulated',
                       ifelse(-M > log2(input$DE_fc),'down-regulated','none'))
             })
  }else{
    DE_species_all() %>% 
      dplyr::select(feature, mean_exp, mean_ctrl, p_value, p_adj, 'M' = 'log2FC') %>% 
      mutate(A = (log2(mean_exp) + log2(mean_ctrl))/2, 
             m.log.p = -log10(p_value),
             sig_log2fc.pvalue_colors = if (input$DE_sig_p == "p"){
               ifelse( M>log2(input$DE_fc) & -log10(p_value) > -log10(input$DE_pval), 'up-regulated',
                       ifelse(-M>log2(input$DE_fc) & -log10(p_value) > -log10(input$DE_pval),'down-regulated','none'))
             }else{
               ifelse( M > log2(input$DE_fc) & -log10(p_adj) > -log10(input$DE_pval), 'up-regulated',
                       ifelse(-M > log2(input$DE_fc) & -log10(p_adj) > -log10(input$DE_pval),'down-regulated','none'))
             })
  }
}) #plot_MA_Vol <- reactive

shinyjs::hide('DE_result_div')

##########################
####  DE Data Source  ####
##########################

#### Output: DE.demo.download ####
output$DE.demo.download <- downloadHandler(
  filename = function() {
    "Differential_expression_example_dataset.zip"
  },
  content = function(file) {
    file.copy("www/download_demo_dataset/DE.zip", file)
  },
  contentType = "application/zip"
) 

#### DE demo dataset ####
observeEvent(input$DE_demo_upload, {
  
  if(input$DE_demo_upload > 0){
    progressSweetAlert(
      session = session, id = "DE_demo_progress",
      title = "Work in progress",
      display_pct = TRUE, value = 0
    )
    
    #### shinyjs show/hide main panel ####
    shinyjs::show('DE_demo_mainPanel_div')
    
    #### import demo dataset ####
    variables$DE.exp.data.demo <- readRDS('www/demo_dataset/DE/exp_data.rds')
    variables$DE.group.info.demo <- readRDS('www/demo_dataset/DE/group_info.rds')
    variables$DE.lipid.char.tab.demo <- readRDS('www/demo_dataset/DE/lipid_char_table.rds')
    
    #### Output: DE.demo.exp.raw ####
    output$DE.demo.exp.raw <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(variables$DE.exp.data.demo, 
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
      id = "DE_demo_progress",
      value = 20
    )
    
    #### Output: DE.demo.exp ####
    output$DE.demo.exp <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(DE_exp_transform_data() %>% mutate_if(is.numeric, ~round(., 5)), 
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
      id = "DE_demo_progress",
      value = 50
    )
    #### Output: DE.demo.group.info ####
    output$DE.demo.group.info <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(DE_group_info() %>% mutate_if(is.numeric, ~round(., 5)), 
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
      id = "DE_demo_progress",
      value = 70
    )
    
    #### Output: DE.demo.lipid.char ####
    output$DE.demo.lipid.char <- renderDataTable(server = FALSE,{
      isolate({
        DT::datatable(DE_lipid_char_table() %>% mutate_if(is.numeric, ~round(., 5)), 
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
      id = "DE_demo_progress",
      value = 100
    )
    closeSweetAlert(session = session)
  }
  
}) #observeEvent(input$DE_demo_upload

#### DE user dataset ####
observeEvent(input$DE_user_upload, {
  progressSweetAlert(
    session = session, id = "DE_user_progress",
    title = "Work in progress",
    display_pct = TRUE, value = 0
  )
  
  #### shinyjs show/hide main panel ####
  shinyjs::show('DE_user_mainPanel_div')
  
  #isolate({
    
    showNotification("Start uploading file...", type = "message")
    tryCatch(
      {
        #### import user dataset ####
        ## exp_data
        variables$DE.exp.data.user <- data.table::fread(input$DE_user_exp$datapath, header = T,
                                                        stringsAsFactors = F, check.names = F,
                                                        data.table = F, na.strings = c('', 'NA'))
        ## group_info
        variables$DE.group.info.user <- data.table::fread(input$DE_user_group$datapath, header = T,
                                                          stringsAsFactors = F, check.names = F,
                                                          data.table = F, na.strings = c('', 'NA'))
        ## lipid_char_table
        if(!is.null(input$DE_user_char)){
          variables$DE.lipid.char.tab.user <- data.table::fread(input$DE_user_char$datapath, header = T,
                                                                stringsAsFactors = F, check.names = F,
                                                                data.table = F, na.strings = c('', 'NA'))
        }else{
          variables$DE.lipid.char.tab.user <- NULL
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
    variables$DE.check.exp.data.user <- data.table::fread(input$DE_user_exp$datapath, header = T,
                                                    stringsAsFactors = F, check.names = F, 
                                                    data.table = F, na.strings = c('', 'NA'))
    DE_Check_exp_data_table <- reactive({variables$DE.check.exp.data.user})
    output$DE_Check_Exp_Data <- renderUI({
      isolate({
        data_check(exp_data=DE_Check_exp_data_table(),data_type="exp",
                   page="DE",file_path=input$DE_user_exp$datapath,
                   remove_na=input$DE_rm_NA,remove_na_pct=input$DE_rm_NA_pct)[[1]]
      })
    })
    updateProgressBar(
      session = session,
      id = "DE_user_progress",
      value = 20
    )
    ## group_info
    variables$DE.check.group.info.user <- data.table::fread(input$DE_user_group$datapath, header = T,
                                                            stringsAsFactors = F, check.names = F, 
                                                            data.table = F, na.strings = c('', 'NA'))
    DE_Check_Group_data_table <- reactive({variables$DE.check.group.info.user})
    output$DE_Check_Group_Data <- renderUI({
      isolate({
        data_check(data=DE_Check_Group_data_table(),exp_data=DE_Check_exp_data_table(),data_type="group",
                   group_name=input$DE_user_ref_group,file_path=input$DE_user_group$datapath)[[1]]
      })
    })
    updateProgressBar(
      session = session,
      id = "DE_user_progress",
      value = 40
    )
    ## lipid_char_table
    if(!is.null(input$DE_user_char)){
      variables$DE.check.lipid.char.tab.user <- data.table::fread(input$DE_user_char$datapath, header = T, 
                                                            stringsAsFactors = F, check.names = F, 
                                                            data.table = F, na.strings = c('', 'NA'))
      DE_Check_lipid_char_data_table <- reactive({variables$DE.check.lipid.char.tab.user})
      output$DE_Check_lipid_char <- renderUI({
        isolate({
          data_check(data=DE_Check_lipid_char_data_table(),exp_data=DE_Check_exp_data_table(),
                     data_type="lipid_char",file_path=input$DE_user_char$datapath)[[1]]
        })
      })
    }else{
      variables$DE.check.lipid.char.tab.user <- NULL
      DE_Check_lipid_char_data_table <- reactive({variables$DE.check.lipid.char.tab.user})
      output$DE_Check_lipid_char <- renderUI({
        isolate({
          data_check(data_type="optional_lipid_char")[[1]]
        })
      })
    }
    updateProgressBar(
      session = session,
      id = "DE_user_progress",
      value = 60
    )
    #### rename column name ####
    ## exp_data
    variables$DE.exp.user.col1 <- colnames(variables$DE.exp.data.user)[1]
    colnames(variables$DE.exp.data.user)[1] <- 'feature'
    ## group_info
    if(data_check(data=DE_Check_Group_data_table(),exp_data=DE_Check_exp_data_table(),data_type="group",
                  group_name=input$DE_user_ref_group,file_path=input$DE_user_group$datapath)[[2]]){
      variables$DE.group.user.col1 <- colnames(variables$DE.group.info.user)[1]
      variables$DE.group.user.col2 <- colnames(variables$DE.group.info.user)[2]
      variables$DE.group.user.col3 <- colnames(variables$DE.group.info.user)[3]
      if(ncol(variables$DE.group.info.user) == 3){
        variables$DE.group.info.user <- variables$DE.group.info.user %>%
          mutate(pair = NA)
        variables$DE.group.user.col4 <- 'pair'
        variables$DE.group.ref.group <- unique(variables$DE.group.info.user[,3])
        colnames(variables$DE.group.info.user) <- c('sample_name', 'label_name', 'group', 'pair')
      }else if(ncol(variables$DE.group.info.user) == 4){
        variables$DE.group.user.col4 <- colnames(variables$DE.group.info.user)[4]
        variables$DE.group.ref.group <- unique(variables$DE.group.info.user[,3])
        colnames(variables$DE.group.info.user) <- c('sample_name', 'label_name', 'group', 'pair')
      }
    }
    
    updateProgressBar(
      session = session,
      id = "DE_user_progress",
      value = 80
    )
    ## lipid_char_table
    if(!is.null(variables$DE.lipid.char.tab.user)){
      variables$DE.lipid.char.user.col1 <- colnames(variables$DE.lipid.char.tab.user)[1]
      colnames(variables$DE.lipid.char.tab.user)[1] <- 'feature'
    }
    
    
    # #### Output: DE.user.exp ####
    # output$DE.user.exp <- renderDataTable(server = FALSE,{
    #   validate(need(!is.null(DE_exp_transform_data()), "Some error is in your expression data, please check your data and re-upload it."))
    #   DT::datatable(DE_exp_transform_data(), 
    #                 #caption = 'Lipid expression data',
    #                 colnames = c(variables$DE.exp.user.col1, colnames(variables$DE.exp.data.user)[-1]),
    #                 escape = FALSE, selection = 'none', rownames = FALSE, 
    #                 class = "nowrap row-border",
    #                 extensions = c('Buttons', 'Scroller'),
    #                 options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
    #                                deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
    #                                dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
    #                                columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    # })
    # #### Output: DE.user.group.info ####
    # output$DE.user.group.info <- renderDataTable(server = FALSE,{
    #   validate(need(!is.null(variables$DE.group.info.user), "Some error is in your group information, please check your data and re-upload it."))
    #   DT::datatable(variables$DE.group.info.user, 
    #                 #caption = 'Lipid expression data',
    #                 colnames = c(variables$DE.group.user.col1, variables$DE.group.user.col2, variables$DE.group.user.col3, variables$DE.group.user.col4),
    #                 escape = FALSE, selection = 'none', rownames = FALSE, 
    #                 class = "nowrap row-border",
    #                 extensions = c('Buttons', 'Scroller'),
    #                 options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
    #                                deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
    #                                dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
    #                                columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    # })
    # #### Output: DE.user.lipid.char ####
    # output$DE.user.lipid.char <- renderDataTable(server = FALSE,{
    #   validate(need(!is.null(DE_lipid_char_table()), "Not uploaded"))
    #   DT::datatable(DE_lipid_char_table(), 
    #                 #caption = 'Lipid expression data',
    #                 colnames = c(variables$DE.lipid.char.user.col1, colnames(variables$DE.lipid.char.tab.user)[-1]),
    #                 escape = FALSE, selection = 'none', rownames = FALSE, 
    #                 class = "nowrap row-border",
    #                 extensions = c('Buttons', 'Scroller'),
    #                 options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
    #                                deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
    #                                dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
    #                                columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    # })
    observe({
      if(is.null(DE_Check_lipid_char_data_table())){
        if(data_check(data=NULL,exp_data = NULL,data_type="optional_lipid_char")[[2]] & 
           data_check(exp_data=DE_Check_exp_data_table(),data_type="exp",page="DE",file_path=input$DE_user_exp$datapath,
                      remove_na=input$DE_rm_NA,remove_na_pct=input$DE_rm_NA_pct)[[2]] &
           data_check(data=DE_Check_Group_data_table(),exp_data=DE_Check_exp_data_table(),data_type="group",
                      group_name=input$DE_user_ref_group,file_path=input$DE_user_group$datapath)[[2]]){
          shinyjs::show('DE_user_input_table_div')
          shinyjs::enable("DE_user_start")
          output$DE_Data_summary <-  renderUI({
            isolate({
              data_summary(exp_data = DE_Check_exp_data_table(),group_info=DE_Check_Group_data_table(),
                           remove_na=input$DE_rm_NA,remove_na_pct=input$DE_rm_NA_pct,
                           fill_na = input$DE_rp_NA,fill_na_method = input$DE_fill_NA,
                           fill_na_Multiply = input$DE_fill_min,
                           PCT_tr = input$DE_pct_trans,log_tr = input$DE_log_trans)
            })
          })
          
          #### Output: DE.user.exp.raw ####
          output$DE.user.exp.raw <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(variables$DE.exp.data.user), "Some error is in your expression data, please check your data and re-upload it."))
              DT::datatable(variables$DE.exp.data.user, 
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
          #### Output: DE.user.exp ####
          output$DE.user.exp <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(DE_exp_transform_data()), "Some error is in your expression data, please check your data and re-upload it."))
              DT::datatable(DE_exp_transform_data() %>% mutate_if(is.numeric, ~round(., 5)), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$DE.exp.user.col1, colnames(variables$DE.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          #### Output: DE.user.group.info ####
          output$DE.user.group.info <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(variables$DE.group.info.user), "Some error is in your group information, please check your data and re-upload it."))
              DT::datatable(variables$DE.group.info.user %>% mutate_if(is.numeric, ~round(., 5)), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$DE.group.user.col1, variables$DE.group.user.col2, variables$DE.group.user.col3, variables$DE.group.user.col4),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          #### Output: DE.user.lipid.char ####
          output$DE.user.lipid.char <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(DE_lipid_char_table()), "Not uploaded"))
              DT::datatable(DE_lipid_char_table() %>% mutate_if(is.numeric, ~round(., 5)), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$DE.lipid.char.user.col1, colnames(variables$DE.lipid.char.tab.user)[-1]),
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
          shinyjs::hide('DE_user_input_table_div')
          shinyjs::disable("DE_user_start")
          output$DE_Data_summary <-  renderUI({
            NULL
          })
        }
      }else{
        if(data_check(data=DE_Check_lipid_char_data_table(),exp_data=DE_Check_exp_data_table(),data_type="lipid_char",file_path=input$DE_user_char$datapath)[[2]] & 
           data_check(exp_data=DE_Check_exp_data_table(),data_type="exp",page="DE",file_path=input$DE_user_exp$datapath,
                      remove_na=input$DE_rm_NA,remove_na_pct=input$DE_rm_NA_pct)[[2]] &
           data_check(data=DE_Check_Group_data_table(),exp_data=DE_Check_exp_data_table(),data_type="group",
                      group_name=input$DE_user_ref_group,file_path=input$DE_user_group$datapath)[[2]]){
          shinyjs::show('DE_user_input_table_div')
          shinyjs::enable("DE_user_start")
          output$DE_Data_summary <-  renderUI({
            isolate({
              data_summary(exp_data = DE_Check_exp_data_table(),group_info=DE_Check_Group_data_table(),
                           remove_na=input$DE_rm_NA,remove_na_pct=input$DE_rm_NA_pct,
                           fill_na = input$DE_rp_NA,fill_na_method = input$DE_fill_NA,
                           fill_na_Multiply = input$DE_fill_min,
                           PCT_tr = input$DE_pct_trans,log_tr = input$DE_log_trans)
            })
          })
          
          #### Output: DE.user.exp.raw ####
          output$DE.user.exp.raw <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(variables$DE.exp.data.user), "Some error is in your expression data, please check your data and re-upload it."))
              DT::datatable(variables$DE.exp.data.user, 
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
          #### Output: DE.user.exp ####
          output$DE.user.exp <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(DE_exp_transform_data()), "Some error is in your expression data, please check your data and re-upload it."))
              DT::datatable(DE_exp_transform_data() %>% mutate_if(is.numeric, ~round(., 5)), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$DE.exp.user.col1, colnames(variables$DE.exp.data.user)[-1]),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          #### Output: DE.user.group.info ####
          output$DE.user.group.info <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(variables$DE.group.info.user), "Some error is in your group information, please check your data and re-upload it."))
              DT::datatable(variables$DE.group.info.user %>% mutate_if(is.numeric, ~round(., 5)), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$DE.group.user.col1, variables$DE.group.user.col2, variables$DE.group.user.col3, variables$DE.group.user.col4),
                            escape = FALSE, selection = 'none', rownames = FALSE, 
                            class = "nowrap row-border",
                            extensions = c('Buttons', 'Scroller'),
                            options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                           deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                           dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
            })
          })
          #### Output: DE.user.lipid.char ####
          output$DE.user.lipid.char <- renderDataTable(server = FALSE,{
            isolate({
              validate(need(!is.null(DE_lipid_char_table()), "Not uploaded"))
              DT::datatable(DE_lipid_char_table() %>% mutate_if(is.numeric, ~round(., 5)), 
                            #caption = 'Lipid expression data',
                            colnames = c(variables$DE.lipid.char.user.col1, colnames(variables$DE.lipid.char.tab.user)[-1]),
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
          shinyjs::hide('DE_user_input_table_div')
          shinyjs::disable("DE_user_start")
          output$DE_Data_summary <-  renderUI({
            NULL
          })
        }
      }
      
    }) #observe
    
    updateProgressBar(
      session = session,
      id = "DE_user_progress",
      value = 100
    )
    closeSweetAlert(session = session)
  #}) #isolate
  
}) #observeEvent(input$DE_user_upload

#### control user reset button ####
observeEvent(input$DE_user_reset, {
  
  #### shinyjs show/hide main panel ####
  shinyjs::hide('DE_user_mainPanel_div')
  
  #### shiny show/hide tab ####
  hideTab(inputId = 'DE_analysis_tab', target = 'Lipid species analysis')
  hideTab(inputId = 'DE_analysis_tab', target = 'Lipid category analysis')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('DE_user_reset_div')
  
  #### clear variables ####
  variables$DE.exp.data.user = NULL
  variables$DE.group.info.user = NULL
  variables$DE.lipid.char.tab.user = NULL
  variables$DE.species.2 = NULL
  variables$DE.char.2 = NULL
  
}) #observeEvent(input$DE_user_reset

#### control user upload button ####
observe({
  
  if(is.null(input$DE_user_exp) || is.null(input$DE_user_group)){
    shinyjs::disable("DE_user_upload")
  }else{
    shinyjs::enable("DE_user_upload")
  }
  
}) #observe


###########################
####  DE analysis tab  ####
###########################

#### control hide/show tabpanel: input$DE_demo_start ####
observeEvent(input$DE_demo_start, {
  
  shinyjs::show('DE_result_div')
  showTab(inputId = 'DE_analysis_tab', target = 'Lipid species analysis')
  showTab(inputId = 'DE_analysis_tab', target = 'Lipid characteristics analysis')
  
  hideTab(inputId = 'DE_species_list', target = 'Dimensionality reduction')
  hideTab(inputId = 'DE_species_list', target = 'Hierarchical clustering')
  hideTab(inputId = 'DE_species_list', target = 'Characteristics association')
  hideTab(inputId = 'DE_species_list', target = 'Enrichment')
  
  hideTab(inputId = 'DE_specific_list', target = 'Dimensionality reduction')
  hideTab(inputId = 'DE_specific_list', target = 'Hierarchical clustering')
  
  #### shinyjs show/hide results ####
  shinyjs::hide('DE_analysis_result_div')
  shinyjs::hide('DE_class_analysis_result_div')
  shinyjs::hide('DE_class_split_result_div')
  
  
}) #observeEvent(input$DE_demo_start

#### control hide/show tabpanel: input$DE_user_start ####
observeEvent(input$DE_user_start, {
  
  shinyjs::show('DE_result_div')
  showTab(inputId = 'DE_analysis_tab', target = 'Lipid species analysis')
  
  hideTab(inputId = 'DE_species_list', target = 'Dimensionality reduction')
  hideTab(inputId = 'DE_species_list', target = 'Hierarchical clustering')
  hideTab(inputId = 'DE_species_list', target = 'Characteristics association')
  hideTab(inputId = 'DE_species_list', target = 'Enrichment')
  
  hideTab(inputId = 'DE_specific_list', target = 'Dimensionality reduction')
  hideTab(inputId = 'DE_specific_list', target = 'Hierarchical clustering')
  
  if(is.null(input$DE_user_char) | is.null(variables$DE.lipid.char.tab.user)){
    hideTab(inputId = 'DE_analysis_tab', target = 'Lipid characteristics analysis')
  }else{
    showTab(inputId = 'DE_analysis_tab', target = 'Lipid characteristics analysis')
  }
  
  #### shinyjs show/hide results ####
  shinyjs::hide('DE_analysis_result_div')
  shinyjs::hide('DE_class_analysis_result_div')
  shinyjs::hide('DE_class_split_result_div')
  
  if(ncol(DE_exp_transform_data())<=4){
    shinyjs::disable('DE_stat_method')
    shinyjs::disable('DE_adj_stat_method')
    shinyjs::disable('DE_sig_p')
    shinyjs::disable('DE_pval')
    shinyjs::disable('DE_class_stat_method')
    shinyjs::disable('DE_class_post_hoc_method')
    shinyjs::disable('DE_class_pval')
  }
  
}) #observeEvent(input$DE_user_start


##########################################
##########################################
#####  Tab1: Lipid species analysis  #####
##########################################
##########################################


###############################################
####  Lipid species analysis: DE analysis  ####
###############################################

#######################
#### Control panel ####
#######################

# #### number of samples ####
# output$DE.group.count <- renderText({
#   paste0(DE_group_count(), ' groups were detected in samples.')
# })
# 
# #### paired or not paired ####
# output$DE.pair.detect <- renderText({
#   #if(length(unique(DE_group_info()$pair)) > 1){
#   #    paste0('Samples are paired.')
#   #}else{
#   paste0('Samples are not paired')
#   #}
# })



#### control reset button ####
observeEvent(input$DE_analysis_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('DE_analysis_result_div')
  shinyjs::hide('DE_species_dim_redu_result_div')
  shinyjs::hide('DE.species.heatmap')
  shinyjs::hide('DE_species_enrichment_result_div')
  shinyjs::hide('DE_species_enrichment_pathview_start_div')
  shinyjs::hide('DE_species_enrichment_pathview_result_div')
  
  #### shinyjs reset control panel ####
  shinyjs::reset("DE_analysis_reset_div")
  
  hideTab(inputId = 'DE_species_list', target = 'Dimensionality reduction')
  hideTab(inputId = 'DE_species_list', target = 'Hierarchical clustering')
  hideTab(inputId = 'DE_species_list', target = 'Characteristics association')
  hideTab(inputId = 'DE_species_list', target = 'Enrichment')
  
}) #observeEvent(input$DE_analysis_reset

#### control start button ####
observeEvent(input$DE_analysis_start, {
  
  #### shiny show/hide tab ####
  showTab(inputId = 'DE_species_list', target = 'Dimensionality reduction')
  showTab(inputId = 'DE_species_list', target = 'Hierarchical clustering')
  if(is.null(input$DE_user_char) & input$DE_data_source == 'DE_user_data'){
    hideTab(inputId = 'DE_species_list', target = 'Characteristics association')
    hideTab(inputId = 'DE_species_list', target = 'Enrichment')
  }else{
    showTab(inputId = 'DE_species_list', target = 'Characteristics association')
    showTab(inputId = 'DE_species_list', target = 'Enrichment')
  }
  
  #### shinyjs show/hide results ####
  shinyjs::show('DE_analysis_result_div')
  shinyjs::hide('DE_species_dim_redu_result_div')
  shinyjs::hide('DE.species.heatmap')
  shinyjs::hide('DE_species_enrichment_result_div')
  shinyjs::hide('DE_species_enrichment_pathview_start_div')
  shinyjs::hide('DE_species_enrichment_pathview_result_div')
  
  #isolate({
    
    #### function: DE_species_2() ####
    if(input$DE_data_source == 'DE_demo_data'){
      variables$DE.species.2 <- DE_species_2(DE_exp_non_log_data(), 
                                             data_transform = T, 
                                             DE_group_info(), 
                                             paired = F,
                                             test = input$DE_stat_method,
                                             adjust_p_method = input$DE_adj_stat_method,
                                             sig_stat = input$DE_sig_p,
                                             sig_pvalue = input$DE_pval,
                                             sig_FC = input$DE_fc)
    }else if(input$DE_data_source == 'DE_user_data'){
      
      paired_sample <- ifelse(length(is.na(DE_group_info()$pair)) == 0, TRUE, FALSE)
      variables$DE.species.2 <- DE_species_2(DE_exp_non_log_data(), 
                                             data_transform = input$DE_log_trans, 
                                             DE_group_info(), 
                                             paired = paired_sample,
                                             test = input$DE_stat_method,
                                             adjust_p_method = input$DE_adj_stat_method,
                                             sig_stat = input$DE_sig_p,
                                             sig_pvalue = input$DE_pval,
                                             sig_FC = input$DE_fc)
    }
    
  
    #### Output: DE.species.tab.all ####
    output$DE.species.tab.all <- renderDataTable(server = FALSE,{
      isolate({
        validate(need(!is.null(DE_species_all()), "Without DE result"))
        
        # DT::datatable(DE_species_all()[,c(-7,-9)],
        #               escape = FALSE, selection = 'none', rownames = TRUE, 
        #               class = "nowrap row-border",
        #               #caption = 'Lipid expression data',
        #               options = list(scrollX = TRUE, pageLength = 5, autoWidth = TRUE))
        if(sum(!is.na(DE_species_all()[,'p_value']))==0){
          DT::datatable(DE_species_all()[,c(-7,-9)] %>%
                          mutate(sig_FC = ifelse(abs(log2FC) > log2(input$DE_fc),'yes','no')) %>%
                          dplyr::select(sig_FC, everything()) %>%
                          arrange(desc(sig_FC)),
                        #caption = 'Lipid expression data',
                        colnames = c('Significance(Fold change)', 'Feature', 'Mean(ctrl)', 'Mean(exp)', 'Method', 'FC', 'Log2(FC)', '-Log10(p-value)', '-Log10(padj)','Significance(p-value)', 'Significance(padj)'),
                        escape = FALSE, selection = 'none', rownames = TRUE, 
                        class = "nowrap row-border",
                        extensions = c('Buttons', 'Scroller'),
                        options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                       deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                       dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                       columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
            formatStyle('sig_FC',target = 'row',backgroundColor = styleEqual(c("yes","no"), c('pink', 'EBECF0')))
        }else{
          if(input$DE_sig_p == "p"){
            DT::datatable(DE_species_all()[,c(-7,-9)] %>%
                            dplyr::select(sig_p, everything()) %>%
                            arrange(desc(sig_p)),
                          #caption = 'Lipid expression data',
                          colnames = c('Significance(p-value)', 'Feature', 'Mean(ctrl)', 'Mean(exp)', 'Method', 'FC', 'Log2(FC)', '-Log10(p-value)', '-Log10(padj)', 'Significance(padj)'),
                          escape = FALSE, selection = 'none', rownames = TRUE, 
                          class = "nowrap row-border",
                          extensions = c('Buttons', 'Scroller'),
                          options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                         deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                         dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                         columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
              formatStyle('sig_p',target = 'row',backgroundColor = styleEqual(c("yes","no"), c('pink', 'EBECF0')))
          }else{
            DT::datatable(DE_species_all()[,c(-7,-9)] %>%
                            dplyr::select(sig_p_adj, everything()) %>%
                            arrange(desc(sig_p_adj)),
                          #caption = 'Lipid expression data',
                          colnames = c('Significance(padj)', 'Feature', 'Mean(ctrl)', 'Mean(exp)', 'Method', 'FC', 'Log2(FC)', '-Log10(p-value)', '-Log10(padj)', 'Significance(p-value)'),
                          escape = FALSE, selection = 'none', rownames = TRUE, 
                          class = "nowrap row-border",
                          extensions = c('Buttons', 'Scroller'),
                          options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                         deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                         dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                         columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
              formatStyle('sig_p_adj',target = 'row',backgroundColor = styleEqual(c("yes","no"), c('pink', 'EBECF0'))) 
          }
        }
      }) #isolate
    }) #output$DE.species.tab.all <- renderDataTable
    
    #### Output: DE.species.dotchart.sig ####
    output$DE.species.dotchart.sig <- renderPlotly({
      isolate({
        validate(need(!is.null(variables$DE.species.2$DE_species_dotchart_sig), "Without DE dotplot"))
        
        variables$DE.species.2$DE_species_dotchart_sig
      })
    }) #output$DE.species.dotchart.sig <- renderPlotly
    
    #### Output: DE.species.maplot ####
    output$DE.species.maplot <- renderPlotly({
      isolate({
        key <- plot_MA_Vol()$feature
        
        plot_ly(data = plot_MA_Vol(),
                x = ~ as.numeric(A),
                y = ~ as.numeric(M),
                type = "scatter",
                mode = "markers",
                color = ~ sig_log2fc.pvalue_colors,
                colors = c("#4169E1", "#DDDDDD", "#FF4500"),
                # color = ~ sig_log2fc_colors,
                # colors = c("#DDDDDD","#FF4500"),
                showlegend = TRUE,
                marker = list(size = 4),
                hoverinfo = "text",
                text = ~ paste(
                  "</br>Lipid:",
                  plot_MA_Vol()$feature,
                  "</br>A value:",
                  round(as.numeric(A), 4),
                  "</br>M value:",
                  round(as.numeric(M), 4)),
                key =  ~ key,
                source = "ma") %>%
          layout(xaxis = list(title = "A = (log<sub>2</sub>(exp)+log<sub>2</sub>(ctrl))/2"),
                 yaxis = list(title = "M = log<sub>2</sub>(exp)-log<sub>2</sub>(ctrl)"),
                 title = "MA Plot",
                 #annotations = annotation,
                 legend = list(title = list(text = "log2FC Significant"),
                               orientation = 'h',
                               xanchor = "center",
                               x = 0.5,
                               y = -0.18)
          )
      }) #isolate
    }) #output$DE.species.maplot <- renderPlotly
    
    #### Output: DE.species.ma.box ####
    output$DE.species.ma.box <- renderPlotly({
      #isolate({
        eventdata <- event_data("plotly_hover", source = "ma")
        validate(need(!is.null(eventdata), "Hover over the point to show lipid's expression level of interest."))
        
        # Get point number
        lipid_id <- eventdata$key
        # Get expression level (Original)
        ma.exp <- DE_exp_non_log_data()[which(DE_exp_non_log_data()$feature == lipid_id), ]
        
        ma.box.tab <- ma.exp %>%
          gather(sample_name, value, -1) %>%
          left_join(DE_group_info(), by = 'sample_name')
        if(input$DE_data_source == 'DE_user_data'){
          exp_raw_name <- variables$DE.group.ref.group[-which(input$DE_user_ref_group%in%variables$DE.group.ref.group)]
          ma.box.tab$group[which(ma.box.tab$group=='ctrl')] <-  input$DE_user_ref_group
          ma.box.tab$group[which(ma.box.tab$group=='exp')] <-  exp_raw_name
        }
        plot_ly(data = ma.box.tab,
                x = ~ group,
                y = ~ value,
                color =  ~ group,
                text = round(ma.box.tab[,3], 4),
                #textposition = "outside",
                showlegend = FALSE,
                type = "box",
                #name = "Raw",
                boxpoints = 'all',
                jitter = 0.85,
                pointpos = 0,
                marker = list(size = 5, opacity = 0.8)) %>%
          layout(
            #xaxis = xform,
            yaxis = list(title = "Expression"),
            title = unique(ma.box.tab[,1]))
      #}) #isolate
    }) #output$DE.species.ma.box <- renderPlotly
    
    #### Output: MAPlotUI ####
    output$MAPlotUI <- renderUI({
      isolate({
        tagList(fluidRow(
          column(8, plotlyOutput("DE.species.maplot", height = '500px') %>% withSpinner()),
          column(4, plotlyOutput("DE.species.ma.box", height = '440px') %>% withSpinner())
        )) 
      }) #isolate
    }) #output$MAPlotUI <- renderUI
    
    #### Output: DE.species.volcano ####
    output$DE.species.volcano <- renderPlotly({
      
      validate(need(sum(!is.na(plot_MA_Vol()$m.log.p))==nrow(plot_MA_Vol()), "Without volcano plot"))
      key <- plot_MA_Vol()$feature
      if (input$DE_sig_p == "p"){
        plot_ly(data = plot_MA_Vol(),
                x = ~ as.numeric(M),
                y = ~ as.numeric(m.log.p),
                type = "scatter",
                mode = "markers",
                color = ~sig_log2fc.pvalue_colors,
                colors = c("#4169E1", "#DDDDDD", "#FF4500"),
                marker = list(size = 4),
                hoverinfo = "text",
                text = ~ paste(
                  "</br>Lipid:",
                  plot_MA_Vol()$feature,
                  "</br>A value:",
                  round(as.numeric(A), 4),
                  "</br>M value:",
                  round(as.numeric(M), 4), 
                  "</br>-log10(p-value):",
                  round(-log10(p_value), 4),
                  #round(p_value, 4),
                  "</br>-log10(padj):",
                  round(-log10(p_adj), 4)),
                #round(p_adj, 4)),
                key =  ~ key,
                source = "vol") %>%
          layout(xaxis = list(title = "M = log<sub>2</sub>(exp)-log<sub>2</sub>(ctrl)"),
                 yaxis = list(title = "-log<sub>10</sub>(p-value)"),
                 title = "Volcano Plot",
                 #annotations = annotation,
                 legend = list(title = list(text = "Significant lipid"),
                               orientation = 'h',
                               xanchor = "center",
                               x = 0.5,
                               y = -0.18)
          )
      }else{
        plot_ly(data = plot_MA_Vol(),
                x = ~ as.numeric(M),
                y = ~ as.numeric(-log10(p_adj)),
                type = "scatter",
                mode = "markers",
                color = ~sig_log2fc.pvalue_colors,
                colors = c("#4169E1", "#DDDDDD", "#FF4500"),
                marker = list(size = 4),
                hoverinfo = "text",
                text = ~ paste(
                  "</br>Lipid:",
                  plot_MA_Vol()$feature,
                  "</br>A value:",
                  round(as.numeric(A), 4),
                  "</br>M value:",
                  round(as.numeric(M), 4), 
                  "</br>-log10(p-value):",
                  round(-log10(p_value), 4),
                  #round(p_value, 4),
                  "</br>-log10(padj):",
                  round(-log10(p_adj), 4)),
                #round(p_adj, 4)),
                key =  ~ key,
                source = "vol") %>%
          layout(xaxis = list(title = "M = log<sub>2</sub>(exp)-log<sub>2</sub>(ctrl)"),
                 yaxis = list(title = "-log<sub>10</sub>(padj)"),
                 title = "Volcano Plot",
                 #annotations = annotation,
                 legend = list(title = list(text = "Significant lipid"),
                               orientation = 'h',
                               xanchor = "center",
                               x = 0.5,
                               y = -0.18)
          )
      }
      
      
    }) #output$DE.species.volcano <- renderPlotly
    
    
    #### Output: DE.species.vol.box ####
    output$DE.species.vol.box <- renderPlotly({
      
      eventdata <- event_data("plotly_hover", source = "vol")
      validate(need(!is.null(eventdata), "Hover over the point to show lipid's expression level of interest."))
      
      # Get point number
      lipid_id <- eventdata$key
      # Get expression level (Original)
      vol.exp <- DE_exp_non_log_data()[which(DE_exp_non_log_data()$feature == lipid_id), ]
      
      vol.box.tab <- vol.exp %>%
        gather(sample_name, value, -1) %>%
        left_join(DE_group_info(), by = 'sample_name')
      if(input$DE_data_source == 'DE_user_data'){
        exp_raw_name <- variables$DE.group.ref.group[-which(input$DE_user_ref_group%in%variables$DE.group.ref.group)]
        vol.box.tab$group[which(vol.box.tab$group=='ctrl')] <-  input$DE_user_ref_group
        vol.box.tab$group[which(vol.box.tab$group=='exp')] <-  exp_raw_name
      }
      plot_ly(data = vol.box.tab,
              x = ~ group,
              y = ~ value,
              color =  ~ group,
              text = round(vol.box.tab[,3], 4),
              #textposition = "outside",
              showlegend = FALSE,
              type = "box",
              #name = "Raw",
              boxpoints = 'all',
              jitter = 0.85,
              pointpos = 0,
              marker = list(size = 5, opacity = 0.8)) %>%
        layout(
          #xaxis = xform,
          yaxis = list(title = "Expression"),
          title = unique(vol.box.tab[,1]))
    }) #output$DE.species.vol.box <- renderPlotly
    
    #### Output: VolPlotUI ####
    output$VolPlotUI <- renderUI({
      tagList(fluidRow(
        column(8, plotlyOutput("DE.species.volcano", height = '500px') %>% withSpinner()),
        column(4, plotlyOutput("DE.species.vol.box", height = '440px') %>% withSpinner())
      ))
    }) #output$MAPlotUI <- renderUI
    
  #}) #isolate
}) #observeEvent(input$DE_analysis_start


############################################################
####  lipid species analysis: Dimensionality reduction  ####
############################################################

#######################
#### Control panel ####
#######################

#### Group assignment ####
#### number of samples ####
output$DE.species.group.count <- renderText({
  isolate({
    paste0(DE_group_count(), ' groups were detected in samples.')
  })
})

#### dbscan ####
#### update numeric input minPts ####
observe({
  updateNumericInput(session, 
                     inputId = 'DE_species_dbscan_minPts', 
                     max = (DE_sample_count()-1)
  ) #updateNumericInput
})

#### control reset button ####
observeEvent(input$DE_species_dim_redu_reset,{
  
  #### shinyjs show/hide results ####
  shinyjs::hide('DE_species_dim_redu_result_div')
  
  #### shinyjs reset control panel ####
  shinyjs::reset("DE_species_dim_redu_reset_div")
  
}) #observeEvent(input$DE_species_dim_redu_reset
shinyjs::hide('DE_species_dim_redu_result_div')

#### control start button ####
observeEvent(input$DE_species_dim_redu_start,{
  if(input$DE_data_source == 'DE_demo_data'){
    
    observeEvent(input$DE_species_dim_redu_method,{
      if(input$DE_species_dim_redu_method == 'pca'){
        feature_num_check=2
        sample_num_check=6
        shinyjs::hide('DE_species_dim_redu_result_div')
      }else if(input$DE_species_dim_redu_method == 'plsda'){
        feature_num_check=6
        sample_num_check=6
        shinyjs::hide('DE_species_dim_redu_result_div')
      }else if(input$DE_species_dim_redu_method == 'tsne'){
        feature_num_check=4
        sample_num_check=2
        shinyjs::hide('DE_species_dim_redu_result_div')
      }else{
        feature_num_check=2
        sample_num_check=2
        shinyjs::hide('DE_species_dim_redu_result_div')
      }
      
      
      if(submit_check(transform_data=DE_exp_transform_data(),sig_data=DE_species_sig(),check_NA=T,feature_num=6,sample_num=sample_num_check,sig_count=2)[[1]]!=TRUE){
        showModal(modalDialog(
          title = "Important message",
          submit_check(transform_data=DE_exp_transform_data(),sig_data=DE_species_sig(),check_NA=T,feature_num=6,sample_num=sample_num_check,sig_count=2)[[2]],
          easyClose = TRUE
        ))
      }else{
        #### shinyjs show/hide results ####
        shinyjs::show('DE_species_dim_redu_result_div')
      }
      #### Function: PCA ####
      if(input$DE_species_dim_redu_method == 'pca'){
        
        if(input$DE_species_cluster_method == 'kmeans'){
          
          variables$DE.species.pca.result <- PCA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                 scaling=input$DE_species_pca_scale, 
                                                 centering=input$DE_species_pca_center, 
                                                 cluster_method=input$DE_species_cluster_method, 
                                                 group_num = input$DE_species_kmeans_group)
          
        }else if(input$DE_species_cluster_method == 'kmedoids'){
          
          variables$DE.species.pca.result <- PCA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                 scaling=input$DE_species_pca_scale, 
                                                 centering=input$DE_species_pca_center, 
                                                 cluster_method=input$DE_species_cluster_method, 
                                                 group_num = input$DE_species_pam_group, 
                                                 var1 = input$DE_species_pam_metric)
          
        }else if(input$DE_species_cluster_method == 'hclustering'){
          
          variables$DE.species.pca.result <- PCA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                 scaling=input$DE_species_pca_scale, 
                                                 centering=input$DE_species_pca_center, 
                                                 cluster_method=input$DE_species_cluster_method, 
                                                 group_num = input$DE_species_hclust_group, 
                                                 var1 = input$DE_species_hclust_dist, 
                                                 var2 = input$DE_species_hclust_hclust)
          
        }else if(input$DE_species_cluster_method == 'dbscan'){
          
          variables$DE.species.pca.result <- PCA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                 scaling=input$DE_species_pca_scale, 
                                                 centering=input$DE_species_pca_center, 
                                                 cluster_method=input$DE_species_cluster_method, 
                                                 var1 = input$DE_species_dbscan_eps, 
                                                 var2 = input$DE_species_dbscan_minPts)
          
        }else if(input$DE_species_cluster_method == 'group_info'){
          
          variables$DE.species.pca.result <- PCA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                 scaling=input$DE_species_pca_scale, 
                                                 centering=input$DE_species_pca_center, 
                                                 cluster_method=input$DE_species_cluster_method)
          
        }
        
        #### Output: DE.species.pca.biplot ####
        output$DE.species.pca.biplot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.species.pca.result[[4]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.species.pca.result[[4]]
          })
        })
        
        #### Output: DE.species.pca.screeplot ####
        output$DE.species.pca.screeplot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.species.pca.result[[5]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.species.pca.result[[5]]
          })
        })
        
        #### Output: DE.species.pca.rotated.data ####
        output$DE.species.pca.rotated.data <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.species.pca.result[[2]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.species.pca.result[[2]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.species.pca.rotated.data <- renderDataTable
        
        #### Output: DE.species.pca.contrib.table ####
        output$DE.species.pca.contrib.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.species.pca.result[[3]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.species.pca.result[[3]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.species.pca.contrib.table <- renderDataTable
        
      }else if(input$DE_species_dim_redu_method == 'plsda'){
        
        if(input$DE_species_cluster_method == 'kmeans'){
          
          variables$DE.species.plsda.result <- PLSDA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature, 
                                                     ncomp = 2, 
                                                     scaling = input$DE_species_plsda_scale,
                                                     cluster_method = input$DE_species_cluster_method, 
                                                     group_num = input$DE_species_kmeans_group)
          
        }else if(input$DE_species_cluster_method == 'kmedoids'){
          
          variables$DE.species.plsda.result <- PLSDA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature, 
                                                     ncomp = 2, 
                                                     scaling = input$DE_species_plsda_scale,
                                                     cluster_method = input$DE_species_cluster_method, 
                                                     group_num = input$DE_species_pam_group, 
                                                     var1 = input$DE_species_pam_metric)
          
        }else if(input$DE_species_cluster_method == 'hclustering'){
          
          variables$DE.species.plsda.result <- PLSDA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature, 
                                                     ncomp = 2, 
                                                     scaling = input$DE_species_plsda_scale,
                                                     cluster_method = input$DE_species_cluster_method, 
                                                     group_num = input$DE_species_hclust_group, 
                                                     var1 = input$DE_species_hclust_dist, 
                                                     var2 = input$DE_species_hclust_hclust)
          
        }else if(input$DE_species_cluster_method == 'dbscan'){
          
          variables$DE.species.plsda.result <- PLSDA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature, 
                                                     ncomp = 2, 
                                                     scaling = input$DE_species_plsda_scale,
                                                     cluster_method = input$DE_species_cluster_method, 
                                                     var1 = input$DE_species_dbscan_eps, 
                                                     var2 = input$DE_species_dbscan_minPts)
          
        }else if(input$DE_species_cluster_method == 'group_info'){
          
          variables$DE.species.plsda.result <- PLSDA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature, 
                                                     ncomp = 2, 
                                                     scaling = input$DE_species_plsda_scale,
                                                     cluster_method = input$DE_species_cluster_method)
          
        }
        
        #### Output: DE.species.plsda.sample.plot ####
        output$DE.species.plsda.sample.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.species.plsda.result[[3]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.species.plsda.result[[3]]
          })
        })
        
        #### Output: DE.species.plsda.variable.plot ####
        output$DE.species.plsda.variable.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.species.plsda.result[[4]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.species.plsda.result[[4]]
          })
        })
        
        #### Output: DE.species.plsda.variate.table ####
        output$DE.species.plsda.variate.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.species.plsda.result[[1]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.species.plsda.result[[1]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #DE.species.plsda.variate.table <- renderDataTable
        
        #### Output: DE.species.plsda.loading.table ####
        output$DE.species.plsda.loading.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.species.plsda.result[[2]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.species.plsda.result[[2]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.species.plsda.loading.table <- renderDataTable
        
      }else if(input$DE_species_dim_redu_method == 'tsne'){
        
        if(input$DE_species_cluster_method == 'kmeans'){
          
          variables$DE.species.tsne.result <- tsne(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   pca=input$DE_species_tsne_pca, 
                                                   perplexity=input$DE_species_tsne_perplexity,
                                                   max_iter=input$DE_species_tsne_max_iter, 
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   group_num = input$DE_species_kmeans_group)
          
        }else if(input$DE_species_cluster_method == 'kmedoids'){
          
          variables$DE.species.tsne.result <- tsne(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   pca=input$DE_species_tsne_pca, 
                                                   perplexity=input$DE_species_tsne_perplexity,
                                                   max_iter=input$DE_species_tsne_max_iter, 
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   group_num = input$DE_species_pam_group, 
                                                   var1 = input$DE_species_pam_metric)
          
        }else if(input$DE_species_cluster_method == 'hclustering'){
          
          variables$DE.species.tsne.result <- tsne(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   pca=input$DE_species_tsne_pca, 
                                                   perplexity=input$DE_species_tsne_perplexity,
                                                   max_iter=input$DE_species_tsne_max_iter, 
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   group_num = input$DE_species_hclust_group, 
                                                   var1 = input$DE_species_hclust_dist, 
                                                   var2 = input$DE_species_hclust_hclust)
          
        }else if(input$DE_species_cluster_method == 'dbscan'){
          
          variables$DE.species.tsne.result <- tsne(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   pca=input$DE_species_tsne_pca, 
                                                   perplexity=input$DE_species_tsne_perplexity,
                                                   max_iter=input$DE_species_tsne_max_iter, 
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   var1 = input$DE_species_dbscan_eps, 
                                                   var2 = input$DE_species_dbscan_minPts)
          
        }else if(input$DE_species_cluster_method == 'group_info'){
          
          variables$DE.species.tsne.result <- tsne(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   pca=input$DE_species_tsne_pca, 
                                                   perplexity=input$DE_species_tsne_perplexity,
                                                   max_iter=input$DE_species_tsne_max_iter, 
                                                   cluster_method=input$DE_species_cluster_method)
          
        }
        
        #### Output: DE.species.tsne.plot ####
        output$DE.species.tsne.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.species.tsne.result[[2]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.species.tsne.result[[2]]
          })
        })
        
        #### Output: DE.species.tsne.table ####
        output$DE.species.tsne.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.species.tsne.result[[1]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.species.tsne.result[[1]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.species.tsne.table <- renderDataTable
        
        
      }else if(input$DE_species_dim_redu_method == 'umap'){
        
        if(input$DE_species_cluster_method == 'kmeans'){
          
          variables$DE.species.umap.result <- UMAP(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   n_neighbors=input$DE_species_umap_n_neighbors, 
                                                   scale=input$DE_species_umap_scale,
                                                   metric=input$DE_species_umap_metric,
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   group_num = input$DE_species_kmeans_group)
          
        }else if(input$DE_species_cluster_method == 'kmedoids'){
          
          variables$DE.species.umap.result <- UMAP(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   n_neighbors=input$DE_species_umap_n_neighbors, 
                                                   scale=input$DE_species_umap_scale,
                                                   metric=input$DE_species_umap_metric,
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   group_num = input$DE_species_pam_group, 
                                                   var1 = input$DE_species_pam_metric)
          
        }else if(input$DE_species_cluster_method == 'hclustering'){
          
          variables$DE.species.umap.result <- UMAP(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   n_neighbors=input$DE_species_umap_n_neighbors, 
                                                   scale=input$DE_species_umap_scale,
                                                   metric=input$DE_species_umap_metric,
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   group_num = input$DE_species_hclust_group, 
                                                   var1 = input$DE_species_hclust_dist, 
                                                   var2 = input$DE_species_hclust_hclust)
          
        }else if(input$DE_species_cluster_method == 'dbscan'){
          
          variables$DE.species.umap.result <- UMAP(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   n_neighbors=input$DE_species_umap_n_neighbors, 
                                                   scale=input$DE_species_umap_scale,
                                                   metric=input$DE_species_umap_metric, 
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   var1 = input$DE_species_dbscan_eps, 
                                                   var2 = input$DE_species_dbscan_minPts)
          
        }else if(input$DE_species_cluster_method == 'group_info'){
          
          variables$DE.species.umap.result <- UMAP(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   n_neighbors=input$DE_species_umap_n_neighbors, 
                                                   scale=input$DE_species_umap_scale,
                                                   metric=input$DE_species_umap_metric,
                                                   cluster_method=input$DE_species_cluster_method)
          
        }
        
        #### Output: DE.species.umap.plot ####
        output$DE.species.umap.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.species.umap.result[[2]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.species.umap.result[[2]]
          })
        })
        
        #### Output: DE.species.umap.table ####
        output$DE.species.umap.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.species.umap.result[[1]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.species.umap.result[[1]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.species.umap.table <- renderDataTable
        
      }
      
    }) #observeEvent(input$DE_species_dim_redu_method
    
  }else if(input$DE_data_source == 'DE_user_data'){
    
    observeEvent(input$DE_species_dim_redu_method,{
      if(input$DE_species_dim_redu_method == 'pca'){
        feature_num_check=2
        sample_num_check=6
        shinyjs::hide('DE_species_dim_redu_result_div')
      }else if(input$DE_species_dim_redu_method == 'plsda'){
        feature_num_check=6
        sample_num_check=6
        shinyjs::hide('DE_species_dim_redu_result_div')
      }else if(input$DE_species_dim_redu_method == 'tsne'){
        feature_num_check=4
        sample_num_check=2
        shinyjs::hide('DE_species_dim_redu_result_div')
      }else{
        feature_num_check=2
        sample_num_check=2
        shinyjs::hide('DE_species_dim_redu_result_div')
      }
      
      
      if(submit_check(transform_data=DE_exp_transform_data(),sig_data=DE_species_sig(),check_NA=T,feature_num=6,sample_num=sample_num_check,sig_count=2)[[1]]!=TRUE){
        showModal(modalDialog(
          title = "Important message",
          submit_check(transform_data=DE_exp_transform_data(),sig_data=DE_species_sig(),check_NA=T,feature_num=6,sample_num=sample_num_check,sig_count=2)[[2]],
          easyClose = TRUE
        ))
      }else{
        #### shinyjs show/hide results ####
        shinyjs::show('DE_species_dim_redu_result_div')
      }
      
      #### Function: PCA ####
      if(input$DE_species_dim_redu_method == 'pca'){
        
        if(input$DE_species_cluster_method == 'kmeans'){
          
          variables$DE.species.pca.result <- PCA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                 scaling=input$DE_species_pca_scale, 
                                                 centering=input$DE_species_pca_center, 
                                                 cluster_method=input$DE_species_cluster_method, 
                                                 group_num = input$DE_species_kmeans_group,
                                                 insert_ref_group = NULL,
                                                 ref_group = NULL)
          
        }else if(input$DE_species_cluster_method == 'kmedoids'){
          
          variables$DE.species.pca.result <- PCA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                 scaling=input$DE_species_pca_scale, 
                                                 centering=input$DE_species_pca_center, 
                                                 cluster_method=input$DE_species_cluster_method, 
                                                 group_num = input$DE_species_pam_group, 
                                                 var1 = input$DE_species_pam_metric,
                                                 insert_ref_group = NULL,
                                                 ref_group = NULL)
          
        }else if(input$DE_species_cluster_method == 'hclustering'){
          
          variables$DE.species.pca.result <- PCA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                 scaling=input$DE_species_pca_scale, 
                                                 centering=input$DE_species_pca_center, 
                                                 cluster_method=input$DE_species_cluster_method, 
                                                 group_num = input$DE_species_hclust_group, 
                                                 var1 = input$DE_species_hclust_dist, 
                                                 var2 = input$DE_species_hclust_hclust,
                                                 insert_ref_group = NULL,
                                                 ref_group = NULL)
          
        }else if(input$DE_species_cluster_method == 'dbscan'){
          
          variables$DE.species.pca.result <- PCA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                 scaling=input$DE_species_pca_scale, 
                                                 centering=input$DE_species_pca_center, 
                                                 cluster_method=input$DE_species_cluster_method, 
                                                 var1 = input$DE_species_dbscan_eps, 
                                                 var2 = input$DE_species_dbscan_minPts,
                                                 insert_ref_group = NULL,
                                                 ref_group = NULL)
          
        }else if(input$DE_species_cluster_method == 'group_info'){
          
          variables$DE.species.pca.result <- PCA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                 scaling=input$DE_species_pca_scale, 
                                                 centering=input$DE_species_pca_center, 
                                                 cluster_method=input$DE_species_cluster_method,
                                                 insert_ref_group = input$DE_user_ref_group,
                                                 ref_group = variables$DE.group.ref.group)
          
        }
        
        #### Output: DE.species.pca.biplot ####
        output$DE.species.pca.biplot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.species.pca.result[[4]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.species.pca.result[[4]]
          })
        })
        
        #### Output: DE.species.pca.screeplot ####
        output$DE.species.pca.screeplot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.species.pca.result[[5]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.species.pca.result[[5]]
          })
        })
        
        #### Output: DE.species.pca.rotated.data ####
        output$DE.species.pca.rotated.data <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.species.pca.result[[2]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.species.pca.result[[2]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.species.pca.rotated.data <- renderDataTable
        
        #### Output: DE.species.pca.contrib.table ####
        output$DE.species.pca.contrib.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.species.pca.result[[3]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.species.pca.result[[3]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.species.pca.contrib.table <- renderDataTable
        
      }else if(input$DE_species_dim_redu_method == 'plsda'){
        
        if(input$DE_species_cluster_method == 'kmeans'){
          
          variables$DE.species.plsda.result <- PLSDA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature, 
                                                     ncomp = 2, 
                                                     scaling = input$DE_species_plsda_scale,
                                                     cluster_method = input$DE_species_cluster_method, 
                                                     group_num = input$DE_species_kmeans_group,
                                                     insert_ref_group = input$DE_user_ref_group,
                                                     ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_species_cluster_method == 'kmedoids'){
          
          variables$DE.species.plsda.result <- PLSDA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature, 
                                                     ncomp = 2, 
                                                     scaling = input$DE_species_plsda_scale,
                                                     cluster_method = input$DE_species_cluster_method, 
                                                     group_num = input$DE_species_pam_group, 
                                                     var1 = input$DE_species_pam_metric,
                                                     insert_ref_group = input$DE_user_ref_group,
                                                     ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_species_cluster_method == 'hclustering'){
          
          variables$DE.species.plsda.result <- PLSDA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature, 
                                                     ncomp = 2, 
                                                     scaling = input$DE_species_plsda_scale,
                                                     cluster_method = input$DE_species_cluster_method, 
                                                     group_num = input$DE_species_hclust_group, 
                                                     var1 = input$DE_species_hclust_dist, 
                                                     var2 = input$DE_species_hclust_hclust,
                                                     insert_ref_group = input$DE_user_ref_group,
                                                     ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_species_cluster_method == 'dbscan'){
          
          variables$DE.species.plsda.result <- PLSDA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature, 
                                                     ncomp = 2, 
                                                     scaling = input$DE_species_plsda_scale,
                                                     cluster_method = input$DE_species_cluster_method, 
                                                     var1 = input$DE_species_dbscan_eps, 
                                                     var2 = input$DE_species_dbscan_minPts,
                                                     insert_ref_group = input$DE_user_ref_group,
                                                     ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_species_cluster_method == 'group_info'){
          
          variables$DE.species.plsda.result <- PLSDA(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature, 
                                                     ncomp = 2, 
                                                     scaling = input$DE_species_plsda_scale,
                                                     cluster_method = input$DE_species_cluster_method,
                                                     insert_ref_group = input$DE_user_ref_group,
                                                     ref_group = variables$DE.group.ref.group)
          
        }
        
        #### Output: DE.species.plsda.sample.plot ####
        output$DE.species.plsda.sample.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.species.plsda.result[[3]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.species.plsda.result[[3]]
          })
        })
        
        #### Output: DE.species.plsda.variable.plot ####
        output$DE.species.plsda.variable.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.species.plsda.result[[4]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.species.plsda.result[[4]]
          })
        })
        
        #### Output: DE.species.plsda.variate.table ####
        output$DE.species.plsda.variate.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.species.plsda.result[[1]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.species.plsda.result[[1]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #DE.species.plsda.variate.table <- renderDataTable
        
        #### Output: DE.species.plsda.loading.table ####
        output$DE.species.plsda.loading.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.species.plsda.result[[2]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.species.plsda.result[[2]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.species.plsda.loading.table <- renderDataTable
        
      }else if(input$DE_species_dim_redu_method == 'tsne'){
        
        if(input$DE_species_cluster_method == 'kmeans'){
          
          variables$DE.species.tsne.result <- tsne(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   pca=input$DE_species_tsne_pca, 
                                                   perplexity=input$DE_species_tsne_perplexity,
                                                   max_iter=input$DE_species_tsne_max_iter, 
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   group_num = input$DE_species_kmeans_group,
                                                   insert_ref_group = input$DE_user_ref_group,
                                                   ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_species_cluster_method == 'kmedoids'){
          
          variables$DE.species.tsne.result <- tsne(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   pca=input$DE_species_tsne_pca, 
                                                   perplexity=input$DE_species_tsne_perplexity,
                                                   max_iter=input$DE_species_tsne_max_iter, 
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   group_num = input$DE_species_pam_group, 
                                                   var1 = input$DE_species_pam_metric,
                                                   insert_ref_group = input$DE_user_ref_group,
                                                   ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_species_cluster_method == 'hclustering'){
          
          variables$DE.species.tsne.result <- tsne(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   pca=input$DE_species_tsne_pca, 
                                                   perplexity=input$DE_species_tsne_perplexity,
                                                   max_iter=input$DE_species_tsne_max_iter, 
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   group_num = input$DE_species_hclust_group, 
                                                   var1 = input$DE_species_hclust_dist, 
                                                   var2 = input$DE_species_hclust_hclust,
                                                   insert_ref_group = input$DE_user_ref_group,
                                                   ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_species_cluster_method == 'dbscan'){
          
          variables$DE.species.tsne.result <- tsne(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   pca=input$DE_species_tsne_pca, 
                                                   perplexity=input$DE_species_tsne_perplexity,
                                                   max_iter=input$DE_species_tsne_max_iter, 
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   var1 = input$DE_species_dbscan_eps, 
                                                   var2 = input$DE_species_dbscan_minPts,
                                                   insert_ref_group = input$DE_user_ref_group,
                                                   ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_species_cluster_method == 'group_info'){
          
          variables$DE.species.tsne.result <- tsne(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   pca=input$DE_species_tsne_pca, 
                                                   perplexity=input$DE_species_tsne_perplexity,
                                                   max_iter=input$DE_species_tsne_max_iter, 
                                                   cluster_method=input$DE_species_cluster_method,
                                                   insert_ref_group = input$DE_user_ref_group,
                                                   ref_group = variables$DE.group.ref.group)
          
        }
        
        #### Output: DE.species.tsne.plot ####
        output$DE.species.tsne.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.species.tsne.result[[2]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.species.tsne.result[[2]]
          })
        })
        
        #### Output: DE.species.tsne.table ####
        output$DE.species.tsne.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.species.tsne.result[[1]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.species.tsne.result[[1]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.species.tsne.table <- renderDataTable
        
        
      }else if(input$DE_species_dim_redu_method == 'umap'){
        
        if(input$DE_species_cluster_method == 'kmeans'){
          
          variables$DE.species.umap.result <- UMAP(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   n_neighbors=input$DE_species_umap_n_neighbors, 
                                                   scale=input$DE_species_umap_scale,
                                                   metric=input$DE_species_umap_metric,
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   group_num = input$DE_species_kmeans_group,
                                                   insert_ref_group = input$DE_user_ref_group,
                                                   ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_species_cluster_method == 'kmedoids'){
          
          variables$DE.species.umap.result <- UMAP(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   n_neighbors=input$DE_species_umap_n_neighbors, 
                                                   scale=input$DE_species_umap_scale,
                                                   metric=input$DE_species_umap_metric,
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   group_num = input$DE_species_pam_group, 
                                                   var1 = input$DE_species_pam_metric,
                                                   insert_ref_group = input$DE_user_ref_group,
                                                   ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_species_cluster_method == 'hclustering'){
          
          variables$DE.species.umap.result <- UMAP(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   n_neighbors=input$DE_species_umap_n_neighbors, 
                                                   scale=input$DE_species_umap_scale,
                                                   metric=input$DE_species_umap_metric,
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   group_num = input$DE_species_hclust_group, 
                                                   var1 = input$DE_species_hclust_dist, 
                                                   var2 = input$DE_species_hclust_hclust,
                                                   insert_ref_group = input$DE_user_ref_group,
                                                   ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_species_cluster_method == 'dbscan'){
          
          variables$DE.species.umap.result <- UMAP(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   n_neighbors=input$DE_species_umap_n_neighbors, 
                                                   scale=input$DE_species_umap_scale,
                                                   metric=input$DE_species_umap_metric, 
                                                   cluster_method=input$DE_species_cluster_method, 
                                                   var1 = input$DE_species_dbscan_eps, 
                                                   var2 = input$DE_species_dbscan_minPts,
                                                   insert_ref_group = input$DE_user_ref_group,
                                                   ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_species_cluster_method == 'group_info'){
          
          variables$DE.species.umap.result <- UMAP(DE_exp_transform_data(), DE_group_info(), DE_species_sig()$feature,
                                                   n_neighbors=input$DE_species_umap_n_neighbors, 
                                                   scale=input$DE_species_umap_scale,
                                                   metric=input$DE_species_umap_metric,
                                                   cluster_method=input$DE_species_cluster_method,
                                                   insert_ref_group = input$DE_user_ref_group,
                                                   ref_group = variables$DE.group.ref.group)
          
        }
        
        #### Output: DE.species.umap.plot ####
        output$DE.species.umap.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.species.umap.result[[2]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.species.umap.result[[2]]
          })
        })
        
        #### Output: DE.species.umap.table ####
        output$DE.species.umap.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.species.umap.result[[1]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.species.umap.result[[1]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.species.umap.table <- renderDataTable
        
      }
    }) #observeEvent(input$DE_species_dim_redu_method
    
  }
  
})

##### PCA #####
#### Function: PCA_variable ####
DE.species.pca.variable <- reactive({
  if(!is.null(variables$DE.species.pca.result[[1]])){
    PCA_variable(variables$DE.species.pca.result[[1]],
                 as.numeric(input$DE_species_pca_variable_topN))
  }else{
    NULL
  }
  
})

#### Function: PCA_contrib ####
DE.species.pca.contrib <- reactive({
  if(!is.null(variables$DE.species.pca.result[[1]])){
    if(input$DE_species_pca_contrib_PC == '1_2'){
      PCA_contrib(variables$DE.species.pca.result[[1]],
                  n_PC = c(1, 2),
                  as.numeric(input$DE_species_pca_contrib_topN))
    }else{
      PCA_contrib(variables$DE.species.pca.result[[1]],
                  n_PC = as.numeric(input$DE_species_pca_contrib_PC),
                  as.numeric(input$DE_species_pca_contrib_topN))
    }
  }else{
    NULL
  }
  
})

#### Output: DE.species.pca.variable ####
output$DE.species.pca.variable <- renderPlotly({
  #isolate({
    validate(need(!is.null(DE.species.pca.variable()[[1]]), "Plot not showing. Missing value imputation is recommended."))
    DE.species.pca.variable()[[1]]
  #})
})

#### Output: DE.species.pca.contrib ####
output$DE.species.pca.contrib <- renderPlotly({
  validate(need(!is.null(DE.species.pca.contrib()[[1]]), "Plot not showing. Missing value imputation is recommended."))
  DE.species.pca.contrib()[[1]]
})

#### Update sliderInput ####
observe({
  VALUE <- ifelse(nrow(variables$DE.species.pca.result[[3]]) < 10, nrow(variables$DE.species.pca.result[[3]]), 10)
  updateSliderInput(session, 
                    inputId = 'DE_species_pca_variable_topN', 
                    value = VALUE,
                    max = nrow(variables$DE.species.pca.result[[3]]))
  updateSliderInput(session, 
                    inputId = 'DE_species_pca_contrib_topN', 
                    value = VALUE,
                    max = nrow(variables$DE.species.pca.result[[3]]))
})

##### tSNE #####
observe({
  VALUE1 <- ifelse(DE_sample_count()%%3 == 0, floor(DE_sample_count()/3)-1, floor(DE_sample_count()/3))
  VALUE2 <- ifelse(VALUE1 < 5, VALUE1, 5)
  updateNumericInput(session, 
                     inputId = 'DE_species_tsne_perplexity', 
                     value = VALUE2,
                     max = VALUE1)
})

##### UMAP ######
observe({
  VALUE <- ifelse(DE_sample_count() < 15, DE_sample_count(), 15)
  updateNumericInput(session, 
                     inputId = 'DE_species_umap_n_neighbors', 
                     value = VALUE,
                     max = DE_sample_count())
})


#############################################################
#####  lipid species analysis: Hierarchical clustering  #####
#############################################################

#### control reset button ####
observeEvent(input$DE_species_cluster_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('DE_species_heatmap_div')
  
  #### shinyjs reset ####
  shinyjs::reset("DE_species_cluster_reset_div")
  
})

#### control start button ####
observeEvent(input$DE_species_cluster_start, {
  if(input$DE_species_cluster_by=='sig_lipid' & 
     submit_check(transform_data=DE_exp_transform_data(),sig_data=DE_species_sig(),check_NA=T,feature_num=2,sig_count=2)[[1]]!=TRUE){
    shinyjs::hide('DE_species_heatmap_div')
    showModal(modalDialog(
      title = "Important message",
      submit_check(transform_data=DE_exp_transform_data(),sig_data=DE_species_sig(),check_NA=T,feature_num=6,sig_count=2)[[2]],
      easyClose = TRUE
    ))
  }else if(input$DE_species_cluster_by=='all_lipid' & 
           submit_check(transform_data=DE_exp_transform_data(),check_NA=T,feature_num=2)[[1]]!=TRUE){
    shinyjs::hide('DE_species_heatmap_div')
    showModal(modalDialog(
      title = "Important message",
      submit_check(transform_data=DE_exp_transform_data(),check_NA=T,feature_num=2)[[2]],
      easyClose = TRUE
    ))
  }else{
    #### shinyjs show/hide results ####
    shinyjs::show('DE_species_heatmap_div')
    shinyjs::show('DE.species.heatmap')
    shinyjs::show('DE.species.heatmap.matrix')
    #### Function: Hclustering ####
    isolate({
      
      if(input$DE_data_source == 'DE_demo_data'){
        if(!is.null(DE_lipid_char_table()) & input$DE_species_sidecolor != 'none'){
          variables$DE.species.hclustering <- Hclustering(DE_exp_transform_data(), DE_species_sig(), DE_group_info(),
                                                          DE_lipid_char_table(), char_var = input$DE_species_sidecolor,
                                                          distfun = input$DE_species_dist, hclustfun = input$DE_species_hclust)
        }else{
          variables$DE.species.hclustering <- Hclustering(DE_exp_transform_data(), DE_species_sig(), DE_group_info(),
                                                          lipid_char_table = NULL, char_var = NULL,
                                                          distfun = input$DE_species_dist, hclustfun = input$DE_species_hclust)
        }
      }else if(input$DE_data_source == 'DE_user_data'){
        if(!is.null(DE_lipid_char_table()) & input$DE_species_sidecolor != 'none'){
          variables$DE.species.hclustering <- Hclustering(DE_exp_transform_data(), DE_species_sig(), DE_group_info(),
                                                          DE_lipid_char_table(), char_var = input$DE_species_sidecolor,
                                                          distfun = input$DE_species_dist, hclustfun = input$DE_species_hclust,
                                                          insert_ref_group = input$DE_user_ref_group,
                                                          ref_group = variables$DE.group.ref.group)
        }else{
          variables$DE.species.hclustering <- Hclustering(DE_exp_transform_data(), DE_species_sig(), DE_group_info(),
                                                          lipid_char_table = NULL, char_var = NULL,
                                                          distfun = input$DE_species_dist, hclustfun = input$DE_species_hclust,
                                                          insert_ref_group = input$DE_user_ref_group,
                                                          ref_group = variables$DE.group.ref.group)
        }
      }
      
      #### Output: DE.species.heatmap ####
      output$DE.species.heatmap <- renderIheatmap({
        
        isolate({
          
          if(input$DE_species_cluster_by == 'all_lipid'){
            validate(need(!is.null(variables$DE.species.hclustering$all.lipid), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.species.hclustering$all.lipid
          }else if(input$DE_species_cluster_by == 'sig_lipid'){
            validate(need(!is.null(variables$DE.species.hclustering$sig.lipid), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.species.hclustering$sig.lipid
          }
          
        }) #isolate
        
      }) #output$DE.species.heatmap <- renderPlotly
      
      #### Output: DE.species.heatmap.matrix ####
      if(input$DE_species_cluster_by == 'all_lipid'){
        output$DE.species.heatmap.matrix <- downloadHandler(
          filename = function() {
            paste0(input$DE_species_dist, '_', input$DE_species_hclust, "_all", ".csv")
          },
          content = function(file) {
            write.csv(variables$DE.species.hclustering$all.lipid.data, file)
          }
        ) #output$DE.species.heatmap.matrix
      }else if(input$DE_species_cluster_by == 'sig_lipid'){
        output$DE.species.heatmap.matrix <- downloadHandler(
          filename = function() {
            paste0(input$DE_species_dist, '_', input$DE_species_hclust, "_sig",".csv")
          },
          content = function(file) {
            write.csv(variables$DE.species.hclustering$sig.lipid.data, file)
          }
        ) #output$DE.species.heatmap.matrix
      }
      
    }) #isolate
    
  }
}) #observeEvent(input$DE_species_cluster_start

#### update DE species cluster side color select input ####
observe({
  if(!is.null(DE_lipid_char_table())){
    FA.char <- grep(pattern = 'FA_', x = colnames(DE_lipid_char_table()), value = T)
    lipid.char <- colnames(DE_lipid_char_table())[-1]
    select.char <- setdiff(lipid.char, FA.char)
    updateSelectInput(session, "DE_species_sidecolor",
                      choices =  c('none', select.char),
                      selected = select.char[1])
  }
}) #observe

#### control disable/enable side color select input ####
observe({
  if(input$DE_data_source == 'DE_user_data' &
     (is.null(input$DE_user_char) || is.null(variables$DE.lipid.char.tab.user))){
    shinyjs::disable("DE_species_sidecolor")
  }else{
    shinyjs::enable("DE_species_sidecolor")
  }
}) #observe


###################################################################
#####  lipid species analysis: Lipid characteristic analysis  #####
###################################################################

#### update DE species lipid characteristic select input ####
observe({
  if(!is.null(DE_lipid_char_table())){
    lipid.char <- colnames(DE_lipid_char_table())[-1]
    updateSelectInput(session, "DE_species_lipid_char",
                      choices =  lipid.char)
  }
})

#### Function: Sig_lipid_feature ####
sig_lipid_feature <- reactive({
  
  if(input$DE_species_lipid_char %in% grep(pattern = 'FA_', x = colnames(DE_lipid_char_table()), value = T)){
    lipid_char_table_ga <- lipid_char_table_gather(DE_lipid_char_table(), input$DE_species_lipid_char)
  }else{
    lipid_char_table_ga <- DE_lipid_char_table()
  }
  
  Sig_lipid_feature(DE_species_sig(), lipid_char_table_ga,
                    char_var = input$DE_species_lipid_char, sig_FC = input$DE_fc)
  # Sig_lipid_feature(DE_species_sig(), DE_lipid_char_table(),
  #                   char_var = input$DE_species_lipid_char, sig_FC = input$DE_fc)
})
observeEvent(input$DE_species_list,{
  if (input$DE_species_list == 'Characteristics association' & 
      submit_check(sig_data=DE_species_sig(),sig_count=1)[[1]]!=TRUE){
    showModal(modalDialog(
      title = "Important message",
      submit_check(sig_data=DE_species_sig(),sig_count=1)[[2]],
      easyClose = TRUE
    ))
    shinyjs::hide('DE_species_lipid_char')
    shinyjs::hide('DE.species.lipid.char.bar')
    shinyjs::hide('DE.species.lipid.char.dot')
    shinyjs::hide('DE.species.lipid.char.word')
    
  }
})

#### Output: DE.species.lipid.char.bar ####
output$DE.species.lipid.char.bar <- renderPlotly({
  sig_lipid_feature()$barPlot
})

#### Output: DE.species.lipid.char.dot ####
output$DE.species.lipid.char.dot <- renderPlotly({
  sig_lipid_feature()$lolipop
})

#### Output: DE.species.lipid.char.word ####
output$DE.species.lipid.char.word <- renderHwordcloud({
  sig_lipid_feature()$word
})


#########################################################
#####  lipid species analysis: Enrichment analysis  #####
#########################################################

#### update DE species lipid characteristic select input ####
observe({
  if(!is.null(DE_lipid_char_table())){
    lipid.char <- colnames(DE_lipid_char_table())[-1]
    updateSelectInput(session, "DE_species_enrichment_lipid_char",
                      choices =  lipid.char)
  }
})

#### control reset button ####
observeEvent(input$DE_species_enrichment_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('DE_species_enrichment_result_div')
  shinyjs::hide('DE_species_enrichment_pathview_start_div')
  shinyjs::hide('DE_species_enrichment_pathview_result_div')
  
  #### shinyjs reset ####
  shinyjs::reset("DE_species_enrichment_reset_div")
  
}) #observeEvent(input$DE_species_enrichment_reset

#### control start button ####
observeEvent(input$DE_species_enrichment_start, {
  if(submit_check(sig_data=DE_species_sig(),sig_count=5)[[1]]!=TRUE){
    showModal(modalDialog(
      title = "Important message",
      submit_check(sig_data=DE_species_sig(),sig_count=5)[[2]],
      easyClose = TRUE
    ))
  }else{
    #### shinyjs show/hide results ####
    shinyjs::show('DE_species_enrichment_result_div')
    
    #### Function: Enrichment ####
    isolate({
      
      if(nrow(DE_species_sig()) == 0){
        variables$DE.enrichment <- NULL
      }else{
        
        if(input$DE_species_enrichment_lipid_char %in% grep(pattern = 'FA_', x = colnames(DE_lipid_char_table()), value = T)){
          lipid_char_table_ga <- lipid_char_table_gather(DE_lipid_char_table(), input$DE_species_enrichment_lipid_char)
          
        }else{
          lipid_char_table_ga <- DE_lipid_char_table()
        }
        
        variables$DE.enrichment <- Enrichment(DE_species_sig(), lipid_char_table_ga, 
                                              char_var = input$DE_species_enrichment_lipid_char, 
                                              sig_pvalue = input$DE_species_enrichment_pval)
        
        #### Output: DE.species.enrichment.barplot ####
        output$DE.species.enrichment.barplot <- renderPlotly({
          
          validate(need(!is.null(variables$DE.enrichment$enrich_char_barplot), "Plot not showing."))
          
          variables$DE.enrichment$enrich_char_barplot
        })
        
        #### Output: DE.species.enrichment.table ####
        output$DE.species.enrichment.table <- renderDataTable(server = FALSE,{
          
          validate(need(!is.null(DE_enrich_char()), "Without enrichment result"))
          
          DT::datatable(DE_enrich_char()[-5] %>% mutate_if(is.numeric, ~round(., 5)),
                        #caption = 'Lipid expression data',
                        colnames = c('condition', input$DE_species_enrichment_lipid_char, 'significant lipids', 'total lipids', '-log10(p-value)', 'significance'),
                        escape = FALSE, selection = 'none', rownames = TRUE, 
                        class = "nowrap row-border",
                        extensions = c('Buttons', 'Scroller'),
                        options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                       deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                       dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                       columnDefs = list(list(className = 'dt-center', targets = "_all"))))
          
        }) #output$DE.species.enrichment.table <- renderDataTable
        
        sig_enrich_class <- DE_enrich_char() %>%
          filter(significant == 'YES') %>% 
          distinct(characteristic) %>% .$characteristic
        
        if(input$DE_species_enrichment_lipid_char == "class" & length(sig_enrich_class) > 0){
          shinyjs::show('DE_species_enrichment_pathview_start_div')
        }else{
          shinyjs::hide('DE_species_enrichment_pathview_start_div')
        }
      }
      
    }) #isolate
  }
  
}) #observeEvent(input$DE_species_enrichment_start
shinyjs::hide('DE_species_enrichment_pathview_result_div')

#### control pathview start button ####
observeEvent(input$DE_species_enrichment_pathview, {
  
  shinyjs::show('DE_species_enrichment_pathview_result_div')
  
  sig_enrich_class <- DE_enrich_char() %>%
    filter(significant == 'YES') %>% 
    distinct(characteristic) %>% .$characteristic
  
  if(length(sig_enrich_class) > 0){
    variables$DE.lipid.gene.path <- readRDS('www/mapping_table/lipid_gene_path.rds')
    variables$DE.pathway.gene.list <- readRDS('www/mapping_table/pathway_gene_list.rds')
    
    KEGG_pathway_tab <- variables$DE.lipid.gene.path %>%
      filter(key_name %in% sig_enrich_class, DB == 'KEGG') %>% 
      filter(!is.na(path_id)) %>%
      mutate(path_id = str_remove_all(path_id, ':')) %>% 
      distinct(key_name, KEGG_ID, path_id, path_name) #%>% 
    # group_by(key_name, KEGG_ID, path_id, path_name) %>% 
    # summarise(gene = paste(gene_name, collapse = ", "))
    
    colnames(KEGG_pathway_tab) <- c('Lipid class name', 'KEGG ID', 'Pathway ID', 'Pathway name')
    
    #### Function: pathview function ####
    
    PATH <- getwd()
    print(paste('1.getwd', PATH))
    
    result <- pathview_function(sig_enrich_class, path = file.path(getwd(), 'www/tmp'),
                                variables$DE.lipid.gene.path,
                                variables$DE.pathway.gene.list)
    print(paste('2.pathview', getwd()))
    setwd(PATH)
    print(paste('3.setwd', PATH))
    
    #### Output: DE.species.enrichment.kegg.pathway ####
    output$DE.species.enrichment.kegg.pathway <-  renderReactable({
      
      validate(need(!is.null(KEGG_pathway_tab), "Without enrichment result"))
      
      reactable(KEGG_pathway_tab,
                selection = "single",
                onClick = "select",
                selectionId = "pathid",
                searchable = TRUE,
                columns = list(
                  gene = colDef(minWidth = 200)),
                defaultColDef = colDef(
                  minWidth = 70,headerStyle = list(background = "#f7f7f8"))
      )
    }) #output$DE.species.enrichment.kegg.pathway <- renderDataTable
    
    #         output$DE.species.enrichment.kegg.pathway <- renderDataTable(server = FALSE,{
    #             
    #             validate(need(!is.null(KEGG_pathway_tab), "Without enrichment result"))
    #             
    #             DT::datatable(KEGG_pathway_tab,
    #                           escape = FALSE, selection = 'none', rownames = TRUE,
    #                           #caption = 'Lipid expression data',
    #                           options = list(scrollX = TRUE, pageLength = 5, autoWidth = TRUE), 
    #                           callback = JS(
    #                               'table.on("click.dt", "tr", function() {
    # var data=table.row(this).data();
    # Shiny.onInputChange("pathid", data[3]);
    #     })'
    #                           ) #JS
    #             ) #DT::datatable
    #             
    #         }) #output$DE.species.enrichment.kegg.pathway <- renderDataTable
    
    #### Output: DE.species.enrichment.kegg.pathview ####
    observeEvent(input$pathid, {
      
      #PATH <- getwd()
      output$DE.species.enrichment.kegg.pathview <- renderImage({
        
        #filename <- normalizePath(file.path('./www/images', paste0(input$pathid, '.pathview.png')))
        #filename <- paste0(input$pathid, '.pathview.png')
        filename<-normalizePath(file.path(getwd(), 'www/tmp', paste0(as.character(KEGG_pathway_tab[input$pathid,'Pathway ID']), '.pathview.png')))
        
        list(src = filename, width = '100%')
        
      }, deleteFile = F)
      
      #setwd(PATH)
      
    }) #observeEvent(input$pathid, 
    
  } #if(length(sig_enrich_class) > 0)
  
}) #observeEvent(input$DE_species_enrichment_pathview


###########################################
###########################################
#####  Tab2: Lipid category analysis  #####
###########################################
###########################################

################################################
####  Lipid category analysis: DE analysis  ####
################################################

#######################
#### Control panel ####
#######################

#### number of samples ####
# output$DE.class.group.count <- renderText({
#   paste0(DE_group_count(), ' groups were detected in samples.')
# })

# #### paired or not paired ####
# output$DE.class.pair.detect <- renderText({
#   if(ncol(variables$) == 3 | length()){
#       paste0('Samples are paired.')
#   }else{
#   paste0('Samples are not paired.')
#   }
# })


#### control reset button ####
observeEvent(input$DE_class_analysis_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('DE_class_analysis_result_div')
  shinyjs::hide('DE_class_split_result_div')
  #shinyjs::hide('DE_class_dim_redu_result_div')
  #shinyjs::hide('DE.class.heatmap')
  
  #### shinyjs reset control panel ####
  shinyjs::reset("DE_class_analysis_reset_div")
  
  hideTab(inputId = 'DE_specific_list', target = 'Dimensionality reduction')
  hideTab(inputId = 'DE_specific_list', target = 'Hierarchical clustering')
  
}) #observeEvent(input$DE_analysis_reset


#### control start button ####
observeEvent(input$DE_class_analysis_start, {
  
  #### shiny show/hide tab ####
  showTab(inputId = 'DE_specific_list', target = 'Dimensionality reduction')
  showTab(inputId = 'DE_specific_list', target = 'Hierarchical clustering')
  
  #### shinyjs show/hide results ####
  shinyjs::show('DE_class_analysis_result_div')
  shinyjs::hide('DE_class_dim_redu_result_div')
  shinyjs::hide('DE.class.heatmap')
  
  # if(variables$DE.class.split.char != 'none'){
  #     shinyjs::show('DE_class_split_result_div')
  # }else{
  #     shinyjs::hide('DE_class_split_result_div')
  # }
  
  #isolate({
    
    # #### DE_class_exp_data() ####
    # DE_class_exp_data <- reactive({
    #   #### Function: Species2Char ####
    #   Species2Char(DE_exp_data(), DE_lipid_char_table(), input$DE_class_analysis_char)
    # })
    # 
    # #### DE_class_exp_transform_data() ####
    # DE_class_exp_transform_data <- reactive({
    #   #### Function: data_process ####
    #   if(input$DE_data_source == 'DE_demo_data'){
    #     DE_class_exp_data()
    #   }else if(input$DE_data_source == 'DE_user_data'){
    #     data_process(DE_class_exp_data(), 
    #                  exclude_var_missing=input$DE_rm_NA, 
    #                  missing_pct_limit=input$DE_rm_NA_pct,
    #                  replace_zero=T, zero2what='NA', xmin=0.5,
    #                  replace_NA=input$DE_rp_NA, NA2what=input$DE_fill_NA, ymin=input$DE_fill_min,
    #                  pct_transform=input$DE_pct_trans,
    #                  data_transform=input$DE_log_trans, trans_type='log',
    #                  centering=F,
    #                  scaling=F)
    #   }
    # })
    # 
    # #### DE_class_exp_non_log_data() ####
    # DE_class_exp_non_log_data <- reactive({
    #   #### Function: data_process ####
    #   if(input$DE_data_source == 'DE_demo_data'){
    #     DE_class_exp_data()
    #   }else if(input$DE_data_source == 'DE_user_data'){
    #     data_process(DE_class_exp_data(), 
    #                  exclude_var_missing=input$DE_rm_NA, 
    #                  missing_pct_limit=input$DE_rm_NA_pct,
    #                  replace_zero=T, zero2what='NA', xmin=0.5,
    #                  replace_NA=input$DE_rp_NA, NA2what=input$DE_fill_NA, ymin=input$DE_fill_min,
    #                  pct_transform=input$DE_pct_trans,
    #                  data_transform=F, trans_type='log',
    #                  centering=F,
    #                  scaling=F)
    #   }
    # })
    # 
    
    #### function: DE_char_2() ####
    if(input$DE_data_source == 'DE_demo_data'){
      variables$DE.char.2 <- DE_char_2(DE_class_exp_non_log_data(),
                                       data_transform=T,
                                       #DE_lipid_char_table(),
                                       #char_var = input$DE_class_analysis_char,
                                       DE_group_info(),
                                       paired=F,
                                       sig_pvalue = input$DE_class_pval,
                                       sig_FC = input$DE_class_fc,
                                       insert_ref_group = NULL,
                                       ref_group = NULL,
                                       char_var = input$DE_class_analysis_char)
    }else if(input$DE_data_source == 'DE_user_data'){
      paired_sample <- ifelse(length(is.na(DE_group_info()$pair)) == 0, TRUE, FALSE)
      variables$DE.char.2 <- DE_char_2(DE_class_exp_non_log_data(), 
                                       data_transform = input$DE_log_trans, 
                                       #DE_lipid_char_table(), 
                                       #char_var = input$DE_class_analysis_char,
                                       DE_group_info(), 
                                       paired=paired_sample, 
                                       sig_pvalue = input$DE_class_pval,
                                       sig_FC = input$DE_class_fc,
                                       insert_ref_group = input$DE_user_ref_group,
                                       ref_group = variables$DE.group.ref.group, 
                                       char_var = input$DE_class_analysis_char)
    }
    
    
    data_type <- ifelse(str_detect(string = input$DE_class_analysis_char, pattern = 'FA_') == T, 'cont', 
                        ifelse(class(DE_lipid_char_table()[,input$DE_class_analysis_char]) == 'numeric', 'cont', 
                               ifelse(class(DE_lipid_char_table()[,input$DE_class_analysis_char]) == 'integer', 'cont', 'cate')))
    print(data_type)
    if(data_type == 'cate'){
      #if(input$DE_class_analysis_char %in% c('class', 'functional_category', 'structural_category')){
      variables$DE.class.split.char <- 'none'
    }else{
      variables$DE.class.split.char <- input$DE_class_split_char 
    }
    
    if(variables$DE.class.split.char != 'none'){
      
      shinyjs::show('DE_class_split_result_div')
      
      #### function: DE_sub_char_2() ####
      paired_sample <- ifelse(length(is.na(DE_group_info()$pair)) == 0, TRUE, FALSE)
      variables$DE.sub.char.2 <- DE_sub_char_2(DE_exp_data(), 
                                               data_transform=input$DE_log_trans, 
                                               DE_lipid_char_table(), 
                                               split_var = variables$DE.class.split.char,
                                               char_var = input$DE_class_analysis_char,
                                               DE_group_info(), 
                                               paired = paired_sample, 
                                               sig_pvalu = input$DE_class_pval, 
                                               sig_FC = input$DE_class_fc, 
                                               exclude_var_missing = input$DE_rm_NA, 
                                               missing_pct_limit = input$DE_rm_NA_pct,
                                               replace_zero=T, zero2what='NA', xmin=0.5,
                                               replace_NA = input$DE_rp_NA, 
                                               NA2what = input$DE_fill_NA, 
                                               ymin = input$DE_fill_min,
                                               pct_transform = input$DE_pct_trans,
                                               trans_type = 'log',
                                               centering=F, scaling=F)
      
    }else{
      
      shinyjs::hide('DE_class_split_result_div')
      
      variables$DE.sub.char.2 <- NULL
    }
    
    
    #### Output: DE.class.tab.all ####
    output$DE.class.tab.all <- renderDataTable(server = FALSE,{
      isolate({
        validate(need(!is.null(variables$DE.char.2$DE_char_table_all), "Table not showing due to category charateristics."))
        if(sum(is.na(variables$DE.char.2$DE_char_table_all$post_hoc_pvalue))==nrow(variables$DE.char.2$DE_char_table_all)){
          DT::datatable(variables$DE.char.2$DE_char_table_all %>%
                          mutate(sig_FC = ifelse(abs(log2FC)>log2(input$DE_class_fc),'yes','no')) %>%
                          dplyr::select(sig_FC, 1:3, 5:10, 4, 11) %>%
                          arrange(desc(sig_FC)) ,
                        #caption = 'Lipid expression data',
                        colnames = c('Significance(Fold change)', input$DE_class_analysis_char, 'method', 'anova p-value', 'mean(ctrl)', 
                                     'sd(ctrl)', 'mean(exp)', 'sd(exp)', 'FC', 'log2(FC)', 'post hoc test', 'post hoc p-value'),
                        escape = FALSE, selection = 'none', rownames = TRUE, 
                        class = "nowrap row-border",
                        extensions = c('Buttons', 'Scroller'),
                        options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                       deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                       dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                       columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
            formatStyle('sig_FC', target = 'row', backgroundColor = styleEqual(c("yes","no"), c('pink', 'EBECF0'))) 
        }else{
          DT::datatable(variables$DE.char.2$DE_char_table_all %>%
                          dplyr::select(sig, 1:3, 5:10, 4, 11) %>%
                          arrange(desc(sig)),
                        #caption = 'Lipid expression data',
                        colnames = c('Significance(p-value)', input$DE_class_analysis_char, 'method', 'anova p-value', 'mean(ctrl)', 
                                     'sd(ctrl)', 'mean(exp)', 'sd(exp)', 'FC', 'log2(FC)', 'post hoc test', 'post hoc p-value'),
                        escape = FALSE, selection = 'none', rownames = TRUE, 
                        class = "nowrap row-border",
                        extensions = c('Buttons', 'Scroller'),
                        options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                       deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                       dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                       columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
            formatStyle('sig', target = 'row', backgroundColor = styleEqual(c("yes","no"), c('pink', 'EBECF0'))) 
        }
      }) #isolate
    }) #output$DE.class.tab.all <- renderDataTable
    
    #### Output: DE.class.barplot ####
    output$DE.class.barplot <- renderPlotly({
      
      validate(need(!is.null(variables$DE.char.2$DE_char_barplot), "Plot not showing due to category charateristics."))
      
      variables$DE.char.2$DE_char_barplot
      
    }) #output$DE.class.barplot <- renderPlotly
    
    #### Output: DE.class.trendplot ####
    output$DE.class.trendplot <- renderPlotly({
      
      validate(need(!is.null(variables$DE.char.2$DE_char_trendplot), "Plot not showing due to category charateristics."))
      
      variables$DE.char.2$DE_char_trendplot
      
    }) #output$DE.class.trendplot <- renderPlotly
    
    #### Output: DE.class.boxplot ####
    output$DE.class.boxplot <- renderPlotly({
      
      validate(need(!is.null(variables$DE.char.2$DE_char_boxplot), "Plot not showing due to category charateristics."))
      
      variables$DE.char.2$DE_char_boxplot
      
    }) #output$DE.class.boxplot <- renderPlotly
    
    #### Output: DE.class.barplot.sqrt ####
    output$DE.class.barplot.sqrt <- renderPlotly({
      isolate({
        validate(need(!is.null(variables$DE.char.2$DE_char_barplot_sqrt), "Plot not showing due to category charateristics."))
        
        variables$DE.char.2$DE_char_barplot_sqrt
      })
    }) #output$DE.class.barplot <- renderPlotly
    
    #### Output: DE.class.trendplot.sqrt ####
    output$DE.class.trendplot.sqrt <- renderPlotly({
      
      validate(need(!is.null(variables$DE.char.2$DE_char_trendplot_sqrt), "Plot not showing due to category charateristics."))
      
      variables$DE.char.2$DE_char_trendplot_sqrt
      
    }) #output$DE.class.trendplot <- renderPlotly
    
    #### Output: DE.class.boxplot.2 ####
    output$DE.class.boxplot.2 <- renderPlotly({
      
      validate(need(!is.null(variables$DE.char.2$DE_char_boxplot), "Plot not showing due to category charateristics."))
      
      variables$DE.char.2$DE_char_boxplot
      
    }) #output$DE.class.boxplot <- renderPlotly
    
    #### Output: DE.class.split.tab.all ####
    output$DE.class.split.tab.all <- renderDataTable(server = FALSE,{
      isolate({
        validate(need(!is.null(variables$DE.sub.char.2[[2]]), "Table not showing due to category charateristics."))
        
        DT::datatable(variables$DE.sub.char.2[[2]] %>% 
                        filter_(variables$DE.class.split.char != '') %>%
                        dplyr::select(sig, 1:4, 6:11, 5, 12) %>%
                        arrange(desc(sig)),
                      #caption = 'Lipid expression data',
                      colnames = c('significance', variables$DE.class.split.char, input$DE_class_analysis_char, 'method', 'anova p-value', 'mean(ctrl)', 
                                   'sd(ctrl)', 'mean(exp)', 'sd(exp)', 'FC', 'log2(FC)', 'post hoc test', 'post hoc p-value'),
                      escape = FALSE, selection = 'none', rownames = TRUE, 
                      class = "nowrap row-border",
                      extensions = c('Buttons', 'Scroller'),
                      options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE, 
                                     deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                     dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                     columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
          formatStyle('sig', target = 'row', backgroundColor = styleEqual(c("yes","no"), c('pink', 'EBECF0'))) 
      }) #isolate
    }) #output$DE.class.split.tab.all <- renderDataTable
    
    #### function: DE_sub_char_plot_2() ####
    DE.sub.char.plot.2 <- reactive({
      
      if(!is.null(variables$DE.sub.char.2)){
        if(input$DE_data_source == 'DE_demo_data'){
          DE_sub_char_plot_2(variables$DE.sub.char.2[[2]], variables$DE.sub.char.2[[3]], 
                             DE_group_info(),
                             char_var = input$DE_class_analysis_char, 
                             split_var = input$DE_class_split_char, 
                             split_class = input$DE_class_split_class, 
                             insert_ref_group = NULL, 
                             ref_group = NULL)
        }else if(input$DE_data_source == 'DE_user_data'){
          DE_sub_char_plot_2(variables$DE.sub.char.2[[2]], variables$DE.sub.char.2[[3]], 
                             DE_group_info(),
                             char_var = input$DE_class_analysis_char, 
                             split_var = input$DE_class_split_char, 
                             split_class = input$DE_class_split_class,
                             insert_ref_group = input$DE_user_ref_group,
                             ref_group = variables$DE.group.ref.group)
        }
      }else{
        NULL
      }
      
    })
    
    observeEvent(input$DE_class_split_class,{
      
      #### Output: DE.class.split.barplot ####
      output$DE.class.split.barplot <- renderPlotly({
        isolate({
          validate(need(!is.null(DE.sub.char.plot.2()[[1]]), "Plot not showing due to category charateristics."))
          
          DE.sub.char.plot.2()[[1]]
        }) #isolate
      }) #output$DE.class.split.barplot <- renderPlotly
      
      #### Output: DE.class.split.trendplot ####
      output$DE.class.split.trendplot <- renderPlotly({
        isolate({
          validate(need(!is.null(DE.sub.char.plot.2()[[2]]), "Plot not showing due to category charateristics."))
          
          DE.sub.char.plot.2()[[2]]
        }) #isolate
      }) #output$DE.class.split.trendplot <- renderPlotly
      
      #### Output: DE.class.split.boxplot ####
      output$DE.class.split.boxplot <- renderPlotly({
        isolate({
          validate(need(!is.null(DE.sub.char.plot.2()[[3]]), "Plot not showing due to category charateristics."))
          
          DE.sub.char.plot.2()[[3]]
        }) #isolate
      }) #output$DE.class.split.boxplot <- renderPlotly
      
      #### Output: DE.class.split.barplot.sqrt ####
      output$DE.class.split.barplot.sqrt <- renderPlotly({
        isolate({
          validate(need(!is.null(DE.sub.char.plot.2()[[4]]), "Plot not showing due to category charateristics."))
          
          DE.sub.char.plot.2()[[4]]
        }) #isolate
      }) #output$DE.class.split.barplot.sqrt <- renderPlotly
      
      #### Output: DE.class.split.trendplot.sqrt ####
      output$DE.class.split.trendplot.sqrt <- renderPlotly({
        isolate({
          validate(need(!is.null(DE.sub.char.plot.2()[[5]]), "Plot not showing due to category charateristics."))
          
          DE.sub.char.plot.2()[[5]]
        }) #isolate
      }) #output$DE.class.split.trendplot.sqrt <- renderPlotly
      
      #### Output: DE.class.split.boxplot.2 ####
      output$DE.class.split.boxplot.2 <- renderPlotly({
        isolate({
          validate(need(!is.null(DE.sub.char.plot.2()[[3]]), "Plot not showing due to category charateristics."))
          
          DE.sub.char.plot.2()[[3]]
        }) #isolate
      }) #output$DE.class.split.boxplot.2 <- renderPlotly
    }) #observeEvent(input$DE_class_split_class
  #}) #isolate
}) #observeEvent(input$DE_class_analysis_start





#### update DE class lipid characteristic select input ####
observe({
  if(!is.null(DE_lipid_char_table()) & input$DE_data_source == 'DE_demo_data'){
    lipid.char <- colnames(DE_lipid_char_table())[-1]
    updateSelectInput(session, "DE_class_analysis_char",
                      choices =  lipid.char, 
                      selected = 'totallength')
  }else if(!is.null(DE_lipid_char_table()) & input$DE_data_source == 'DE_user_data'){
    lipid.char <- colnames(DE_lipid_char_table())[-1]
    updateSelectInput(session, "DE_class_analysis_char",
                      choices =  lipid.char, 
                      selected = lipid.char[1])
  }
})

#### update DE class split lipid characteristic select input ####
observe({
  
  if(!is.null(DE_lipid_char_table()) & input$DE_data_source == 'DE_demo_data'){
    lipid.char <- colnames(DE_lipid_char_table())[-1]
    select.char <- input$DE_class_analysis_char
    updateSelectInput(session, "DE_class_split_char",
                      choices =  c('none', lipid.char[which(lipid.char != select.char)]), 
                      selected = 'class')
    
  }else if(!is.null(DE_lipid_char_table()) & input$DE_data_source == 'DE_user_data'){
    lipid.char <- colnames(DE_lipid_char_table())[-1]
    select.char <- input$DE_class_analysis_char
    updateSelectInput(session, "DE_class_split_char",
                      choices =  c('none', lipid.char[which(lipid.char != select.char)]), 
                      selected = lipid.char[which(lipid.char != select.char)][1])
  }
  
})

#### update DE class split class of lipid characteristic select input ####
observe({
  
  if(!is.null(variables$DE.sub.char.2[[2]])){
    SORT.CHAR <- variables$DE.sub.char.2[[2]] %>% arrange(desc(sig))
    char.class <- unique(SORT.CHAR[,1])
    updateSelectInput(session, "DE_class_split_class",
                      choices =  char.class,
                      selected = char.class[1])
  }
  
  # if(!is.null(variables$DE.sub.char.2[[2]]) & input$DE_data_source == 'DE_demo_data' & input$DE_class_split_char == 'class'){
  #   char.class <- unique(variables$DE.sub.char.2[[2]][1])
  #   updateSelectInput(session, "DE_class_split_class",
  #                     choices =  char.class, 
  #                     selected = 'LPE')
  # }else if(!is.null(variables$DE.sub.char.2[[2]])){
  #   char.class <- unique(variables$DE.sub.char.2[[2]][1])
  #   updateSelectInput(session, "DE_class_split_class",
  #                     choices =  char.class)
  # }
})

#### control DE class split lipid characteristic select input ####
observe({
  data_type <- ifelse(str_detect(string = input$DE_class_analysis_char, pattern = 'FA_') == T, 'cont', 
                      ifelse(class(DE_lipid_char_table()[,which(colnames(DE_lipid_char_table())==input$DE_class_analysis_char)]) == 'numeric', 'cont', 
                             ifelse(class(DE_lipid_char_table()[,which(colnames(DE_lipid_char_table())==input$DE_class_analysis_char)]) == 'integer', 'cont', 'cate')))
  print(data_type)
  if(data_type == 'cate'){
    shinyjs::disable('DE_class_split_char')
  }else{
    shinyjs::enable('DE_class_split_char')
  }
  
  # if(input$DE_class_analysis_char %in% c('class', 'functional_category', 'structural_category')){
  #   shinyjs::disable('DE_class_split_char')
  # }else{
  #   shinyjs::enable('DE_class_split_char')
  # }
})

#############################################################
####  lipid category analysis: Dimensionality reduction  ####
#############################################################

#######################
#### Control panel ####
#######################

# #### Group assignment ####
#### number of samples ####
output$DE.class.group.count <- renderText({
  isolate({
    paste0(DE_group_count(), ' groups were detected in samples.')
  })
})

#### dbscan ####
#### update numeric input minPts ####
observe({
  updateNumericInput(session, 
                     inputId = 'DE_class_dbscan_minPts', 
                     max = (DE_sample_count()-1)
  ) #updateNumericInput
})

#### control reset button ####
observeEvent(input$DE_class_dim_redu_reset,{
  
  #### shinyjs show/hide results ####
  shinyjs::hide('DE_class_dim_redu_result_div')
  
  #### shinyjs reset control panel ####
  shinyjs::reset("DE_class_dim_redu_reset_div")
  
}) #observeEvent(input$DE_class_dim_redu_reset

shinyjs::hide('DE_class_dim_redu_result_div')
#### control start button ####
observeEvent(input$DE_class_dim_redu_start,{
  
  if(input$DE_data_source == 'DE_demo_data'){
    observeEvent(input$DE_class_dim_redu_method,{
      if(input$DE_class_dim_redu_method == 'pca'){
        feature_num_check=2
        sample_num_check=6
        shinyjs::hide('DE_class_dim_redu_result_div')
      }else if(input$DE_class_dim_redu_method == 'plsda'){
        feature_num_check=6
        sample_num_check=6
        shinyjs::hide('DE_class_dim_redu_result_div')
      }else if(input$DE_class_dim_redu_method == 'tsne'){
        feature_num_check=4
        sample_num_check=2
        shinyjs::hide('DE_class_dim_redu_result_div')
      }else{
        feature_num_check=2
        sample_num_check=2
        shinyjs::hide('DE_class_dim_redu_result_div')
      }
      
      
      if(submit_check(transform_data=DE_exp_transform_data(),sig_data=DE_class_sig(),check_NA=T,feature_num=6,sample_num=sample_num_check,sig_count=2)[[1]]!=TRUE){
        showModal(modalDialog(
          title = "Important message",
          submit_check(transform_data=DE_exp_transform_data(),sig_data=DE_class_sig(),check_NA=T,feature_num=6,sample_num=sample_num_check,sig_count=2)[[2]],
          easyClose = TRUE
        ))
      }else{
        #### shinyjs show/hide results ####
        shinyjs::show('DE_class_dim_redu_result_div')
        
      }
      #### Function: PCA ####
      if(input$DE_class_dim_redu_method == 'pca'){
        
        if(input$DE_class_cluster_method == 'kmeans'){
          
          variables$DE.class.pca.result <- PCA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                               scaling=input$DE_class_pca_scale,
                                               centering=input$DE_class_pca_center,
                                               cluster_method=input$DE_class_cluster_method,
                                               group_num = input$DE_class_kmeans_group)
          
        }else if(input$DE_class_cluster_method == 'kmedoids'){
          
          variables$DE.class.pca.result <- PCA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                               scaling=input$DE_class_pca_scale,
                                               centering=input$DE_class_pca_center,
                                               cluster_method=input$DE_class_cluster_method,
                                               group_num = input$DE_class_pam_group,
                                               var1 = input$DE_class_pam_metric)
          
        }else if(input$DE_class_cluster_method == 'hclustering'){
          
          variables$DE.class.pca.result <- PCA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                               scaling=input$DE_class_pca_scale,
                                               centering=input$DE_class_pca_center,
                                               cluster_method=input$DE_class_cluster_method,
                                               group_num = input$DE_class_hclust_group,
                                               var1 = input$DE_class_hclust_dist,
                                               var2 = input$DE_class_hclust_hclust)
          
        }else if(input$DE_class_cluster_method == 'dbscan'){
          
          variables$DE.class.pca.result <- PCA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                               scaling=input$DE_class_pca_scale,
                                               centering=input$DE_class_pca_center,
                                               cluster_method=input$DE_class_cluster_method,
                                               var1 = input$DE_class_dbscan_eps,
                                               var2 = input$DE_class_dbscan_minPts)
          
        }else if(input$DE_class_cluster_method == 'group_info'){
          
          variables$DE.class.pca.result <- PCA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                               scaling=input$DE_class_pca_scale,
                                               centering=input$DE_class_pca_center,
                                               cluster_method=input$DE_class_cluster_method)
          
        }
        
        #### Output: DE.class.pca.biplot ####
        output$DE.class.pca.biplot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.class.pca.result[[4]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.class.pca.result[[4]]
          })
        })
        
        #### Output: DE.class.pca.screeplot ####
        output$DE.class.pca.screeplot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.class.pca.result[[5]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.class.pca.result[[5]]
          })
        })
        
        #### Output: DE.class.pca.rotated.data ####
        output$DE.class.pca.rotated.data <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.class.pca.result[[2]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.class.pca.result[[2]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.class.pca.rotated.data <- renderDataTable
        
        #### Output: DE.class.pca.contrib.table ####
        output$DE.class.pca.contrib.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.class.pca.result[[3]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.class.pca.result[[3]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.class.pca.contrib.table <- renderDataTable
        
      }else if(input$DE_class_dim_redu_method == 'plsda'){
        
        if(input$DE_class_cluster_method == 'kmeans'){
          
          variables$DE.class.plsda.result <- PLSDA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                   ncomp = 2,
                                                   scaling = input$DE_class_plsda_scale,
                                                   cluster_method = input$DE_class_cluster_method,
                                                   group_num = input$DE_class_kmeans_group)
          
        }else if(input$DE_class_cluster_method == 'kmedoids'){
          
          variables$DE.class.plsda.result <- PLSDA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                   ncomp = 2,
                                                   scaling = input$DE_class_plsda_scale,
                                                   cluster_method = input$DE_class_cluster_method,
                                                   group_num = input$DE_class_pam_group,
                                                   var1 = input$DE_class_pam_metric)
          
        }else if(input$DE_class_cluster_method == 'hclustering'){
          
          variables$DE.class.plsda.result <- PLSDA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                   ncomp = 2,
                                                   scaling = input$DE_class_plsda_scale,
                                                   cluster_method = input$DE_class_cluster_method,
                                                   group_num = input$DE_class_hclust_group,
                                                   var1 = input$DE_class_hclust_dist,
                                                   var2 = input$DE_class_hclust_hclust)
          
        }else if(input$DE_class_cluster_method == 'dbscan'){
          
          variables$DE.class.plsda.result <- PLSDA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                   ncomp = 2,
                                                   scaling = input$DE_class_plsda_scale,
                                                   cluster_method = input$DE_class_cluster_method,
                                                   var1 = input$DE_class_dbscan_eps,
                                                   var2 = input$DE_class_dbscan_minPts)
          
        }else if(input$DE_class_cluster_method == 'group_info'){
          
          variables$DE.class.plsda.result <- PLSDA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                   ncomp = 2,
                                                   scaling = input$DE_class_plsda_scale,
                                                   cluster_method = input$DE_class_cluster_method)
          
        }
        
        #### Output: DE.class.plsda.sample.plot ####
        output$DE.class.plsda.sample.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.class.plsda.result[[3]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.class.plsda.result[[3]]
          })
        })
        
        #### Output: DE.class.plsda.variable.plot ####
        output$DE.class.plsda.variable.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.class.plsda.result[[4]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.class.plsda.result[[4]]
          })
        })
        
        #### Output: DE.class.plsda.variate.table ####
        output$DE.class.plsda.variate.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.class.plsda.result[[1]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.class.plsda.result[[1]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #DE.class.plsda.variate.table <- renderDataTable
        
        #### Output: DE.class.plsda.loading.table ####
        output$DE.class.plsda.loading.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.class.plsda.result[[2]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.class.plsda.result[[2]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.class.plsda.loading.table <- renderDataTable
        
      }else if(input$DE_class_dim_redu_method == 'tsne'){
        
        if(input$DE_class_cluster_method == 'kmeans'){
          
          variables$DE.class.tsne.result <- tsne(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 pca=input$DE_class_tsne_pca,
                                                 perplexity=input$DE_class_tsne_perplexity,
                                                 max_iter=input$DE_class_tsne_max_iter,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 group_num = input$DE_class_kmeans_group)
          
        }else if(input$DE_class_cluster_method == 'kmedoids'){
          
          variables$DE.class.tsne.result <- tsne(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 pca=input$DE_class_tsne_pca,
                                                 perplexity=input$DE_class_tsne_perplexity,
                                                 max_iter=input$DE_class_tsne_max_iter,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 group_num = input$DE_class_pam_group,
                                                 var1 = input$DE_class_pam_metric)
          
        }else if(input$DE_class_cluster_method == 'hclustering'){
          
          variables$DE.class.tsne.result <- tsne(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 pca=input$DE_class_tsne_pca,
                                                 perplexity=input$DE_class_tsne_perplexity,
                                                 max_iter=input$DE_class_tsne_max_iter,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 group_num = input$DE_class_hclust_group,
                                                 var1 = input$DE_class_hclust_dist,
                                                 var2 = input$DE_class_hclust_hclust)
          
        }else if(input$DE_class_cluster_method == 'dbscan'){
          
          variables$DE.class.tsne.result <- tsne(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 pca=input$DE_class_tsne_pca,
                                                 perplexity=input$DE_class_tsne_perplexity,
                                                 max_iter=input$DE_class_tsne_max_iter,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 var1 = input$DE_class_dbscan_eps,
                                                 var2 = input$DE_class_dbscan_minPts)
          
        }else if(input$DE_class_cluster_method == 'group_info'){
          
          variables$DE.class.tsne.result <- tsne(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 pca=input$DE_class_tsne_pca,
                                                 perplexity=input$DE_class_tsne_perplexity,
                                                 max_iter=input$DE_class_tsne_max_iter,
                                                 cluster_method=input$DE_class_cluster_method)
          
        }
        
        #### Output: DE.class.tsne.plot ####
        output$DE.class.tsne.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.class.tsne.result[[2]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.class.tsne.result[[2]]
          })
        })
        
        #### Output: DE.class.tsne.table ####
        output$DE.class.tsne.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.class.tsne.result[[1]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.class.tsne.result[[1]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.class.tsne.table <- renderDataTable
        
        
      }else if(input$DE_class_dim_redu_method == 'umap'){
        
        if(input$DE_class_cluster_method == 'kmeans'){
          
          variables$DE.class.umap.result <- UMAP(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 n_neighbors=input$DE_class_umap_n_neighbors,
                                                 scale=input$DE_class_umap_scale,
                                                 metric=input$DE_class_umap_metric,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 group_num = input$DE_class_kmeans_group)
          
        }else if(input$DE_class_cluster_method == 'kmedoids'){
          
          variables$DE.class.umap.result <- UMAP(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 n_neighbors=input$DE_class_umap_n_neighbors,
                                                 scale=input$DE_class_umap_scale,
                                                 metric=input$DE_class_umap_metric,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 group_num = input$DE_class_pam_group,
                                                 var1 = input$DE_class_pam_metric)
          
        }else if(input$DE_class_cluster_method == 'hclustering'){
          
          variables$DE.class.umap.result <- UMAP(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 n_neighbors=input$DE_class_umap_n_neighbors,
                                                 scale=input$DE_class_umap_scale,
                                                 metric=input$DE_class_umap_metric,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 group_num = input$DE_class_hclust_group,
                                                 var1 = input$DE_class_hclust_dist,
                                                 var2 = input$DE_class_hclust_hclust)
          
        }else if(input$DE_class_cluster_method == 'dbscan'){
          
          variables$DE.class.umap.result <- UMAP(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 n_neighbors=input$DE_class_umap_n_neighbors,
                                                 scale=input$DE_class_umap_scale,
                                                 metric=input$DE_class_umap_metric,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 var1 = input$DE_class_dbscan_eps,
                                                 var2 = input$DE_class_dbscan_minPts)
          
        }else if(input$DE_class_cluster_method == 'group_info'){
          
          variables$DE.class.umap.result <- UMAP(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 n_neighbors=input$DE_class_umap_n_neighbors,
                                                 scale=input$DE_class_umap_scale,
                                                 metric=input$DE_class_umap_metric,
                                                 cluster_method=input$DE_class_cluster_method)
          
        }
        
        #### Output: DE.class.umap.plot ####
        output$DE.class.umap.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.class.umap.result[[2]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.class.umap.result[[2]]
          })
        })
        
        #### Output: DE.class.umap.table ####
        output$DE.class.umap.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.class.umap.result[[1]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.class.umap.result[[1]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.class.umap.table <- renderDataTable
        
      }
    }) #observeEvent(input$DE_class_dim_redu_method
    
  }else if(input$DE_data_source == 'DE_user_data'){
    observeEvent(input$DE_class_dim_redu_method,{
      if(input$DE_class_dim_redu_method == 'pca'){
        feature_num_check=2
        sample_num_check=6
        shinyjs::hide('DE_class_dim_redu_result_div')
      }else if(input$DE_class_dim_redu_method == 'plsda'){
        feature_num_check=6
        sample_num_check=6
        shinyjs::hide('DE_class_dim_redu_result_div')
      }else if(input$DE_class_dim_redu_method == 'tsne'){
        feature_num_check=4
        sample_num_check=2
        shinyjs::hide('DE_class_dim_redu_result_div')
      }else{
        feature_num_check=2
        sample_num_check=2
        shinyjs::hide('DE_class_dim_redu_result_div')
      }
      
      
      if(submit_check(transform_data=DE_exp_transform_data(),sig_data=DE_class_sig(),check_NA=T,feature_num=6,sample_num=sample_num_check,sig_count=2)[[1]]!=TRUE){
        showModal(modalDialog(
          title = "Important message",
          submit_check(transform_data=DE_exp_transform_data(),sig_data=DE_class_sig(),check_NA=T,feature_num=6,sample_num=sample_num_check,sig_count=2)[[2]],
          easyClose = TRUE
        ))
      }else{
        #### shinyjs show/hide results ####
        shinyjs::show('DE_class_dim_redu_result_div')
        
      }
      #### Function: PCA ####
      if(input$DE_class_dim_redu_method == 'pca'){
        
        if(input$DE_class_cluster_method == 'kmeans'){
          
          variables$DE.class.pca.result <- PCA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                               scaling=input$DE_class_pca_scale,
                                               centering=input$DE_class_pca_center,
                                               cluster_method=input$DE_class_cluster_method,
                                               group_num = input$DE_class_kmeans_group,
                                               insert_ref_group = NULL,
                                               ref_group = NULL)
          
        }else if(input$DE_class_cluster_method == 'kmedoids'){
          
          variables$DE.class.pca.result <- PCA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                               scaling=input$DE_class_pca_scale,
                                               centering=input$DE_class_pca_center,
                                               cluster_method=input$DE_class_cluster_method,
                                               group_num = input$DE_class_pam_group,
                                               var1 = input$DE_class_pam_metric,
                                               insert_ref_group = NULL,
                                               ref_group = NULL)
          
        }else if(input$DE_class_cluster_method == 'hclustering'){
          
          variables$DE.class.pca.result <- PCA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                               scaling=input$DE_class_pca_scale,
                                               centering=input$DE_class_pca_center,
                                               cluster_method=input$DE_class_cluster_method,
                                               group_num = input$DE_class_hclust_group,
                                               var1 = input$DE_class_hclust_dist,
                                               var2 = input$DE_class_hclust_hclust,
                                               insert_ref_group = NULL,
                                               ref_group = NULL)
          
        }else if(input$DE_class_cluster_method == 'dbscan'){
          
          variables$DE.class.pca.result <- PCA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                               scaling=input$DE_class_pca_scale,
                                               centering=input$DE_class_pca_center,
                                               cluster_method=input$DE_class_cluster_method,
                                               var1 = input$DE_class_dbscan_eps,
                                               var2 = input$DE_class_dbscan_minPts,
                                               insert_ref_group = NULL,
                                               ref_group = NULL)
          
        }else if(input$DE_class_cluster_method == 'group_info'){
          
          variables$DE.class.pca.result <- PCA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                               scaling=input$DE_class_pca_scale,
                                               centering=input$DE_class_pca_center,
                                               cluster_method=input$DE_class_cluster_method,
                                               insert_ref_group = input$DE_user_ref_group,
                                               ref_group = variables$DE.group.ref.group)
          
        }
        
        #### Output: DE.class.pca.biplot ####
        output$DE.class.pca.biplot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.class.pca.result[[4]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.class.pca.result[[4]]
          })
        })
        
        #### Output: DE.class.pca.screeplot ####
        output$DE.class.pca.screeplot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.class.pca.result[[5]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.class.pca.result[[5]]
          })
        })
        
        #### Output: DE.class.pca.rotated.data ####
        output$DE.class.pca.rotated.data <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.class.pca.result[[2]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.class.pca.result[[2]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.class.pca.rotated.data <- renderDataTable
        
        #### Output: DE.class.pca.contrib.table ####
        output$DE.class.pca.contrib.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.class.pca.result[[3]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.class.pca.result[[3]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.class.pca.contrib.table <- renderDataTable
        
      }else if(input$DE_class_dim_redu_method == 'plsda'){
        
        if(input$DE_class_cluster_method == 'kmeans'){
          
          variables$DE.class.plsda.result <- PLSDA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                   ncomp = 2,
                                                   scaling = input$DE_class_plsda_scale,
                                                   cluster_method = input$DE_class_cluster_method,
                                                   group_num = input$DE_class_kmeans_group,
                                                   insert_ref_group = input$DE_user_ref_group,
                                                   ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_class_cluster_method == 'kmedoids'){
          
          variables$DE.class.plsda.result <- PLSDA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                   ncomp = 2,
                                                   scaling = input$DE_class_plsda_scale,
                                                   cluster_method = input$DE_class_cluster_method,
                                                   group_num = input$DE_class_pam_group,
                                                   var1 = input$DE_class_pam_metric,
                                                   insert_ref_group = input$DE_user_ref_group,
                                                   ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_class_cluster_method == 'hclustering'){
          
          variables$DE.class.plsda.result <- PLSDA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                   ncomp = 2,
                                                   scaling = input$DE_class_plsda_scale,
                                                   cluster_method = input$DE_class_cluster_method,
                                                   group_num = input$DE_class_hclust_group,
                                                   var1 = input$DE_class_hclust_dist,
                                                   var2 = input$DE_class_hclust_hclust,
                                                   insert_ref_group = input$DE_user_ref_group,
                                                   ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_class_cluster_method == 'dbscan'){
          
          variables$DE.class.plsda.result <- PLSDA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                   ncomp = 2,
                                                   scaling = input$DE_class_plsda_scale,
                                                   cluster_method = input$DE_class_cluster_method,
                                                   var1 = input$DE_class_dbscan_eps,
                                                   var2 = input$DE_class_dbscan_minPts,
                                                   insert_ref_group = input$DE_user_ref_group,
                                                   ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_class_cluster_method == 'group_info'){
          
          variables$DE.class.plsda.result <- PLSDA(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                   ncomp = 2,
                                                   scaling = input$DE_class_plsda_scale,
                                                   cluster_method = input$DE_class_cluster_method,
                                                   insert_ref_group = input$DE_user_ref_group,
                                                   ref_group = variables$DE.group.ref.group)
          
        }
        
        #### Output: DE.class.plsda.sample.plot ####
        output$DE.class.plsda.sample.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.class.plsda.result[[3]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.class.plsda.result[[3]]
          })
        })
        
        #### Output: DE.class.plsda.variable.plot ####
        output$DE.class.plsda.variable.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.class.plsda.result[[4]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.class.plsda.result[[4]]
          })
        })
        
        #### Output: DE.class.plsda.variate.table ####
        output$DE.class.plsda.variate.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.class.plsda.result[[1]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.class.plsda.result[[1]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #DE.class.plsda.variate.table <- renderDataTable
        
        #### Output: DE.class.plsda.loading.table ####
        output$DE.class.plsda.loading.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.class.plsda.result[[2]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.class.plsda.result[[2]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.class.plsda.loading.table <- renderDataTable
        
      }else if(input$DE_class_dim_redu_method == 'tsne'){
        
        if(input$DE_class_cluster_method == 'kmeans'){
          
          variables$DE.class.tsne.result <- tsne(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 pca=input$DE_class_tsne_pca,
                                                 perplexity=input$DE_class_tsne_perplexity,
                                                 max_iter=input$DE_class_tsne_max_iter,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 group_num = input$DE_class_kmeans_group,
                                                 insert_ref_group = input$DE_user_ref_group,
                                                 ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_class_cluster_method == 'kmedoids'){
          
          variables$DE.class.tsne.result <- tsne(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 pca=input$DE_class_tsne_pca,
                                                 perplexity=input$DE_class_tsne_perplexity,
                                                 max_iter=input$DE_class_tsne_max_iter,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 group_num = input$DE_class_pam_group,
                                                 var1 = input$DE_class_pam_metric,
                                                 insert_ref_group = input$DE_user_ref_group,
                                                 ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_class_cluster_method == 'hclustering'){
          
          variables$DE.class.tsne.result <- tsne(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 pca=input$DE_class_tsne_pca,
                                                 perplexity=input$DE_class_tsne_perplexity,
                                                 max_iter=input$DE_class_tsne_max_iter,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 group_num = input$DE_class_hclust_group,
                                                 var1 = input$DE_class_hclust_dist,
                                                 var2 = input$DE_class_hclust_hclust,
                                                 insert_ref_group = input$DE_user_ref_group,
                                                 ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_class_cluster_method == 'dbscan'){
          
          variables$DE.class.tsne.result <- tsne(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 pca=input$DE_class_tsne_pca,
                                                 perplexity=input$DE_class_tsne_perplexity,
                                                 max_iter=input$DE_class_tsne_max_iter,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 var1 = input$DE_class_dbscan_eps,
                                                 var2 = input$DE_class_dbscan_minPts,
                                                 insert_ref_group = input$DE_user_ref_group,
                                                 ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_class_cluster_method == 'group_info'){
          
          variables$DE.class.tsne.result <- tsne(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 pca=input$DE_class_tsne_pca,
                                                 perplexity=input$DE_class_tsne_perplexity,
                                                 max_iter=input$DE_class_tsne_max_iter,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 insert_ref_group = input$DE_user_ref_group,
                                                 ref_group = variables$DE.group.ref.group)
          
        }
        
        #### Output: DE.class.tsne.plot ####
        output$DE.class.tsne.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.class.tsne.result[[2]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.class.tsne.result[[2]]
          })
        })
        
        #### Output: DE.class.tsne.table ####
        output$DE.class.tsne.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.class.tsne.result[[1]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.class.tsne.result[[1]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.class.tsne.table <- renderDataTable
        
        
      }else if(input$DE_class_dim_redu_method == 'umap'){
        
        if(input$DE_class_cluster_method == 'kmeans'){
          
          variables$DE.class.umap.result <- UMAP(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 n_neighbors=input$DE_class_umap_n_neighbors,
                                                 scale=input$DE_class_umap_scale,
                                                 metric=input$DE_class_umap_metric,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 group_num = input$DE_class_kmeans_group,
                                                 insert_ref_group = input$DE_user_ref_group,
                                                 ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_class_cluster_method == 'kmedoids'){
          
          variables$DE.class.umap.result <- UMAP(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 n_neighbors=input$DE_class_umap_n_neighbors,
                                                 scale=input$DE_class_umap_scale,
                                                 metric=input$DE_class_umap_metric,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 group_num = input$DE_class_pam_group,
                                                 var1 = input$DE_class_pam_metric,
                                                 insert_ref_group = input$DE_user_ref_group,
                                                 ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_class_cluster_method == 'hclustering'){
          
          variables$DE.class.umap.result <- UMAP(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 n_neighbors=input$DE_class_umap_n_neighbors,
                                                 scale=input$DE_class_umap_scale,
                                                 metric=input$DE_class_umap_metric,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 group_num = input$DE_class_hclust_group,
                                                 var1 = input$DE_class_hclust_dist,
                                                 var2 = input$DE_class_hclust_hclust,
                                                 insert_ref_group = input$DE_user_ref_group,
                                                 ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_class_cluster_method == 'dbscan'){
          
          variables$DE.class.umap.result <- UMAP(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 n_neighbors=input$DE_class_umap_n_neighbors,
                                                 scale=input$DE_class_umap_scale,
                                                 metric=input$DE_class_umap_metric,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 var1 = input$DE_class_dbscan_eps,
                                                 var2 = input$DE_class_dbscan_minPts,
                                                 insert_ref_group = input$DE_user_ref_group,
                                                 ref_group = variables$DE.group.ref.group)
          
        }else if(input$DE_class_cluster_method == 'group_info'){
          
          variables$DE.class.umap.result <- UMAP(DE_class_exp_transform_data(), DE_group_info(), DE_class_sig()[,1],
                                                 n_neighbors=input$DE_class_umap_n_neighbors,
                                                 scale=input$DE_class_umap_scale,
                                                 metric=input$DE_class_umap_metric,
                                                 cluster_method=input$DE_class_cluster_method,
                                                 insert_ref_group = input$DE_user_ref_group,
                                                 ref_group = variables$DE.group.ref.group)
          
        }
        
        #### Output: DE.class.umap.plot ####
        output$DE.class.umap.plot <- renderPlotly({
          isolate({
            validate(need(!is.null(variables$DE.class.umap.result[[2]]), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.class.umap.result[[2]]
          })
        })
        
        #### Output: DE.class.umap.table ####
        output$DE.class.umap.table <- renderDataTable(server = FALSE,{
          isolate({
            validate(need(!is.null(variables$DE.class.umap.result[[1]]), "Table not showing. Missing value imputation is recommended."))
            
            DT::datatable(variables$DE.class.umap.result[[1]] %>% mutate_if(is.numeric, ~round(., 5)),
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
        }) #output$DE.class.umap.table <- renderDataTable
        
      }
      
    }) #observeEvent(input$DE_class_dim_redu_method
  }

})

##### PCA #####
#### Function: PCA_variable ####
DE.class.pca.variable <- reactive({
  PCA_variable(variables$DE.class.pca.result[[1]],
               as.numeric(input$DE_class_pca_variable_topN))
})

#### Function: PCA_contrib ####
DE.class.pca.contrib <- reactive({
  if(input$DE_class_pca_contrib_PC == '1_2'){
    PCA_contrib(variables$DE.class.pca.result[[1]],
                n_PC = c(1, 2),
                as.numeric(input$DE_class_pca_contrib_topN))
  }else{
    PCA_contrib(variables$DE.class.pca.result[[1]],
                n_PC = as.numeric(input$DE_class_pca_contrib_PC),
                as.numeric(input$DE_class_pca_contrib_topN))
  }
})

#### Output: DE.class.pca.variable ####
output$DE.class.pca.variable <- renderPlotly({
  validate(need(!is.null(DE.class.pca.variable()[[1]]), "Plot not showing. Missing value imputation is recommended."))
  DE.class.pca.variable()[[1]]
})

#### Output: DE.class.pca.contrib ####
output$DE.class.pca.contrib <- renderPlotly({
  validate(need(!is.null(DE.class.pca.contrib()[[1]]), "Plot not showing. Missing value imputation is recommended."))
  DE.class.pca.contrib()[[1]]
})

#### Update sliderInput ####
observe({
  VALUE <- ifelse(nrow(variables$DE.class.pca.result[[3]]) < 10, nrow(variables$DE.class.pca.result[[3]]), 10)
  updateSliderInput(session,
                    inputId = 'DE_class_pca_variable_topN',
                    value = VALUE,
                    max = nrow(variables$DE.class.pca.result[[3]]))
  updateSliderInput(session,
                    inputId = 'DE_class_pca_contrib_topN',
                    value = VALUE,
                    max = nrow(variables$DE.class.pca.result[[3]]))
})

##### tSNE #####
observe({
  VALUE1 <- ifelse(DE_sample_count()%%3 == 0, floor(DE_sample_count()/3)-1, floor(DE_sample_count()/3))
  VALUE2 <- ifelse(VALUE1 < 5, VALUE1, 5)
  updateNumericInput(session, 
                     inputId = 'DE_class_tsne_perplexity', 
                     value = VALUE2,
                     max = VALUE1)
})

##### UMAP ######
observe({
  VALUE <- ifelse(DE_sample_count() < 15, DE_sample_count(), 15)
  updateNumericInput(session, 
                     inputId = 'DE_class_umap_n_neighbors', 
                     value = VALUE,
                     max = DE_sample_count())
})

##############################################################
#####  Lipid category analysis: Hierarchical clustering  #####
##############################################################

#### control reset button ####
observeEvent(input$DE_class_cluster_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('DE_class_heatmap_div')
  
  #### shinyjs reset ####
  shinyjs::reset('DE_class_cluster_reset_div')
  
})

#### control start button ####
observeEvent(input$DE_class_cluster_start, {
  if(input$DE_class_cluster_by=='sig_lipid' & 
     submit_check(transform_data=DE_exp_transform_data(),sig_data=DE_class_sig(),check_NA=T,feature_num=2,sig_count=2)[[1]]!=TRUE){
    shinyjs::hide('DE_class_heatmap_div')
    showModal(modalDialog(
      title = "Important message",
      submit_check(transform_data=DE_exp_transform_data(),sig_data=DE_class_sig(),check_NA=T,feature_num=2,sig_count=2)[[2]],
      easyClose = TRUE
    ))
  }else if (input$DE_class_cluster_by=='all_lipid' & 
            submit_check(transform_data=DE_exp_transform_data(),check_NA=T,feature_num=2)[[1]]!=TRUE){
    shinyjs::hide('DE_class_heatmap_div')
    showModal(modalDialog(
      title = "Important message",
      submit_check(transform_data=DE_exp_transform_data(),check_NA=T,sig_count=2)[[2]],
      easyClose = TRUE
    ))
  }else{
    #### shinyjs show/hide results ####
    shinyjs::show('DE_class_heatmap_div')
    shinyjs::show('DE.class.heatmap')
    shinyjs::show('DE.class.heatmap.matrix')
    
    #### Function: Hclustering ####
    isolate({
      
      if(input$DE_data_source == 'DE_demo_data'){
        if(nrow(DE_class_sig()) == 0){
          variables$DE.class.hclustering <- Hclustering(DE_class_exp_transform_data(), DE_class_sig(), DE_group_info(),
                                                        lipid_char_table = NULL, char_var = NULL,
                                                        distfun = input$DE_class_dist, hclustfun = input$DE_class_hclust)
          variables$DE.class.hclustering$sig.lipid <- NULL
        }else{
          variables$DE.class.hclustering <- Hclustering(DE_class_exp_transform_data(), DE_class_sig(), DE_group_info(),
                                                        lipid_char_table = NULL, char_var = NULL,
                                                        distfun = input$DE_class_dist, hclustfun = input$DE_class_hclust)
        }
      }else if(input$DE_data_source == 'DE_user_data'){
        if(nrow(DE_class_sig()) == 0){
          variables$DE.class.hclustering <- Hclustering(DE_class_exp_transform_data(), DE_class_sig(), DE_group_info(),
                                                        lipid_char_table = NULL, char_var = NULL,
                                                        distfun = input$DE_class_dist, hclustfun = input$DE_class_hclust,
                                                        insert_ref_group = input$DE_user_ref_group,
                                                        ref_group = variables$DE.group.ref.group)
          variables$DE.class.hclustering$sig.lipid <- NULL
        }else{
          variables$DE.class.hclustering <- Hclustering(DE_class_exp_transform_data(), DE_class_sig(), DE_group_info(),
                                                        lipid_char_table = NULL, char_var = NULL,
                                                        distfun = input$DE_class_dist, hclustfun = input$DE_class_hclust,
                                                        insert_ref_group = input$DE_user_ref_group,
                                                        ref_group = variables$DE.group.ref.group)
        }
      }
      
      #### Output: DE.class.heatmap ####
      output$DE.class.heatmap <- renderIheatmap({
        
        isolate({
          
          if(input$DE_class_cluster_by == 'all_lipid'){
            validate(need(!is.null(variables$DE.class.hclustering$all.lipid), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.class.hclustering$all.lipid
          }else if(input$DE_class_cluster_by == 'sig_lipid'){
            validate(need(!is.null(variables$DE.class.hclustering$sig.lipid), "Plot not showing. Missing value imputation is recommended."))
            variables$DE.class.hclustering$sig.lipid
          }
          
        })
        
      }) #output$DE.class.heatmap <- renderPlotly
      
      #### Output: DE.class.heatmap.matrix ####
      if(input$DE_class_cluster_by == 'all_lipid'){
        output$DE.class.heatmap.matrix <- downloadHandler(
          filename = function() {
            paste0(input$DE_class_dist, '_', input$DE_class_hclust, "_all", ".csv")
          },
          content = function(file) {
            write.csv(variables$DE.class.hclustering$all.lipid.data, file)
          }
        ) #output$DE.class.heatmap.matrix
      }else if(input$DE_class_cluster_by == 'sig_lipid'){
        output$DE.class.heatmap.matrix <- downloadHandler(
          filename = function() {
            paste0(input$DE_class_dist, '_', input$DE_class_hclust, "_sig",".csv")
          },
          content = function(file) {
            write.csv(variables$DE.class.hclustering$sig.lipid.data, file)
          }
        ) #output$DE.class.heatmap.matrix
      }
      
    }) #isolate
  }
  
}) #observeEvent(input$DE_class_cluster_start
