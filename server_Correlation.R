##################################
##################################
######                      ######
######   Correlation Page   ######
######                      ######
##################################
##################################

############################
####  CORR Data Source  ####
############################

#### Output: CORR.demo.download ####
output$CORR.demo.download <- shiny::downloadHandler(
  filename=function() {
    "Correlation_example_dataset.zip"
  },
  content=function(file) {
    file.copy("www/download_demo_dataset/Corr.zip", file)
  },
  contentType="application/zip"
)
shiny::outputOptions(output, "CORR.demo.download", suspendWhenHidden=FALSE)

#### control user reset button ####
shiny::observeEvent(input$CORR_user_reset, {
  
  
  #### shinyjs show/hide main panel ####
  shinyjs::hide('CORR_data_check_progress')
  
  #### shiny show/hide tab ####
  shiny::hideTab(inputId='CORR_analysis_tab', target='Lipid species analysis')
  shiny::hideTab(inputId='CORR_analysis_tab', target='Lipid characteristics analysis')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('CORR_user_reset_div')
  
  variables$CORR.raw.abundance <- NULL
  variables$CORR.raw.cond.tab <- NULL
  variables$CORR.raw.adj.tab <- NULL
  
}) #observeEvent(input$CORR_user_reset

#### CORR demo cont dataset ####
shiny::observeEvent(input$CORR_demo_cont_upload, {
  shiny::isolate({
    shiny::withProgress(message='Correlation analysis', style='notification', detail="Upload data", value=0, {
      shinyjs::show('CORR_data_check_progress')
      shinyjs::show('CORR_data_check_successful')
      variables$CORR.data.check.progress <- tags$div(
        h2('Upload progress'),
        h4('(1/3) Check data frame format.'),
        style="font-size: 0px;")
      #### import demo dataset ####
      variables$CORR.SE <- readRDS('www/demo_dataset/Corr.rds')
      variables$CORR.SE.list <- LipidSigR::extract_summarized_experiment(variables$CORR.SE)
      variables$CORR.raw.abundance <- variables$CORR.SE.list$abundance
      variables$CORR.condition.col <- c("sample_name", "FEV1_FVC", "Emphysema", "Exacerbations")
      variables$CORR.raw.cond.tab <- variables$CORR.SE.list$group_info %>% 
        dplyr::select(dplyr::all_of(variables$CORR.condition.col))
      variables$CORR.adjusted.col <- c("sample_name", "Age", "Sex", "Smoking", "BMI", "FEV1")
      variables$CORR.raw.adj.tab <- variables$CORR.SE.list$group_info %>% 
        dplyr::select(dplyr::all_of(variables$CORR.adjusted.col))
      shiny::incProgress(0.33, detail='Check data format')
      variables$CORR.check.step1 <- inputFormat(abundance=variables$CORR.raw.abundance,
                                                group_info=NULL,
                                                condition_table=variables$CORR.raw.cond.tab, 
                                                adjusted_table=variables$CORR.raw.adj.tab,
                                                abundance_path='demo_exp.csv',
                                                condition_table_path='demo_condition_table.csv',
                                                adjusted_table_path='demo_adjusted_table.csv',
                                                analysis_type="Correlation",
                                             variables=variables, session=session)
      shiny::incProgress(0.33, detail='Check data format')
      if(variables$CORR.check.step1$logical == TRUE){
        variables$CORR.check.step2 <- data_check(abundance=variables$CORR.raw.abundance,
                                                 group_info=NULL,
                                                 condition_table=variables$CORR.raw.cond.tab, 
                                                 adjusted_table=variables$CORR.raw.adj.tab,
                                                 abundance_path='demo_exp.csv',
                                                 condition_table_path='demo_condition_table.csv',
                                                 adjusted_table_path='demo_adjusted_table.csv',
                                                 analysis_type="Correlation",
                                             variables=variables, session=session)
        output$CORR.Check.SE <- shiny::renderUI({
          shiny::isolate({
            variables$CORR.check.step2$return_div
          })
        })
        
        if(grepl('class="fas fa-xmark"',variables$CORR.check.step2$return_div)){
          shinyjs::show('CORR_data_warning_div')
          shinyjs::hide('CORR_data_processing_div')
        }else{
          shinyjs::show('CORR_data_warning_div')
          shinyjs::show('CORR_data_processing_div')
          variables$CORR.SE <- variables$CORR.check.step2$rawSE
        }
      }else{
        shinyjs::show('CORR_data_warning_div')
        shinyjs::hide('CORR_data_processing_div')
        output$CORR.Check.SE <- shiny::renderUI({
          isolate({
            variables$check_step1$return_div
          })
        })
      }
      variables$CORR.data.check.progress <- tags$div(
        h2('Upload progress'),
        h4('(2/3) Data check finish.'),
        style="font-size: 0px;"
      )
      incProgress(0.34, detail='Check data format finish')
    })
  })
}) #observeEvent(input$CORR_demo_cont_upload

#### CORR user dataset ####
shiny::observeEvent(input$CORR_user_upload, {
  shiny::isolate({
    shiny::withProgress(message='Correlation analysis', style='notification', detail="Upload data", value=0, {
      shinyjs::show('CORR_data_check_successful')
      shinyjs::show('CORR_data_check_progress')
      shiny::showNotification("Start uploading file...", type="message")
      #### import user dataset ####
      tryCatch(
        {
          ## abundance
          if(grepl('.xlsx',input$CORR_user_exp$datapath)){
            variables$CORR.raw.abundance <- readxl::read_excel(
              input$CORR_user_exp$datapath, na=c('', 'NA', 'na')) %>%  as.data.frame()
          }else{
            variables$CORR.raw.abundance <- data.table::fread(
              input$CORR_user_exp$datapath, header=TRUE, stringsAsFactors=FALSE,
              check.names=FALSE, data.table=FALSE, na.strings=c('', 'NA', 'na'))
          }
          ## condition_table
          if(grepl('.xlsx',input$CORR_user_cond$datapath)){
            variables$CORR.raw.cond.tab <- readxl::read_excel(
              input$CORR_user_cond$datapath,na=c('', 'NA', 'na')) %>% as.data.frame()
          }else{
            variables$CORR.raw.cond.tab <- data.table::fread(
              input$CORR_user_cond$datapath, header=TRUE, stringsAsFactors=FALSE,
              check.names=FALSE, data.table=FALSE, na.strings=c('', 'NA', 'na'))
            
          }
          variables$CORR.condition.col <- colnames(variables$CORR.raw.cond.tab)
          ## adjust_table
          if(!is.null(input$CORR_user_adj)){
            if(grepl('.xlsx',input$CORR_user_adj$datapath)){
              variables$CORR.raw.adj.tab <- readxl::read_excel(input$CORR_user_adj$datapath,na=c('', 'NA','na')) %>% as.data.frame()
            }else{
              variables$CORR.raw.adj.tab <- data.table::fread(input$CORR_user_adj$datapath, header=T,
                                                          stringsAsFactors=F, check.names=F, 
                                                          data.table=F, na.strings=c('', 'NA','na'))
            }
            variables$CORR.adjusted.col <- colnames(variables$CORR.raw.adj.tab)
          }else{
            variables$CORR.raw.adj.tab <- NULL
            variables$CORR.adjusted.col <- NULL
          }
          shiny::showNotification("Received uploaded file.", type="message")
        },
        error=function(e) {
          shinyWidgets::sendSweetAlert(
            session=session,
            title="Input data error!",
            text=as.character(message(e)),
            type="error")
          return()
        },
        warning=function(w) {
          shinyWidgets::sendSweetAlert(
            session=session,
            title="Input data warning!",
            text="Some error is in your dataset, it maybe cause some problem we cannot expected.",
            type="warning"
          )
          return()
        }
      )
      tryCatch(
        {
          variables$CORR.data.check.progress <- htmltools::tags$div(
            h2('Upload progress'),
            h4('(1/3) Check data frame format.'),
            style="font-size: 0px;")
          shiny::incProgress(0.33, detail='Check data format')
          variables$CORR.checkUTF8 <- check_utf8Format(abundance=variables$CORR.raw.abundance,
                                                       group_info=NULL,
                                                       condition_table=variables$CORR.raw.cond.tab, 
                                                       adjusted_table=variables$CORR.raw.adj.tab,
                                                       abundance_path=input$CORR_user_exp$datapath,
                                                       condition_table_path=input$CORR_user_cond$datapath,
                                                       adjusted_table_path=input$CORR_user_adj$datapath,
                                                       analysis_type="Correlation",
                                             variables=variables, session=session)
          
        },
        error=function(e) {
          shinyWidgets::show_alert(
            title='Error',
            text=HTML("<h4>Detect unknown input data format errors.</h4>
                               <h4>For the correct data format guidelines, please refer to the <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank'>FAQ</a></h4>.
                               <h4>If you need further assistance, please email us your data.(<a href='mailto:bioinfomics.web@gmail.com' target='_blank' style='color: darkblue;'>bioinfomics.web@gmail.com</a>)</h4>"),
            html=TRUE,
            type ='error'
          )
          return()
        }
      )
      if(variables$CORR.checkUTF8$logical == TRUE){
        shinyjs::show('CORR_data_Uploaded')
        variables$CORR.check.step1 <- inputFormat(abundance=variables$CORR.raw.abundance,
                                                  group_info=NULL,
                                                  condition_table=variables$CORR.raw.cond.tab, 
                                                  adjusted_table=variables$CORR.raw.adj.tab,
                                                  abundance_path=input$CORR_user_exp$datapath,
                                                  condition_table_path=input$CORR_user_cond$datapath,
                                                  adjusted_table_path=input$CORR_user_adj$datapath,
                                                  analysis_type="Correlation",
                                             variables=variables, session=session)
        shiny::incProgress(0.33, detail='Check data format')
        if(variables$CORR.check.step1$logical == TRUE){
          variables$CORR.raw.abundance <- variables$CORR.raw.abundance %>%
            dplyr::select(c(1, variables$CORR.raw.cond.tab$sample_name))
          variables$CORR.check.step2 <- data_check(abundance=variables$CORR.raw.abundance,
                                                   group_info=NULL,
                                                   condition_table=variables$CORR.raw.cond.tab, 
                                                   adjusted_table=variables$CORR.raw.adj.tab,
                                                   abundance_path=input$CORR_user_exp$datapath,
                                                   condition_table_path=input$CORR_user_cond$datapath,
                                                   adjusted_table_path=input$CORR_user_adj$datapath,
                                                   analysis_type="Correlation",
                                             variables=variables, session=session)
          if(variables$CORR.check.step2$lipid_id_pct >= 20){
            shinyWidgets::show_alert(
              title='Warning',
              text=HTML("<h4>Warning! The unrecognized lipids in the uploaded abundance data are more than 20%.</h4> 
                              <h4>We recommend revising the lipids' names. Please use Shorthand notation or refer to HMDB, SwissLipids, and LIPID MAPS LMSD styles for lipid input.</h4>
                              <h4>You can access the detailed instructions in our <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ12' target='_blank'>FAQ</a>.</h4>"),
              html=TRUE,
              type ='warning'
            )
          }
          output$CORR.Check.SE <- renderUI({
            isolate({
              variables$CORR.check.step2$return_div
            })
          })
          if(grepl('class="fas fa-xmark"',variables$CORR.check.step2$return_div)){
            shinyjs::show('CORR_data_warning_div')
            shinyjs::hide('CORR_data_processing_div')
          }else{
            variables$CORR.SE <- variables$CORR.check.step2$rawSE
            shinyjs::show('CORR_data_warning_div')
            shinyjs::show('CORR_data_processing_div')
          }
        }else{
          shinyjs::show('CORR_data_warning_div')
          shinyjs::hide('CORR_data_processing_div')
          output$CORR.Check.SE <- shiny::renderUI({
            isolate({
              variables$CORR.check.step1$return_div
            })
          })
        }
      }else{
        shinyjs::hide('CORR_data_Uploaded')
        shinyjs::show('CORR_data_warning_div')
        shinyjs::hide('CORR_data_processing_div')
        output$CORR.Check.SE <- shiny::renderUI({
          isolate({
            variables$CORR.checkUTF8$return_div
          })
        })
      }
      variables$CORR.data.check.progress <- tags$div(
        h2('Upload progress'),
        h4('(2/3) Data check finish.'),
        style="font-size: 0px;"
      )
      incProgress(0.34, detail='Check data format finish')
    })
  })
}) #observeEvent(input$CORR_user_upload

#### Output: CORR.raw.abundance ####
output$CORR.raw.abundance <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$CORR.raw.abundance), "Some error is in your expression data, please check your data and re-upload it."))
  DT::datatable(variables$CORR.raw.abundance,
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})

#### Output: CORR.cond.raw ####
output$CORR.cond.raw <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$CORR.raw.cond.tab), "Some error is in your condition table, please check your data and re-upload it."))
  DT::datatable(variables$CORR.raw.cond.tab %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})

#### Output: CORR.adj.raw ####
output$CORR.adj.raw <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$CORR.raw.adj.tab), "Some error is in your adjusted table, please check your data and re-upload it."))
  DT::datatable(variables$CORR.raw.adj.tab %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})


#### control user upload button ####
shiny::observe({
  
  if(is.null(input$CORR_user_exp) || is.null(input$CORR_user_cond)){
    shinyjs::disable("CORR_user_upload")
  }else{
    shinyjs::enable("CORR_user_upload")
  }
  
}) #observe

#### control user reset button ####
shiny::observeEvent(input$CORR_processing_start, {
  shiny::isolate({
    tryCatch(
      {
        shiny::withProgress(message='Correlation analysis', style='notification', detail="Data processing", value=0, {
          imputation_param <- switch(input$CORR_fill_NA,
                                     mean=NULL,
                                     median=NULL,
                                     min=input$CORR_fill_min,
                                     QRILC=input$CORR_fill_QRILC,
                                     SVD=input$CORR_fill_param,
                                     KNN=input$CORR_fill_KNN,
                                     IRMI=NULL,
                                     PPCA=input$CORR_fill_param,
                                     BPCA=input$CORR_fill_param)
          if(is.numeric(input$CORR_filtration_param)){
            filtration_param <- input$CORR_filtration_param
            if(input$CORR_filtration_param > 100){
              filtration_param <- 100
              shiny::showNotification("The value of remove features with more than % missing values must be between 5 and 100, so it is calculated by replacing it with 100.", type="warning")
              shiny::updateNumericInput(inputId='CORR_filtration_param', value=100)
            }else if(input$CORR_filtration_param < 5){
              filtration_param <- 5
              shiny::showNotification("The value of remove features with more than % missing values must be between 5 and 100, so it is calculated by replacing it with 5.", type="warning")
              shiny::updateNumericInput(inputId='CORR_filtration_param', value=5)
            }
          }else{
            filtration_param <- 70
            shiny::showNotification("The value of remove features with more than % missing values must be numeric, so it is calculated by replacing it with 70", type="warning")
            shiny::updateNumericInput(inputId='CORR_filtration_param', value=70)
          }
          if(!is.null(imputation_param)){
            if(input$CORR_fill_NA == 'min'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 0.5){
                  imputation_param <- 0.5
                  shiny::showNotification("The value of multiply by minimum must be between 0.1 and 0.5, so it is calculated by replacing it with 0.5.", type="warning")
                  shiny::updateNumericInput(inputId='CORR_fill_min', value=0.5)
                }else if(imputation_param < 0.1){
                  imputation_param <- 0.1
                  shiny::showNotification("The value of multiply by minimum must be between 0.1 and 0.5, so it is calculated by replacing it with 0.1.", type="warning")
                  shiny::updateNumericInput(inputId='CORR_fill_min',value=0.1)
                }
              }else{
                imputation_param <- 0.5
                shiny::showNotification("The value of multiply by minimum must be numeric, so it is calculated by replacing it with 0.5.", type="warning")
                shiny::updateNumericInput(inputId='CORR_fill_min',value=0.5)
              }
            }else if(input$CORR_fill_NA == 'QRILC'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of tune sigma must be between 0.1 and 1, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='CORR_fill_QRILC', value=1)
                }else if(imputation_param < 0.1){
                  imputation_param <- 0.1
                  shiny::showNotification("The value of tune sigma must be between 0.1 and 1, so it is calculated by replacing it with 0.1.", type="warning")
                  shiny::updateNumericInput(inputId='CORR_fill_QRILC', value=0.1)
                }
              }else{
                imputation_param <- 1
                shiny::showNotification("The value of tune sigma must be numeric, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='CORR_fill_QRILC', value=1)
              }
            }else if(input$CORR_fill_NA == 'SVD'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='CORR_fill_param', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='CORR_fill_param', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The value of nPCs must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='CORR_fill_param', value=3)
              }
            }else if(input$CORR_fill_NA == 'KNN'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The number of neighbors must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='CORR_fill_KNN', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The number of neighbors must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='CORR_fill_KNN', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The number of neighbors must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='CORR_fill_KNN', value=3)
              }
            }else if(input$CORR_fill_NA == 'PPCA'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='CORR_fill_param', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='CORR_fill_param', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The value of nPCs must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='CORR_fill_param', value=3)
              }
            }else if(input$CORR_fill_NA == 'BPCA'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='CORR_fill_param', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='CORR_fill_param', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The value of nPCs must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='CORR_fill_param', value=3)
              }
            }
          }
          
          variables$CORR.processed.SE <- LipidSigR::data_process(
            se=variables$CORR.SE, exclude_missing=input$CORR_rm_NA,
            exclude_missing_pct=filtration_param, 
            replace_na_method=input$CORR_fill_NA, replace_na_method_ref=imputation_param, 
            normalization=input$CORR_normalization, transform=input$CORR_transformation)
          variables$CORR.processed.SE.list <- LipidSigR::extract_summarized_experiment(variables$CORR.processed.SE)
          variables$CORR.processed.abundance <- variables$CORR.processed.SE.list$abundance
          variables$CORR.processed.lipid.char.tab <- variables$CORR.processed.SE.list$lipid_char_table
          variables$CORR.processed.lipid.char.tab <- char.tab.url(variables$CORR.processed.lipid.char.tab)
          shiny::incProgress(0.33, detail='Data processing')
          variables$CORR.processed.plot <- LipidSigR::plot_data_process(variables$CORR.SE, variables$CORR.processed.SE)
          
          variables$CORR.data.summary.div <- data_summary(se=variables$CORR.SE,
                                                          analysis_type="Correlation",
                                                          exclude_missing=input$CORR_rm_NA,
                                                          exclude_missing_pct=filtration_param,
                                                          replace_na_method=input$CORR_fill_NA,
                                                          replace_na_method_ref=imputation_param,
                                                          normalization=input$CORR_normalization,
                                                          transform=input$CORR_transformation)
          
          variables$CORR.species.lipid.char <- data.frame(aspect=names(LipidSigR::list_lipid_char(variables$CORR.processed.SE)$common_list),
                                                          characteristic=LipidSigR::list_lipid_char(variables$CORR.processed.SE)$common_list) %>%
            dplyr::add_row(aspect='Without side color', characteristic='none') %>%
            dplyr::mutate(aspect=factor(aspect),
                          characteristic=factor(characteristic)) %>% 
            dplyr::arrange(characteristic)
          output$CORR.species.corr.sidecolor <- shiny::renderUI({
            shiny::selectInput(inputId='CORR_species_corr_sidecolor', label='Side color by lipid characteristics:',
                               choices=split(as.list(levels(variables$CORR.species.lipid.char$characteristic)), variables$CORR.species.lipid.char$aspect),
                               selected='none', multiple=FALSE)
          })
          output$CORR.species.linear.sidecolor <- shiny::renderUI({
            shiny::selectInput(inputId='CORR_species_linear_sidecolor', label='Side color by lipid characteristics:',
                               choices=split(as.list(levels(variables$CORR.species.lipid.char$characteristic)), variables$CORR.species.lipid.char$aspect),
                               selected='none', multiple=FALSE)
          })
          variables$CORR.class.lipid.char <- data.frame(aspect=names(LipidSigR::list_lipid_char(variables$CORR.processed.SE)$common_list),
                                                        characteristic=LipidSigR::list_lipid_char(variables$CORR.processed.SE)$common_list) %>%
            dplyr::mutate(aspect=factor(aspect),
                          characteristic=factor(characteristic)) %>% 
            dplyr::arrange(characteristic)
          output$CORR.class.linear.lipid.char <- shiny::renderUI({
            shiny::selectInput(inputId='CORR_class_linear_lipid_char', label='Select the lipid characteristic:',
                               choices=split(as.list(levels(variables$CORR.class.lipid.char$characteristic)), variables$CORR.class.lipid.char$aspect),
                               selected='class', multiple=FALSE)
          })
          output$CORR.class.corr.lipid.char <- shiny::renderUI({
            shiny::selectInput(inputId='CORR_class_corr_lipid_char', label='Select the lipid characteristic:',
                               choices=split(as.list(levels(variables$CORR.class.lipid.char$characteristic)), variables$CORR.class.lipid.char$aspect),
                               selected='class', multiple=FALSE)
          })
          
          shiny::incProgress(0.33, detail='Data processing')
          variables$CORR.data.check.progress <-  htmltools::tags$div(
            h2('Upload progress'),
            h4('(3/3) Data Processing finish.'),
            style="font-size: 0px;")
          shinyjs::show('CORR_data_summary_div')
          shinyjs::show('CORR_data_process_table_div')
          shinyjs::show('CORR_start_div')
          variables$CORR.processed.plot.download.log <- 1
          shiny::incProgress(0.34, detail='Data processing finish')
        })
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Data processing error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) #observeEvent(input$CORR_processing_start

#### control user reset button ####
shiny::observeEvent(input$CORR_processing_reset, {
  
  #### shinyjs reset control panel ####
  shinyjs::reset('CORR_data_processing_div')

  #### clear processed variables ####
  variables$CORR.processed.SE             <- NULL
  variables$CORR.processed.SE.list        <- NULL
  variables$CORR.processed.abundance      <- NULL
  variables$CORR.processed.lipid.char.tab <- NULL
  variables$CORR.processed.plot           <- NULL
  variables$CORR.data.summary.div         <- NULL
  variables$CORR.species.lipid.char       <- NULL
  variables$CORR.class.lipid.char         <- NULL
  variables$CORR.species.corr.result      <- NULL
  variables$CORR.species.corr.download.log <- 0
  variables$CORR.species.linear.result    <- NULL
  variables$CORR.species.linear.download.log <- 0
  variables$CORR.class.corr.result        <- NULL
  variables$CORR.class.corr.download.log  <- 0
  variables$CORR.class.linear.result      <- NULL
  variables$CORR.class.linear.download.log <- 0

}) #observeEvent(input$CORR_processing_reset

#### Output: CORR.processed.abundance ####
output$CORR.processed.abundance <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$CORR.processed.abundance), "Some error is in your expression data, please check your data and re-upload it."))
  DT::datatable(variables$CORR.processed.abundance,
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller','FixedColumns'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             fixedColumns=list(leftColumns=1),
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})

#### Output: CORR.processed.cond ####
output$CORR.processed.cond <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$CORR.raw.cond.tab), "Some error is in your condition table, please check your data and re-upload it."))
  DT::datatable(variables$CORR.raw.cond.tab %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller','FixedColumns'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             fixedColumns=list(leftColumns=1),
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})

#### Output: CORR.processed.adj ####
output$CORR.processed.adj <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$CORR.raw.adj.tab), "Some error is in your adjusted table, please check your data and re-upload it."))
  DT::datatable(variables$CORR.raw.adj.tab %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller','FixedColumns'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             fixedColumns=list(leftColumns=1),
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})
#### Output: CORR.processed.lipid.char ####
output$CORR.processed.lipid.char <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$CORR.processed.lipid.char.tab), "Some error is in your lipid characteristics, please check your data and re-upload it."))
  DT::datatable(variables$CORR.processed.lipid.char.tab %>% 
                  dplyr::select(-c(LION.ID,LIPID.MAPS.ID,SwissLipids.ID,HMDB.ID,ChEBI.ID,KEGG.ID,LipidBank.ID,PubChem.CID,MetaNetX.ID,PlantFA.ID)) %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)), 
                escape=FALSE, selection='none', rownames=FALSE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller','FixedColumns'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             fixedColumns=list(leftColumns=1),
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})
#### Output: CORR.lipid.id ####
output$CORR.lipid.id <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$CORR.processed.lipid.char.tab), "Some error is in your lipid characteristics, please check your data and re-upload it."))
  DT::datatable(variables$CORR.processed.lipid.char.tab %>% 
                  dplyr::select(feature,LION.ID,LIPID.MAPS.ID,SwissLipids.ID,HMDB.ID,ChEBI.ID,KEGG.ID,LipidBank.ID,PubChem.CID,MetaNetX.ID,PlantFA.ID) %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)), 
                escape=FALSE, selection='none', rownames=FALSE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller','FixedColumns'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             fixedColumns=list(leftColumns=1)))
})

shiny::observeEvent(input$CORR.processed.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$CORR.processed.plot.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",
            display_pct=TRUE, value=33)
          output$CORR.processed.download <- shiny::downloadHandler(
            filename=function(){
              paste("CORR.processed.download.zip", sep="")
            },
            content=function(file){
              
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              
              pdf(file.path(temp_directory,'BoxPlot.before.process.pdf'),width=8, height=6)
              print(variables$CORR.processed.plot$static_boxPlot_before)
              dev.off()
              
              pdf(file.path(temp_directory,'Boxplot.after.process.pdf'),width=8, height=6)
              print(variables$CORR.processed.plot$static_boxPlot_after)
              dev.off()
              
              pdf(file.path(temp_directory,'Densityplot.before.process.pdf'),width=8, height=6)
              print(variables$CORR.processed.plot$static_densityPlot_before)
              dev.off()
              
              pdf(file.path(temp_directory,'Densityplot.after.process.pdf'),width=8, height=6)
              print(variables$CORR.processed.plot$static_densityPlot_after)
              dev.off()
              
              zip::zip(
                zipfile=file,
                files=dir(temp_directory),
                root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",
            display_pct=TRUE, value=66)
          variables$CORR.processed.plot.download.log <- 2
          shinyjs::runjs("document.getElementById('CORR.processed.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('CORR.processed.download').click();")
          shinyWidgets::updateProgressBar(
            session=session,
            id="data_progress",
            title="done",
            value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Data quality download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

output$CORR_data_check_progress <- shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$CORR.data.check.progress), ""))
  variables$CORR.data.check.progress
})

#### Output: CORR.data.summary ####
output$CORR.data.summary <-  shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$CORR.data.summary.div), ""))
  variables$CORR.data.summary.div
})

#### Output: CORR.before.processed.boxplot ####
output$CORR.before.processed.boxplot <-  plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$CORR.processed.plot$interactive_boxPlot_before), ""))
  variables$CORR.processed.plot$interactive_boxPlot_before
})

#### Output: CORR.after.processed.boxplot ####
output$CORR.after.processed.boxplot <-  plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$CORR.processed.plot$interactive_boxPlot_after), ""))
  variables$CORR.processed.plot$interactive_boxPlot_after
})

#### Output: CORR.before.processed.density ####
output$CORR.before.processed.density <-  plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$CORR.processed.plot$interactive_densityPlot_before), ""))
  variables$CORR.processed.plot$interactive_densityPlot_before
})

#### Output: CORR.after.processed.density ####
output$CORR.after.processed.density <-  plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$CORR.processed.plot$interactive_densityPlot_after), ""))
  variables$CORR.processed.plot$interactive_densityPlot_after
})


####################################
####  Correlation analysis tab  ####
####################################

#### control hide/show tabpanel: input$CORR_start ####
shiny::observeEvent(input$CORR_start, {
  
  shinyjs::show('CORR_tabPanel_div')
  shiny::showTab(inputId='CORR_analysis_tab', target='Lipid species analysis')
  shiny::showTab(inputId='CORR_analysis_tab', target='Lipid characteristics analysis')
  shiny::showTab(inputId='CORR_species_list', target='Correlation')
  shiny::showTab(inputId='CORR_species_list', target='Linear regression')
  shiny::showTab(inputId='CORR_class_list', target='Correlation')
  shiny::showTab(inputId='CORR_class_list', target='Linear regression')
}) #observeEvent(input$CORR_start


##########################################
##########################################
#####  Tab1: Lipid species analysis  #####
##########################################
##########################################

###############################################
####  Lipid species analysis: Correlation  ####
###############################################
shiny::observeEvent(input$CORR_species_list, {
  shiny::isolate({
    if(input$CORR_species_list == 'Correlation'){
      shinyjs::hide('CORR_species_linear_result_div')
      if(variables$CORR.species.corr.download.log != 0){
        shinyjs::show('CORR_species_corr_result_div')
      }
    }else if(input$CORR_species_list == 'Linear regression'){
      shinyjs::hide('CORR_species_corr_result_div')
      if(variables$CORR.species.linear.download.log != 0){
        shinyjs::show('CORR_species_linear_result_div')
      }
    }
  })
})

#### control reset button ####
shiny::observeEvent(input$CORR_species_corr_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('CORR_species_corr_result_div')
  
  #### shinyjs reset ####
  shinyjs::reset("CORR_species_corr_reset_div")
  
}) #observeEvent(input$CORR_species_corr_reset

#### control start button ####
shiny::observeEvent(input$CORR_species_corr_start, {
  #### Function: Clin_Cor_heatmap ####
  shiny::isolate({
    tryCatch(
      {
        if(is.numeric(input$CORR_species_corr_pval)){
          sig_pvalue <- input$CORR_species_corr_pval
          if(input$CORR_species_corr_pval > 1){
            sig_pvalue <- 1
            shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='CORR_species_corr_pval', value=1)
          }else if(input$CORR_species_corr_pval < 0.001){
            sig_pvalue <- 0.001
            shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
            shiny::updateNumericInput(inputId='CORR_species_corr_pval', value=0.001)
          }
        }else{
          sig_pvalue <- 1
          shiny::showNotification("The p-value must be numeric, so it is calculated by replacing it with 1.", type="warning")
          shiny::updateNumericInput(inputId='CORR_species_corr_pval', value=1)
        }
        if(is.numeric(input$CORR_species_corr_coef)){
          sig_cor_coef <- input$CORR_species_corr_coef
          if(input$CORR_species_corr_coef > 1){
            sig_cor_coef <- 1
            shiny::showNotification("The correlation coefficient cutoff must be between 0 and 1, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='CORR_species_corr_coef', value=1)
          }else if(input$CORR_species_corr_coef < 0){
            sig_cor_coef <- 0
            shiny::showNotification("The correlation coefficient cutoff must be between 0 and 1, so it is calculated by replacing it with 0.", type="warning")
            shiny::updateNumericInput(inputId='CORR_species_corr_coef', value=0)
          }
        }else{
          sig_cor_coef <- 0
          shiny::showNotification("The p-value must be numeric, so it is calculated by replacing it with 0.", type="warning")
          shiny::updateNumericInput(inputId='CORR_species_corr_coef', value=0)
        }
        if(input$CORR_species_corr_sidecolor != 'none'){
          side_color_char <- input$CORR_species_corr_sidecolor
        }else{
          side_color_char <- NULL
        }
        variables$CORR.species.corr.result <- LipidSigR::corr_cor_heatmap(
          processed_se=variables$CORR.processed.SE, char=NULL,
          condition_col=variables$CORR.condition.col[-1],
          side_color_char=side_color_char, 
          correlation=input$CORR_species_corr_method,
          significant=input$CORR_species_corr_sig_p,
          p_cutoff=sig_pvalue, 
          adjust_p_method=input$CORR_species_corr_adj_stat_method,
          cor_coef_cutoff=sig_cor_coef, 
          distfun=input$CORR_species_corr_dist,
          hclustfun=input$CORR_species_corr_hclust,
          heatmap_col=input$CORR_species_corr_color, 
          transform=input$CORR_transformation,
          type='Sp')
        variables$CORR.species.corr.download.log <- 1
        #### shinyjs show/hide results ####
        shinyjs::show('CORR_species_corr_result_div')
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species correlation analysis error!",
          text=as.character(e$message),
          type="error")
        #### shinyjs show/hide results ####
        shinyjs::hide('CORR_species_corr_result_div')
      }
    )
  }) #isolate
}) #observeEvent(input$CORR_species_corr_start



#### Output: CORR.species.corr.heatmap ####
output$CORR.species.corr.heatmap <- plotly::renderPlotly({
  
  shiny::validate(shiny::need(!is.null(variables$CORR.species.corr.result$interactive_heatmap), "Plot not showing. Please adjust the cutoffs."))
  variables$CORR.species.corr.result$interactive_heatmap
  
}) #output$CORR.species.corr.heatmap <- renderPlotly

#### Output: CORR.species.corr.download ####
shiny::observeEvent(input$CORR.species.corr.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$CORR.species.corr.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$CORR.species.corr.download <- shiny::downloadHandler(
            filename=function(){
              paste("CORR.species.corr.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## heatmap ##
              grDevices::pdf(file.path(temp_directory,'heatmap.pdf'), width=8, height=6)
              print(variables$CORR.species.corr.result$static_heatmap)
              grDevices::dev.off()
              ## table  ##
              utils::write.csv(variables$CORR.species.corr.result$heatmap_matrix, file=file.path(temp_directory,'heatmap.matrix.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$CORR.species.corr.download.log <- 2
          shinyjs::runjs("document.getElementById('CORR.species.corr.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('CORR.species.corr.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species correlation analysis download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

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
shiny::observeEvent(input$CORR_species_linear_start, {
  shiny::isolate({
    tryCatch(
      {
        if(is.numeric(input$CORR_species_linear_pval)){
          sig_pvalue <- input$CORR_species_linear_pval
          if(input$CORR_species_linear_pval > 1){
            sig_pvalue <- 1
            shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='CORR_species_linear_pval', value=1)
          }else if(input$CORR_species_linear_pval < 0.001){
            sig_pvalue <- 0.001
            shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
            shiny::updateNumericInput(inputId='CORR_species_linear_pval', value=0.001)
          }
        }else{
          sig_pvalue <- 1
          shiny::showNotification("The p-value must be numeric, so it is calculated by replacing it with 1.", type="warning")
          shiny::updateNumericInput(inputId='CORR_species_linear_pval', value=1)
        }
        if(input$CORR_species_linear_sidecolor != 'none'){
          side_color_char <- input$CORR_species_linear_sidecolor
        }else{
          side_color_char <- NULL
        }
        variables$CORR.species.linear.result <- LipidSigR::corr_lr_heatmap(
          processed_se=variables$CORR.processed.SE, char=NULL,
          condition_col=variables$CORR.condition.col[-1],
          adjusted_col=variables$CORR.adjusted.col[-1],
          side_color_char=side_color_char,
          significant=input$CORR_species_linear_sig_p,
          p_cutoff=sig_pvalue,
          adjust_p_method=input$CORR_species_linear_adj_stat_method,
          distfun=input$CORR_species_linear_dist,
          hclustfun=input$CORR_species_linear_hclust,
          heatmap_col=input$CORR_species_linear_color,
          transform=input$CORR_transformation,
          type='Sp')
        variables$CORR.species.linear.download.log <- 1
        #### shinyjs show/hide results ####
        shinyjs::show('CORR_species_linear_result_div')
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species linear regression analysis error!",
          text=as.character(e$message),
          type="error")
        #### shinyjs show/hide results ####
        shinyjs::hide('CORR_species_linear_result_div')
      }
    )
  }) #isolate
}) #observeEvent(input$CORR_species_linear_start

#### Output: CORR.species.linear.heatmap ####
output$CORR.species.linear.heatmap <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$CORR.species.linear.result$interactive_heatmap), "Plot not showing. Please adjust the cutoffs."))
  variables$CORR.species.linear.result$interactive_heatmap
}) #output$CORR.species.linear.heatmap <- renderPlotly

#### Output: CORR.species.linear.download ####
shiny::observeEvent(input$CORR.species.linear.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$CORR.species.linear.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$CORR.species.linear.download <- shiny::downloadHandler(
            filename=function(){
              paste("CORR.species.linear.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## heatmap ##
              grDevices::pdf(file.path(temp_directory,'heatmap.pdf'), width=8, height=6)
              print(variables$CORR.species.corr.result$static_heatmap)
              grDevices::dev.off()
              ## table  ##
              utils::write.csv(variables$CORR.species.corr.result$heatmap_matrix, file=file.path(temp_directory,'heatmap.matrix.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$CORR.species.linear.download.log <- 2
          shinyjs::runjs("document.getElementById('CORR.species.linear.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('CORR.species.linear.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species linear regression analysis download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

###########################################
###########################################
#####  Tab2: Lipid category analysis  #####
###########################################
###########################################

#############################################
####  Lipid class analysis: Correlation  ####
#############################################
observeEvent(input$CORR_class_list, {
  isolate({
    if(input$CORR_class_list == 'Correlation'){
      shinyjs::hide('CORR_class_linear_result_div')
      if(variables$CORR.class.corr.download.log != 0){
        shinyjs::show('CORR_class_corr_result_div')
      }
    }else if(input$CORR_class_list == 'Linear regression'){
      shinyjs::hide('CORR_class_corr_result_div')
      if(variables$CORR.class.linear.download.log != 0){
        shinyjs::show('CORR_class_linear_result_div')
      }
    }
  })
})
#### control reset button ####
observeEvent(input$CORR_class_corr_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('CORR_class_corr_result_div')
  
  #### shinyjs reset ####
  shinyjs::reset("CORR_class_corr_reset_div")
  
}) #observeEvent(input$CORR_class_corr_reset

#### control start button ####
observeEvent(input$CORR_class_corr_start, {
  shiny::isolate({
    tryCatch(
      {
        if(is.numeric(input$CORR_class_corr_pval)){
          sig_pvalue <- input$CORR_class_corr_pval
          if(input$CORR_class_corr_pval > 1){
            sig_pvalue <- 1
            shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='CORR_class_corr_pval',value=1)
          }else if(input$CORR_class_corr_pval < 0.001){
            sig_pvalue <- 0.001
            shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
            shiny::updateNumericInput(inputId='CORR_class_corr_pval',value=0.001)
          }
        }else{
          sig_pvalue <- 1
          shiny::showNotification("The p-value must be numeric, so it is calculated by replacing it with 1.", type="warning")
          shiny::updateNumericInput(inputId='CORR_class_corr_pval', value=1)
        }
        if(is.numeric(input$CORR_class_corr_coef)){
          sig_cor_coef <- input$CORR_class_corr_coef
          if(input$CORR_class_corr_coef > 1){
            sig_cor_coef <- 1
            shiny::showNotification("The correlation coefficient cutoff must be between 0 and 1, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='CORR_class_corr_coef', value=1)
          }else if(input$CORR_class_corr_coef < 0){
            sig_cor_coef <- 0
            shiny::showNotification("The correlation coefficient cutoff must be between 0 and 1, so it is calculated by replacing it with 0.", type="warning")
            shiny::updateNumericInput(inputId='CORR_class_corr_coef', value=0)
          }
        }else{
          sig_cor_coef <- 0
          shiny::showNotification("The p-value must be numeric, so it is calculated by replacing it with 0.", type="warning")
          shiny::updateNumericInput(inputId='CORR_class_corr_coef', value=0)
        }
        variables$CORR.class.corr.result <- LipidSigR::corr_cor_heatmap(
          processed_se=variables$CORR.processed.SE, 
          char=input$CORR_class_corr_lipid_char, 
          condition_col=variables$CORR.condition.col[-1],
          side_color_char=NULL,
          correlation=input$CORR_class_corr_method,
          significant=input$CORR_class_corr_sig_p,
          p_cutoff=sig_pvalue,
          adjust_p_method=input$CORR_class_corr_adj_stat_method,
          cor_coef_cutoff=sig_cor_coef, 
          distfun=input$CORR_class_corr_dist,
          hclustfun=input$CORR_class_corr_hclust,
          heatmap_col=input$CORR_class_corr_color, 
          transform=input$CORR_transformation,
          type='Char')
        variables$CORR.class.corr.download.log <- 1
        #### shinyjs show/hide results ####
        shinyjs::show('CORR_class_corr_result_div')
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics correlation analysis error!",
          text=as.character(e$message),
          type="error")
        #### shinyjs show/hide results ####
        shinyjs::hide('CORR_class_corr_result_div')
      }
    )
  }) #isolate
}) #observeEvent(input$CORR_class_corr_start

#### Output: CORR.class.corr.heatmap ####
output$CORR.class.corr.heatmap <- plotly::renderPlotly({
  
  shiny::validate(shiny::need(!is.null(variables$CORR.class.corr.result$interactive_heatmap), "Plot not showing. Please adjust the cutoffs."))
  variables$CORR.class.corr.result$interactive_heatmap
  
}) #output$CORR.class.corr.heatmap <- renderPlotly

shiny::observeEvent(input$CORR.class.corr.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$CORR.class.corr.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$CORR.class.corr.download <- shiny::downloadHandler(
            filename=function(){
              paste("CORR.class.corr.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## heatmap ##
              grDevices::pdf(file.path(temp_directory,'heatmap.pdf'), width=8, height=6)
              print(variables$CORR.class.corr.result$static_heatmap)
              grDevices::dev.off()
              ## table  ##
              utils::write.csv(variables$CORR.class.corr.result$heatmap_matrix, file=file.path(temp_directory,'heatmap.matrix.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$CORR.class.corr.download.log <- 2
          shinyjs::runjs("document.getElementById('CORR.class.corr.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('CORR.class.corr.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics correlation analysis download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
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
shiny::observeEvent(input$CORR_class_linear_start, {
  shiny::isolate({
    tryCatch(
      {
        if(is.numeric(input$CORR_class_linear_pval)){
          sig_pvalue <- input$CORR_class_linear_pval
          if(input$CORR_class_linear_pval > 1){
            sig_pvalue <- 1
            shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='CORR_class_linear_pval',value=1)
          }else if(input$CORR_class_linear_pval < 0.001){
            sig_pvalue <- 0.001
            shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
            shiny::updateNumericInput(inputId='CORR_class_linear_pval',value=0.001)
          }
        }else{
          sig_pvalue <- 1
          shiny::showNotification("The p-value must be numeric, so it is calculated by replacing it with 1.", type="warning")
          shiny::updateNumericInput(inputId='CORR_class_linear_pval', value=1)
        }
        variables$CORR.class.linear.result <- LipidSigR::corr_lr_heatmap(
          processed_se=variables$CORR.processed.SE,
          char=input$CORR_class_linear_lipid_char,
          condition_col=variables$CORR.condition.col[-1],
          adjusted_col=variables$CORR.adjusted.col[-1],
          side_color_char=NULL,
          significant=input$CORR_class_linear_sig_p,
          p_cutoff=sig_pvalue,
          adjust_p_method=input$CORR_class_linear_adj_stat_method,
          distfun=input$CORR_class_linear_dist,
          hclustfun=input$CORR_class_linear_hclust,
          heatmap_col=input$CORR_class_linear_color,
          transform=input$CORR_transformation,
          type='Char')
        variables$CORR.class.linear.download.log <- 1
        #### shinyjs show/hide results ####
        shinyjs::show('CORR_class_linear_result_div')
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics linear regression analysis error!",
          text=as.character(e$message),
          type="error")
        #### shinyjs show/hide results ####
        shinyjs::hide('CORR_class_linear_result_div')
      }
    )
  }) #isolate
}) #observeEvent(input$CORR_class_linear_start

#### Output: CORR.class.linear.heatmap ####
output$CORR.class.linear.heatmap <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$CORR.class.linear.result$interactive_heatmap), "Plot not showing. Please adjust the cutoffs."))
  variables$CORR.class.linear.result$interactive_heatmap
}) #output$CORR.class.linear.heatmap <- renderPlotly

#### Output: CORR.class.linear.download ####
shiny::observeEvent(input$CORR.class.linear.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$CORR.class.linear.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$CORR.class.linear.download <- shiny::downloadHandler(
            filename=function(){
              paste("CORR.class.linear.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## heatmap ##
              grDevices::pdf(file.path(temp_directory,'heatmap.pdf'), width=8, height=6)
              print(variables$CORR.class.linear.result$static_heatmap)
              grDevices::dev.off()
              ## table  ##
              utils::write.csv(variables$CORR.class.linear.result$heatmap_matrix, file=file.path(temp_directory,'heatmap.matrix.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$CORR.class.linear.download.log <- 2
          shinyjs::runjs("document.getElementById('CORR.class.linear.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('CORR.class.linear.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics linear regression analysis download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

