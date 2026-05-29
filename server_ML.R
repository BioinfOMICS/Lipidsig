##LipidSig v2 test1.v1

#######################################
#######################################
######                           ######
######   Machine Learning Page   ######
######                           ######
#######################################
#######################################

#### output ML.data.check.progress ####
output$ML.data.check.progress <- shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$ML.data.check.progress), ""))
  variables$ML.data.check.progress
})

##########################
####  ML Data Source  ####
##########################

#### Output: ML.demo.download ####
output$ML.demo.download <- shiny::downloadHandler(
  filename=function() {
    "Machine_learning_example_dataset.zip"
  },
  content=function(file) {
    file.copy("www/download_demo_dataset/ML.zip", file)
  },
  contentType="application/zip"
) 
shiny::outputOptions(output, "ML.demo.download", suspendWhenHidden=FALSE)

#### ML demo dataset ####
shiny::observeEvent(input$ML_demo_upload, {
  shiny::isolate({
    shiny::withProgress(message='Machine learning analysis', style='notification', detail="Upload data", value=0, {
      shinyjs::show('ML_data_check_successful')
      shinyjs::show('ML_data_check_progress')
      variables$ML.data.check.progress <- htmltools::tags$div(
        htmltools::h2('Upload progress'),
        htmltools::h4('(1/3) Check data frame format.'),
        style="font-size: 0px;"
      )
      #### import demo dataset ####
      variables$ML.SE <- readRDS('www/demo_dataset/ML.rds')
      variables$ML.SE.list <- LipidSigR::extract_summarized_experiment(variables$ML.SE)
      variables$ML.raw.abundance <- variables$ML.SE.list$abundance
      variables$ML.cond.tab <- variables$ML.SE.list$group_info 
      shiny::incProgress(0.33, detail='Check data format')
      variables$ML.check.step1 <- inputFormat(abundance=variables$ML.raw.abundance,
                                              group_info=NULL,
                                              condition_table=variables$ML.cond.tab,
                                              abundance_path='demo_exp.csv',
                                              condition_table_path='demo_condition_table.csv',
                                              analysis_type="ML",
                                             variables=variables, session=session)
      shiny::incProgress(0.33, detail='Check data format')
      if(variables$ML.check.step1$logical == TRUE){
        variables$ML.check.step2 <- data_check(abundance=variables$ML.raw.abundance,
                                               group_info=NULL,
                                               condition_table=variables$ML.cond.tab,
                                               abundance_path='demo_exp.csv',
                                               condition_table_path='demo_condition_table.csv',
                                               analysis_type="ML",
                                             variables=variables, session=session)
        output$ML.Check.SE <- shiny::renderUI({
          shiny::isolate({
            variables$ML.check.step2$return_div
          })
        })
        variables$ML.SE <- variables$ML.check.step2$rawSE
        if(grepl('class="fas fa-xmark"', variables$ML.check.step2$return_div)){
          shinyjs::show('ML_data_warning_div')
          shinyjs::hide('ML_data_processing_div')
          shinyjs::hide('ML_result_div')
        }else{
          shinyjs::show('ML_data_warning_div')
          shinyjs::show('ML_data_processing_div')
          shinyjs::hide('ML_result_div')
          abundance_mat <- SummarizedExperiment::assay(variables$ML.check.step2$rawSE) %>%
            as.data.frame
          lipid_char <- SummarizedExperiment::rowData(variables$ML.check.step2$rawSE) %>%
            as.data.frame
          variables$ML.SE <- SummarizedExperiment::SummarizedExperiment(
            assays=list(abundance=as.matrix(abundance_mat)),
            rowData=S4Vectors::DataFrame(lipid_char, row.names=lipid_char$feature),
            colData=variables$ML.cond.tab)
        }
        variables$ML.data.check.progress <- htmltools::tags$div(
          htmltools::h2('Upload progress'),
          htmltools::h4('(2/3) Data check finish.'),
          style="font-size: 0px;")
      }else{
        shinyjs::show('ML_data_warning_div')
        shinyjs::hide('ML_data_processing_div')
        shinyjs::hide('ML_result_div')
      }
      shiny::incProgress(0.34, detail='Check data format finish')
    })
  })
  
}) #shiny::observeEvent(input$ML_demo_upload

#### ML user dataset ####
shiny::observeEvent(input$ML_user_upload, {
  shiny::isolate({
    shiny::withProgress(message='Machine learning analysis', style='notification', detail="Upload data", value=0, {
      shinyjs::show('ML_data_check_successful')
      shinyjs::show('ML_data_check_progress')
      shiny::showNotification("Start uploading file...", type="message")
      tryCatch(
        {
          #### import demo dataset ####
          ## abundance
          if(grepl('.xlsx',input$ML_user_abundance$datapath)){
            variables$ML.raw.abundance <- readxl::read_excel(
              input$ML_user_abundance$datapath,na=c('',  'NA', 'na')) %>% as.data.frame()
          }else{
            variables$ML.raw.abundance <- data.table::fread(
              input$ML_user_abundance$datapath, header=TRUE,
              stringsAsFactors=FALSE, check.names=FALSE, 
              data.table=FALSE, na.strings=c('', 'NA', 'na'))
          }
          ## condition_table
          if(grepl('.xlsx', input$ML_user_cond$datapath)){
            variables$ML.cond.tab <- readxl::read_excel(
              input$ML_user_cond$datapath ,na=c('', 'NA', 'na')) %>% as.data.frame()
          }else{
            variables$ML.cond.tab <- data.table::fread(
              input$ML_user_cond$datapath, header=TRUE,
              stringsAsFactors=FALSE, check.names=FALSE, 
              data.table=FALSE, na.strings=c('', 'NA', 'na'))
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
            type="warning")
          return()
        }
      )
      
      tryCatch(
        {
          variables$ML.data.check.progress <- htmltools::HTML('<div style="font-size: 0px;">
                                                                   <h2>Upload progress</h2>
                                                                   <h4>(1/3) Check data frame format.</h4></div>')
          shiny::incProgress(0.33, detail='Check data format')
          variables$ML.checkUTF8 <- check_utf8Format(abundance=variables$ML.raw.abundance,
                                                  group_info=NULL,
                                                  condition_table=variables$ML.cond.tab,
                                                  abundance_path=input$ML_user_abundance$datapath,
                                                  condition_table_path=input$ML_user_cond$datapath,
                                                  analysis_type="ML",
                                             variables=variables, session=session)
          if(variables$ML.checkUTF8$logical == TRUE){
            shinyjs::show('ML_data_Uploaded')
            #### data check ####
            variables$ML.check.step1 <- inputFormat(abundance=variables$ML.raw.abundance,
                                                    group_info=NULL,
                                                    condition_table=variables$ML.cond.tab,
                                                    abundance_path=input$ML_user_abundance$datapath,
                                                    condition_table_path=input$ML_user_cond$datapath,
                                                    analysis_type="ML",
                                             variables=variables, session=session)
            shiny::incProgress(0.33, detail='Check data format')
            if(variables$ML.check.step1$logical == TRUE){
              variables$ML.raw.abundance <- variables$ML.raw.abundance %>%
                dplyr::select(c(1, variables$ML.cond.tab$sample_name))
              variables$ML.check.step2 <- data_check(abundance=variables$ML.raw.abundance,
                                                     group_info=NULL,
                                                     condition_table=variables$ML.cond.tab,
                                                     abundance_path=input$ML_user_abundance$datapath,
                                                     condition_table_path=input$ML_user_cond$datapath,
                                                     analysis_type="ML",
                                             variables=variables, session=session)
              if(variables$ML.check.step2$lipid_id_pct >= 20){
                shinyWidgets::show_alert(
                  title='Warning',
                  text=HTML("<h4>Warning! The unrecognized lipids in the uploaded abundance data are more than 20%.</h4> 
                              <h4>We recommend revising the lipids' names. Please use Shorthand notation or refer to HMDB, SwissLipids, and LIPID MAPS LMSD styles for lipid input.</h4>
                              <h4>You can access the detailed instructions in our <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ12' target='_blank'>FAQ</a>.</h4>"),
                  html=TRUE, type ='warning' )
              }
              output$ML.Check.SE <- shiny::renderUI({
                shiny::isolate({
                  variables$ML.check.step2$return_div
                })
              })
              variables$ML.SE <- variables$ML.check.step2$rawSE
              if(grepl('class="fas fa-xmark"', variables$ML.check.step2$return_div)){
                shinyjs::show('ML_data_warning_div')
                shinyjs::hide('ML_data_processing_div')
                shinyjs::hide('ML_result_div')
              }else{
                shinyjs::show('ML_data_warning_div')
                shinyjs::show('ML_data_processing_div')
                shinyjs::hide('ML_result_div')
              }
            }else{
              shinyjs::show('ML_data_warning_div')
              shinyjs::hide('ML_data_processing_div')
              shinyjs::hide('ML_result_div')
              output$ML.Check.SE <- shiny::renderUI({
                shiny::isolate({
                  variables$ML.check.step1$return_div
                })
              })
            }
          }else{
            shinyjs::hide('ML_data_Uploaded')
            shinyjs::show('ML_data_warning_div')
            shinyjs::hide('ML_data_processing_div')
            shinyjs::hide('ML_result_div')
            output$ML.Check.SE <- shiny::renderUI({
              shiny::isolate({
                variables$ML.checkUTF8$return_div
              })
            })
          }
          variables$ML.data.check.progress <- htmltools::HTML('<div style="font-size: 0px;">
                                                                   <h2>Upload progress</h2>
                                                                   <h4>(2/3) Data check finish.</h4></div>')
          shiny::incProgress(0.34, detail='Check data format finish')
        },
        error=function(e) {
          shinyWidgets::show_alert(
            title='Error',
            text=HTML("<h4>Detect unknown input data format errors.</h4>
                               <h4>For the correct data format guidelines, please refer to the <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank'>FAQ</a></h4>.
                               <h4>If you need further assistance, please email us your data.(<a href='mailto:bioinfomics.web@gmail.com' target='_blank' style='color: darkblue;'>bioinfomics.web@gmail.com</a>)</h4>"),
            html=TRUE,type ='error')
          return()
        }
      )
    })
  })
  
}) #shiny::observeEvent(input$ML_user_upload

#### Output: ML.raw.exp ####
output$ML.raw.exp <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$ML.raw.abundance), "Some error is in your expression data, please check your data and re-upload it."))
  DT::datatable(variables$ML.raw.abundance, 
                escape=FALSE, selection='none', rownames=FALSE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})
#### Output: ML.raw.cond ####
output$ML.raw.cond <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$ML.cond.tab), "Some error is in your condition table, please check your data and re-upload it."))
  DT::datatable(variables$ML.cond.tab %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)), 
                escape=FALSE, selection='none', rownames=FALSE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})

shiny::observeEvent(input$ML_processing_start, {
  shiny::isolate({
    tryCatch(
      {
        shiny::withProgress(message='Machine learning analysis', style='notification', detail="Data processing", value=0, {
          imputation_param <- switch(input$ML_fill_NA,
                                     mean=NULL,
                                     median=NULL,
                                     min=input$ML_fill_min,
                                     QRILC=input$ML_fill_QRILC,
                                     SVD=input$ML_fill_param,
                                     KNN=input$ML_fill_KNN,
                                     IRMI=NULL,
                                     PPCA=input$ML_fill_param,
                                     BPCA=input$ML_fill_param) 
          if(is.numeric(input$ML_filtration_param)){
            filtration_param <- input$ML_filtration_param
            if(input$ML_filtration_param > 100){
              filtration_param <- 100
              shiny::showNotification("The value of remove features with more than % missing values must be between 5 and 100, so it is calculated by replacing it with 100.", type="warning")
              shiny::updateNumericInput(inputId='ML_filtration_param', value=100)
            }else if(input$ML_filtration_param < 5){
              filtration_param <- 5
              shiny::showNotification("The value of remove features with more than % missing values must be between 5 and 100, so it is calculated by replacing it with 5.", type="warning")
              shiny::updateNumericInput(inputId='ML_filtration_param', value=5)
            }
          }else{
            filtration_param <- 70
            shiny::showNotification("The value of remove features with more than % missing values must be numeric, so it is calculated by replacing it with 70", type="warning")
            shiny::updateNumericInput(inputId='ML_filtration_param', value=70)
          }
          if(!is.null(imputation_param)){
            if(input$ML_fill_NA == 'min'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 0.5){
                  imputation_param <- 0.5
                  shiny::showNotification("The value of multiply by minimum must be between 0.1 and 0.5, so it is calculated by replacing it with 0.5.", type="warning")
                  shiny::updateNumericInput(inputId='ML_fill_min', value=0.5)
                }else if(imputation_param < 0.1){
                  imputation_param <- 0.1
                  shiny::showNotification("The value of multiply by minimum must be between 0.1 and 0.5, so it is calculated by replacing it with 0.1.", type="warning")
                  shiny::updateNumericInput(inputId='ML_fill_min',value=0.1)
                }
              }else{
                imputation_param <- 0.5
                shiny::showNotification("The value of multiply by minimum must be numeric, so it is calculated by replacing it with 0.5.", type="warning")
                shiny::updateNumericInput(inputId='ML_fill_min',value=0.5)
              }
            }else if(input$ML_fill_NA == 'QRILC'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of tune sigma must be between 0.1 and 1, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='ML_fill_QRILC', value=1)
                }else if(imputation_param < 0.1){
                  imputation_param <- 0.1
                  shiny::showNotification("The value of tune sigma must be between 0.1 and 1, so it is calculated by replacing it with 0.1.", type="warning")
                  shiny::updateNumericInput(inputId='ML_fill_QRILC', value=0.1)
                }
              }else{
                imputation_param <- 1
                shiny::showNotification("The value of tune sigma must be numeric, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='ML_fill_QRILC', value=1)
              }
            }else if(input$ML_fill_NA == 'SVD'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='ML_fill_param', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='ML_fill_param', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The value of nPCs must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='ML_fill_param', value=3)
              }
            }else if(input$ML_fill_NA == 'KNN'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The number of neighbors must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='ML_fill_KNN', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The number of neighbors must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='ML_fill_KNN', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The number of neighbors must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='ML_fill_KNN', value=3)
              }
            }else if(input$ML_fill_NA == 'PPCA'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='ML_fill_param', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='ML_fill_param', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The value of nPCs must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='ML_fill_param', value=3)
              }
            }else if(input$ML_fill_NA == 'BPCA'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='ML_fill_param', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='ML_fill_param', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The value of nPCs must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='ML_fill_param', value=3)
              }
            }
          }
          shiny::incProgress(0.33, detail='Data processing')
          variables$ML.processed.SE <- LipidSigR::data_process(
            se=variables$ML.SE, exclude_missing=input$ML_rm_NA,
            exclude_missing_pct=filtration_param, 
            replace_na_method=input$ML_fill_NA, replace_na_method_ref=imputation_param, 
            normalization=input$ML_normalization, transform=input$ML_transformation)
          variables$ML.processed.SE.list <- LipidSigR::extract_summarized_experiment(variables$ML.processed.SE)
          variables$ML.processed.abundance <- variables$ML.processed.SE.list$abundance
          variables$ML.processed.lipid.char.tab <- variables$ML.processed.SE.list$lipid_char_table
          variables$ML.processed.lipid.char.tab <- char.tab.url(variables$ML.processed.lipid.char.tab)
          
          variables$ML.processed.plot <- LipidSigR::plot_data_process(variables$ML.SE, variables$ML.processed.SE)
          
          variables$ML.data.summary.div <- data_summary(se=variables$ML.SE,
                                                        analysis_type="ML",
                                                        exclude_missing=input$ML_rm_NA,
                                                        exclude_missing_pct=filtration_param,
                                                        replace_na_method=input$ML_fill_NA,
                                                        replace_na_method_ref=imputation_param,
                                                        normalization=input$ML_normalization,
                                                        transform=input$ML_transformation)
          
          variables$ML.lipid.char <- data.frame(aspect=names(LipidSigR::list_lipid_char(variables$ML.processed.SE)$ml_char_list),
                                                characteristic=LipidSigR::list_lipid_char(variables$ML.processed.SE)$ml_char_list) %>%
            dplyr::add_row(aspect='Without side color', characteristic='none') %>%
            dplyr::mutate(aspect=factor(aspect),
                          characteristic=factor(characteristic)) %>% 
            dplyr::arrange(characteristic)
          output$ML.add.var <- shiny::renderUI({
            shiny::selectInput(inputId='ML_add_var', label='Additional variable:',
                               choices=split(as.list(levels(variables$ML.lipid.char$characteristic)), variables$ML.lipid.char$aspect),
                               selected='class', multiple=TRUE)
          })
          shinyjs::show('ML_data_summary_div')
          shinyjs::show('ML_data_process_table_div')
          shinyjs::show('ML_data_control_panel_div')
          variables$ML.data.check.progress <-   tags$div(
            htmltools::h2('Upload progress'),
            htmltools::h4('(3/3) Data Processing finish.'),
            style="font-size: 0px;")
          variables$ML.processed.plot.download.log <- 1
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
}) #shiny::observeEvent(input$ML_processing_start

#### Output: ML.processed.abundance ####
output$ML.processed.abundance <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$ML.processed.abundance), "Some error is in your expression data, please check your data and re-upload it."))
  DT::datatable(variables$ML.processed.abundance %>% 
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
#### Output: ML.processed.cond ####
output$ML.processed.cond <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$ML.cond.tab), "Some error is in your condition table, please check your data and re-upload it."))
  DT::datatable(variables$ML.cond.tab %>% 
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
#### Output: ML.processed.lipid.char ####
output$ML.processed.lipid.char <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$ML.processed.lipid.char.tab), "Some error is in your condition table, please check your data and re-upload it."))
  DT::datatable(variables$ML.processed.lipid.char.tab %>% 
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
#### Output: ML.lipid.id ####
output$ML.lipid.id <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$ML.processed.lipid.char.tab), "Some error is in your condition table, please check your data and re-upload it."))
  DT::datatable(variables$ML.processed.lipid.char.tab %>% 
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

#### Output: ML.data.summary ####
output$ML.data.summary <-  shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$ML.data.summary.div), ""))
  variables$ML.data.summary.div
})

#### Output: ML.before.processed.boxplot ####
output$ML.before.processed.boxplot <-  plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.processed.plot$interactive_boxPlot_before), ""))
  variables$ML.processed.plot$interactive_boxPlot_before
})

#### Output: ML.after.processed.boxplot ####
output$ML.after.processed.boxplot <-  plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.processed.plot$interactive_boxPlot_after), ""))
  variables$ML.processed.plot$interactive_boxPlot_after
})

#### Output: ML.before.processed.density ####
output$ML.before.processed.density <-  plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.processed.plot$interactive_densityPlot_before), ""))
  variables$ML.processed.plot$interactive_densityPlot_before
})

#### Output: ML.after.processed.density ####
output$ML.after.processed.density <-  plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.processed.plot$interactive_densityPlot_after), ""))
  variables$ML.processed.plot$interactive_densityPlot_after
})

shiny::observeEvent(input$ML.processed.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$ML.processed.plot.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=33)
          output$ML.processed.download <- shiny::downloadHandler(
            filename=function(){
              paste("ML.processed.download.zip", sep="")
            },
            content=function(file){
              
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              
              pdf(file.path(temp_directory,'BoxPlot.before.process.pdf'),width=8, height=6)
              print(variables$ML.processed.plot$static_boxPlot_before)
              dev.off()
              
              pdf(file.path(temp_directory,'Boxplot.after.process.pdf'),width=8, height=6)
              print(variables$ML.processed.plot$static_boxPlot_after)
              dev.off()
              
              pdf(file.path(temp_directory,'Densityplot.before.process.pdf'),width=8, height=6)
              print(variables$ML.processed.plot$static_densityPlot_before)
              dev.off()
              
              pdf(file.path(temp_directory,'Densityplot.after.process.pdf'),width=8, height=6)
              print(variables$ML.processed.plot$static_densityPlot_after)
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
            title="Download", display_pct=TRUE, value=66 )
          variables$ML.processed.plot.download.log <- 2
          shinyjs::runjs("document.getElementById('ML.processed.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('ML.processed.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress",
            title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Data process download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

#### control user reset button ####
shiny::observeEvent(input$ML_user_reset, {
  
  #### shiny show/hide tab ####
  shiny::hideTab(inputId='ML_analysis_tab', target='ROC/PR curve')
  shiny::hideTab(inputId='ML_analysis_tab', target='Model predictivity')
  shiny::hideTab(inputId='ML_analysis_tab', target='Sample probability')
  shiny::hideTab(inputId='ML_analysis_tab', target='Feature importance')
  shiny::hideTab(inputId='ML_analysis_tab', target='Network')

  #### shinyjs reset control panel ####
  shinyjs::reset('ML_user_reset_div')

  #### clear variables ####
  variables$ML.raw.abundance             <- NULL
  variables$ML.cond.tab                  <- NULL
  variables$ML.check.step1               <- NULL
  variables$ML.check.step2               <- NULL
  variables$ML.processed.SE              <- NULL
  variables$ML.processed.SE.list         <- NULL
  variables$ML.processed.abundance       <- NULL
  variables$ML.processed.lipid.char.tab  <- NULL
  variables$ML.data.summary.div          <- NULL
  variables$ML.model.SE                  <- NULL
  variables$ML.ROC.result                <- NULL
  variables$ML.PR.result                 <- NULL
  variables$ML.evaluation.result         <- NULL
  variables$ML.probability.result        <- NULL
  variables$ML.feature.result            <- NULL
  variables$ML.shap.se                   <- NULL
  variables$ML.shap.dependence.list      <- NULL
  variables$ML.shap.plot.result          <- NULL
  variables$ML.shap.force.plot.result    <- NULL
  variables$ML.shap.dependence.plot.result <- NULL
  variables$ML.network.result            <- NULL
  variables$ML.data.check.progress       <- htmltools::HTML('<div style="font-size: 0px;"><h2>Upload progress</h2><h4>(1/3) Check upload data format.</h4></div>')
  
}) #shiny::observeEvent(input$ML_user_reset

#### control user upload button ####
shiny::observe({
  if(is.null(input$ML_user_abundance) | is.null(input$ML_user_cond)){
    shinyjs::disable("ML_user_upload")
  }else{
    shinyjs::enable("ML_user_upload")
  }
}) #observe

#### control reset2 button ####
shiny::observeEvent(input$ML_reset2, {
  
  #### shiny show/hide tab ####
  shiny::hideTab(inputId='ML_analysis_tab', target='ROC/PR curve')
  shiny::hideTab(inputId='ML_analysis_tab', target='Model predictivity')
  shiny::hideTab(inputId='ML_analysis_tab', target='Sample probability')
  shiny::hideTab(inputId='ML_analysis_tab', target='Feature importance')
  shiny::hideTab(inputId='ML_analysis_tab', target='Network')

  #### shinyjs reset control panel ####
  shinyjs::reset('ML_reset2_div')
  shinyjs::hide('ML_result_div')

  #### clear analysis result variables ####
  variables$ML.model.SE                    <- NULL
  variables$ML.ROC.result                  <- NULL
  variables$ML.PR.result                   <- NULL
  variables$ML.ROC.PR.all.download.log     <- 1
  variables$ML.ROC.PR.feature.download.log <- 1
  variables$ML.evaluation.result           <- NULL
  variables$ML.evalution.download.log      <- 1
  variables$ML.probability.result          <- NULL
  variables$ML.probability.download.log    <- 1
  variables$ML.feature.result              <- NULL
  variables$ML.feature.download.log        <- 1
  variables$ML.shap.se                     <- NULL
  variables$ML.shap.dependence.list        <- NULL
  variables$ML.shap.plot.result            <- NULL
  variables$ML.shap.download.log           <- 1
  variables$ML.shap.force.plot.result      <- NULL
  variables$ML.SHAP.force.download.log     <- 1
  variables$ML.shap.dependence.plot.result <- NULL
  variables$ML.SHAP.dependence.download.log <- 1
  variables$ML.network.result              <- NULL
  variables$ML.network.download.log        <- 1
}) #shiny::observeEvent(input$ML_user_reset2


###########################
####  ML analysis tab  ####
###########################


#### control hide/show tabpanel: input$ML_user_start ####
shiny::observeEvent(input$ML_start, {
  shiny::isolate({
    tryCatch(
      {
        shiny::withProgress(message='Machine learning analysis', style='notification', detail="Upload data", value=0, {
          if(input$ML_feature_rank_method == "ElasticNet" | 
             input$ML_classification_method == "ElasticNet"){
            alpha <- input$ML_alpha
          }else{
            alpha <- NULL
          }
          variables$ML.model.SE <- LipidSigR::ml_model(
            processed_se=variables$ML.processed.SE,
            char=input$ML_add_var,
            transform=input$ML_transformation,
            ranking_method=input$ML_feature_rank_method,
            ml_method=input$ML_classification_method, 
            split_prop=as.numeric(input$ML_split_for_test),
            nfold=as.numeric(input$ML_cross_vali_time),
            alpha=alpha)
          variables$ML.select.feature.option <- LipidSigR::extract_summarized_experiment(variables$ML.model.SE)$feature_option
          variables$ML.select.feature.select <- ifelse(10 %in% variables$ML.select.feature.option,10,variables$ML.select.feature.option[1])
          shiny::updateSelectInput(session, inputId='ML_ROC_PR_feature_number', 
                                   choices=variables$ML.select.feature.option, 
                                   selected=variables$ML.select.feature.select)
          shiny::updateSelectInput(session, inputId='ML_sam_prob_feature_number',
                                   choices=variables$ML.select.feature.option, 
                                   selected=variables$ML.select.feature.select)
          shiny::updateSelectInput(session, inputId='ML_SHAP_feature_number', 
                                   choices=variables$ML.select.feature.option, 
                                   selected=variables$ML.select.feature.select)
          shiny::updateSelectInput(session, inputId='ML_network_feature_number', 
                                   choices=variables$ML.select.feature.option, 
                                   selected=variables$ML.select.feature.select)
          shiny::updateSelectInput(session, inputId='ML_algorithm_feature_number', 
                                   choices=variables$ML.select.feature.option, 
                                   selected=variables$ML.select.feature.select)
          
          shiny::incProgress(0.6, detail='Data process done....')
          variables$ML.ROC.result <- LipidSigR::plot_ml_roc(
            ml_se=variables$ML.model.SE, feature_num=as.numeric(input$ML_ROC_PR_feature_number))
          variables$ML.PR.result <- LipidSigR::plot_ml_pr(
            ml_se=variables$ML.model.SE, feature_num=as.numeric(input$ML_ROC_PR_feature_number))
          variables$ML.ROC.PR.all.download.log <- 1
          variables$ML.ROC.PR.feature.download.log <- 1
          shiny::incProgress(0.1, detail='ROC/PR curve done....')
          variables$ML.evaluation.result <- LipidSigR::plot_ml_evaluation(
            ml_se=variables$ML.model.SE, eval_method=input$ML_evaluation_method)
          variables$ML.evalution.download.log <- 1
          shiny::incProgress(0.1, detail='Model performance done....')
          variables$ML.probability.result <- LipidSigR::plot_ml_probability(
            ml_se=variables$ML.model.SE, feature_num=as.numeric(input$ML_sam_prob_feature_number))
          variables$ML.probability.download.log <- 1
          shiny::incProgress(0.1, detail='Predicted probability done....')
          variables$ML.feature.result <- LipidSigR::plot_ml_feature(
            ml_se=variables$ML.model.SE, feature_num=as.numeric(input$ML_algorithm_feature_number))
          variables$ML.feature.download.log <- 1
          shiny::incProgress(0.1, detail='Feature importance done....')
          #### shiny show/hide tab ####
          shinyjs::show('ML_result_div')
          shiny::showTab(inputId='ML_analysis_tab', target='ROC/PR curve')
          shiny::showTab(inputId='ML_analysis_tab', target='Model predictivity')
          shiny::showTab(inputId='ML_analysis_tab', target='Sample probability')
          shiny::showTab(inputId='ML_analysis_tab', target='Feature importance')
          shiny::showTab(inputId='ML_analysis_tab', target='Network')
        })
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning model error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) #shiny::observeEvent(input$ML_user_start


################################
################################
#####  Tab1: ROC/PR curve  #####
################################
################################

shiny::observeEvent(input$ML_ROC_PR_feature_number, {
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$ML.model.SE)){
          variables$ML.ROC.result <- LipidSigR::plot_ml_roc(
            ml_se=variables$ML.model.SE, feature_num=as.numeric(input$ML_ROC_PR_feature_number))
          variables$ML.PR.result <- LipidSigR::plot_ml_pr(
            ml_se=variables$ML.model.SE, feature_num=as.numeric(input$ML_ROC_PR_feature_number))
          variables$ML.ROC.PR.all.download.log <- 1
          variables$ML.ROC.PR.feature.download.log <- 1
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning ROC/PR curve error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$ML.ROC.PR.all.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$ML.ROC.PR.all.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$ML.ROC.PR.all.download <- shiny::downloadHandler(
            filename=function(){
              paste("ML.ROC.PR.download.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## ROC plot ##
              grDevices::pdf(file.path(temp_directory,'ROC.pdf'), width=8, height=6)
              print(variables$ML.ROC.result$static_mean_auc)
              grDevices::dev.off()
              ## PR plot ##
              grDevices::pdf(file.path(temp_directory,'PR.pdf'), width=8, height=6)
              print(variables$ML.PR.result$static_mean_auc)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$ML.ROC.result$table_mean_auc_plot, file=file.path(temp_directory,'ROC.csv'))
              write.csv(variables$ML.PR.result$table_mean_auc_plot, file=file.path(temp_directory,'PR.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$ML.ROC.PR.all.download.log <- 2
          shinyjs::runjs("document.getElementById('ML.ROC.PR.all.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('ML.ROC.PR.all.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning ROC/PR curve download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

#### Output: ML.ROC.all ####
output$ML.ROC.all <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.ROC.result$interactive_mean_auc), "Without ROC plot"))
  variables$ML.ROC.result$interactive_mean_auc
})

#### Output: ML.ROC.all ####
output$ML.PR.all <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.PR.result$interactive_mean_auc), "Without PR plot"))
  variables$ML.PR.result$interactive_mean_auc
})

shiny::observeEvent(input$ML.ROC.PR.feature.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$ML.ROC.PR.feature.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$ML.ROC.PR.feature.download <- shiny::downloadHandler(
            filename=function(){
              paste("ML.ROC.PR.feature.download.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## ROC plot ##
              grDevices::pdf(file.path(temp_directory,'ROC.pdf'), width=8, height=6)
              print(variables$ML.ROC.result$static_roc)
              grDevices::dev.off()
              ## PR plot ##
              grDevices::pdf(file.path(temp_directory,'PR.pdf'), width=8, height=6)
              print(variables$ML.PR.result$static_pr)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$ML.ROC.result$table_roc, file=file.path(temp_directory,'ROC.csv'))
              write.csv(variables$ML.PR.result$table_pr, file=file.path(temp_directory,'PR.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$ML.ROC.PR.feature.download.log <- 2
          shinyjs::runjs("document.getElementById('ML.ROC.PR.feature.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('ML.ROC.PR.feature.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning ROC/PR curve plot of N features download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

#### Output: ML.ROC ####
output$ML.ROC <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.ROC.result$interactive_roc), "Feature number less than selected"))
  variables$ML.ROC.result$interactive_roc
})

#### Output: ML.ROC ####
output$ML.PR <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.PR.result$interactive_pr), "Feature number less than selected"))
  variables$ML.PR.result$interactive_pr
})

######################################
######################################
#####  Tab2: Model predictivity  #####
######################################
######################################

shiny::observeEvent(input$ML_evaluation_method, {
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$ML.model.SE)){
          variables$ML.evaluation.result <- LipidSigR::plot_ml_evaluation(
            ml_se=variables$ML.model.SE, eval_method=input$ML_evaluation_method)
          variables$ML.evalution.download.log <- 1
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning model performance error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$ML.evalution.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$ML.evalution.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$ML.evalution.download <- shiny::downloadHandler(
            filename=function(){
              paste("ML.evalution.download.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## UMAP plot ##
              grDevices::pdf(file.path(temp_directory,'Model.performance.pdf'), width=8, height=6)
              print(variables$ML.evaluation.result$static_evaluation_plot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$ML.evaluation.result$table_evaluation_plot, file=file.path(temp_directory,'Model.performance.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$ML.evalution.download.log <- 2
          shinyjs::runjs("document.getElementById('ML.evalution.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('ML.evalution.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning model performance download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) ## observeEvent #ML.evalution.download.start

#### Output: ML.evalution.plot ####
output$ML.evalution.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.evaluation.result$interactive_evaluation_plot), "Without evalution plot"))
  variables$ML.evaluation.result$interactive_evaluation_plot
  
})

#### Output: ML.evalution.table ####
output$ML.evalution.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$ML.evaluation.result$table_evaluation), "Without evalution table"))
  DT::datatable(variables$ML.evaluation.result$table_evaluation %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)), 
                colnames=c('Selection method', 'Classifier', 'CV run', 'Feature number', 'Evaluation index', 'Value', 'Mean value over all CVs'),
                escape=FALSE, selection='none', rownames=TRUE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})

######################################
######################################
#####  Tab3: Sample probability  #####
######################################
######################################
shiny::observeEvent(input$ML_sam_prob_feature_number, {
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$ML.model.SE)){
          variables$ML.probability.result <- LipidSigR::plot_ml_probability(
            ml_se=variables$ML.model.SE, feature_num=as.numeric(input$ML_sam_prob_feature_number))
          variables$ML.probability.download.log <- 1
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning predicted probability error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$ML.probability.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$ML.probability.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$ML.probability.download <- shiny::downloadHandler(
            filename=function(){
              paste("ML.probability.download.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## Probability plot ##
              grDevices::pdf(file.path(temp_directory,'Probability.pdf'), width=8, height=6)
              print(variables$ML.probability.result$static_probability_plot)
              grDevices::dev.off()
              ## Confusion matrix plot ##
              grDevices::pdf(file.path(temp_directory,'Confusion.matrix.pdf'), width=8, height=6)
              print(variables$ML.probability.result$static_confusion_matrix)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$ML.probability.result$table_probability_plot, file=file.path(temp_directory,'Probability.csv'))
              write.csv(variables$ML.probability.result$table_confusion_matrix, file=file.path(temp_directory,'Confusion.matrix.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$ML.probability.download.log <- 2
          shinyjs::runjs("document.getElementById('ML.probability.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('ML.probability.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning predicted probability download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) ## observeEvent #ML.probability.download.start

#### Output: ML.probability.plot ####
output$ML.probability.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.probability.result$interactive_probability_plot), "Feature number less than selected"))
  variables$ML.probability.result$interactive_probability_plot
  
})

#### Output: ML.cm.plot ####
output$ML.cm.plot <- shiny::renderPlot({
  shiny::validate(shiny::need(!is.null(variables$ML.probability.result$static_confusion_matrix), "Feature number less than selected"))
  variables$ML.probability.result$static_confusion_matrix
})

#### Output: ML.probability.table ####
output$ML.probability.table <- DT::renderDataTable(server=FALSE,{
  
  shiny::validate(shiny::need(!is.null(variables$ML.probability.result$table_probability_plot), "Without probability table"))
  DT::datatable(variables$ML.probability.result$table_probability_plot %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)), 
                colnames=c('Feature selection', 'Classifier', 'Feature number', 'Sample ID', 'Actual group', 'Predicted probability', 'Predicted group'),
                escape=FALSE, selection='none', rownames=TRUE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
  
})

######################################
######################################
#####  Tab4: Feature importance  #####
######################################
######################################

shiny::observeEvent(input$ML_fea_impo_list, {
  shiny::isolate({
    if(input$ML_fea_impo_list == 'Algorithm-based'){
      shinyjs::show('ML_algorithm_feature_result_div')
      shinyjs::hide('ML_SHAP_feature_result_div')
    }else if(input$ML_fea_impo_list == 'SHAP analysis'){
      shinyjs::hide('ML_algorithm_feature_result_div')
      shinyjs::show('ML_SHAP_feature_result_div')
    }
  })
})

##################################
####  List1: Algorithm-based  ####
##################################

shiny::observeEvent(input$ML_algorithm_feature_number, {
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$ML.model.SE)){
          variables$ML.feature.result <- LipidSigR::plot_ml_feature(
            ml_se=variables$ML.model.SE, feature_num=as.numeric(input$ML_algorithm_feature_number))
          variables$ML.feature.download.log <- 1
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning algorithm-based error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$ML.feature.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$ML.feature.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$ML.feature.download <- shiny::downloadHandler(
            filename=function(){
              paste("ML.feature.download.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## Probability plot ##
              grDevices::pdf(file.path(temp_directory,'Selected.frequency.pdf'), width=8, height=6)
              print(variables$ML.feature.result$static_selected_frequency)
              grDevices::dev.off()
              ## Confusion matrix plot ##
              grDevices::pdf(file.path(temp_directory,'Feature.importance.pdf'), width=8, height=6)
              print(variables$ML.feature.result$static_feature_importance)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$ML.feature.result$table_selected_frequency, file=file.path(temp_directory,'Selected.frequency.csv'))
              write.csv(variables$ML.feature.result$table_feature_importance, file=file.path(temp_directory,'Feature.importance.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$ML.feature.download.log <- 2
          shinyjs::runjs("document.getElementById('ML.feature.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('ML.feature.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning algorithm-based download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) ## observeEvent #ML.feature.download.start

#### Output: ML.feature.freq.plot ####
output$ML.fea.freq.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.feature.result$interactive_selected_frequency), "Feature number less than selected"))
  variables$ML.feature.result$interactive_selected_frequency
})

#### Output: ML.fea.impo.plot ####
output$ML.fea.impo.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.feature.result$interactive_feature_importance), "Feature number less than selected"))
  variables$ML.feature.result$interactive_feature_importance
})

#### Output: ML.fea.freq.table ####
output$ML.fea.freq.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$ML.feature.result$table_selected_frequency), "Without feature frequency table"))
  DT::datatable(variables$ML.feature.result$table_selected_frequency %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)), 
                colnames=c('Feature selection', 'Classifier', 'Feature number', 'Feature', 'Selected frequency'),
                escape=FALSE, selection='none', rownames=TRUE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})

#### Output: ML.fea.impo.table ####
output$ML.fea.impo.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$ML.feature.result$table_feature_importance), "Without feature importance table"))
  DT::datatable(variables$ML.feature.result$table_feature_importance %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)), 
                colnames=c('Feature selection', 'Classifier', 'Feature number', 'Feature', 'Average importance'),
                escape=FALSE, selection='none', rownames=TRUE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})

################################
####  List2: SHAP analysis  ####
################################

#### control hide/show tabpanel: input$ML_SHAP_start ####
shiny::observeEvent(input$ML_SHAP_start, {
  shiny::isolate({
    tryCatch(
      {
        #### shinyjs show/hide results ####
        shinyjs::show('ML.mean.shapley.plot')
        shinyjs::show('ML.all.shapley.plot')
        shinyjs::show('ML.shap.long.table')
        shinyjs::show('ML_SHAP_forceplot_control_panel_div')
        shinyjs::show('ML_SHAP_dependence_control_panel_div')
        shinyjs::hide('ML.SHAP.forceplot')
        shinyjs::hide('ML.SHAP.forceplot.table')
        shinyjs::hide('ML.SHAP.dependence.plot')
        #### Function: SHAP ####
        if(is.numeric(input$ML_SHAP_n_sim)){
          nsim <- input$ML_SHAP_n_sim
          if(input$ML_SHAP_n_sim > 100){
            nsim <- 100
            shiny::showNotification("The simulation times must be between 1 and 100, so it is calculated by replacing it with 100.", type="warning")
            shiny::updateNumericInput(inputId='ML_SHAP_n_sim', value=100)
          }else if(input$ML_SHAP_n_sim < 1){
            nsim <- 1
            shiny::showNotification("The simulation times must be between 1 and 100, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='ML_SHAP_n_sim', value=1)
          }
        }else{
          nsim <- 10
          shiny::showNotification("The simulation times must be numeric, so it is calculated by replacing it with 10.", type="warning")
          shiny::updateNumericInput(inputId='ML_SHAP_n_sim', value=10)
        }
        variables$ML.shap.se <- LipidSigR::ml_shap(ml_se=variables$ML.model.SE,
                                                   feature_num=as.numeric(input$ML_SHAP_feature_number),
                                                   nsim=nsim)
        variables$ML.shap.plot.result <- LipidSigR::plot_ml_shap(shap_se=variables$ML.shap.se)
        variables$ML.shap.download.log <- 1
        variables$ML.shap.dependence.list <- as.character(unique(S4Vectors::metadata(variables$ML.shap.se)$shap_result$variable))
        updateSelectInput(session, inputId='ML_SHAP_dependence_x', 
                          choices=variables$ML.shap.dependence.list,  selected=variables$ML.shap.dependence.list[1])
        updateSelectInput(session,  inputId='ML_SHAP_dependence_y', 
                          choices=variables$ML.shap.dependence.list, selected=variables$ML.shap.dependence.list[1])
        updateSelectInput(session,  inputId='ML_SHAP_dependence_color', 
                          choices=variables$ML.shap.dependence.list,  selected=variables$ML.shap.dependence.list[1])
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning SHAP analysis error!",
          text=as.character(e$message),
          type="error")
      }
    )
  }) #isolate
}) #shiny::observeEvent(input$ML_SHAP_start

shiny::observeEvent(input$ML.shap.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$ML.shap.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$ML.shap.download <- shiny::downloadHandler(
            filename=function(){
              paste("ML.shap.download.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## Probability plot ##
              grDevices::pdf(file.path(temp_directory,'SHAP.feature.importance.pdf'), width=8, height=6)
              print(variables$ML.shap.plot.result$static_feature_importance)
              grDevices::dev.off()
              ## Confusion matrix plot ##
              grDevices::pdf(file.path(temp_directory,'SHAP.summary.pdf'), width=8, height=6)
              print(variables$ML.shap.plot.result$static_summary_plot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$ML.shap.plot.result$table_feature_importance, file=file.path(temp_directory,'SHAP.feature.importance.csv'))
              write.csv(variables$ML.shap.plot.result$table_summary_plot, file=file.path(temp_directory,'SHAP.summary.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$ML.shap.download.log <- 2
          shinyjs::runjs("document.getElementById('ML.shap.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('ML.shap.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning SHAP analysis download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) ## observeEvent #ML.shap.download.start

#### Output: ML.mean.shapley.plot ####
output$ML.mean.shapley.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.shap.plot.result$interactive_feature_importance), "Feature number less than selected"))
  variables$ML.shap.plot.result$interactive_feature_importance
})

#### Output: ML.all.shapley.plot ####
output$ML.all.shapley.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.shap.plot.result$interactive_summary_plot), "Feature number less than selected"))
  variables$ML.shap.plot.result$interactive_summary_plot
})

#### Output: ML.shap.long.table ####
output$ML.shap.long.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$ML.shap.plot.result$table_feature_importance), "Without shap long table"))
  DT::datatable(variables$ML.shap.plot.result$table_feature_importance %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)), 
                colnames=c('Feature', 'Mean shapley value'),
                escape=FALSE, selection='none', rownames=FALSE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})

#### control user reset button ####
shiny::observeEvent(input$ML_SHAP_reset, {
  
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
  
}) #shiny::observeEvent(input$ML_SHAP_reset

#### control user reset button ####
shiny::observeEvent(input$ML_SHAP_forceplot_reset, {
  
  #### shinyjs show/hide main panel ####
  shinyjs::hide('ML.SHAP.forceplot')
  shinyjs::hide('ML.SHAP.forceplot.table')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('ML_SHAP_forceplot_reset_div')
  
}) #shiny::observeEvent(input$ML_SHAP_forceplot_reset

#### control user reset button ####
shiny::observeEvent(input$ML_SHAP_dependence_reset, {
  
  #### shinyjs show/hide main panel ####
  shinyjs::hide('ML.SHAP.dependence.plot')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('ML_SHAP_dependence_reset_div')
  
}) #shiny::observeEvent(input$ML_SHAP_dependence_reset

shiny::observe({
  VALUE <- ifelse(input$ML_SHAP_feature_number < 10, input$ML_SHAP_feature_number, 10)
  SELECT <- ifelse(input$ML_SHAP_feature_number < 5, input$ML_SHAP_feature_number, 5)
  shiny::updateSelectInput(session, inputId='ML_SHAP_forceplot_top_n_feature', 
                           choices=1:VALUE, 
                           selected=SELECT)
})

#### control hide/show tabpanel: input$ML_SHAP_forceplot_start ####
shiny::observeEvent(input$ML_SHAP_forceplot_start, {
  shiny::isolate({
    tryCatch(
      {
        #### shinyjs show/hide results ####
        shinyjs::show('ML.SHAP.forceplot')
        shinyjs::show('ML.SHAP.forceplot.table')
        variables$ML.shap.force.plot.result <- LipidSigR::plot_shap_force(
          shap_se=variables$ML.shap.se, top_feature=as.numeric(input$ML_SHAP_forceplot_top_n_feature),
          cluster_method="ward.D", group_num=as.numeric(input$ML_SHAP_forceplot_group_number))
        variables$ML.SHAP.force.download.log <- 1
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning SHAP force plot error!",
          text=as.character(e$message),
          type="error")
      }
    )
  }) #isolate
}) #shiny::observeEvent(input$ML_SHAP_forceplot_start

shiny::observeEvent(input$ML.SHAP.force.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$ML.SHAP.force.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$ML.SHAP.force.download <- shiny::downloadHandler(
            filename=function(){
              paste("ML.SHAP.force.download.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## SHAP force plot ##
              grDevices::pdf(file.path(temp_directory,'SHAP.force.pdf'), width=8, height=6)
              print(variables$ML.shap.force.plot.result$static_forcePlot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$ML.shap.force.plot.result$table_forcePlot, file=file.path(temp_directory,'SHAP.force.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$ML.SHAP.force.download.log <- 2
          shinyjs::runjs("document.getElementById('ML.SHAP.force.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('ML.SHAP.force.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning SHAP force plot download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) ## observeEvent #ML.SHAP.force.download.start

#### Output: ML.SHAP.forceplot ####
output$ML.SHAP.forceplot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.shap.force.plot.result$interactive_forcePlot), "Without SHAP forceplot"))
  variables$ML.shap.force.plot.result$interactive_forcePlot
})

#### Output: ML.SHAP.forceplot.table ####
output$ML.SHAP.forceplot.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$ML.shap.force.plot.result$table_forcePlot), "Without SHAP forceplot table"))
  DT::datatable(variables$ML.shap.force.plot.result$table_forcePlot %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)), 
                escape=FALSE, selection='none', rownames=FALSE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
  
})

#### control hide/show tabpanel: input$ML_SHAP_dependence_start ####
shiny::observeEvent(input$ML_SHAP_dependence_start, {
  shiny::isolate({
    tryCatch(
      {
        shinyjs::show('ML.SHAP.dependence.plot')
        variables$ML.shap.dependence.plot.result <- LipidSigR::plot_shap_dependence(
          shap_se=variables$ML.shap.se,
          feature=input$ML_SHAP_dependence_x, shap_feature=input$ML_SHAP_dependence_y,
          interaction_index=input$ML_SHAP_dependence_color)
        variables$ML.SHAP.dependence.download.log <- 1
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning SHAP dependence plot error!",
          text=as.character(e$message),
          type="error")
      }
    )
  }) #isolate
}) #shiny::observeEvent(input$ML_SHAP_dependence_start

shiny::observeEvent(input$ML.SHAP.dependence.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$ML.SHAP.dependence.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$ML.SHAP.dependence.download <- shiny::downloadHandler(
            filename=function(){
              paste("ML.SHAP.dependence.download.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## SHAP force plot ##
              grDevices::pdf(file.path(temp_directory,'SHAP.dependence.pdf'), width=8, height=6)
              print(variables$ML.shap.dependence.plot.result$static_dependence_plot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$ML.shap.dependence.plot.result$table_dependence_plot, file=file.path(temp_directory,'SHAP.dependence.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$ML.SHAP.dependence.download.log <- 2
          shinyjs::runjs("document.getElementById('ML.SHAP.dependence.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('ML.SHAP.dependence.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning SHAP dependence plot error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) ## observeEvent #ML.SHAP.dependence.download.start

#### Output: ML.SHAP.dependence.plot ####
output$ML.SHAP.dependence.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$ML.shap.dependence.plot.result$interactive_dependence_plot), "Without SHAP dependence plot"))
  variables$ML.shap.dependence.plot.result$interactive_dependence_plot
})

###########################
###########################
#####  Tab5: Network  #####
###########################
###########################

#### control start button ####
shiny::observeEvent(input$ML_network_start, {
  shiny::isolate({
    tryCatch(
      {
        shinyjs::show('ML_network_result_div')
        if(is.numeric(input$ML_network_corr_coef)){
          edge_cutoff <- input$ML_network_corr_coef
          if(input$ML_network_corr_coef > 1){
            edge_cutoff <- 1
            shiny::showNotification("The coefficient cutoff must be between 0 and 1, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='ML_network_corr_coef', value=1)
          }else if(input$ML_network_corr_coef < 0){
            edge_cutoff <- 0
            shiny::showNotification("The coefficient cutoff must be between 0 and 1, so it is calculated by replacing it with 0.", type="warning")
            shiny::updateNumericInput(inputId='ML_network_corr_coef', value=0)
          }
        }else{
          edge_cutoff <- 0
          shiny::showNotification("The coefficient cutoff must be numeric, so it is calculated by replacing it with 0.", type="warning")
          shiny::updateNumericInput(inputId='ML_network_corr_coef',value=0)
        }
        if(is.numeric(input$ML_network_n_sim)){
          nsim <- input$ML_network_n_sim
          if(input$ML_network_n_sim > 100){
            nsim <- 100
            shiny::showNotification("The simulation times must be between 1 and 100, so it is calculated by replacing it with 100.", type="warning")
            shiny::updateNumericInput(inputId='ML_network_n_sim', value=100)
          }else if(input$ML_network_n_sim < 1){
            nsim <- 1
            shiny::showNotification("The simulation times must be between 1 and 100, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='ML_network_n_sim', value=1)
          }
        }else{
          nsim <- 10
          shiny::showNotification("The simulation times must be numeric, so it is calculated by replacing it with 10.", type="warning")
          shiny::updateNumericInput(inputId='ML_network_n_sim', value=10)
        }
        variables$ML.network.result <- LipidSigR::ml_corr_network(
          ml_se=variables$ML.model.SE,
          feature_importance=input$ML_network_fea_impo_method,
          correlation='pearson', 
          edge_cutoff=edge_cutoff, feature_num=as.numeric(input$ML_network_feature_number),
          nsim=nsim)
        variables$ML.network.download.log <- 1
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning network error!",
          text=as.character(e$message),
          type="error")
      }
    )
  }) #isolate
}) #shiny::observeEvent(input$ML_network_start

shiny::observeEvent(input$ML.network.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$ML.network.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$ML.network.download <- shiny::downloadHandler(
            filename=function(){
              paste("ML.network.download.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## SHAP force plot ##
              grDevices::pdf(file.path(temp_directory,'Network.pdf'), width=8, height=6)
              print(variables$ML.network.result$static_correlation_network)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$ML.network.result$edge_table, file=file.path(temp_directory,'Network.edge.csv'))
              ## table  ##
              write.csv(variables$ML.network.result$node_table, file=file.path(temp_directory,'Network.node.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$ML.network.download.log <- 2
          shinyjs::runjs("document.getElementById('ML.network.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('ML.network.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Machine learning network download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) ## observeEvent #ML.network.download.start

#### control user reset button ####
shiny::observeEvent(input$ML_network_reset, {
  #### shinyjs show/hide main panel ####
  shinyjs::hide('ML_network_result_div')
  #### shinyjs reset control panel ####
  shinyjs::reset('ML_network_reset_div')
  
}) #shiny::observeEvent(input$ML_network_reset

#### Output: ML.network ####
output$ML.network <- visNetwork::renderVisNetwork({
  shiny::validate(shiny::need(!is.null(variables$ML.network.result$interactive_correlation_network), "Feature number less than selected"))
  variables$ML.network.result$interactive_correlation_network
}) #output$ML.network <- renderVisNetwork


shiny::observeEvent(input$ML_network_refresh, {
  output$ML.network <- visNetwork::renderVisNetwork({
    shiny::validate(shiny::need(!is.null(variables$ML.network.result$interactive_correlation_network), "Feature number less than selected"))
    variables$ML.network.result$interactive_correlation_network
  }) #output$ML.network <- renderVisNetwork
})