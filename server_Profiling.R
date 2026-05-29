################################
################################
######                    ######
######   Profiling Page   ######
######                    ######
################################
################################

#### Output: PRO.demo.download ####
output$PRO.demo.download <- shiny::downloadHandler(
  filename=function() {
    "Profiling_example_dataset.zip"
  },
  content=function(file) {
    file.copy("www/download_demo_dataset/Profiling.zip", file)
  },
  contentType="application/zip"
)
shiny::outputOptions(output, "PRO.demo.download", suspendWhenHidden=FALSE)

#### PRO demo dataset ####
shiny::observeEvent(input$PRO_demo_upload, {
  shiny::isolate({
    shiny::withProgress(message='Profiling analysis', style='notification', detail="Upload data", value=0, {
      shinyjs::show('PRO_data_check_successful')
      variables$PRO.SE <- readRDS('www/demo_dataset/Profiling.rds')
      variables$PRO.SE.list <- LipidSigR::extract_summarized_experiment(variables$PRO.SE)
      variables$PRO.raw.abundance <- variables$PRO.SE.list$abundance
      variables$PRO.data.check.progress <- htmltools::tags$div(
         h2('Upload progress'),
         h4('(1/3) Check data frame format.'),
         style="font-size: 0px;"
      )
      shiny::incProgress(0.33, detail='Check data format')
      variables$PRO.check.step1 <- inputFormat(abundance=variables$PRO.raw.abundance,
                                           abundance_path='demo_exp.csv',
                                           analysis_type="Profiling",
                                             variables=variables, session=session)
      shiny::incProgress(0.33, detail='Check data format')
      if(variables$PRO.check.step1$logical == TRUE){
        variables$PRO.check.step2 <- data_check(
          abundance=variables$PRO.raw.abundance, group_info=NULL,
          abundance_path='demo_exp.csv', analysis_type="Profiling",
                                             variables=variables, session=session)
        output$PRO.Check.SE <- renderUI({
          isolate({
            variables$PRO.check.step2$return_div
          })
        })
        variables$PRO.SE <- variables$PRO.check.step2$rawSE
        if(grepl('class="fas fa-xmark"', variables$PRO.check.step2$return_div)){
          shinyjs::show('PRO_data_warning')
          shinyjs::hide('PRO_data_processing_div')
        }else{
          shinyjs::show('PRO_data_warning')
          shinyjs::show('PRO_data_processing_div')
        }
        variables$PRO.data.check.progress <- htmltools::tags$div(
          style="font-size: 0px;",
          htmltools::h2('Upload progress'),
          htmltools::h4('(2/3) Data check finish.')
        )
      }else{
        output$PRO.Check.SE <- shiny::renderUI({
          shiny::isolate({
            variables$PRO.check.step1$return_div
            })
        })
      }
      shiny::incProgress(0.34, detail='Check data format finish')
    })
  })
}) ## observeEvent # input$PRO_demo_upload

#### PRO user dataset ####
shiny::observeEvent(input$PRO_user_upload, {
  shiny::isolate({
    shiny::withProgress(message='Profiling analysis', style='notification', detail="Upload data", value=0, {
      shinyjs::show('PRO_upload')
      shinyjs::show('PRO_data_check_successful')
      showNotification("Start uploading file...", type="message")
      variables$PRO.data.check.progress <- tags$div(
         h2('Upload progress'),
         h4('(1/3) Check data frame format.'),
         style="font-size: 0px;")
      #### import user dataset ####
      tryCatch(
        {
          ## abundance ##
          if(grepl('.xlsx',input$PRO_user_exp$datapath)){
            variables$PRO.raw.abundance <- readxl::read_excel(
              input$PRO_user_exp$datapath, na=c('', 'NA','na')) %>%
              as.data.frame()
          }else{
            variables$PRO.raw.abundance <- data.table::fread(
              input$PRO_user_exp$datapath, header=TRUE, stringsAsFactors=FALSE,
              check.names=FALSE, data.table=FALSE, na.strings=c('', 'NA', 'na'))
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
      ## Check input format ##
      tryCatch(
        {
          shiny::incProgress(0.33, detail='Check data format')
          variables$PRO.checkUTF8 <- check_utf8Format(abundance=variables$PRO.raw.abundance,
                                                     abundance_path=input$PRO_user_exp$datapath,
                                                     analysis_type="Profiling",
                                             variables=variables, session=session)
          if(variables$PRO.checkUTF8$logical == TRUE){
            shinyjs::show('PRO_data_Uploaded')
            variables$PRO.check.step1 <- inputFormat(abundance=variables$PRO.raw.abundance,
                                                     abundance_path=input$PRO_user_exp$datapath,
                                                     analysis_type="Profiling",
                                             variables=variables, session=session)
            shiny::incProgress(0.33, detail='Check data format')
            if(variables$PRO.check.step1$logical == TRUE){
              variables$PRO.check.step2 <- data_check(abundance=variables$PRO.raw.abundance,group_info=NULL,
                                                      abundance_path=input$PRO_user_exp$datapath,
                                                      analysis_type="Profiling",
                                             variables=variables, session=session)
              if(variables$PRO.check.step2$lipid_id_pct >= 20){
                shinyWidgets::show_alert(
                  title='Warning',
                  text=htmltools::HTML("<h4>Warning! The unrecognized lipids in the uploaded abundance data are more than 20%.</h4>
                                      <h4>We recommend revising the lipids' names. Please use Shorthand notation or refer to HMDB, SwissLipids, and LIPID MAPS LMSD styles for lipid input.</h4>
                                      <h4>You can access the detailed instructions in our <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ12' target='_blank'>FAQ</a>.</h4>"),
                  html=TRUE, type ='warning')
              }
              output$PRO.Check.SE <- shiny::renderUI({
                shiny::isolate({
                  variables$PRO.check.step2$return_div
                })
              })
              variables$PRO.SE <- variables$PRO.check.step2$rawSE
              variables$PRO.data.check.progress <- htmltools::tags$div(
                style="font-size: 0px;",
                htmltools::h2('Upload progress'),
                htmltools::h4('(2/3) Data check finish.'))
              if(grepl('class="fas fa-xmark"', variables$PRO.check.step2$return_div)){
                shinyjs::show('PRO_data_warning')
                shinyjs::hide('PRO_data_processing_div')
              }else{
                shinyjs::show('PRO_data_warning')
                shinyjs::show('PRO_data_processing_div')
              }
            }else{
              shinyjs::show('PRO_data_warning')
              shinyjs::hide('PRO_data_processing_div')
              output$PRO.Check.SE <- shiny::renderUI({
                shiny::isolate({
                  variables$PRO.check.step1$return_div
                })
              })
            }
          }else{
            shiny::incProgress(0.33, detail='Check data format')
            variables$PRO.raw.abundance <- NULL 
            shinyjs::hide('PRO_data_Uploaded')
            shinyjs::show('PRO_data_warning')
            shinyjs::hide('PRO_data_processing_div')
            output$PRO.Check.SE <- shiny::renderUI({
              shiny::isolate({
                variables$PRO.checkUTF8$return_div
              })
            })
            variables$PRO.data.check.progress <- htmltools::tags$div(
              style="font-size: 0px;",
              htmltools::h2('Upload progress'),
              htmltools::h4('(2/3) Data check finish.'))
          }
          shiny::incProgress(0.34, detail='Check data format finish')
        },
        error=function(e) {
          shinyWidgets::show_alert(
            title='Error',
            text=htmltools::HTML("<h4>Detect unknown input data format errors.</h4>
                                  <h4>For the correct data format guidelines, please refer to the <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank'>FAQ</a></h4>.
                                  <h4>If you need further assistance, please email us your data.(<a href='mailto:bioinfomics.web@gmail.com' target='_blank' style='color: darkblue;'>bioinfomics.web@gmail.com</a>)</h4>"),
            html=TRUE, type ='error')
          return()
        }
      )
    })
  })
}) ## observeEvent # input$PRO_user_upload

#### Output: PRO.data.check.progress ####
output$PRO.data.check.progress <- shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$PRO.data.check.progress), ""))
  variables$PRO.data.check.progress
})

#### Output: PRO.raw.abundance ####
output$PRO.raw.abundance <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$PRO.raw.abundance), ""))
  DT::datatable(variables$PRO.raw.abundance,
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                               deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                               dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                               columnDefs=list(list(className='dt-center', targets="_all"))))
})

#### control user reset button ####
shiny::observeEvent(input$PRO_user_reset, {

  #### shinyjs show/hide main panel ####
  shinyjs::hide('PRO_user_mainPanel_div')

  #### shiny show/hide tab ####
  shiny::hideTab(inputId='PRO_analysis_tab', target='Cross-sample variability')
  shiny::hideTab(inputId='PRO_analysis_tab', target='Dimensionality reduction')
  shiny::hideTab(inputId='PRO_analysis_tab', target='Correlation heatmap')
  shiny::hideTab(inputId='PRO_analysis_tab', target='Lipid characteristics profiling')

  #### shinyjs reset control panel ####
  shinyjs::reset('PRO_user_reset_div')

  #### clear variables ####
  variables$PRO.raw.abundance             <- NULL
  variables$PRO.SE                        <- NULL
  variables$PRO.SE.list                   <- NULL
  variables$PRO.exp.user.col1             <- NULL
  variables$PRO.lipid.char.user.col1      <- NULL
  variables$PRO.pca.result                <- NULL
  variables$PRO.tsne.result               <- NULL
  variables$PRO.umap.result               <- NULL
  variables$PRO.corr.heatmap.result       <- NULL
  variables$PRO.data.check.progress       <- htmltools::HTML('<div style="font-size: 0px;"><h2>Upload progress</h2><h4>(1/2) Check upload data format.</h4></div>')

}) #observeEvent(input$PRO_user_reset

#### control user upload button ####
shiny::observe({
  if(is.null(input$PRO_user_exp)){
    shinyjs::disable("PRO_user_upload")
  }else{
    shinyjs::enable("PRO_user_upload")
  }
}) #observe

shiny::observeEvent(input$PRO_processing_start, {
  shiny::isolate({
    tryCatch(
      {
        shiny::withProgress(message='Profiling analysis', style='notification', detail="Data processing", value=0, {
          imputation_param <- switch(input$PRO_fill_NA,
                                     mean=NULL,
                                     median=NULL,
                                     min=input$PRO_fill_min,
                                     QRILC=input$PRO_fill_QRILC,
                                     SVD=input$PRO_fill_param,
                                     KNN=input$PRO_fill_KNN,
                                     IRMI=NULL,
                                     PPCA=input$PRO_fill_param,
                                     BPCA=input$PRO_fill_param)
          if(is.numeric(input$PRO_filtration_param)){
            filtration_param <- input$PRO_filtration_param
            if(input$PRO_filtration_param > 100){
              filtration_param <- 100
              shiny::showNotification("The value of remove features with more than % missing values must be between 5 and 100, so it is calculated by replacing it with 100.", type="warning")
              shiny::updateNumericInput(inputId='PRO_filtration_param', value=100)
            }else if(input$PRO_filtration_param < 5){
              filtration_param <- 5
              shiny::showNotification("The value of remove features with more than % missing values must be between 5 and 100, so it is calculated by replacing it with 5.", type="warning")
              shiny::updateNumericInput(inputId='PRO_filtration_param', value=5)
            }
          }else{
            filtration_param <- 70
            shiny::showNotification("The value of remove features with more than % missing values must be numeric, so it is calculated by replacing it with 70", type="warning")
            shiny::updateNumericInput(inputId='PRO_filtration_param', value=70)
          }
          if(!is.null(imputation_param)){
            if(input$PRO_fill_NA == 'min'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 0.5){
                  imputation_param <- 0.5
                  shiny::showNotification("The value of multiply by minimum must be between 0.1 and 0.5, so it is calculated by replacing it with 0.5.", type="warning")
                  shiny::updateNumericInput(inputId='PRO_fill_min', value=0.5)
                }else if(imputation_param < 0.1){
                  imputation_param <- 0.1
                  shiny::showNotification("The value of multiply by minimum must be between 0.1 and 0.5, so it is calculated by replacing it with 0.1.", type="warning")
                  shiny::updateNumericInput(inputId='PRO_fill_min',value=0.1)
                }
              }else{
                imputation_param <- 0.5
                shiny::showNotification("The value of multiply by minimum must be numeric, so it is calculated by replacing it with 0.5.", type="warning")
                shiny::updateNumericInput(inputId='PRO_fill_min',value=0.5)
              }
            }else if(input$PRO_fill_NA == 'QRILC'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of tune sigma must be between 0.1 and 1, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='PRO_fill_QRILC', value=1)
                }else if(imputation_param < 0.1){
                  imputation_param <- 0.1
                  shiny::showNotification("The value of tune sigma must be between 0.1 and 1, so it is calculated by replacing it with 0.1.", type="warning")
                  shiny::updateNumericInput(inputId='PRO_fill_QRILC', value=0.1)
                }
              }else{
                imputation_param <- 1
                shiny::showNotification("The value of tune sigma must be numeric, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='PRO_fill_QRILC', value=1)
              }
            }else if(input$PRO_fill_NA == 'SVD'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='PRO_fill_param', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='PRO_fill_param', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The value of nPCs must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='PRO_fill_param', value=3)
              }
            }else if(input$PRO_fill_NA == 'KNN'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The number of neighbors must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='PRO_fill_KNN', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The number of neighbors must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='PRO_fill_KNN', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The number of neighbors must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='PRO_fill_KNN', value=3)
              }
            }else if(input$PRO_fill_NA == 'PPCA'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='PRO_fill_param', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='PRO_fill_param', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The value of nPCs must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='PRO_fill_param', value=3)
              }
            }else if(input$PRO_fill_NA == 'BPCA'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='PRO_fill_param', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='PRO_fill_param', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The value of nPCs must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='PRO_fill_param',value=3)
              }
            }
          }
          variables$PRO.processed.SE <- LipidSigR::data_process(
            se=variables$PRO.SE, exclude_missing=input$PRO_rm_NA,
            exclude_missing_pct=filtration_param,
            replace_na_method=input$PRO_fill_NA, replace_na_method_ref=imputation_param,
            normalization=input$PRO_normalization, transform=input$PRO_transformation)
          variables$PRO.processed.SE.list <- LipidSigR::extract_summarized_experiment(variables$PRO.processed.SE)
          variables$PRO.processed.abundance <- variables$PRO.processed.SE.list$abundance
          #### update input ####
          Nsample <- ncol(variables$PRO.processed.abundance)-1
          shiny::updateNumericInput(session, inputId='PRO_dbscan_minPts', max=Nsample)
          VALUE1 <- ifelse(Nsample %% 3 == 0, floor(Nsample/3)-1, floor(Nsample/3))
          VALUE2 <- ifelse(VALUE1 < 5, VALUE1, 5)
          shiny::updateNumericInput(session, inputId='PRO_tsne_perplexity', value=VALUE2, max=VALUE1)
          shiny::updateNumericInput(session, inputId='PRO_umap_n_neighbors', value=ifelse(Nsample < 15, Nsample, 15), max=Nsample)
          shiny::updateSliderInput(session, "PRO_kmeans_group", max=Nsample-1)
          shiny::updateSliderInput(session, "PRO_pam_group", max=Nsample-1)
          shiny::updateSliderInput(session, "PRO_hclust_group", max=Nsample)
          
          variables$PRO.processed.lipid.char.tab <- variables$PRO.processed.SE.list$lipid_char_table
          variables$PRO.processed.lipid.char.tab <- char.tab.url(variables$PRO.processed.lipid.char.tab)
          variables$PRO.processed.plot <- LipidSigR::plot_data_process(variables$PRO.SE, variables$PRO.processed.SE)
          variables$PRO.data.summary.div <- data_summary(se = variables$PRO.SE,
                                                         analysis_type = "Profiling",
                                                         exclude_missing = input$PRO_rm_NA,
                                                         exclude_missing_pct = filtration_param,
                                                         replace_na_method = input$PRO_fill_NA,
                                                         replace_na_method_ref = imputation_param,
                                                         normalization = input$PRO_normalization,
                                                         transform = input$PRO_transformation)
          
          variables$PRO.lipid.char <- data.frame(aspect=names(LipidSigR::list_lipid_char(variables$PRO.processed.SE)$common_list),
                                                 characteristic=LipidSigR::list_lipid_char(variables$PRO.processed.SE)$common_list) %>%
            dplyr::mutate(aspect=factor(aspect),
                          characteristic=factor(characteristic)) %>%
            dplyr::arrange(characteristic)
          #### update PRO lipid characteristic select input ####
          output$PRO.corr.heatmap.char.select <- shiny::renderUI({
            shiny::selectInput(
              inputId='PRO_corr_heatmap_char', label='Characteristics:',
              choices=split(as.list(levels(variables$PRO.lipid.char$characteristic)), variables$PRO.lipid.char$aspect),
              selected='class', multiple=FALSE)
          })
          output$PRO.lipid.char <- shiny::renderUI({
            shiny::selectInput(
              inputId='PRO_lipid_char', label='Select the lipid characteristics:',
              choices=split(as.list(levels(variables$PRO.lipid.char$characteristic)), variables$PRO.lipid.char$aspect),
              selected='class', multiple=FALSE)
          })
          output$CORR.class.linear.lipid.char <- shiny::renderUI({
            shiny::selectInput(inputId='CORR_class_linear_lipid_char', label='Select the lipid characteristic:',
                               choices=split(as.list(levels(variables$CORR.class.lipid.char$characteristic)), variables$CORR.class.lipid.char$aspect),
                               selected='class', multiple=FALSE)
          })
          
          output$PRO.data.check.progress <- shiny::renderUI({
            htmltools::tags$div(
              style="font-size: 0px;",
              htmltools::h2('Upload progress'),
              htmltools::h4('(3/3) Data Processing finish.'))
          })
          shinyjs::show('data_summary_div')
          shinyjs::show('data_process_description_div')
          shinyjs::show('data_process_table_div')
          shinyjs::show('PRO_start_div')
          variables$PRO.processed.plot.download.log <- 1
          shiny::incProgress(0.34, detail='Data processing finish')
        })
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Data process error!",
          text=as.character(e$message),
          type="error")
        #### shinyjs show/hide results ####
        shinyjs::hide('data_summary_div')
        shinyjs::hide('data_process_description_div')
        shinyjs::hide('data_process_table_div')
        shinyjs::hide('PRO_start_div')
      }
    )
  })
}) ## observeEvent # input$PRO_processing_start

shiny::observeEvent(input$PRO.processed.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$PRO.processed.plot.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$PRO.processed.download <- shiny::downloadHandler(
            filename=function(){
              paste("PRO.processed.plot.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## Boxplot before process ##
              grDevices::pdf(file.path(temp_directory,'BoxPlot.before.process.pdf'), width=8, height=6)
              print(variables$PRO.processed.plot$static_boxPlot_before)
              grDevices::dev.off()
              ## Boxplot after process ##
              grDevices::pdf(file.path(temp_directory,'Boxplot.after.process.pdf'), width=8, height=6)
              print(variables$PRO.processed.plot$static_boxPlot_after)
              grDevices::dev.off()
              ## Densityplot before process ##
              grDevices::pdf(file.path(temp_directory,'Densityplot.before.process.pdf'), width=8, height=6)
              print(variables$PRO.processed.plot$static_densityPlot_before)
              grDevices::dev.off()
              ## Densityplot after process ##
              grDevices::pdf(file.path(temp_directory,'Densityplot.after.process.pdf'), width=8, height=6)
              print(variables$PRO.processed.plot$static_densityPlot_after)
              grDevices::dev.off()
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$PRO.processed.plot.download.log <- 2
          shinyjs::runjs("document.getElementById('PRO.processed.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('PRO.processed.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
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

#### Output: PRO.processed.abundance ####
output$PRO.processed.abundance <- DT::renderDataTable(server=FALSE, {
   shiny::validate(need(!is.null(variables$PRO.processed.abundance), ""))
   DT::datatable(variables$PRO.processed.abundance %>%
                   dplyr::mutate_if(is.numeric, ~round(., 5)),
                 escape=FALSE, selection='none', rownames=FALSE,
                 class="nowrap row-border",
                 extensions=c('Buttons', 'Scroller','FixedColumns'),
                 options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                                deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                                dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                                columnDefs=list(list(className='dt-center', targets="_all"))))
})
#### Output: PRO.processed.lipid.char.tab ####
output$PRO.processed.lipid.char.tab <- DT::renderDataTable(server=FALSE,{
   shiny::validate(shiny::need(!is.null(variables$PRO.processed.lipid.char.tab), ""))
   DT::datatable(variables$PRO.processed.lipid.char.tab %>%
                   dplyr::select(-c(LION.ID,LIPID.MAPS.ID,SwissLipids.ID,HMDB.ID,ChEBI.ID,KEGG.ID,LipidBank.ID,PubChem.CID,MetaNetX.ID,PlantFA.ID)) %>%
                   dplyr::mutate_if(is.numeric, ~round(., 5)),
                 escape=FALSE, selection='none', rownames=FALSE,
                 class="nowrap row-border",
                 extensions=c('Buttons', 'Scroller','FixedColumns'),
                 options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                                deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                                dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                                columnDefs=list(list(className='dt-center', targets="_all"))))
})
#### Output: PRO.lipid.id ####
output$PRO.lipid.id <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$PRO.processed.lipid.char.tab), ""))
  DT::datatable(variables$PRO.processed.lipid.char.tab %>%
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
#### Output: PRO.Data.summary ####
output$PRO.Data.summary <-  shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$PRO.data.summary.div), ""))
  variables$PRO.data.summary.div
})

#### Output: PRO.before.processed.boxplot ####
output$PRO.before.processed.boxplot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$PRO.processed.plot$interactive_boxPlot_before), ""))
  variables$PRO.processed.plot$interactive_boxPlot_before
})

#### Output: PRO.after.processed.boxplot ####
output$PRO.after.processed.boxplot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$PRO.processed.plot$interactive_boxPlot_after), ""))
  variables$PRO.processed.plot$interactive_boxPlot_after
})

#### Output: PRO.before.processed.density ####
output$PRO.before.processed.density <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$PRO.processed.plot$interactive_densityPlot_before), ""))
  variables$PRO.processed.plot$interactive_densityPlot_before
})

#### Output: PRO.after.processed.density ####
output$PRO.after.processed.density <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$PRO.processed.plot$interactive_densityPlot_after), ""))
  variables$PRO.processed.plot$interactive_densityPlot_after
})

##################################
####  Profiling analysis tab  ####
##################################

#### control hide/show tabpanel: input$PRO_start ####
shiny::observeEvent(input$PRO_start, {

  isolate({
    shinyjs::show('PRO_tabPanel_div')
    shiny::showTab(inputId='PRO_analysis_tab', target='Cross-sample variability')
    shiny::showTab(inputId='PRO_analysis_tab', target='Dimensionality reduction')
    shiny::showTab(inputId='PRO_analysis_tab', target='Correlation heatmap')
    shiny::showTab(inputId='PRO_analysis_tab', target='Lipid characteristics profiling')
    variables$cross.sample.variability <- LipidSigR::cross_sample_variability(se=variables$PRO.SE)
    variables$PRO.cross.sample.variability.download.log <- 1
    #### shinyjs show/hide results ####
    shinyjs::hide('PRO_dim_redu_result_div')
  })

}) #observeEvent(input$PRO_start

############################################
############################################
#####  Tab1: Cross-sample variability  #####
############################################
############################################

#### Output: PRO.lipid.number ####
output$PRO.lipid.number <- plotly::renderPlotly({
  variables$cross.sample.variability$interactive_lipid_number_barPlot
})

#### Output: PRO.lipid.amount ####
output$PRO.lipid.amount <- plotly::renderPlotly({
  variables$cross.sample.variability$interactive_lipid_amount_barPlot
})

#### Output: PRO.lipid.distribution ####
output$PRO.lipid.distribution <- plotly::renderPlotly({
  variables$cross.sample.variability$interactive_lipid_distribution
})

shiny::observeEvent(input$PRO.cross.sample.variability.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$PRO.cross.sample.variability.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$PRO.cross.sample.variability.download <- shiny::downloadHandler(
            filename=function(){
              paste("PRO.cross.sample.variability.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## Histogram of expressed lipid numbers ##
              grDevices::pdf(file.path(temp_directory,'Lipid.number.pdf'), width=8, height=6)
              print(variables$cross.sample.variability$static_lipid_number_barPlot)
              grDevices::dev.off()
              ## Histogram of lipid amount ##
              grDevices::pdf(file.path(temp_directory,'Lipid.amount.pdf'), width=8, height=6)
              print(variables$cross.sample.variability$static_lipid_amount_barPlot)
              grDevices::dev.off()
              ## Density plot of lipid abundance distribution ##
              grDevices::pdf(file.path(temp_directory,'Lipid.distribution.pdf'), width=8, height=6)
              print(variables$cross.sample.variability$static_lipid_distribution)
              grDevices::dev.off()
              ## table  ##
              utils::write.csv(variables$cross.sample.variability$table_total_lipid, file=file.path(temp_directory,'Total.lipid.csv'))
              utils::write.csv(variables$cross.sample.variability$table_lipid_distribution, file=file.path(temp_directory,'Lipid.distribution.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$PRO.cross.sample.variability.download.log <- 2
          shinyjs::runjs("document.getElementById('PRO.cross.sample.variability.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('PRO.cross.sample.variability.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Cross-sample variability download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
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

shiny::observeEvent(input$PRO_cluster_method, {
  isolate({
    shinyjs::show('PRO_dim_redu_result_div')
  })
}) ## observeEvent # PRO_cluster_method

shiny::observeEvent(input$PRO_dim_redu_start, {
  shiny::isolate({
    tryCatch(
      {
        shinyjs::show('PRO_dim_redu_result_div')
        ## cluster method parameter ##
        cluster_num <- switch(input$PRO_cluster_method,
                              kmeans=input$PRO_kmeans_group,
                              kmedoids=input$PRO_pam_group,
                              hclustering=input$PRO_hclust_group,
                              dbscan=NULL)
        kmedoids_metric <- if(input$PRO_cluster_method == 'kmedoids'){ input$PRO_pam_metric }else{ NULL }
        distfun <- if(input$PRO_cluster_method == 'hclustering'){ input$PRO_hclust_dist }else{ NULL }
        hclustfun <- if(input$PRO_cluster_method == 'hclustering'){ input$PRO_hclust_hclust }else{ NULL }
        eps <- if(input$PRO_cluster_method == 'dbscan'){ input$PRO_dbscan_eps }else{ NULL }
        minPts <- if(input$PRO_cluster_method == 'dbscan'){ input$PRO_dbscan_minPts }else{ NULL }
        n_PC <- if(input$PRO_pca_contrib_PC == '1_2'){ c(1, 2) }else{ as.numeric(input$PRO_pca_contrib_PC) }
        
        if(input$PRO_cluster_method == 'dbscan'){
          if(is.numeric(eps)){
            if(eps < 0.1){
              eps <- 0.1
              shiny::showNotification("The epsilon must be greater than 0.1, so it is calculated by replacing it with 0.1.", type="warning")
              shiny::updateNumericInput(inputId='PRO_dbscan_eps', value=0.1)
            }
          }else{
            eps <- 0.5
            shiny::showNotification("The epsilon must be numeric, so it is calculated by replacing it with 0.5.", type="warning")
            shiny::updateNumericInput(inputId='PRO_dbscan_eps', value=0.5)
          }
          
          if(is.numeric(minPts)){
            if(minPts > 22){
              minPts <- 22
              shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 22.", type="warning")
              shiny::updateNumericInput(inputId='PRO_dbscan_minPts', value=22)
            }else if(input$PRO_dbscan_minPts < 1){
              minPts <- 1
              shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='PRO_dbscan_minPts', value=1)
            }
          }else{
            minPts <- 1
            shiny::showNotification("The minPts must be numeric, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='PRO_dbscan_minPts',value=1)
          }
        }
        
        if(input$PRO_dim_redu_method == 'pca'){
          shinyjs::hide('PRO_dim_redu_result_tsne_div')
          shinyjs::hide('PRO_dim_redu_result_umap_div')
          variables$PRO.dim.redu.pca.download.log <- 1
          variables$PRO.dim.redu.pca.topN.download.log <- 1
          variables$PRO.pca.result <- LipidSigR::dr_pca(
            processed_se=variables$PRO.processed.SE, scaling=TRUE,
            centering=TRUE, clustering=input$PRO_cluster_method,
            cluster_num=cluster_num, kmedoids_metric=kmedoids_metric, distfun=distfun,
            hclustfun=hclustfun, eps=eps, minPts=minPts, feature_contrib_pc=n_PC, plot_topN=input$PRO_pca_variable_topN)
          
          if(!is.null(variables$PRO.pca.result)){
            VALUE <- ifelse(nrow(variables$PRO.pca.result$table_pca_contribution) < 10, nrow(variables$PRO.pca.result$table_pca_contribution), 10)
            shiny::updateSliderInput(session,
                                     inputId='PRO_pca_variable_topN',
                                     value=VALUE,
                                     max=nrow(variables$PRO.pca.result$table_pca_contribution))
            shiny::updateSliderInput(session,
                                     inputId='PRO_pca_contrib_topN',
                                     value=VALUE,
                                     max=nrow(variables$PRO.pca.result$table_pca_contribution))
            shinyjs::show('PRO_dim_redu_result_pca_div')
          }
        }else if(input$PRO_dim_redu_method == 'tsne'){
          shinyjs::hide('PRO_dim_redu_result_pca_div')
          shinyjs::hide('PRO_dim_redu_result_umap_div')
          if(is.numeric(input$PRO_tsne_perplexity)){
            perplexity <- input$PRO_tsne_perplexity
            if(input$PRO_tsne_perplexity > 7){
              perplexity <- 7
              shiny::showNotification("The perplexity must be between 3 and 7, so it is calculated by replacing it with 7.", type="warning")
              shiny::updateNumericInput(inputId='PRO_tsne_perplexity', value=7)
            }else if(input$PRO_tsne_perplexity < 3){
              perplexity <- 3
              shiny::showNotification("The perplexity must be between 3 and 7, so it is calculated by replacing it with 3.", type="warning")
              shiny::updateNumericInput(inputId='PRO_tsne_perplexity', value=3)
            }
          }else{
            perplexity <- 5
            shiny::showNotification("The perplexity must be numeric, so it is calculated by replacing it with 5.", type="warning")
            shiny::updateNumericInput(inputId='PRO_tsne_perplexity', value=5)
          }
          if(is.numeric(input$PRO_tsne_max_iter)){
            max_iter <- input$PRO_tsne_max_iter
            if(input$PRO_tsne_max_iter > 5000){
              max_iter <- 5000
              shiny::showNotification("The number of iterations must be between 100 and 5000, so it is calculated by replacing it with 5000.", type="warning")
              shiny::updateNumericInput(inputId='PRO_tsne_max_iter', value=5000)
            }else if(input$PRO_tsne_max_iter < 100){
              max_iter <- 100
              shiny::showNotification("The number of iterations must be between 100 and 5000, so it is calculated by replacing it with 100.", type="warning")
              shiny::updateNumericInput(inputId='PRO_tsne_max_iter', value=100)
            }
          }else{
            max_iter <- 500
            shiny::showNotification("The number of iterations must be numeric, so it is calculated by replacing it with 500.", type="warning")
            shiny::updateNumericInput(inputId='PRO_tsne_max_iter', value=500)
          }
          variables$PRO.dim.redu.tsne.download.log <- 1
          variables$PRO.tsne.result <- LipidSigR::dr_tsne(
            processed_se=variables$PRO.processed.SE,
            pca=TRUE, perplexity=perplexity, max_iter=max_iter,
            clustering=input$PRO_cluster_method, cluster_num=cluster_num,
            kmedoids_metric=kmedoids_metric, distfun=distfun,
            hclustfun=hclustfun, eps=eps, minPts=minPts)
          shinyjs::show('PRO_dim_redu_result_tsne_div')
          
        }else if(input$PRO_dim_redu_method == 'umap'){
          shinyjs::hide('PRO_dim_redu_result_pca_div')
          shinyjs::hide('PRO_dim_redu_result_tsne_div')
          if(is.numeric(input$PRO_umap_n_neighbors)){
            n_neighbors <- input$PRO_umap_n_neighbors
            if(input$PRO_umap_n_neighbors > 23){
              n_neighbors <- 23
              shiny::showNotification("The number of neighbors must be between 2 and 23, so it is calculated by replacing it with 23.", type="warning")
              shiny::updateNumericInput(inputId='PRO_umap_n_neighbors', value=23)
            }else if(input$PRO_umap_n_neighbors < 2){
              n_neighbors <- 2
              shiny::showNotification("The number of neighbors must be between 2 and 23, so it is calculated by replacing it with 2.", type="warning")
              shiny::updateNumericInput(inputId='PRO_umap_n_neighbors', value=2)
            }
          }else{
            n_neighbors <- 15
            shiny::showNotification("The number of neighbors must be numeric, so it is calculated by replacing it with 15.", type="warning")
            shiny::updateNumericInput(inputId='PRO_umap_n_neighbors', value=15)
          }
          variables$PRO.dim.redu.umap.download.log <- 1
          variables$PRO.umap.result <- LipidSigR::dr_umap(
            processed_se=variables$PRO.processed.SE,
            n_neighbors=n_neighbors, scaling=TRUE,
            umap_metric=input$PRO_umap_metric,
            clustering=input$PRO_cluster_method, cluster_num=cluster_num,
            kmedoids_metric=kmedoids_metric, distfun=distfun,
            hclustfun=hclustfun, eps=eps, minPts=minPts)
          shinyjs::show('PRO_dim_redu_result_umap_div')
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Dimensionality reduction error!",
          text=as.character(e$message),
          type="error")
        #### shinyjs show/hide results ####
        shinyjs::hide('PRO_dim_redu_result_div')
      }
    )
  })
}) ## observeEvent # PRO_dim_redu_start

#### control reset button ####
shiny::observeEvent(input$PRO_dim_redu_reset, {

  #### shinyjs show/hide results ####
  shinyjs::hide('PRO_dim_redu_result_div')

  #### shinyjs reset control panel ####
  shinyjs::reset("PRO_dim_redu_reset_div")

}) ## observeEvent # PRO_dim_redu_reset


##### PCA #####
#### Function: PCA_variable ####
shiny::observeEvent(input$PRO_pca_variable_topN, {
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$PRO.pca.result)){
          ## cluster method parameter ##
          cluster_num <- switch(input$PRO_cluster_method,
                                kmeans=input$PRO_kmeans_group,
                                kmedoids=input$PRO_pam_group,
                                hclustering=input$PRO_hclust_group,
                                dbscan=NULL)
          kmedoids_metric <- if(input$PRO_cluster_method == 'kmedoids'){ input$PRO_pam_metric }else{ NULL }
          distfun <- if(input$PRO_cluster_method == 'hclustering'){ input$PRO_hclust_dist }else{ NULL }
          hclustfun <- if(input$PRO_cluster_method == 'hclustering'){ input$PRO_hclust_hclust }else{ NULL }
          eps <- if(input$PRO_cluster_method == 'dbscan'){ input$PRO_dbscan_eps }else{ NULL }
          minPts <- if(input$PRO_cluster_method == 'dbscan'){ input$PRO_dbscan_minPts }else{ NULL }
          n_PC <- if(input$PRO_pca_contrib_PC == '1_2'){ c(1, 2) }else{ as.numeric(input$PRO_pca_contrib_PC) }
          
          if(input$PRO_cluster_method == 'dbscan'){
            if(is.numeric(eps)){
              if(eps < 0.1){
                eps <- 0.1
                shiny::showNotification("The epsilon must be greater than 0.1, so it is calculated by replacing it with 0.1.", type="warning")
                shiny::updateNumericInput(inputId='PRO_dbscan_eps', value=0.1)
              }
            }else{
              eps <- 0.5
              shiny::showNotification("The epsilon must be numeric, so it is calculated by replacing it with 0.5.", type="warning")
              shiny::updateNumericInput(inputId='PRO_dbscan_eps', value=0.5)
            }
            
            if(is.numeric(minPts)){
              if(minPts > 22){
                minPts <- 22
                shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 22.", type="warning")
                shiny::updateNumericInput(inputId='PRO_dbscan_minPts', value=22)
              }else if(input$PRO_dbscan_minPts < 1){
                minPts <- 1
                shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='PRO_dbscan_minPts', value=1)
              }
            }else{
              minPts <- 1
              shiny::showNotification("The minPts must be numeric, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='PRO_dbscan_minPts',value=1)
            }
          }
          variables$PRO.dim.redu.pca.download.log <- 1
          variables$PRO.dim.redu.pca.topN.download.log <- 1
          variables$PRO.pca.result <- LipidSigR::dr_pca(
            processed_se=variables$PRO.processed.SE, scaling=TRUE,
            centering=TRUE, clustering=input$PRO_cluster_method,
            cluster_num=cluster_num, kmedoids_metric=kmedoids_metric, distfun=distfun,
            hclustfun=hclustfun, eps=eps, minPts=minPts, feature_contrib_pc=n_PC, plot_topN=input$PRO_pca_variable_topN)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Dimensionality reduction PCA topN error!",
          text=as.character(e$message),
          type="error")
        #### shinyjs show/hide results ####
        shinyjs::hide('PRO_dim_redu_result_div')
      }
    )
  })
})

shiny::observeEvent(input$PRO_pca_contrib_PC, {
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$PRO.pca.result)){
          ## cluster method parameter ##
          cluster_num <- switch(input$PRO_cluster_method,
                                kmeans=input$PRO_kmeans_group,
                                kmedoids=input$PRO_pam_group,
                                hclustering=input$PRO_hclust_group,
                                dbscan=NULL)
          kmedoids_metric <- if(input$PRO_cluster_method == 'kmedoids'){ input$PRO_pam_metric }else{ NULL }
          distfun <- if(input$PRO_cluster_method == 'hclustering'){ input$PRO_hclust_dist }else{ NULL }
          hclustfun <- if(input$PRO_cluster_method == 'hclustering'){ input$PRO_hclust_hclust }else{ NULL }
          eps <- if(input$PRO_cluster_method == 'dbscan'){ input$PRO_dbscan_eps }else{ NULL }
          minPts <- if(input$PRO_cluster_method == 'dbscan'){ input$PRO_dbscan_minPts }else{ NULL }
          n_PC <- if(input$PRO_pca_contrib_PC == '1_2'){ c(1, 2) }else{ as.numeric(input$PRO_pca_contrib_PC) }
          
          if(input$PRO_cluster_method == 'dbscan'){
            if(is.numeric(eps)){
              if(eps < 0.1){
                eps <- 0.1
                shiny::showNotification("The epsilon must be greater than 0.1, so it is calculated by replacing it with 0.1.", type="warning")
                shiny::updateNumericInput(inputId='PRO_dbscan_eps', value=0.1)
              }
            }else{
              eps <- 0.5
              shiny::showNotification("The epsilon must be numeric, so it is calculated by replacing it with 0.5.", type="warning")
              shiny::updateNumericInput(inputId='PRO_dbscan_eps', value=0.5)
            }
            
            if(is.numeric(minPts)){
              if(minPts > 22){
                minPts <- 22
                shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 22.", type="warning")
                shiny::updateNumericInput(inputId='PRO_dbscan_minPts', value=22)
              }else if(input$PRO_dbscan_minPts < 1){
                minPts <- 1
                shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='PRO_dbscan_minPts', value=1)
              }
            }else{
              minPts <- 1
              shiny::showNotification("The minPts must be numeric, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='PRO_dbscan_minPts',value=1)
            }
          }
          variables$PRO.dim.redu.pca.download.log <- 1
          variables$PRO.dim.redu.pca.topN.download.log <- 1
          variables$PRO.pca.result <- LipidSigR::dr_pca(
            processed_se=variables$PRO.processed.SE, scaling=TRUE,
            centering=TRUE, clustering=input$PRO_cluster_method,
            cluster_num=cluster_num, kmedoids_metric=kmedoids_metric, distfun=distfun,
            hclustfun=hclustfun, eps=eps, minPts=minPts, feature_contrib_pc=n_PC, plot_topN=input$PRO_pca_variable_topN)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Dimensionality reduction PCA principal component error!",
          text=as.character(e$message),
          type="error")
        #### shinyjs show/hide results ####
        shinyjs::hide('PRO_dim_redu_result_div')
      }
    )
  })
})

#### Output: PRO.pca.plot ####
output$PRO.pca.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$PRO.pca.result$interactive_pca), "Plot not showing. Missing value imputation is recommended."))
  variables$PRO.pca.result$interactive_pca
})

#### Output: PRO.pca.screeplot ####
output$PRO.pca.screeplot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$PRO.pca.result$interactive_screePlot), "Plot not showing. Missing value imputation is recommended."))
  variables$PRO.pca.result$interactive_screePlot
})

#### Output: PRO.pca.rotated.data ####
output$PRO.pca.rotated.data <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$PRO.pca.result$pca_rotated_data), "Table not showing. Missing value imputation is recommended."))
  DT::datatable(variables$PRO.pca.result$pca_rotated_data %>%
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=TRUE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$PRO.pca.rotated.data <- renderDataTable

#### Output: PRO.pca.contrib.table ####
output$PRO.pca.contrib.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$PRO.pca.result$table_pca_contribution), "Table not showing. Missing value imputation is recommended."))

  DT::datatable(variables$PRO.pca.result$table_pca_contribution %>%
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=TRUE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$PRO.pca.contrib.table <- renderDataTable

#### Output: PRO.pca.variable ####
output$PRO.pca.variable <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$PRO.pca.result$interactive_variablePlot), "Plot not showing. Missing value imputation is recommended."))
  variables$PRO.pca.result$interactive_variablePlot
})

#### Output: PRO.pca.contrib ####
output$PRO.pca.contrib <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$PRO.pca.result$interactive_feature_contribution), "Plot not showing. Missing value imputation is recommended."))
  variables$PRO.pca.result$interactive_feature_contribution
})


shiny::observeEvent(input$PRO.dim.redu.pca.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$PRO.dim.redu.pca.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$PRO.dim.redu.pca.download <- shiny::downloadHandler(
            filename=function(){
              paste("PRO.dim.redu.pca.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## PCA plot ##
              grDevices::pdf(file.path(temp_directory,'PCA.pdf'), width=8, height=6)
              print(variables$PRO.pca.result$static_pca)
              grDevices::dev.off()
              ## PCA scree plot ##
              grDevices::pdf(file.path(temp_directory,'PCA.scree.pdf'), width=8, height=6)
              print(variables$PRO.pca.result$static_screePlot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$PRO.pca.result$pca_rotated_data, file=file.path(temp_directory,'PCA.rotated.csv'))
              write.csv(variables$PRO.pca.result$table_pca_contribution, file=file.path(temp_directory,'PCA.contribution.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$PRO.dim.redu.pca.download.log <- 2
          shinyjs::runjs("document.getElementById('PRO.dim.redu.pca.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('PRO.dim.redu.pca.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Dimensionality reduction PCA download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$PRO.dim.redu.pca.topN.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$PRO.dim.redu.pca.topN.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$PRO.dim.redu.pca.topN.download <- shiny::downloadHandler(
            filename=function(){
              paste("PRO.dim.redu.pca.topN.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## PCA correlation circle plot ##
              grDevices::pdf(file.path(temp_directory,'PCA.topN.variable.pdf'), width=8, height=6)
              print(variables$PRO.pca.result$static_variablePlot)
              grDevices::dev.off()
              ## Feature contribution histogram ##
              grDevices::pdf(file.path(temp_directory,'PCA.topN.contribution.pdf'), width=8, height=6)
              print(variables$PRO.pca.result$static_feature_contribution)
              grDevices::dev.off()
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$PRO.dim.redu.pca.topN.download.log <- 2
          shinyjs::runjs("document.getElementById('PRO.dim.redu.pca.topN.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('PRO.dim.redu.pca.topN.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Dimensionality reduction PCA topN download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})
##### tSNE #####

#### Output: PRO.tsne.plot ####
output$PRO.tsne.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$PRO.tsne.result$interactive_tsne), "Plot not showing. Missing value imputation is recommended."))
  variables$PRO.tsne.result$interactive_tsne
})

#### Output: PRO.tsne.table ####
output$PRO.tsne.table <- DT::renderDataTable(server=FALSE, {
  shiny::isolate({
    shiny::validate(shiny::need(!is.null(variables$PRO.tsne.result$tsne_result), "Table not showing. Missing value imputation is recommended."))
    DT::datatable(variables$PRO.tsne.result$tsne_result,
                  escape=FALSE, selection='none', rownames=TRUE,
                  class="nowrap row-border",
                  extensions=c('Buttons', 'Scroller'),
                  options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                               deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                               dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                               columnDefs=list(list(className='dt-center', targets="_all"))))
  })
}) #output$PRO.tsne.table <- renderDataTable

shiny::observeEvent(input$PRO.dim.redu.tsne.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$PRO.dim.redu.tsne.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$PRO.dim.redu.tsne.download <- shiny::downloadHandler(
            filename=function(){
              paste("PRO.dim.redu.tsne.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## UMAP plot ##
              grDevices::pdf(file.path(temp_directory,'Tsne.pdf'), width=8, height=6)
              print(variables$PRO.tsne.result$static_tsne)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$PRO.tsne.result$tsne_result, file=file.path(temp_directory,'Tsne.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$PRO.dim.redu.tsne.download.log <- 2
          shinyjs::runjs("document.getElementById('PRO.dim.redu.tsne.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('PRO.dim.redu.tsne.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Dimensionality reduction t-SNE download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) ## observeEvent #PRO.dim.redu.tsne.download.start

##### UMAP ######

#### Output: PRO.umap.plot ####
output$PRO.umap.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$PRO.umap.result$interactive_umap), "Plot not showing. Missing value imputation is recommended."))
  variables$PRO.umap.result$interactive_umap
})

#### Output: PRO.umap.table ####
output$PRO.umap.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$PRO.umap.result$umap_result), "Table not showing. Missing value imputation is recommended."))
  DT::datatable(variables$PRO.umap.result$umap_result,
                escape=FALSE, selection='none', rownames=TRUE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$PRO.umap.table <- renderDataTable

shiny::observeEvent(input$PRO.dim.redu.umap.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$PRO.dim.redu.umap.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$PRO.dim.redu.umap.download <- shiny::downloadHandler(
            filename=function(){
              paste("PRO.dim.redu.umap.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## UMAP plot ##
              grDevices::pdf(file.path(temp_directory,'UMAP.pdf'), width=8, height=6)
              print(variables$PRO.umap.result$static_umap)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$PRO.umap.result$umap_result, file=file.path(temp_directory,'UMAP.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$PRO.dim.redu.umap.download.log <- 2
          shinyjs::runjs("document.getElementById('PRO.dim.redu.umap.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('PRO.dim.redu.umap.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Dimensionality reduction UMAP download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) ## observeEvent #PRO.dim.redu.umap.download.start

#######################################
#######################################
#####  Tab3: Correlation heatmap  #####
#######################################
#######################################

#### control start button ####
shiny::observeEvent(input$PRO_corr_heatmap_start, {
  shiny::isolate({
    tryCatch(
      {
        #### shinyjs show/hide results ####
        shinyjs::show('PRO_corr_heatmap_result_div')
        #### Function: corr_heatmap ####
        variables$PRO.corr.heatmap.sample.result <-
          LipidSigR::heatmap_correlation(
            processed_se=variables$PRO.processed.SE, char=NULL,
            transform=input$PRO_transformation, correlation=input$PRO_corr_heatmap_method,
            distfun=input$PRO_corr_heatmap_dist, hclustfun=input$PRO_corr_heatmap_hclust, type='sample')
        variables$PRO.corr.heatmap.class.result <-
          LipidSigR::heatmap_correlation(
            processed_se=variables$PRO.processed.SE, char=input$PRO_corr_heatmap_char,
            transform=input$PRO_transformation, correlation=input$PRO_corr_heatmap_method,
            distfun=input$PRO_corr_heatmap_dist, hclustfun=input$PRO_corr_heatmap_hclust, type='class')
        variables$PRO.corr.heatmap.download.start.log <- 1
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Correlation heatmap error!",
          text=as.character(e$message),
          type="error")
        #### shinyjs show/hide results ####
        shinyjs::hide('PRO_corr_heatmap_result_div')
      }
    )
  }) #isolate
}) #observeEvent(input$PRO_corr_heatmap_start

#### control user reset button ####
shiny::observeEvent(input$PRO_corr_heatmap_reset, {

  #### shinyjs show/hide main panel ####
  shinyjs::hide('PRO_corr_heatmap_result_div')

  #### shinyjs reset control panel ####
  shinyjs::reset('PRO_corr_heatmap_reset_div')

  #### clear variables ####
  variables$PRO.corr.heatmap.sample.result <- NULL
  variables$PRO.corr.heatmap.class.result <- NULL
  variables$PRO.corr.heatmap.download.start.log <- 1

}) ## observeEvent # PRO_corr_heatmap_reset


#### Output: PRO.corr.heatmap.class ####
output$PRO.corr.heatmap.class <- iheatmapr::renderIheatmap({

  variables$PRO.corr.heatmap.class.result$interactive_heatmap

}) #output$PRO.corr.heatmap.class <- renderIheatmap

#### Output: PRO.corr.heatmap.sample ####
output$PRO.corr.heatmap.sample <- iheatmapr::renderIheatmap({
  variables$PRO.corr.heatmap.sample.result$interactive_heatmap
}) #output$PRO.corr.heatmap.sample <- renderIheatmap

#### control start button ####
shiny::observeEvent(input$PRO.corr.heatmap.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$PRO.corr.heatmap.download.start.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$PRO.corr.heatmap.download <- shiny::downloadHandler(
            filename=function(){
              paste("PRO.corr.heatmap.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## Heatmap by lipid characteristics ##
              grDevices::pdf(file.path(temp_directory,'Characteristics.pdf'), width=8, height=6)
              print(variables$PRO.corr.heatmap.class.result$static_heatmap)
              grDevices::dev.off()
              ## Heatmap by sample ##
              grDevices::pdf(file.path(temp_directory,'Sample.pdf'), width=8, height=6)
              print(variables$PRO.corr.heatmap.sample.result$static_heatmap)
              grDevices::dev.off()
              ## table  ##
              utils::write.csv(variables$PRO.corr.heatmap.class.result$corr_coef_matrix, file=file.path(temp_directory,'Characteristics.csv'))
              utils::write.csv(variables$PRO.corr.heatmap.sample.result$corr_coef_matrix, file=file.path(temp_directory,'Sample.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$PRO.corr.heatmap.download.start.log <- 2
          shinyjs::runjs("document.getElementById('PRO.corr.heatmap.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('PRO.corr.heatmap.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Correlation heatmap download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

##################################################
##################################################
#####  Tab4: Lipid characteristics analysis  #####
##################################################
##################################################

#### Function: exp_compo_by_lipidinfo ####
shiny::observeEvent(input$PRO_lipid_char, {
  shiny::isolate({
    tryCatch(
      {
        variables$PRO.lipid.profiling <- LipidSigR::lipid_profiling(
          processed_se=variables$PRO.processed.SE, char=input$PRO_lipid_char)
        variables$PRO.lipid.char.download.log <- 1
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics profiling error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

#### Output: PRO.lipid.char.barplot ####
output$PRO.lipid.char.barplot <- plotly::renderPlotly({
  variables$PRO.lipid.profiling$interactive_char_barPlot
})

#### Output: PRO.lipid.char.composition ####
output$PRO.lipid.char.composition <- plotly::renderPlotly({
  variables$PRO.lipid.profiling$interactive_lipid_composition
})

shiny::observeEvent(input$PRO.lipid.char.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$PRO.lipid.char.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$PRO.lipid.char.download <- shiny::downloadHandler(
            filename=function(){
              paste("PRO.lipid.char.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## Bar plot classified by selected characteristic ##
              grDevices::pdf(file.path(temp_directory,paste0(input$PRO_lipid_char,'.barPlot.pdf')), width=8, height=6)
              print(variables$PRO.lipid.profiling$static_char_barPlot)
              grDevices::dev.off()
              ## Stacked horizontal bar chart of lipid class composition ##
              grDevices::pdf(file.path(temp_directory,paste0(input$PRO_lipid_char,'.lipid.composition.pdf')), width=8, height=6)
              print(variables$PRO.lipid.profiling$static_lipid_composition)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$PRO.lipid.profiling$table_char_barPlot, file=file.path(temp_directory,paste0(input$PRO_lipid_char,'.barPlot.csv')))
              write.csv(variables$PRO.lipid.profiling$table_lipid_composition, file=file.path(temp_directory,paste0(input$PRO_lipid_char,'.lipid.composition.csv')))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$PRO.lipid.char.download.log <- 2
          shinyjs::runjs("document.getElementById('PRO.lipid.char.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('PRO.lipid.char.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics profiling download error!",
          text=as.character(e$message),
          type="error")
        #### shinyjs show/hide results ####
        shinyjs::hide('PRO_dim_redu_result_div')
      }
    )
  })
})
