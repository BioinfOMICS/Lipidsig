#############################################
#############################################
######                                 ######
######   Differential Expressed Page   ######
######                                 ######
#############################################
#############################################

##########################
####  DE Data Source  ####
##########################

#### Output: DE.demo.download ####
output$DE.demo.download <- shiny::downloadHandler(
  filename=function() {
    "Differential_expression_example_dataset.zip"
  },
  content=function(file) {
    file.copy("www/download_demo_dataset/DE.zip", file)
  },
  contentType="application/zip"
)
shiny::outputOptions(output, "DE.demo.download", suspendWhenHidden=FALSE)

#### DE demo dataset ####
shiny::observeEvent(input$DE_demo_upload, {
  shiny::isolate({
    shiny::withProgress(message='Differential expression analysis', style='notification', detail="Upload data", value=0, {
      shinyjs::show('DE_data_check_successful')
      shinyjs::hide('DE_tabPanel_div')
      shinyjs::hide('DE_data_summary_div')
      shinyjs::hide('DE_data_process_table_div')
      shinyjs::hide('DE_start_div')
      variables$DE.data.check.progress <- htmltools::HTML('<div style="font-size: 0px;">
                                                            <h2>Upload progress</h2>
                                                            <h4>(1/3) Check data frame format</h4>
                                                            </div>')
      if(input$DE_data_source == "DE_demo_data"){
        variables$DE.SE <- readRDS('www/demo_dataset/Profiling.rds')
        variables$DE.Ngroup <- 'two'
        variables$DE.ref.group <- 'ctrl'
        variables$DE.exclude.ref.group <- 'exp'
      }else if(input$DE_data_source == "DE_demo_multiple_data"){
        variables$DE.SE <- readRDS('www/demo_dataset/DE_3group_before_process.rds')
        variables$DE.Ngroup <- 'multiple'
        variables$DE.ref.group <- 'ctrl'
        variables$DE.exclude.ref.group <- c('DHA','AA')
      }
      variables$DE.SE.list <- LipidSigR::extract_summarized_experiment(variables$DE.SE)
      variables$DE.raw.abundance <- variables$DE.SE.list$abundance
      variables$DE.group.info <- variables$DE.SE.list$group_info
      variables$DE.group.info.check <- variables$DE.group.info
      variables$DE.all.group.name <- variables$DE.group.info %>%
        dplyr::pull(group) %>% unique()
      variables$DE.group.compare.message <- htmltools::HTML('<div> <h4 style="font-weight: bold;font-size: 15px;">Reference group : ',
                                                             variables$DE.ref.group, '</h4>
                                                             <h4 style="font-weight: bold;font-size: 15px;">Testing group : ',
                                                             paste0(variables$DE.exclude.ref.group,collapse=' and '),'</h4>
                                                             </div>')
      variables$DE.data.check.progress <- htmltools::HTML('<div style="font-size: 0px;">
                                                            <h2>Upload progress</h2>
                                                            <h4>(1/3) Check data frame format.</h4>
                                                            </div>')
      shiny::incProgress(0.33, detail='Check data format')
      #### import demo dataset ####
      if(input$DE_data_source == "DE_demo_data"){
        variables$DE.check.step1 <- inputFormat(variables$DE.raw.abundance, variables$DE.group.info,
                                             abundance_path='demo_exp.csv',
                                             group_info_path='demo_group_info.csv', analysis_type="DE", nGroup="two",
                                             variables=variables, session=session)
      }else if(input$DE_data_source == "DE_demo_multiple_data"){
        variables$DE.check.step1 <- inputFormat(variables$DE.raw.abundance, variables$DE.group.info,
                                             abundance_path='demo_exp.csv',
                                             group_info_path='demo_group_info.csv', analysis_type="DE", nGroup="multiple",
                                             variables=variables, session=session)
      }
      shiny::incProgress(0.33, detail='Check data format')
      if(variables$DE.check.step1$logical == TRUE){
        if(input$DE_data_source == "DE_demo_data"){
          variables$DE.check.step2 <- data_check(abundance=variables$DE.raw.abundance,
                                              group_info=variables$DE.group.info,
                                              abundance_path='demo_exp.csv',
                                              group_info_path='demo_group_info.csv',
                                              ref_group=variables$DE.ref.group,
                                              analysis_type="DE",
                                              nGroup="two",
                                             variables=variables, session=session)
        }else if(input$DE_data_source == "DE_demo_multiple_data"){
          variables$DE.check.step2 <- data_check(abundance=variables$DE.raw.abundance,
                                              group_info=variables$DE.group.info,
                                              abundance_path='demo_exp.csv',
                                              group_info_path='demo_group_info.csv',
                                              ref_group=variables$DE.ref.group,
                                              analysis_type="DE", nGroup="multiple",
                                             variables=variables, session=session)
        }
        output$DE.Check.SE <- shiny::renderUI({
          shiny::isolate({
            variables$DE.check.step2$return_div
          })
        })
        if(grepl('class="fas fa-xmark"',variables$DE.check.step2$return_div)){
          shinyjs::show('DE_data_warning')
          shinyjs::hide('DE_data_processing_div')
        }else{
          shinyjs::show('DE_data_warning')
          shinyjs::show('DE_data_processing_div')
          variables$DE.SE <- variables$DE.check.step2$rawSE
        }
      }else{
        output$DE.Check.SE <- shiny::renderUI({
          shiny::isolate({
            variables$DE.check.step1$return_div
          })
        })
        shinyjs::show('DE_data_warning')
        shinyjs::hide('DE_data_processing_div')
      }
      variables$DE.data.check.progress <- htmltools::HTML('<div style="font-size: 0px;">
                                                            <h2>Upload progress</h2>
                                                            <h4>(2/3) Data check finish.</h4>
                                                            </div>')
      shiny::incProgress(0.34, detail='Check data format finish')
    })
  })
}) #shiny::observeEvent(input$DE_demo_upload


#### DE user dataset ####
shiny::observeEvent(input$DE_user_upload, {
  shiny::isolate({
    tryCatch(
      {
        #### import user dataset ####
        ## group_info
        if(grepl('.xlsx',input$DE_user_group$datapath)){
          variables$DE.group.info.check <- readxl::read_excel(
            input$DE_user_group$datapath,na=c('', 'NA', 'na')) %>% as.data.frame()
        }else{
          variables$DE.group.info.check <- data.table::fread(
            input$DE_user_group$datapath, header=TRUE, stringsAsFactors=FALSE,
            check.names=FALSE, data.table=FALSE, na.strings=c('', 'NA', 'na'))
        }
        shiny::showNotification("Received uploaded file.", type="message")
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Input data error!",
          text=as.character(message(e)),
          type="error")
        return()
      },
      warning=function(w) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Input data warning!",
          text="Some error is in your dataset, it maybe cause some problem we cannot expected.",
          type="warning")
        return()
      }
    )
    tryCatch(
      {
        if(!is.null(variables$DE.group.info.check) & 'group' %in% colnames(variables$DE.group.info.check)){
          choices=unique(variables$DE.group.info.check$group)
          shiny::updateSelectInput(session, 'DE_user_ref_group',
                                   choices=choices, selected=choices[1])
          shinyjs::show('DE_user_ref_group_div')
        }else{
          shinyjs::hide('DE_user_ref_group_div')
          variables$DE.data.check.progress <- htmltools::tags$div(
            htmltools::h2('Upload progress'),
            htmltools::h4('(1/3) Check data frame format.'),
            htmltools::tags$p(
              shiny::icon("times"), 'The group file must contain a column with name as "group"',
              style="font-size: 16px;color:red;"),
            htmltools::h4("For a more detailed examination, please visit the ",
                          htmltools::HTML("<a href='javascript:void(0)' onclick='switchTab(\"Data_Check\")' style='color: darkblue;'>Data Check</a>"), "."),
            style="font-size: 0px;")
        }
      },
      error=function(e) {
        shinyWidgets::show_alert(
          title='Error',
          text=HTML("<h4>Detect unknown input data format errors.</h4>
                               <h4>For the correct data format guidelines, please refer to the <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank'>FAQ</a></h4>.
                               <h4>If you need further assistance, please email us your data.</h4>"),
          html=TRUE,type ='error')
        return()
      }
    )
  })
}) #shiny::observeEvent(input$DE_user_upload

shiny::observeEvent(input$DE_user_submit, {
  shiny::isolate({
    tryCatch(
      {
        shiny::withProgress(message='Differential expression analysis', style='notification', detail="Upload data", value=0, {
          shinyjs::show('DE_data_check_successful')
          shinyjs::hide('DE_tabPanel_div')
          shinyjs::hide('DE_data_summary_div')
          shinyjs::hide('DE_data_process_table_div')
          shinyjs::hide('DE_start_div')
          shiny::showNotification("Start uploading file...", type="message")
          #### import user dataset ####
          tryCatch(
            {
              ## abundance
              if(grepl('.xlsx',input$DE_user_abundance$datapath)){
                variables$DE.raw.abundance <- readxl::read_excel(
                  input$DE_user_abundance$datapath,na=c('', 'NA', 'na')) %>% as.data.frame()
              }else{
                variables$DE.raw.abundance <- data.table::fread(
                  input$DE_user_abundance$datapath, header=TRUE, stringsAsFactors=FALSE,
                  check.names=FALSE, data.table=FALSE, na.strings=c('', 'NA'))
              }
              ## group_info
              if(grepl('.xlsx',input$DE_user_group$datapath)){
                variables$DE.group.info <- readxl::read_excel(
                  input$DE_user_group$datapath, na=c('', 'NA', 'na')) %>% as.data.frame()
              }else{
                variables$DE.group.info <- data.table::fread(
                  input$DE_user_group$datapath, header=TRUE, stringsAsFactors=FALSE,
                  check.names=FALSE, data.table=FALSE, na.strings=c('', 'NA','na'))
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
          variables$DE.data.check.progress <- htmltools::HTML('<div style="font-size: 0px;">
                                                            <h2>Upload progress</h2>
                                                            <h4>(1/3) Check data frame format.</h4>
                                                            </div>')
          ### data check ####
          ## abundance
          variables$DE.Ngroup <- input$DE_user_nGroup
          variables$DE.ref.group <- input$DE_user_ref_group
          variables$DE.exclude.ref.group <- unique(variables$DE.group.info$group)[-which(unique(variables$DE.group.info$group) == variables$DE.ref.group)]
          variables$DE.all.group.name <- variables$DE.group.info %>%
            dplyr::pull(group) %>% unique()
          variables$DE.group.compare.message <- htmltools::HTML('<div> <h4 style="font-weight: bold;font-size: 15px;">Reference group : ',
                                                                variables$DE.ref.group, '</h4>
                                                             <h4 style="font-weight: bold;font-size: 15px;">Testing group : ',
                                                                paste0(variables$DE.exclude.ref.group,collapse=' and '),'</h4>
                                                             </div>')
          if(variables$DE.Ngroup == 'two'){
            variables$DE.checkUTF8 <- check_utf8Format(variables$DE.raw.abundance, variables$DE.group.info,
                                                       abundance_path=input$DE_user_abundance$datapath,
                                                       group_info_path=input$DE_user_group$datapath,
                                                       analysis_type="DE", nGroup="two",
                                             variables=variables, session=session)
          }else{
            variables$DE.checkUTF8 <- check_utf8Format(variables$DE.raw.abundance, variables$DE.group.info,
                                                       abundance_path=input$DE_user_abundance$datapath,
                                                       group_info_path=input$DE_user_group$datapath,
                                                       analysis_type="DE", nGroup="multiple",
                                             variables=variables, session=session)
          }
          shiny::incProgress(0.33, detail='Check data format')
          if(variables$DE.checkUTF8$logical == TRUE){
            shinyjs::show('DE_data_Uploaded')
            if(variables$DE.Ngroup == 'two'){
              variables$DE.check.step1 <- inputFormat(variables$DE.raw.abundance,variables$DE.group.info,
                                                      abundance_path=input$DE_user_abundance$datapath,
                                                      group_info_path=input$DE_user_group$datapath,
                                                      analysis_type="DE", nGroup="two",
                                             variables=variables, session=session)
            }else{
              variables$DE.check.step1 <- inputFormat(variables$DE.raw.abundance,variables$DE.group.info,
                                                      abundance_path=input$DE_user_abundance$datapath,
                                                      group_info_path=input$DE_user_group$datapath,
                                                      analysis_type="DE", nGroup="multiple",
                                             variables=variables, session=session)
            }
            shiny::incProgress(0.33, detail='Check data format')
            if(variables$DE.check.step1$logical == TRUE){
              #colnames(variables$DE.raw.abundance)[1] <- "feature"
              variables$DE.raw.abundance <- variables$DE.raw.abundance %>%
                dplyr::select(c(1, variables$DE.group.info$sample_name))
              if(variables$DE.Ngroup == 'two'){
                variables$DE.check.step2 <- data_check(abundance=variables$DE.raw.abundance,
                                                       group_info=variables$DE.group.info,
                                                       abundance_path=input$DE_user_abundance$datapath,
                                                       group_info_path=input$DE_user_group$datapath,
                                                       ref_group=variables$DE.ref.group,
                                                       analysis_type="DE", nGroup="two",
                                             variables=variables, session=session)
              }else{
                variables$DE.check.step2 <- data_check(abundance=variables$DE.raw.abundance,
                                                       group_info=variables$DE.group.info,
                                                       abundance_path=input$DE_user_abundance$datapath,
                                                       group_info_path=input$DE_user_group$datapath,
                                                       ref_group=variables$DE.ref.group,
                                                       analysis_type="DE", nGroup="multiple",
                                             variables=variables, session=session)
              }
              if(variables$DE.check.step2$lipid_id_pct >= 20){
                shinyWidgets::show_alert(
                  title='Warning',
                  text=htmltools::HTML("<h4>Warning! The unrecognized lipids in the uploaded abundance data are more than 20%.</h4>
                              <h4>We recommend revising the lipids' names. Please use Shorthand notation or refer to HMDB, SwissLipids, and LIPID MAPS LMSD styles for lipid input.</h4>
                              <h4>You can access the detailed instructions in our <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ12' target='_blank'>FAQ</a>.</h4>"),
                  html=TRUE,type ='warning')
              }
              output$DE.Check.SE <- shiny::renderUI({
                shiny::isolate({
                  variables$DE.check.step2$return_div
                })
              })
              if(grepl('class="fas fa-xmark"', variables$DE.check.step2$return_div)){
                shinyjs::show('DE_data_warning')
                shinyjs::hide('DE_data_processing_div')
              }else{
                shinyjs::show('DE_data_warning')
                shinyjs::show('DE_data_processing_div')
                abundance_mat <- SummarizedExperiment::assay(variables$DE.check.step2$rawSE) %>%
                  as.data.frame
                lipid_char <- SummarizedExperiment::rowData(variables$DE.check.step2$rawSE) %>%
                  as.data.frame
                variables$DE.SE <- SummarizedExperiment::SummarizedExperiment(
                  assays=list(abundance=as.matrix(abundance_mat)),
                  rowData=S4Vectors::DataFrame(lipid_char, row.names=lipid_char$feature),
                  colData=variables$DE.group.info)
              }
            }else{
              shinyjs::show('DE_data_warning')
              shinyjs::hide('DE_data_processing_div')
              output$DE.Check.SE <- shiny::renderUI({
                shiny::isolate({
                  variables$DE.check.step1$return_div
                })
              })
            }
          }else{
            variables$DE.raw.abundance <- variables$DE.group.info <- NULL
            shinyjs::hide('DE_data_Uploaded')
            shinyjs::show('DE_data_warning')
            shinyjs::hide('DE_data_processing_div')
            output$DE.Check.SE <- shiny::renderUI({
              shiny::isolate({
                variables$DE.checkUTF8$return_div
              })
            })
          }
          variables$DE.data.check.progress <- htmltools::HTML('<div style="font-size: 0px;">
                                                            <h2>Upload progress</h2>
                                                            <h4>(2/3) Data check finish.</h4>
                                                            </div>')
          shiny::incProgress(0.34, detail='Check data format finish')
        })
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
}) #shiny::observeEvent(input$DE_user_upload

#### Output: DE.data.check.progress ####
output$DE.data.check.progress <- shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$DE.data.check.progress), ""))
  variables$DE.data.check.progress
})
#### Output: DE.species.groupInfo.compare.message ####
output$DE.species.groupInfo.compare.message <- shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$DE.group.compare.message), ""))
  variables$DE.group.compare.message
})
#### Output: DE.species.PCA.groupInfo.compare.message ####
output$DE.species.PCA.groupInfo.compare.message <- shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$DE.group.compare.message), ""))
  variables$DE.group.compare.message
})
#### Output: DE.class.groupInfo.compare.message ####
output$DE.class.groupInfo.compare.message <- shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$DE.group.compare.message), ""))
  variables$DE.group.compare.message
})
#### Output: DE.class.PCA.groupInfo.compare.message ####
output$DE.class.PCA.groupInfo.compare.message <- shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$DE.group.compare.message), ""))
  variables$DE.group.compare.message
})
#### Output: DE.class.twoChar.groupInfo.compare.message ####
output$DE.class.twoChar.groupInfo.compare.message <- shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$DE.group.compare.message), ""))
  variables$DE.group.compare.message
})
#### Output: DE.raw.abundance ####
output$DE.raw.abundance <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.raw.abundance), "Some error is in your expression data, please check your data and re-upload it."))
  DT::datatable(variables$DE.raw.abundance,
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})
#### Output: DE.user.group.info ####
output$DE.group.info <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.group.info.check), "Some error is in your group information, please check your data and re-upload it."))
  DT::datatable(variables$DE.group.info.check %>%
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})

#### control user reset button ####
shiny::observeEvent(input$DE_user_reset, {
  shiny::isolate({
    #### shinyjs show/hide main panel ####
    shinyjs::hide('DE_user_mainPanel_div')
    #### shiny show/hide tab ####
    shiny::hideTab(inputId='DE_analysis_tab', target='Lipid species analysis')
    shiny::hideTab(inputId='DE_analysis_tab', target='Lipid category analysis')
    #### shinyjs reset control panel ####
    shinyjs::reset('DE_user_reset_div')
    #### clear variables ####
    variables$DE.raw.abundance        <- NULL
    variables$DE.group.info.check     <- NULL
    variables$DE.group.info           <- NULL
    variables$DE.Ngroup               <- NULL
    variables$DE.SE                   <- NULL
    variables$DE.SE.list              <- NULL
    variables$DE.processed.SE         <- NULL
    variables$DE.processed.SE.list    <- NULL
    variables$DE.processed.abundance  <- NULL
    variables$DE.processed.lipid.char.tab <- NULL
    variables$DE.processed.plot       <- NULL
    variables$DE.data.summary.div     <- NULL
    variables$deSp.se                 <- NULL
    variables$DE.class.char           <- NULL
    variables$DE.class.sub.char       <- NULL
    variables$DE.data.check.progress  <- htmltools::HTML('<div style="font-size: 0px;"><h2>Upload progress</h2><h4>(1/3) Check data frame format.</h4></div>')
  })
}) #shiny::observeEvent(input$DE_user_reset

#### control user upload button ####
shiny::observe({
  if(is.null(input$DE_user_abundance) || is.null(input$DE_user_group) || is.null(input$DE_user_ref_group)){
    shinyjs::disable("DE_user_upload")
  }else{
    shinyjs::enable("DE_user_upload")
  }
}) #observe

#### control user reset button ####
shiny::observeEvent(input$DE_processing_start, {
  shiny::isolate({
    tryCatch(
      {
        shiny::withProgress(message='Differential expression analysis', style='notification', detail="Data processing", value=0, {
          imputation_param <- switch(input$DE_fill_NA,
                                     mean=NULL,
                                     median=NULL,
                                     min=input$DE_fill_min,
                                     QRILC=input$DE_fill_QRILC,
                                     SVD=input$DE_fill_param,
                                     KNN=input$DE_fill_KNN,
                                     IRMI=NULL,
                                     PPCA=input$DE_fill_param,
                                     BPCA=input$DE_fill_param)
          if(is.numeric(input$DE_filtration_param)){
            filtration_param <- input$DE_filtration_param
            if(input$DE_filtration_param > 100){
              filtration_param <- 100
              shiny::showNotification("The value of remove features with more than % missing values must be between 5 and 100, so it is calculated by replacing it with 100.", type="warning")
              shiny::updateNumericInput(inputId='DE_filtration_param', value=100)
            }else if(input$DE_filtration_param < 5){
              filtration_param <- 5
              shiny::showNotification("The value of remove features with more than % missing values must be between 5 and 100, so it is calculated by replacing it with 5.", type="warning")
              shiny::updateNumericInput(inputId='DE_filtration_param', value=5)
            }
          }else{
            filtration_param <- 70
            shiny::showNotification("The value of remove features with more than % missing values must be numeric, so it is calculated by replacing it with 70", type="warning")
            shiny::updateNumericInput(inputId='DE_filtration_param', value=70)
          }
          if(!is.null(imputation_param)){
            if(input$DE_fill_NA == 'min'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 0.5){
                  imputation_param <- 0.5
                  shiny::showNotification("The value of multiply by minimum must be between 0.1 and 0.5, so it is calculated by replacing it with 0.5.", type="warning")
                  shiny::updateNumericInput(inputId='DE_fill_min', value=0.5)
                }else if(imputation_param < 0.1){
                  imputation_param <- 0.1
                  shiny::showNotification("The value of multiply by minimum must be between 0.1 and 0.5, so it is calculated by replacing it with 0.1.", type="warning")
                  shiny::updateNumericInput(inputId='DE_fill_min',value=0.1)
                }
              }else{
                imputation_param <- 0.5
                shiny::showNotification("The value of multiply by minimum must be numeric, so it is calculated by replacing it with 0.5.", type="warning")
                shiny::updateNumericInput(inputId='DE_fill_min',value=0.5)
              }
            }else if(input$DE_fill_NA == 'QRILC'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of tune sigma must be between 0.1 and 1, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='DE_fill_QRILC', value=1)
                }else if(imputation_param < 0.1){
                  imputation_param <- 0.1
                  shiny::showNotification("The value of tune sigma must be between 0.1 and 1, so it is calculated by replacing it with 0.1.", type="warning")
                  shiny::updateNumericInput(inputId='DE_fill_QRILC', value=0.1)
                }
              }else{
                imputation_param <- 1
                shiny::showNotification("The value of tune sigma must be numeric, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='DE_fill_QRILC', value=1)
              }
            }else if(input$DE_fill_NA == 'SVD'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='DE_fill_param', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='DE_fill_param', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The value of nPCs must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='DE_fill_param', value=3)
              }
            }else if(input$DE_fill_NA == 'KNN'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The number of neighbors must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='DE_fill_KNN', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The number of neighbors must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='DE_fill_KNN', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The number of neighbors must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='DE_fill_KNN', value=3)
              }
            }else if(input$DE_fill_NA == 'PPCA'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='DE_fill_param', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='DE_fill_param', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The value of nPCs must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='DE_fill_param', value=3)
              }
            }else if(input$DE_fill_NA == 'BPCA'){
              if(is.numeric(imputation_param)){
                if(imputation_param > 10){
                  imputation_param <- 10
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 10.", type="warning")
                  shiny::updateNumericInput(inputId='DE_fill_param', value=10)
                }else if(imputation_param < 1){
                  imputation_param <- 1
                  shiny::showNotification("The value of nPCs must be between 1 and 10, so it is calculated by replacing it with 1.", type="warning")
                  shiny::updateNumericInput(inputId='DE_fill_param', value=1)
                }
              }else{
                imputation_param <- 3
                shiny::showNotification("The value of nPCs must be numeric, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='DE_fill_param',value=3)
              }
            }
          }
          variables$DE.processed.SE <- LipidSigR::data_process(
            se=variables$DE.SE, exclude_missing=input$DE_rm_NA,
            exclude_missing_pct=filtration_param,
            replace_na_method=input$DE_fill_NA, replace_na_method_ref=imputation_param,
            normalization=input$DE_normalization, transform=input$DE_transformation)
          variables$DE.processed.SE.list <- LipidSigR::extract_summarized_experiment(variables$DE.processed.SE)
          variables$DE.processed.abundance <- variables$DE.processed.SE.list$processed_abund

          variables$DE.processed.lipid.char.tab <- variables$DE.processed.SE.list$lipid_char_table
          variables$DE.processed.lipid.char.tab <- char.tab.url(variables$DE.processed.lipid.char.tab)
          shiny::incProgress(0.4, detail='Data processing')
          variables$DE.processed.plot <- LipidSigR::plot_data_process(variables$DE.SE, variables$DE.processed.SE)
          variables$DE.data.summary.div <- data_summary(se=variables$DE.SE,
                                                        analysis_type="DE",
                                                        exclude_missing=input$DE_rm_NA,
                                                        exclude_missing_pct=filtration_param,
                                                        replace_na_method=input$DE_fill_NA,
                                                        replace_na_method_ref=imputation_param,
                                                        normalization=input$DE_normalization,
                                                        transform=input$DE_transformation)

          variables$DE.species.lipid.char <- data.frame(aspect=names(LipidSigR::list_lipid_char(variables$DE.processed.SE)$common_list),
                                                        characteristic=LipidSigR::list_lipid_char(variables$DE.processed.SE)$common_list) %>%
            dplyr::mutate(aspect=factor(aspect),
                          characteristic=factor(characteristic)) %>%
            dplyr::arrange(characteristic)
          #### update DE species characteristic select input ####
          output$DE.species.sidecolor <- shiny::renderUI({
            shiny::selectInput(
              inputId='DE_species_sidecolor', label='Characteristics:',
              choices=split(as.list(levels(variables$DE.species.lipid.char$characteristic)), variables$DE.species.lipid.char$aspect),
              selected='class', multiple=FALSE)
          })
          output$DE.species.lipid.char <- shiny::renderUI({
            shiny::selectInput(
              inputId='DE_species_lipid_char', label='Select the lipid characteristics:',
              choices=split(as.list(levels(variables$DE.species.lipid.char$characteristic)), variables$DE.species.lipid.char$aspect),
              selected='class', multiple=FALSE)
          })
          #### update DE dimensionality reduction select input ####
          DE.cluster_num <- ncol(variables$DE.processed.abundance)-1
          shiny::updateSliderInput(session, "DE_species_kmeans_group", max=DE.cluster_num-1)
          shiny::updateSliderInput(session, "DE_species_pam_group", max=DE.cluster_num-1)
          shiny::updateSliderInput(session, "DE_species_hclust_group", max=DE.cluster_num)
          shiny::updateSliderInput(session, "DE_class_kmeans_group", max=DE.cluster_num-1)
          shiny::updateSliderInput(session, "DE_class_pam_group", max=DE.cluster_num-1)
          shiny::updateSliderInput(session, "DE_class_hclust_group", max=DE.cluster_num)

          #### update DE class twoWayAnova select input ####
          variables$DE.class.char.twoWayAnova <- LipidSigR::char_2wayAnova(
            processed_se=variables$DE.processed.SE, ratio_transform='log2', char_transform=input$DE_transformation)
          shiny::updateSelectInput(session, "DE_class_twoWayAnova_select",
                                   choices= c('All', unique(variables$DE.class.char.twoWayAnova$aspect)),
                                   selected='All')
          #### update DE class analysis char & sub-char ####
          variables$DE.class.lipid.char <- variables$DE.class.char.twoWayAnova %>%
            dplyr::select(aspect, characteristic) %>%
            dplyr::mutate(aspect=factor(aspect),
                          characteristic=factor(characteristic)) %>%
            dplyr::arrange(characteristic)
          select <- ifelse('Total.C' %in% variables$DE.class.lipid.char$characteristic,'Total.C',variables$DE.class.lipid.char$characteristic[1])
          sub_select <- ifelse('class' %in% variables$DE.class.lipid.char$characteristic,'class',variables$DE.class.lipid.char$characteristic[1])
          output$DE.class.analysis.char <- shiny::renderUI({
            shiny::selectInput(
              inputId='DE_class_analysis_char', label='Characteristics:',
              choices=split(as.list(levels(variables$DE.class.lipid.char$characteristic)), variables$DE.class.lipid.char$aspect),
              selected=select, multiple=FALSE)
          })
          output$DE.class.split.char <- shiny::renderUI({
            shiny::selectInput(
              inputId='DE_class_split_char', label='Subgroup of characteristics:',
              choices=split(as.list(levels(variables$DE.class.lipid.char$characteristic)), variables$DE.class.lipid.char$aspect),
              selected=sub_select, multiple=FALSE)
          })
          shiny::incProgress(0.4, detail='Plot data processing')
          variables$DE.data.check.progress <- htmltools::HTML('<div style="font-size: 0px;">
                                                            <h2>Upload progress</h2>
                                                            <h4>(3/3) Data Processing successful.</h4>
                                                            </div>')
          shinyjs::show('DE_data_summary_div')
          shinyjs::show('DE_data_process_table_div')
          shinyjs::show('DE_start_div')
          variables$DE.processed.plot.download.log <- 1
          shinyjs::runjs('document.getElementById("DE_data_processing_div").scrollIntoView();')
          shiny::incProgress(0.2, detail='Data processing finish')
        })
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Data process error!",
          text=as.character(e$message),
          type="error")
        shinyjs::hide('DE_data_summary_div')
        shinyjs::hide('DE_data_process_table_div')
        shinyjs::hide('DE_start_div')
      }
    )
  })
}) #shiny::observeEvent(input$DE_processing_start

shiny::observeEvent(input$DE.processed.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.processed.plot.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=33)
          output$DE.processed.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE_processed_download.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)

              grDevices::pdf(file.path(temp_directory,'BoxPlot.before.process.pdf'), width=8, height=6)
              print(variables$DE.processed.plot$static_boxPlot_before)
              grDevices::dev.off()

              grDevices::pdf(file.path(temp_directory,'Boxplot.after.process.pdf'), width=8, height=6)
              print(variables$DE.processed.plot$static_boxPlot_after)
              grDevices::dev.off()

              grDevices::pdf(file.path(temp_directory,'Densityplot.before.process.pdf'), width=8, height=6)
              print(variables$DE.processed.plot$static_densityPlot_before)
              grDevices::dev.off()

              grDevices::pdf(file.path(temp_directory,'Densityplot.after.process.pdf'), width=8, height=6)
              print(variables$DE.processed.plot$static_densityPlot_after)
              grDevices::dev.off()

              zip::zip(
                zipfile=file,
                files=dir(temp_directory),
                root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=66)
          variables$DE.processed.plot.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.processed.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.processed.download').click();")
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
#### Output: DE_Data_summary ####
output$DE.data.summary <- shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$DE.data.summary.div), ""))
  variables$DE.data.summary.div
})
#### Output: DE.processed.abundance ####
output$DE.processed.abundance <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.processed.abundance), "Some error is in your expression data, please check your data and re-upload it."))
  DT::datatable(variables$DE.processed.abundance %>%
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
#### Output: DE.processed.group.info ####
output$DE.processed.group.info <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$DE.group.info), "Some error is in your group information, please check your data and re-upload it."))
  DT::datatable(variables$DE.group.info %>%
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
#### Output: DE.processed.lipid.char.tab ####
output$DE.processed.lipid.char.tab <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$DE.processed.lipid.char.tab), "Some error is in your lipid characteristics, please check your data and re-upload it."))
  DT::datatable(variables$DE.processed.lipid.char.tab %>%
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
#### Output: DE.lipid.id ####
output$DE.lipid.id <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$DE.processed.lipid.char.tab), "Some error is in your lipid characteristics, please check your data and re-upload it."))
  DT::datatable(variables$DE.processed.lipid.char.tab %>%
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
#### Output: DE.before.processed.boxplot ####
output$DE.before.processed.boxplot <-  plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.processed.plot$interactive_boxPlot_before), ""))
  variables$DE.processed.plot$interactive_boxPlot_before
})

#### Output: DE.after.processed.boxplot ####
output$DE.after.processed.boxplot <-  plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.processed.plot$interactive_boxPlot_after), ""))
  variables$DE.processed.plot$interactive_boxPlot_after
})

#### Output: DE.before.processed.density ####
output$DE.before.processed.density <-  plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.processed.plot$interactive_densityPlot_before), ""))
  variables$DE.processed.plot$interactive_densityPlot_before
})

#### Output: DE.after.processed.density ####
output$DE.after.processed.density <-  plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.processed.plot$interactive_densityPlot_after), ""))
  variables$DE.processed.plot$interactive_densityPlot_after
})
#### control user reset button ####
shiny::observeEvent(input$DE_processing_reset, {
  shiny::isolate({
    #### shinyjs reset control panel ####
    shinyjs::reset('DE_data_processing_div')
    #### clear variables ####
    variables$DE.processed.SE <- NULL
    variables$DE.processed.SE.list <- NULL
    variables$DE.processed.abundance <- NULL
    variables$DE.processed.lipid.char.tab <- NULL
    variables$DE.processed.plot <- NULL
    variables$DE.data.summary.div <- NULL
    variables$DE.processed.plot.download.log <- 1
  })
}) #shiny::observeEvent(input$DE_processing_reset

###########################
####  DE analysis tab  ####
###########################

#### control hide/show tabpanel: input$DE_user_start ####
shiny::observeEvent(input$DE_start, {
  shiny::isolate({
    shinyjs::show('DE_tabPanel_div')
    shiny::showTab(inputId='DE_analysis_tab', target='Lipid species analysis')
    shiny::hideTab(inputId='DE_species_list', target='Result download')
    shiny::hideTab(inputId='DE_species_list', target='Dimensionality reduction')
    shiny::hideTab(inputId='DE_species_list', target='Hierarchical clustering')
    shiny::hideTab(inputId='DE_species_list', target='Characteristics association')
    shiny::hideTab(inputId='DE_specific_list', target='Dimensionality reduction')
    shiny::hideTab(inputId='DE_specific_list', target='Hierarchical clustering')
    shiny::hideTab(inputId='DE_specific_list', target='Two characteristics analysis')
    #### shinyjs show/hide results ####
    shinyjs::hide('DE_species_analysis_result_div')
    shinyjs::hide('DE_class_analysis_result_div')
    shinyjs::hide('DE_class_split_result_div')

    if(ncol(variables$DE.processed.abundance)<=4){
      shinyjs::disable('DE_stat_method')
      shinyjs::disable('DE_adj_stat_method')
      shinyjs::disable('DE_sig_p')
      shinyjs::disable('DE_pval')
      shinyjs::disable('DE_class_stat_method')
      shinyjs::disable('DE_class_post_hoc_method')
      shinyjs::disable('DE_class_pval')
    }
    if(variables$DE.Ngroup == 'two'){
      shiny::updateSelectInput(session, "DE_stat_method",
                        choices=c('t-test','Wilcoxon test'),
                        selected='t-test')
      shiny::updateSelectInput(session, "DE_class_post_hoc_method",
                        choices=c('t-test', 'Wilcoxon test'),
                        selected='t-test')
      shiny::updateSelectInput(session, "DE_class_twoCharHeatmap_post_hoc_method",
                        choices=c('t-test', 'Wilcoxon test'),
                        selected='t-test')
      shinyjs::show('DE_fc_div')
      shinyjs::hide('DE_postHoc_div')
      shinyjs::show('DE_class_fc_div')
      shinyjs::show('DE_class_twoCharHeatmap_fc_div')
      shinyjs::hide('DE_class_twoCharHeatmap_postHoc_div')
      shinyjs::show('DE_species_lipidchar_bar_div')
    }else{
      shiny::updateSelectInput(session, "DE_stat_method",
                        choices=c('One-way ANOVA', 'Kruskal-Wallis test'),
                        selected='One-way ANOVA')
      shiny::updateSelectInput(session, "DE_class_post_hoc_method",
                        choices=c('One-way ANOVA', 'Kruskal-Wallis test'),
                        selected='One-way ANOVA')
      shiny::updateSelectInput(session, "DE_class_twoCharHeatmap_post_hoc_method",
                        choices=c('One-way ANOVA', 'Kruskal-Wallis test'),
                        selected='One-way ANOVA')
      shinyjs::hide('DE_fc_div')
      shinyjs::show('DE_postHoc_div')
      shinyjs::hide('DE_class_fc_div')
      shinyjs::hide('DE_class_twoCharHeatmap_fc_div')
      shinyjs::show('DE_class_twoCharHeatmap_postHoc_div')
      shinyjs::hide('DE_species_lipidchar_bar_div')
    }
  })
}) #shiny::observeEvent(input$DE_user_start

##########################################
##########################################
#####  Tab1: Lipid species analysis  #####
##########################################
##########################################

###############################################
####  Lipid species analysis: DE analysis  ####
###############################################

shiny::observeEvent(input$DE_species_list,{
  shiny::isolate({
    if(input$DE_species_list == "Differential expression"){
      shinyjs::show('DE_species_analysis_result_div')
      shinyjs::hide('DE_species_dim_redu_result_div')
      shinyjs::hide('DE_species_heatmap_result_div')
      shinyjs::hide('DE_species_lipid_char_association_result_div')
    }else if(input$DE_species_list == "Result download"){
      shinyjs::hide('DE_species_analysis_result_div')
      shinyjs::hide('DE_species_dim_redu_result_div')
      shinyjs::hide('DE_species_heatmap_result_div')
      shinyjs::hide('DE_species_lipid_char_association_result_div')
    }else if(input$DE_species_list == "Dimensionality reduction"){
      shinyjs::hide('DE_species_analysis_result_div')
      shinyjs::show('DE_species_dim_redu_result_div')
      shinyjs::hide('DE_species_heatmap_result_div')
      shinyjs::hide('DE_species_lipid_char_association_result_div')
    }else if(input$DE_species_list == "Hierarchical clustering"){
      shinyjs::hide('DE_species_analysis_result_div')
      shinyjs::hide('DE_species_dim_redu_result_div')
      shinyjs::show('DE_species_heatmap_result_div')
      shinyjs::hide('DE_species_lipid_char_association_result_div')
    }else if(input$DE_species_list == "Characteristics association"){
      shinyjs::hide('DE_species_analysis_result_div')
      shinyjs::hide('DE_species_dim_redu_result_div')
      shinyjs::hide('DE_species_heatmap_result_div')
      if(variables$DE.species.char.association.check$logic){
        shinyjs::hide('DE_species_lipid_char_association_result_div')
        shiny:::showModal(shiny::modalDialog(
          title="Important message",
          variables$DE.species.char.association.check$message,
          easyClose=TRUE))
      }else{
        shinyjs::show('DE_species_lipid_char_association_result_div')
      }
    }
  })
})

#######################
#### Control panel ####
#######################

#### control reset button ####
shiny::observeEvent(input$DE_analysis_reset, {
  shiny::isolate({
    #### shinyjs show/hide results ####
    shinyjs::hide('DE_species_analysis_result_div')
    shinyjs::hide('DE_species_dim_redu_result_div')
    #### shinyjs reset control panel ####
    shinyjs::reset("DE_analysis_reset_div")
    #### shinyjs show/hide tabpanel ####
    shiny::hideTab(inputId='DE_species_list', target='Result download')
    shiny::hideTab(inputId='DE_species_list', target='Dimensionality reduction')
    shiny::hideTab(inputId='DE_species_list', target='Hierarchical clustering')
    shiny::hideTab(inputId='DE_species_list', target='Characteristics association')
  })
}) #shiny::observeEvent(input$DE_analysis_reset

#### control observeEvent ####
shiny::observeEvent(input$DE_stat_method,{
  shiny::isolate({
    if(input$DE_stat_method == 'One-way ANOVA'){
      updateRadioButtons(session, "DE_postHoc_method",
                         choices="Tukey's HSD",
                         selected="Tukey's HSD")
    }else if(input$DE_stat_method == 'Kruskal-Wallis test'){
      updateRadioButtons(session, "DE_postHoc_method",
                         choices="Dunn's Test",
                         selected="Dunn's Test")
    }
  })
}) #shiny::observeEvent(input$DE_stat_method

#### control start button ####
shiny::observeEvent(input$DE_analysis_start, {
  shiny::isolate({
    tryCatch(
      {
        #### shiny show/hide tab ####
        shiny::showTab(inputId='DE_species_list', target='Result download')
        shiny::showTab(inputId='DE_species_list', target='Dimensionality reduction')
        shiny::showTab(inputId='DE_species_list', target='Hierarchical clustering')
        shiny::showTab(inputId='DE_species_list', target='Characteristics association')
        #### shinyjs show/hide results ####
        shinyjs::show('DE_species_analysis_result_div')
        shinyjs::hide('DE_species_dim_redu_result_div')
        variables$DE.species.dotchart.download.log <- 1
        variables$DE.species.maplot.download.log <- 1
        variables$DE.species.boxplot.download.log <- 1
        ### Check parameter ###
        if(is.numeric(input$DE_pval)){
          p_cutoff <- input$DE_pval
          if(input$DE_pval > 1){
            p_cutoff <- 1
            shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='DE_pval', value=1)
          }else if(input$DE_pval < 0.001){
            p_cutoff <- 0.001
            shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
            shiny::updateNumericInput(inputId='DE_pval', value=0.001)
          }
        }else{
          p_cutoff <- 0.05
          shiny::showNotification("The p-value must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
          shiny::updateNumericInput(inputId='DE_pval', value=0.05)
        }
        if(input$DE_transformation == 'none' & input$DE_stat_method %in% c('Wilcoxon test', 'Kruskal-Wallis test')){
          shinyWidgets::sendSweetAlert(
            session=session, title="Suggestion!",
            text="For parametric test, it is recommended that lipidomics data be transformed.",
            type="warning")
        }
        if(variables$DE.Ngroup == 'two'){
          shinyjs::show('DE_species_volcano_div')
          shinyjs::enable('DE.species.network.download.start')
          shinyjs::enable('DE.species.network.activity.url')
          shinyjs::enable('DE.species.network.reaction.url')
          shinyjs::enable('DE.species.network.gatom.url')
          if(is.numeric(input$DE_fc)){
            FC_cutoff <- input$DE_fc
            if(input$DE_fc > 8){
              FC_cutoff <- 8
              shiny::showNotification("The Fold change (FC) must be between 1 and 8, so it is calculated by replacing it with 8.", type="warning")
              shiny::updateNumericInput(inputId='DE_fc', value=8)
            }else if(input$DE_fc < 1){
              FC_cutoff <- 1
              shiny::showNotification("The Fold change (FC) must be between 1 and 8, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='DE_fc', value=1)
            }
          }else{
            FC_cutoff <- 1
            shiny::showNotification("The Fold change (FC) must be numeric, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='DE_fc',value=1)
          }
          variables$deSp.se <- LipidSigR::deSp_twoGroup(
            processed_se=variables$DE.processed.SE,
            ref_group=variables$DE.ref.group,
            test=input$DE_stat_method,
            significant=input$DE_sig_p, p_cutoff=p_cutoff, FC_cutoff=FC_cutoff, transform=input$DE_transformation)
          variables$DE.species.enrichment.download.log <- 1
          variables$DE.species.network.download.log <- 1
          variables$deSp.plot <- LipidSigR::plot_deSp_twoGroup(variables$deSp.se)
          variables$DE.species.tab.all <- S4Vectors::metadata(variables$deSp.se)$all_deSp_result %>%
            dplyr::select(feature,dplyr::starts_with('mean'), method, FC, log2FC, statistic, pval, negLog10pval,padj, negLog10padj, sig_pval, sig_padj)
          if(input$DE_sig_p == "pval"){
            variables$DE.species.tab.all <- variables$DE.species.tab.all %>%
              dplyr::select(sig_pval, dplyr::everything()) %>%
              dplyr::arrange(dplyr::desc(sig_pval))
            colnames(variables$DE.species.tab.all) <- c('Significance(p-value)', 'Feature', 'Mean(ctrl)', 'Mean(exp)', 'Method', 'FC', 'Log2(FC)', 'Statistic', 'p-value', '-Log10(p-value)','padj', '-Log10(padj)', 'Significance(padj)')
          }else{
            variables$DE.species.tab.all <- variables$DE.species.tab.all %>%
              dplyr::select(sig_padj, dplyr::everything()) %>%
              dplyr::arrange(dplyr::desc(sig_padj))
            colnames(variables$DE.species.tab.all) <- c('Significance(padj)', 'Feature', 'Mean(ctrl)', 'Mean(exp)', 'Method', 'FC', 'Log2(FC)', 'Statistic', 'p-value', '-Log10(p-value)','padj', '-Log10(padj)', 'Significance(p-value)')
          }
          variables$DE.species.tab.all <- variables$DE.species.tab.all %>%
            dplyr::mutate(`Mean(ctrl)`=format(`Mean(ctrl)`, digits=2,scientific=TRUE),
                          `Mean(exp)`=format(`Mean(exp)`, digits=2,scientific=TRUE),
                          FC=round(FC,3),
                          `Log2(FC)`=round(`Log2(FC)`,3),
                          Statistic=round(Statistic,2),
                          `p-value`=format(`p-value`, digits=2,scientific=TRUE),
                          `-Log10(p-value)`=format(`-Log10(p-value)`, digits=2,scientific=TRUE),
                          `padj`=format(`padj`, digits=2,scientific=TRUE),
                          `-Log10(padj)`=format(`-Log10(padj)`, digits=2,scientific=TRUE))
          #### Output: DE.species.maplot ####
          output$DE.species.maplot <- plotly::renderPlotly({
            shiny::isolate({
              plotly::plot_ly(
                data=variables$deSp.plot$table_ma_volcano,
                x=~ as.numeric(A), y=~ as.numeric(M),
                type="scatter", mode="markers", color=~ sig_fc.pval_color,
                colors=c("#4169E1", "#DDDDDD", "#FF4500"), showlegend=TRUE,
                marker=list(size=4), hoverinfo="text",
                key= ~variables$deSp.plot$table_ma_volcano$feature, source="DE.species.MA",
                hovertext=~ paste(
                  "</br>Lipid:", feature, "</br>A value:",
                  round(as.numeric(A), 4), "</br>M value:",
                  round(as.numeric(M), 4),"</br>-log10(p-value):",
                  round(-log10(as.numeric(pval)), 4),"</br>-log10(padj):",
                  round(-log10(as.numeric(padj)), 4))
              ) %>%
                plotly::layout(
                  xaxis=list(title="A=(log<sub>2</sub>(exp)+log<sub>2</sub>(ctrl))/2"),
                  yaxis=list(title="M=log<sub>2</sub>(exp)-log<sub>2</sub>(ctrl)"),
                  title="MA Plot",
                  legend=list(
                    title=list(text="log2FC Significant"), orientation='h',
                    xanchor="center", x=0.5, y=-0.18))
            }) #isolate
          }) #output$DE.species.maplot <- renderPlotly

          #### Output: DE.species.ma.box ####
          output$DE.species.ma.box <- plotly::renderPlotly({
            eventdata <- plotly::event_data("plotly_hover", source="DE.species.MA")
            shiny::validate(shiny::need(!is.null(eventdata), "Hover over the point to show lipid's expression level of interest."))
            # Get expression level (Original)
            variables$DE.processed.SE.list$abundance[which(variables$DE.processed.SE.list$abundance$feature == eventdata$key), ] %>%
              tidyr::gather(sample_name, value, -1) %>%
              dplyr::left_join(variables$DE.group.info, by='sample_name') %>%
              plotly::plot_ly(x=~ group,y=~ value,
                              color=~ group,
                              showlegend=FALSE, type="box",
                              boxpoints='all', jitter=0.85,
                              pointpos=0, marker=list(size=5, opacity=0.8)) %>%
              plotly::layout(yaxis=list(title="Abundance"),
                             title=eventdata$key)
          }) #output$DE.species.ma.box <- renderPlotly

          #### Output: DE.species.volcano ####
          output$DE.species.volcano <- plotly::renderPlotly({
            shiny::validate(shiny::need(sum(!is.na(variables$deSp.plot$table_ma_volcano$negLogP))==nrow(variables$deSp.plot$table_ma_volcano), "Without volcano plot"))
            significant_text <- ifelse(input$DE_sig_p == 'pval','P-value','padj')
            plotly::plot_ly(
              data=variables$deSp.plot$table_ma_volcano, x=~as.numeric(M), y=~as.numeric(negLogP),
              type="scatter", mode="markers", color=~sig_fc.pval_color,
              colors=c("#4169E1", "#DDDDDD", "#FF4500"), marker=list(size=4),
              key= ~variables$deSp.plot$table_ma_volcano$feature, source="DE.species.vol",
              hoverinfo="text", text=~ paste(
                "</br>Lipid:", feature, "</br>A value:",
                round(as.numeric(A), 4), "</br>M value:",
                round(as.numeric(M), 4), "</br>-log10(p-value):",
                round(-log10(pval), 4), "</br>-log10(padj):",
                round(-log10(padj), 4))) %>%
              plotly::layout(
                xaxis=list(
                  title="M=log<sub>2</sub>(exp)-log<sub>2</sub>(ctrl)"),
                yaxis=list(title=paste0("-log<sub>10</sub>(", significant_text, ")")),
                title="Volcano Plot", legend=list(
                  title=list(text="Significant lipid"), orientation='h',
                  xanchor="center", x=0.5, y=-0.18))
          }) #output$DE.species.volcano <- renderPlotly

          #### Output: DE.species.vol.box ####
          output$DE.species.vol.box <- plotly::renderPlotly({

            eventdata <- plotly::event_data("plotly_hover", source="DE.species.vol")
            shiny::validate(shiny::need(!is.null(eventdata), "Hover over the point to show lipid's expression level of interest."))
            # Get point number
            variables$DE.processed.SE.list$abundance[which(variables$DE.processed.SE.list$abundance$feature == eventdata$key), ] %>%
              tidyr::gather(sample_name, value, -1) %>%
              dplyr::left_join(variables$DE.group.info, by='sample_name') %>%
              plotly::plot_ly(x=~ group,y=~ value,
                              color=~ group,
                              showlegend=FALSE, type="box",
                              boxpoints='all', jitter=0.85,
                              pointpos=0, marker=list(size=5, opacity=0.8)) %>%
              plotly::layout(yaxis=list(title="Abundance"),
                             title=eventdata$key)
          }) #output$DE.species.vol.box <- renderPlotly
        }else{
          shinyjs::hide('DE_species_volcano_div')
          shinyjs::disable('DE.species.network.download.start')
          shinyjs::disable('DE.species.network.activity.url')
          shinyjs::disable('DE.species.network.reaction.url')
          shinyjs::disable('DE.species.network.gatom.url')
          variables$deSp.se <- LipidSigR::deSp_multiGroup(
            processed_se=variables$DE.processed.SE,
            ref_group=variables$DE.ref.group, test=input$DE_stat_method,
            significant=input$DE_sig_p, p_cutoff=p_cutoff, transform=input$DE_transformation)
          variables$DE.species.enrichment.download.log <- 1
          variables$deSp.plot <- LipidSigR::plot_deSp_multiGroup(variables$deSp.se)
          variables$DE.species.tab.all <- S4Vectors::metadata(variables$deSp.se)$all_deSp_result   %>%
            dplyr:: arrange(dplyr::desc(sig_pval))

          #### Output: DE.species.maplot ####
          output$DE.species.maplot <- plotly::renderPlotly({
            shiny::isolate({
              if(input$DE_sig_p == "pval"){
                plot_data <- variables$deSp.plot$table_dotPlot %>%
                  dplyr::rename(value=negLog10pval)
                title="-log10(pval)"
              }else{
                plot_data <- variables$deSp.plot$table_dotPlot %>%
                  dplyr::rename(value=negLog10padj)
                title="-log10(padj)"
              }
              plotly::plot_ly(
                data=plot_data,
                x=~ index, y=~ value,
                type="scatter", mode="markers", color=~ class,
                colors=unique(c(RColorBrewer::brewer.pal(9, "Set1"),
                                RColorBrewer::brewer.pal(8, "Dark2"),
                                RColorBrewer::brewer.pal(8, "Set2"),
                                RColorBrewer::brewer.pal(8, "Accent"),
                                RColorBrewer::brewer.pal(12, "Set3"),
                                RColorBrewer::brewer.pal(9, "Pastel1"),
                                RColorBrewer::brewer.pal(8, "Pastel2"))),
                marker=list(opacity=~alpha), hoverinfo="text",
                key= ~variables$deSp.plot$table_dotPlot$feature, source="DE.species.MA",
                hovertext=~ hover) %>%
                plotly::layout(
                  xaxis=list(title="Lipid"),
                  yaxis=list(title=title),
                  legend=list(
                    title=list(text="class"), orientation='h',
                    xanchor="center", x=0.5, y=-0.18))
            }) #isolate
          }) #output$DE.species.maplot <- renderPlotly

          #### Output: DE.species.ma.box ####
          output$DE.species.ma.box <- plotly::renderPlotly({
            eventdata <- plotly::event_data("plotly_hover", source="DE.species.MA")
            shiny::validate(shiny::need(!is.null(eventdata), "Hover over the point to show lipid's expression level of interest."))
            # Get expression level (Original)
            variables$DE.processed.SE.list$abundance[which(variables$DE.processed.SE.list$abundance$feature == eventdata$key), ] %>%
              tidyr::gather(sample_name, value, -1) %>%
              dplyr::left_join(variables$DE.group.info, by='sample_name') %>%
              plotly::plot_ly(x=~ group,y=~ value,
                              color=~ group,
                              showlegend=FALSE, type="box",
                              boxpoints='all', jitter=0.85,
                              pointpos=0, marker=list(size=5, opacity=0.8)) %>%
              plotly::layout(yaxis=list(title="Abundance"),
                             title=eventdata$key)
          }) #output$DE.species.ma.box <- renderPlotly
        }
        if(!is.null(S4Vectors::metadata(variables$deSp.se)$sig_deSp_result)){
          if(!is.character(S4Vectors::metadata(variables$deSp.se)$sig_deSp_result)){
            update <- S4Vectors::metadata(variables$deSp.se)$sig_deSp_result %>% dplyr::pull('feature')
            shiny::updateSelectInput(session, "DE_species_boxplot_download_lipid",
                                     choices=update,
                                     selected=update[1])
            VALUE <- ifelse(length(update) < 10, length(update), 10)
            shiny::updateSliderInput(session,
                                     inputId='DE_species_pca_variable_topN',
                                     value=VALUE,
                                     max=length(update))
            variables$DE.species.sig.check <- submit.check(abundance=variables$DE.processed.abundance,
                                                           sig_result=S4Vectors::metadata(variables$deSp.se)$sig_deSp_result,
                                                           check.NA=TRUE, Nfeature=2, Nsig=2)
            variables$DE.species.char.association.check <- submit.check(abundance=NULL,
                                                                        sig_result=S4Vectors::metadata(variables$deSp.se)$sig_deSp_result,
                                                                        check.NA=FALSE, Nfeature=NULL, Nsig=1)
          }else{
            variables$DE.species.sig.check$logic <- TRUE
            variables$DE.species.sig.check$message <- "No significant lipids."
            variables$DE.species.char.association.check$logic <- TRUE
            variables$DE.species.char.association.check$message <- "No significant lipids."
          }
        }
        variables$DE.species.hclustering.all.check <- submit.check(abundance=variables$DE.processed.abundance,
                                                                   sig_result=NULL,
                                                                   check.NA=TRUE, Nfeature=2, Nsig=NULL)

        variables$DE.species.dotchart.download.log <- 1
        variables$DE.species.maplot.download.log <- 1
        variables$DE.species.boxplot.download.log <- 1
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species differential expression error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) #shiny::observeEvent(input$DE_analysis_start

shiny::observeEvent(input$DE.species.enrichment.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.species.enrichment.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.species.enrichment.download <- shiny::downloadHandler(
            filename=function(){
              paste("Enrichment.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              dir.create(file.path(temp_directory,'analysis_results'))
              dir.create(file.path(temp_directory,'web_assets'))
              ## table  ##
              write.csv(S4Vectors::metadata(variables$deSp.se)$all_deSp_result, file=file.path(temp_directory,'analysis_results/DE.all.deSp.result.csv'))
              write.csv(S4Vectors::metadata(variables$deSp.se)$sig_deSp_result, file=file.path(temp_directory,'analysis_results/DE.sig.deSp.result.csv'))
              readr::write_lines('Differential expression analysis condition', file.path(temp_directory,"analysis_results/condition.txt"), append=TRUE)
              readr::write_lines(paste0('Significant criteria: ',
                                        ifelse(S4Vectors::metadata(variables$deSp.se)$significant=="pval",'P-value','Adjusted p-value'),' < ',
                                        S4Vectors::metadata(variables$deSp.se)$p_cutoff, ' & Fold change (FC) > ',
                                        S4Vectors::metadata(variables$deSp.se)$FC_cutoff), file.path(temp_directory,"analysis_results/condition.txt"), append=TRUE)
              saveRDS(variables$deSp.se,file=file.path(temp_directory,'web_assets/deSp.se.rds'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.species.enrichment.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.species.enrichment.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.species.enrichment.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species differential expression download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE.species.enrichment.url,{
  shiny::isolate({
    tryCatch(
      {
        ## Show "Preload DE results" in the radio so the source is unambiguous
        shiny::updateRadioButtons(session, inputId='Enrichment_source',
          choices=c('Example dataset(two group)'='Enrichment_demo_data',
                    'Example dataset(Multi-groups)'='Enrichment_demo_Multi_data',
                    'Upload your data!'='Enrichment_user_data',
                    'Preload DE results'='DE'),
          selected='DE')
        shinyjs::show('Enrichment_progress_div')
        shinyjs::click('Enrichment_url_start')
        shinyjs::runjs("switchTab('Enrichment')")
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Start Enrichment analysis error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE.species.network.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.species.network.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.species.network.download <- shiny::downloadHandler(
            filename=function(){
              paste("Network.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              dir.create(file.path(temp_directory,'analysis_results'))
              dir.create(file.path(temp_directory,'web_assets'))
              ## table  ##
              write.csv(S4Vectors::metadata(variables$deSp.se)$all_deSp_result, file=file.path(temp_directory,'analysis_results/DE.all.deSp.result.csv'))
              write.csv(S4Vectors::metadata(variables$deSp.se)$sig_deSp_result, file=file.path(temp_directory,'analysis_results/DE.sig.deSp.result.csv'))
              readr::write_lines('Differential expression analysis condition', file.path(temp_directory,"analysis_results/condition.txt"), append=TRUE)
              readr::write_lines(paste0('Significant criteria: ',
                                        ifelse(S4Vectors::metadata(variables$deSp.se)$significant=="pval",'P-value','Adjusted p-value'),' < ',
                                        S4Vectors::metadata(variables$deSp.se)$p_cutoff, ' & Fold change (FC) > ',
                                        S4Vectors::metadata(variables$deSp.se)$FC_cutoff), file.path(temp_directory,"analysis_results/condition.txt"), append=TRUE)
              saveRDS(variables$deSp.se,file=file.path(temp_directory,'web_assets/deSp.se.rds'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.species.network.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.species.network.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.species.network.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species differential expression download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE.species.network.activity.url,{
  shiny::isolate({
    tryCatch(
      {
        ## Integrated app: pass deSp.se directly — no disk I/O required
        variables$NET.pathwayActivity.deSp.se <- variables$deSp.se
        ## Show "Preload DE results" so the data source is unambiguous
        shiny::updateRadioButtons(session, inputId='NET_pathwayActivity_source',
          choices=c('Example dataset'='NET_demo_data',
                    'Upload your data!'='NET_user_data',
                    'Preload DE results'='DE'),
          selected='DE')
        shinyjs::click('NET_pathwayActivity_url_start')
        shiny::updateNavlistPanel(session, 'NET_analysis_tab', "Pathway activity network")
        shinyjs::runjs("switchTab('Network')")
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Start Network analysis error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE.species.network.reaction.url,{
  shiny::isolate({
    tryCatch(
      {
        ## Integrated app: pass deSp.se directly — no disk I/O required
        variables$NET.lipidReaction.deSp.se <- variables$deSp.se
        ## Show "Preload DE results" so the data source is unambiguous
        shiny::updateRadioButtons(session, inputId='NET_lipidReaction_source',
          choices=c('Example dataset'='NET_demo_data',
                    'Upload your data!'='NET_user_data',
                    'Preload DE results'='DE'),
          selected='DE')
        shinyjs::click('NET_lipidReaction_url_start')
        shiny::updateNavlistPanel(session, 'NET_analysis_tab', "Lipid reaction network")
        shinyjs::runjs("switchTab('Network')")
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Start Network analysis error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE.species.network.gatom.url,{
  shiny::isolate({
    tryCatch(
      {
        ## Integrated app: pass deSp.se directly — no disk I/O required
        variables$NET.gatom.deSp.se <- variables$deSp.se
        ## Show "Preload DE results" so the data source is unambiguous
        shiny::updateRadioButtons(session, inputId='NET_gatom_source',
          choices=c('Example dataset'='NET_demo_data',
                    'Upload your data!'='NET_user_data',
                    'Preload DE results'='DE'),
          selected='DE')
        shinyjs::click('NET_gatom_url_start')
        shiny::updateNavlistPanel(session, 'NET_analysis_tab', "GATOM network")
        shinyjs::runjs("switchTab('Network')")
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Start Network analysis error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

#### Output: DE.species.tab.all ####
output$DE.species.tab.all <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.species.tab.all), "Without DE result"))
  DT::datatable(variables$DE.species.tab.all,
                escape=FALSE, selection='none', rownames=TRUE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all")))) %>%
    DT::formatStyle(1, target='row',backgroundColor=DT::styleEqual(c("yes","no"), c('pink', '#EBECF0')))
}) #output$DE.species.tab.all <- renderDataTable

#### Output: DE.species.dotchart.sig ####
output$DE.species.dotchart.sig <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$deSp.plot$interactive_de_lipid), "Plot not showing. Missing value imputation is recommended."))
  variables$deSp.plot$interactive_de_lipid
}) #output$DE.species.dotchart.sig <- renderPlotly

shiny::observeEvent(input$DE_species_boxplot_download_lipid, {
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$DE.processed.SE)){
          if(variables$DE.Ngroup == 'two'){
            variables$DE.species.lipid.boxplot <- LipidSigR::boxPlot_feature_twoGroup(
              processed_se=variables$DE.processed.SE,
              feature=input$DE_species_boxplot_download_lipid, ref_group=variables$DE.ref.group,
              test=input$DE_stat_method,
              transform=input$DE_transformation)
            variables$DE.species.boxplot.download.log <- 1
          }else{
            variables$DE.species.lipid.boxplot <- LipidSigR::boxPlot_feature_multiGroup(
              processed_se=variables$DE.processed.SE,
              feature=input$DE_species_boxplot_download_lipid, ref_group=variables$DE.ref.group,
              test=input$DE_stat_method,
              post_hoc_sig=input$DE_sig_p,
              transform=input$DE_transformation)
          }
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species differential expression boxplot error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

#### Output: DE.species.maplot ####
output$DE.species.boxplot.download.plot <- shiny::renderPlot({
  shiny::validate(shiny::need(!is.null(variables$DE.species.lipid.boxplot$static_boxPlot), "Without DE dotplot"))
  variables$DE.species.lipid.boxplot$static_boxPlot
}) #output$DE_species_boxplot_download_lipid <- renderPlot

#### Output: DE.species.boxplot.table ####
output$DE.species.boxplot.table <- DT::renderDataTable({
  shiny::validate(shiny::need(!is.null(variables$DE.species.lipid.boxplot$table_stat), ""))
  DT::datatable(variables$DE.species.lipid.boxplot$table_stat,
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$DE.species.boxplot.table <- renderPlot

shiny::observeEvent(input$DE.species.dotchart.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.species.dotchart.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.species.dotchart.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.species.dotchart.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## Plot ##
              grDevices::pdf(file.path(temp_directory,'DE.species.dotchart.pdf'), width=8, height=6)
              print(variables$deSp.plot$static_de_lipid)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$deSp.plot$table_de_lipid, file=file.path(temp_directory,'DE.species.dotchart.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.species.dotchart.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.species.dotchart.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.species.dotchart.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species differential expression dotchart download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE.species.maplot.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.species.maplot.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.species.maplot.download <- shiny::downloadHandler(
            filename=function(){
              if(variables$DE.Ngroup == 'two') "DE.species.maplot.volcano.zip"
              else "DE.species.dotplot.zip"
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              if(variables$DE.Ngroup == 'two'){
                ## plot ##
                grDevices::pdf(file.path(temp_directory,'DE.species.maplot.pdf'), width=8, height=6)
                print(variables$deSp.plot$static_maPlot)
                grDevices::dev.off()
                grDevices::pdf(file.path(temp_directory,'DE.species.volcano.plot.pdf'), width=8, height=6)
                print(variables$deSp.plot$static_volcanoPlot)
                grDevices::dev.off()
                ## table ##
                write.csv(variables$deSp.plot$table_ma_volcano, file=file.path(temp_directory,'DE.species.ma.volcano.csv'))
              }else{
                ## plot ##
                grDevices::pdf(file.path(temp_directory,'DE.species.dotplot.pdf'), width=8, height=6)
                print(variables$deSp.plot$static_dotPlot)
                grDevices::dev.off()
                ## table ##
                write.csv(variables$deSp.plot$table_dotPlot, file=file.path(temp_directory,'DE.species.dotplot.csv'))
              }
              ## zip all files ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )

          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.species.maplot.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.species.maplot.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.species.maplot.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species differential expression MA plot download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE.species.boxplot.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.species.boxplot.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=33)

          output$DE.species.boxplot.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.species.boxplot.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## Histogram of expressed lipid numbers ##
              grDevices::pdf(file.path(temp_directory,'DE.species.boxplot.pdf'), width=8, height=6)
              print(variables$DE.species.lipid.boxplot$static_boxPlot)
              grDevices::dev.off()
              ## table  ##
              utils::write.csv(variables$DE.species.lipid.boxplot$table_boxplot, file=file.path(temp_directory,'DE.species.boxplot.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",
            display_pct=TRUE, value=66)
          variables$DE.species.boxplot.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.species.boxplot.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.species.boxplot.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress",
            title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species differential expression boxplot download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

############################################################
####  lipid species analysis: Dimensionality reduction  ####
############################################################

#######################
#### Control panel ####
#######################

#### Group assignment ####
#### number of samples ####
output$DE.species.group.count <- shiny::renderText({
  paste0(length(unique(variables$DE.group.info$group)), ' groups were detected in samples.')
})

#### dbscan ####
#### update numeric input minPts ####
shiny::observe({
  shiny::updateNumericInput(session,
                            inputId='DE_species_dbscan_minPts',
                            max=(nrow(variables$DE.group.info)-1)
  ) #updateNumericInput
})

#### control reset button ####
shiny::observeEvent(input$DE_species_dim_redu_reset,{
  shiny::isolate({
    #### shinyjs show/hide results ####
    shinyjs::hide('DE_species_dim_redu_result_div')

    #### shinyjs reset control panel ####
    shinyjs::reset("DE_species_dim_redu_reset_div")
  })
}) #shiny::observeEvent(input$DE_species_dim_redu_reset

#### control start button ####
shiny::observeEvent(input$DE_species_dim_redu_start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.species.sig.check$logic){
          shinyjs::hide('DE_species_dim_redu_result_div')
          shiny:::showModal(shiny::modalDialog(
            title="Important message",
            variables$DE.species.sig.check$message,
            easyClose=TRUE))
        }else{
          ## cluster method parameter ##
          cluster_num <- switch(input$DE_species_cluster_method,
                                group_info=length(variables$DE.all.group.name),
                                kmeans=input$DE_species_kmeans_group,
                                kmedoids=input$DE_species_pam_group,
                                hclustering=input$DE_species_hclust_group,
                                dbscan=NULL)
          kmedoids_metric <- if(input$DE_species_cluster_method == 'kmedoids'){ input$DE_species_pam_metric }else{ NULL }
          distfun <- if(input$DE_species_cluster_method == 'hclustering'){ input$DE_species_hclust_dist }else{ NULL }
          hclustfun <- if(input$DE_species_cluster_method == 'hclustering'){ input$DE_species_hclust_hclust }else{ NULL }
          eps <- if(input$DE_species_cluster_method == 'dbscan'){ input$DE_species_dbscan_eps }else{ NULL }
          minPts <- if(input$DE_species_cluster_method == 'dbscan'){ input$DE_species_dbscan_minPts }else{ NULL }
          n_PC <- if(input$DE_species_pca_contrib_PC == '1_2'){ c(1, 2) }else{ as.numeric(input$DE_species_pca_contrib_PC) }
          if(input$DE_species_cluster_method == 'dbscan'){
            if(is.numeric(eps)){
              if(eps < 0.1){
                eps <- 0.1
                shiny::showNotification("The epsilon must be greater than 0.1, so it is calculated by replacing it with 0.1.", type="warning")
                shiny::updateNumericInput(inputId='DE_species_dbscan_eps', value=0.1)
              }
            }else{
              eps <- 0.5
              shiny::showNotification("The epsilon must be numeric, so it is calculated by replacing it with 0.5.", type="warning")
              shiny::updateNumericInput(inputId='DE_species_dbscan_eps', value=0.5)
            }
            if(is.numeric(minPts)){
              if(minPts > 22){
                minPts <- 22
                shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 22.", type="warning")
                shiny::updateNumericInput(inputId='DE_species_dbscan_minPts', value=22)
              }else if(minPts < 1){
                minPts <- 1
                shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='DE_species_dbscan_minPts', value=1)
              }
            }else{
              minPts <- 1
              shiny::showNotification("The minPts must be numeric, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='DE_species_dbscan_minPts',value=1)
            }
          }
          shinyjs::show('DE_species_dim_redu_result_div')
          if(input$DE_species_dim_redu_method == 'pca'){
            shinyjs::hide('DE_species_dim_redu_plsda_result_div')
            shinyjs::hide('DE_species_dim_redu_tsne_result_div')
            shinyjs::hide('DE_species_dim_redu_umap_result_div')
            variables$DE.species.dim.redu.pca.download.log <- 1
            variables$DE.species.dim.redu.pca.topN.download.log <- 1
            variables$DE.species.pca.result <- LipidSigR::dr_pca(
              processed_se=variables$deSp.se, scaling=TRUE,
              centering=TRUE, clustering=input$DE_species_cluster_method,
              cluster_num=cluster_num, kmedoids_metric=kmedoids_metric, distfun=distfun,
              hclustfun=hclustfun, eps=eps, minPts=minPts, feature_contrib_pc=n_PC, plot_topN=input$DE_species_pca_variable_topN)
            if(!is.null(variables$DE.species.pca.result)){
              VALUE <- ifelse(nrow(variables$DE.species.pca.result$table_pca_contribution) < 10, nrow(variables$DE.species.pca.result$table_pca_contribution), 10)
              shiny::updateSliderInput(session,
                                       inputId='DE_species_pca_variable_topN',
                                       value=VALUE,
                                       max=nrow(variables$DE.species.pca.result$table_pca_contribution))
              shinyjs::show('DE_species_dim_redu_pca_result_div')
            }
          }else if(input$DE_species_dim_redu_method == 'plsda'){
            variables$DE.species.dim.redu.plsda.download.log <- 1
            variables$DE.species.plsda.result <- LipidSigR::dr_plsda(
              de_se=variables$deSp.se, ncomp=2, scaling=TRUE,
              clustering=input$DE_species_cluster_method,
              cluster_num=cluster_num, kmedoids_metric=kmedoids_metric, distfun=distfun,
              hclustfun=hclustfun, eps=eps, minPts=minPts)
            shinyjs::hide('DE_species_dim_redu_pca_result_div')
            shinyjs::hide('DE_species_dim_redu_tsne_result_div')
            shinyjs::hide('DE_species_dim_redu_umap_result_div')
            shinyjs::show('DE_species_dim_redu_plsda_result_div')
          }else if(input$DE_species_dim_redu_method == 'tsne'){
            variables$DE.species.dim.redu.tsne.download.log <- 1
            if(is.numeric(input$DE_species_tsne_perplexity)){
              perplexity <- input$DE_species_tsne_perplexity
              if(input$DE_species_tsne_perplexity > 7){
                perplexity <- 7
                shiny::showNotification("The perplexity must be between 3 and 7, so it is calculated by replacing it with 7.", type="warning")
                shiny::updateNumericInput(inputId='DE_species_tsne_perplexity', value=7)
              }else if(input$DE_species_tsne_perplexity < 3){
                perplexity <- 3
                shiny::showNotification("The perplexity must be between 3 and 7, so it is calculated by replacing it with 3.", type="warning")
                shiny::updateNumericInput(inputId='DE_species_tsne_perplexity', value=3)
              }
            }else{
              perplexity <- 5
              shiny::showNotification("The perplexity must be numeric, so it is calculated by replacing it with 5.", type="warning")
              shiny::updateNumericInput(inputId='DE_species_tsne_perplexity', value=5)
            }
            if(is.numeric(input$DE_species_tsne_max_iter)){
              max_iter <- input$DE_species_tsne_max_iter
              if(input$DE_species_tsne_max_iter > 5000){
                max_iter <- 5000
                shiny::showNotification("The number of iterations must be between 100 and 5000, so it is calculated by replacing it with 5000.", type="warning")
                shiny::updateNumericInput(inputId='DE_species_tsne_max_iter', value=5000)
              }else if(input$DE_species_tsne_max_iter < 100){
                max_iter <- 100
                shiny::showNotification("The number of iterations must be between 100 and 5000, so it is calculated by replacing it with 100.", type="warning")
                shiny::updateNumericInput(inputId='DE_species_tsne_max_iter', value=100)
              }
            }else{
              max_iter <- 500
              shiny::showNotification("The number of iterations must be numeric, so it is calculated by replacing it with 500.", type="warning")
              shiny::updateNumericInput(inputId='DE_species_tsne_max_iter', value=500)
            }
            variables$DE.species.tsne.result <- LipidSigR::dr_tsne(
              processed_se=variables$deSp.se,
              pca=TRUE, perplexity=perplexity, max_iter=max_iter,
              clustering=input$DE_species_cluster_method, cluster_num=cluster_num,
              kmedoids_metric=kmedoids_metric, distfun=distfun,
              hclustfun=hclustfun, eps=eps, minPts=minPts)
            shinyjs::hide('DE_species_dim_redu_pca_result_div')
            shinyjs::show('DE_species_dim_redu_tsne_result_div')
            shinyjs::hide('DE_species_dim_redu_umap_result_div')
            shinyjs::hide('DE_species_dim_redu_plsda_result_div')
          }else if(input$DE_species_dim_redu_method == 'umap'){
            variables$DE.species.dim.redu.umap.download.log <- 1
            if(is.numeric(input$DE_species_umap_n_neighbors)){
              n_neighbors <- input$DE_species_umap_n_neighbors
              if(input$DE_species_umap_n_neighbors > 23){
                n_neighbors <- 23
                shiny::showNotification("The number of neighbors must be between 2 and 23, so it is calculated by replacing it with 23.", type="warning")
                shiny::updateNumericInput(inputId='DE_species_umap_n_neighbors', value=23)
              }else if(input$DE_species_umap_n_neighbors < 2){
                n_neighbors <- 2
                shiny::showNotification("The number of neighbors must be between 2 and 23, so it is calculated by replacing it with 2.", type="warning")
                shiny::updateNumericInput(inputId='DE_species_umap_n_neighbors', value=2)
              }
            }else{
              n_neighbors <- 15
              shiny::showNotification("The number of neighbors must be numeric, so it is calculated by replacing it with 15.", type="warning")
              shiny::updateNumericInput(inputId='DE_species_umap_n_neighbors', value=15)
            }
            variables$DE.species.umap.result <- LipidSigR::dr_umap(
              processed_se=variables$deSp.se,
              n_neighbors=n_neighbors, scaling=TRUE,
              umap_metric=input$DE_species_umap_metric,
              clustering=input$DE_species_cluster_method, cluster_num=cluster_num,
              kmedoids_metric=kmedoids_metric, distfun=distfun,
              hclustfun=hclustfun, eps=eps, minPts=minPts)
            shinyjs::hide('DE_species_dim_redu_pca_result_div')
            shinyjs::hide('DE_species_dim_redu_tsne_result_div')
            shinyjs::show('DE_species_dim_redu_umap_result_div')
            shinyjs::hide('DE_species_dim_redu_plsda_result_div')
          }
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species dimensionality reduction error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE_species_pca_variable_topN, {
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$DE.species.pca.result)){
          ## cluster method parameter ##
          cluster_num <- switch(input$DE_species_cluster_method,
                                group_info=length(variables$DE.all.group.name),
                                kmeans=input$DE_species_kmeans_group,
                                kmedoids=input$DE_species_pam_group,
                                hclustering=input$DE_species_hclust_group,
                                dbscan=NULL)
          kmedoids_metric <- if(input$DE_species_cluster_method == 'kmedoids'){ input$DE_species_pam_metric }else{ NULL }
          distfun <- if(input$DE_species_cluster_method == 'hclustering'){ input$DE_species_hclust_dist }else{ NULL }
          hclustfun <- if(input$DE_species_cluster_method == 'hclustering'){ input$DE_species_hclust_hclust }else{ NULL }
          eps <- if(input$DE_species_cluster_method == 'dbscan'){ input$DE_species_dbscan_eps }else{ NULL }
          minPts <- if(input$DE_species_cluster_method == 'dbscan'){ input$DE_species_dbscan_minPts }else{ NULL }
          n_PC <- if(input$DE_species_pca_contrib_PC == '1_2'){ c(1, 2) }else{ as.numeric(input$DE_species_pca_contrib_PC) }
          if(input$DE_species_cluster_method == 'dbscan'){
            if(is.numeric(eps)){
              if(eps < 0.1){
                eps <- 0.1
                shiny::showNotification("The epsilon must be greater than 0.1, so it is calculated by replacing it with 0.1.", type="warning")
                shiny::updateNumericInput(inputId='DE_species_dbscan_eps', value=0.1)
              }
            }else{
              eps <- 0.5
              shiny::showNotification("The epsilon must be numeric, so it is calculated by replacing it with 0.5.", type="warning")
              shiny::updateNumericInput(inputId='DE_species_dbscan_eps', value=0.5)
            }

            if(is.numeric(minPts)){
              if(minPts > 22){
                minPts <- 22
                shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 22.", type="warning")
                shiny::updateNumericInput(inputId='DE_species_dbscan_minPts', value=22)
              }else if(minPts < 1){
                minPts <- 1
                shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='DE_species_dbscan_minPts', value=1)
              }
            }else{
              minPts <- 1
              shiny::showNotification("The minPts must be numeric, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='DE_species_dbscan_minPts',value=1)
            }
          }
          shinyjs::show('DE_species_dim_redu_pca_result_div')
          shinyjs::hide('DE_species_dim_redu_plsda_result_div')
          shinyjs::hide('DE_species_dim_redu_tsne_result_div')
          shinyjs::hide('DE_species_dim_redu_umap_result_div')
          variables$DE.species.dim.redu.pca.download.log <- 1
          variables$DE.species.dim.redu.pca.topN.download.log <- 1
          variables$DE.species.pca.result <- LipidSigR::dr_pca(
            processed_se=variables$deSp.se, scaling=TRUE,
            centering=TRUE, clustering=input$DE_species_cluster_method,
            cluster_num=cluster_num, kmedoids_metric=kmedoids_metric, distfun=distfun,
            hclustfun=hclustfun, eps=eps, minPts=minPts, feature_contrib_pc=n_PC, plot_topN=input$DE_species_pca_variable_topN)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species dimensionality reduction (PCA) error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE_species_pca_contrib_PC, {
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$DE.species.pca.result)){
          ## cluster method parameter ##
          cluster_num <- switch(input$DE_species_cluster_method,
                                group_info=length(variables$DE.all.group.name),
                                kmeans=input$DE_species_kmeans_group,
                                kmedoids=input$DE_species_pam_group,
                                hclustering=input$DE_species_hclust_group,
                                dbscan=NULL)
          kmedoids_metric <- if(input$DE_species_cluster_method == 'kmedoids'){ input$DE_species_pam_metric }else{ NULL }
          distfun <- if(input$DE_species_cluster_method == 'hclustering'){ input$DE_species_hclust_dist }else{ NULL }
          hclustfun <- if(input$DE_species_cluster_method == 'hclustering'){ input$DE_species_hclust_hclust }else{ NULL }
          eps <- if(input$DE_species_cluster_method == 'dbscan'){ input$DE_species_dbscan_eps }else{ NULL }
          minPts <- if(input$DE_species_cluster_method == 'dbscan'){ input$DE_species_dbscan_minPts }else{ NULL }
          n_PC <- if(input$DE_species_pca_contrib_PC == '1_2'){ c(1, 2) }else{ as.numeric(input$DE_species_pca_contrib_PC) }
          if(input$DE_species_cluster_method == 'dbscan'){
            if(is.numeric(eps)){
              if(eps < 0.1){
                eps <- 0.1
                shiny::showNotification("The epsilon must be greater than 0.1, so it is calculated by replacing it with 0.1.", type="warning")
                shiny::updateNumericInput(inputId='DE_species_dbscan_eps', value=0.1)
              }
            }else{
              eps <- 0.5
              shiny::showNotification("The epsilon must be numeric, so it is calculated by replacing it with 0.5.", type="warning")
              shiny::updateNumericInput(inputId='DE_species_dbscan_eps', value=0.5)
            }

            if(is.numeric(minPts)){
              if(minPts > 22){
                minPts <- 22
                shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 22.", type="warning")
                shiny::updateNumericInput(inputId='DE_species_dbscan_minPts', value=22)
              }else if(minPts < 1){
                minPts <- 1
                shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='DE_species_dbscan_minPts', value=1)
              }
            }else{
              minPts <- 1
              shiny::showNotification("The minPts must be numeric, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='DE_species_dbscan_minPts',value=1)
            }
          }
          shinyjs::show('DE_species_dim_redu_pca_result_div')
          shinyjs::hide('DE_species_dim_redu_plsda_result_div')
          shinyjs::hide('DE_species_dim_redu_tsne_result_div')
          shinyjs::hide('DE_species_dim_redu_umap_result_div')
          variables$DE.species.dim.redu.pca.download.log <- 1
          variables$DE.species.dim.redu.pca.topN.download.log <- 1
          variables$DE.species.pca.result <- LipidSigR::dr_pca(
            processed_se=variables$deSp.se, scaling=TRUE,
            centering=TRUE, clustering=input$DE_species_cluster_method,
            cluster_num=cluster_num, kmedoids_metric=kmedoids_metric, distfun=distfun,
            hclustfun=hclustfun, eps=eps, minPts=minPts, feature_contrib_pc=n_PC, plot_topN=input$DE_species_pca_variable_topN)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species dimensionality reduction (PCA) error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

##### PCA ######
#### Output: DE.species.pca.biplot ####
output$DE.species.pca.biplot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.species.pca.result$interactive_pca), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.species.pca.result$interactive_pca
})

#### Output: DE.species.pca.screeplot ####
output$DE.species.pca.screeplot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.species.pca.result$interactive_screePlot), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.species.pca.result$interactive_screePlot
})

#### Output: DE.species.pca.rotated.data ####
output$DE.species.pca.rotated.data <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.species.pca.result$pca_rotated_data), "The table is not displayed due to an insufficient number of significant lipids."))
  DT::datatable(variables$DE.species.pca.result$pca_rotated_data %>%
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$DE.species.pca.rotated.data <- renderDataTable

#### Output: DE.species.pca.contrib.table ####
output$DE.species.pca.contrib.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.species.pca.result$table_pca_contribution), "The table is not displayed due to an insufficient number of significant lipids."))
  DT::datatable(variables$DE.species.pca.result$table_pca_contribution %>%
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$DE.species.pca.contrib.table <- renderDataTable

#### Output: DE.species.pca.variable ####
output$DE.species.pca.variable <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.species.pca.result$interactive_variablePlot), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.species.pca.result$interactive_variablePlot
})

#### Output: DE.species.pca.contrib ####
output$DE.species.pca.contrib <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.species.pca.result$interactive_feature_contribution), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.species.pca.result$interactive_feature_contribution
})


shiny::observeEvent(input$DE.species.dim.redu.pca.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.species.dim.redu.pca.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.species.dim.redu.pca.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.species.dim.redu.pca.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.species.PCA.pdf'), width=8, height=6)
              print(variables$DE.species.pca.result$static_pca)
              grDevices::dev.off()
              grDevices::pdf(file.path(temp_directory,'DE.species.PCA.scree.pdf'), width=8, height=6)
              print(variables$DE.species.pca.result$static_screePlot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.species.pca.result$pca_rotated_data, file=file.path(temp_directory,'DE.species.PCA.rotated.csv'))
              write.csv(variables$DE.species.pca.result$table_pca_contribution, file=file.path(temp_directory,'DE.species.PCA.contribution.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.species.dim.redu.pca.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.species.dim.redu.pca.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.species.dim.redu.pca.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species dimensionality reduction (PCA) download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE.species.dim.redu.pca.topN.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.species.dim.redu.pca.topN.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.species.dim.redu.pca.topN.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.species.dim.redu.pca.topN.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.species.PCA.topN.variable.pdf'), width=8, height=6)
              print(variables$DE.species.pca.result$static_variablePlot)
              grDevices::dev.off()
              grDevices::pdf(file.path(temp_directory,'DE.species.PCA.topN.contribution.pdf'), width=8, height=6)
              print(variables$DE.species.pca.result$static_feature_contribution)
              grDevices::dev.off()
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.species.dim.redu.pca.topN.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.species.dim.redu.pca.topN.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.species.dim.redu.pca.topN.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species dimensionality reduction (PCA) download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

##### PLSDA #####
#### Output: DE.species.plsda.sample.plot ####
output$DE.species.plsda.sample.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.species.plsda.result$interacitve_plsda), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.species.plsda.result$interacitve_plsda
})
#### Output: DE.species.plsda.variable.plot ####
output$DE.species.plsda.variable.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.species.plsda.result$interactive_loadingPlot), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.species.plsda.result$interactive_loadingPlot
})

#### Output: DE.species.plsda.variate.table ####
output$DE.species.plsda.variate.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.species.plsda.result$plsda_result), "The table is not displayed due to an insufficient number of significant lipids."))
  DT::datatable(variables$DE.species.plsda.result$plsda_result %>%
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #DE.species.plsda.variate.table <- renderDataTable

#### Output: DE.species.plsda.loading.table ####
output$DE.species.plsda.loading.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.species.plsda.result$table_plsda_loading), "The table is not displayed due to an insufficient number of significant lipids."))
  DT::datatable(variables$DE.species.plsda.result$table_plsda_loading %>%
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=TRUE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$DE.species.plsda.loading.table <- renderDataTable

shiny::observeEvent(input$DE.species.dim.redu.plsda.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.species.dim.redu.plsda.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.species.dim.redu.plsda.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.species.dim.redu.plsda.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.species.plsda.pdf'), width=8, height=6)
              print(variables$DE.species.plsda.result$static_plsda)
              grDevices::dev.off()
              grDevices::pdf(file.path(temp_directory,'DE.species.plsda.loadingplot.pdf'), width=8, height=6)
              print(variables$DE.species.plsda.result$static_loadingPlot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.species.plsda.result$plsda_result, file=file.path(temp_directory,'DE.species.plsda.csv'))
              write.csv(variables$DE.species.plsda.result$table_plsda_loading, file=file.path(temp_directory,'DE.species.plsda.loadingplot.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.species.dim.redu.plsda.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.species.dim.redu.plsda.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.species.dim.redu.plsda.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species dimensionality reduction (PLSDA) download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

##### tSNE #####
shiny::observe({
  VALUE1 <- ifelse(nrow(variables$DE.group.info)%%3 == 0,
                   floor(nrow(variables$DE.group.info)/3)-1, floor(nrow(variables$DE.group.info)/3))
  VALUE2 <- ifelse(VALUE1 < 5, VALUE1, 5)
  shiny::updateNumericInput(session,
                            inputId='DE_species_tsne_perplexity',
                            value=VALUE2, max=VALUE1)
})
#### Output: DE.species.tsne.plot ####
output$DE.species.tsne.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.species.tsne.result$interactive_tsne), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.species.tsne.result$interactive_tsne
})

#### Output: DE.species.tsne.table ####
output$DE.species.tsne.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.species.tsne.result$tsne_result), "The table is not displayed due to an insufficient number of significant lipids."))
  DT::datatable(variables$DE.species.tsne.result$tsne_result %>%
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$DE.species.tsne.table <- renderDataTable

shiny::observeEvent(input$DE.species.dim.redu.tsne.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.species.dim.redu.tsne.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.species.dim.redu.tsne.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.species.dim.redu.tsne.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.species.tsne.pdf'), width=8, height=6)
              print(variables$DE.species.tsne.result$static_tsne)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.species.tsne.result$tsne_result, file=file.path(temp_directory,'DE.species.tsne.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.species.dim.redu.tsne.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.species.dim.redu.tsne.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.species.dim.redu.tsne.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species dimensionality reduction (t-SNE) download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

##### UMAP ######
shiny::observe({
  VALUE <- ifelse(nrow(variables$DE.group.info) < 15, nrow(variables$DE.group.info), 15)
  shiny::updateNumericInput(session,
                            inputId='DE_species_umap_n_neighbors',
                            value=VALUE, max=nrow(variables$DE.group.info))
})
#### Output: DE.species.umap.plot ####
output$DE.species.umap.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.species.umap.result$interactive_umap), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.species.umap.result$interactive_umap
})

#### Output: DE.species.umap.table ####
output$DE.species.umap.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.species.umap.result$umap_result), "The table is not displayed due to an insufficient number of significant lipids."))
  DT::datatable(variables$DE.species.umap.result$umap_result %>%
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$DE.species.umap.table <- renderDataTable

shiny::observeEvent(input$DE.species.dim.redu.umap.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.species.dim.redu.umap.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.species.dim.redu.umap.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.species.dim.redu.umap.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.species.umap.pdf'), width=8, height=6)
              print(variables$DE.species.umap.result$static_umap)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.species.umap.result$umap_result, file=file.path(temp_directory,'DE.species.umap.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.species.dim.redu.umap.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.species.dim.redu.umap.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.species.dim.redu.umap.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species dimensionality reduction (UMAP) download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

#############################################################
#####  lipid species analysis: Hierarchical clustering  #####
#############################################################

#### control reset button ####
shiny::observeEvent(input$DE_species_cluster_reset, {

  #### shinyjs show/hide results ####
  shinyjs::hide('DE_species_heatmap_result_div')

  #### shinyjs reset ####
  shinyjs::reset("DE_species_cluster_reset_div")

})

#### control start button ####
shiny::observeEvent(input$DE_species_cluster_start, {
  shiny::isolate({
    tryCatch(
      {
        if(input$DE_species_cluster_by == 'all' & variables$DE.species.hclustering.all.check$logic){
          shinyjs::hide('DE_species_heatmap_div')
          shiny:::showModal(shiny::modalDialog(
            title="Important message",
            variables$DE.species.hclustering.all.check$message,
            easyClose = TRUE))
        }else if(input$DE_species_cluster_by == 'sig' & variables$DE.species.sig.check$logic){
          shinyjs::hide('DE_species_heatmap_div')
          shiny:::showModal(shiny::modalDialog(
            title="Important message",
            variables$DE.species.sig.check$message,
            easyClose=TRUE))
        }else{
          shinyjs::show('DE_species_heatmap_div')
          variables$DE.species.hclustering <- LipidSigR::heatmap_clustering(
            de_se=variables$deSp.se, char=input$DE_species_sidecolor,
            distfun=input$DE_species_dist,
            hclustfun=input$DE_species_hclust,
            type=input$DE_species_cluster_by)
          variables$DE.species.clustering.download.log <- 1
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species hierarchical clustering error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) #shiny::observeEvent(input$DE_species_cluster_start

#### Output: DE.species.heatmap.text ####
output$DE.species.heatmap.text <- shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$DE.species.hclustering$interactive_heatmap), "The plot is not displayed due to an insufficient number of significant lipids."))
  NULL
}) #output$DE.species.heatmap.text <- renderPlotly
#### Output: DE.species.heatmap ####
output$DE.species.heatmap <- iheatmapr::renderIheatmap({
  variables$DE.species.hclustering$interactive_heatmap
}) #output$DE.species.heatmap <- renderIheatmap

shiny::observeEvent(input$DE.species.clustering.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.species.clustering.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.species.clustering.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.species.clustering.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.species.clustering.pdf'), width=8, height=6)
              print(variables$DE.species.hclustering$static_heatmap)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.species.hclustering$corr_coef_matrix, file=file.path(temp_directory,'DE.species.clustering.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.species.clustering.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.species.clustering.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.species.clustering.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species hierarchical clustering download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

###################################################################
#####  lipid species analysis: Lipid characteristic analysis  #####
###################################################################

shiny::observeEvent(input$DE_species_lipid_char, {
  shiny::isolate({
    tryCatch(
      {
        variables$DE.species.char.association <-
          LipidSigR::char_association(deSp_se=variables$deSp.se,
                                      char=input$DE_species_lipid_char)
        variables$DE.species.lipid.char.download.log <- 1
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species characteristics association error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})
#### Output: DE.species.lipid.char.bar ####
output$DE.species.lipid.char.bar <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.species.char.association$interactive_barPlot), ""))
  variables$DE.species.char.association$interactive_barPlot
})
#### Output: DE.species.lipid.char.dot ####
output$DE.species.lipid.char.dot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.species.char.association$interactive_lollipop), ""))
  variables$DE.species.char.association$interactive_lollipop
})
#### Output: DE.species.lipid.char.word ####
output$DE.species.lipid.char.word <- hwordcloud::renderHwordcloud({
  shiny::validate(shiny::need(!is.null(variables$DE.species.char.association$interactive_wordCloud), ""))
  variables$DE.species.char.association$interactive_wordCloud
})

shiny::observeEvent(input$DE.species.lipid.char.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.species.lipid.char.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.species.lipid.char.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.species.lipid.char.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.species.lipid.lollipop.pdf'), width=8, height=6)
              print(variables$DE.species.char.association$static_lollipop)
              grDevices::dev.off()
              grDevices::pdf(file.path(temp_directory,'DE.species.lipid.char.wordcloud.pdf'), width=8, height=6)
              #print(variables$DE.species.char.association$static_wordCloud)
              #grDevices::replayPlot(variables$DE.species.char.association$static_wordCloud)
              wordcloud::wordcloud(
                variables$DE.species.char.association$table_wordCloud$characteristic,
                variables$DE.species.char.association$table_wordCloud$freqs, min.freq=1, random.order=FALSE,
                ordered.colors=FALSE, colors=grDevices::rainbow(nrow(variables$DE.species.char.association$table_wordCloud)))
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.species.char.association$table_lollipop, file=file.path(temp_directory,'DE.species.lipid.lollipop.csv'))
              write.csv(variables$DE.species.char.association$table_wordCloud, file=file.path(temp_directory,'DE.species.lipid.char.wordcloud.csv'))
              if(variables$DE.Ngroup == 'two'){
                grDevices::pdf(file.path(temp_directory,'DE.species.lipid.char.barplot.pdf'), width=8, height=6)
                print(variables$DE.species.char.association$static_barPlot)
                grDevices::dev.off()
                write.csv(variables$DE.species.char.association$table_barPlot, file=file.path(temp_directory,'DE.species.lipid.char.barplot.csv'))
              }
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.species.lipid.char.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.species.lipid.char.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.species.lipid.char.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid species characteristics association download error!",
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

#### Output: DE.class.twoWayAnova.tab ####
output$DE.class.twoWayAnova.tab <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$DE.class.char.twoWayAnova), "Table not showing due to category charateristics."))
  if(input$DE_class_twoWayAnova_select == 'All'){
    DT::datatable(variables$DE.class.char.twoWayAnova,
                  escape=FALSE, selection='none', rownames=TRUE,
                  class="nowrap row-border",
                  extensions=c('Buttons', 'Scroller'),
                  options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                               deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                               dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                               columnDefs=list(list(className='dt-center', targets="_all")))) %>%
      DT::formatStyle('pval_2factors', target='row', backgroundColor=DT::styleInterval(0.05, c('pink', '#EBECF0')))
  }else{
    DT::datatable(variables$DE.class.char.twoWayAnova %>% dplyr::filter(aspect == input$DE_class_twoWayAnova_select),
                  escape = FALSE, selection = 'none', rownames = TRUE,
                  class = "nowrap row-border",
                  extensions = c('Buttons', 'Scroller'),
                  options = list(scrollX = TRUE, pageLength = 5, autoWidth = FALSE,
                                 deferRender = TRUE, scrollY = 200, scroller = TRUE, #Scroller
                                 dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                 columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
      DT::formatStyle('pval_2factors', target = 'row', backgroundColor=DT::styleInterval(0.05, c('pink', '#EBECF0')))
  }
})

################################################
####  Lipid category analysis: DE analysis  ####
################################################

shiny::observeEvent(input$DE_specific_list,{
  shiny::isolate({
    if(input$DE_specific_list == "Differential expression"){
      shinyjs::show('DE_class_analysis_result_div')
      shinyjs::hide('DE_class_dim_redu_result_div')
      shinyjs::hide('DE_class_heatmap_result_div')
      shinyjs::hide('DE_class_twoCharHeatmap_div')
    }else if(input$DE_specific_list == "Dimensionality reduction"){
      shinyjs::hide('DE_class_analysis_result_div')
      shinyjs::show('DE_class_dim_redu_result_div')
      shinyjs::hide('DE_class_heatmap_result_div')
      shinyjs::hide('DE_class_twoCharHeatmap_div')
    }else if(input$DE_specific_list == "Hierarchical clustering"){
      shinyjs::hide('DE_class_analysis_result_div')
      shinyjs::hide('DE_class_dim_redu_result_div')
      shinyjs::show('DE_class_heatmap_result_div')
      shinyjs::hide('DE_class_twoCharHeatmap_div')
    }else if(input$DE_specific_list == "Two characteristics analysis"){
      shinyjs::hide('DE_class_analysis_result_div')
      shinyjs::hide('DE_class_dim_redu_result_div')
      shinyjs::hide('DE_class_heatmap_result_div')
      shinyjs::show('DE_class_twoCharHeatmap_div')
    }
  })
})
#######################
#### Control panel ####
#######################

#### control reset button ####
shiny::observeEvent(input$DE_class_analysis_reset, {
  shiny::isolate({
    #### shinyjs show/hide results ####
    shinyjs::hide('DE_class_analysis_result_div')
    shinyjs::hide('DE_class_split_result_div')
    #### shinyjs reset control panel ####
    shinyjs::reset("DE_class_analysis_reset_div")
    shiny::hideTab(inputId='DE_specific_list', target='Dimensionality reduction')
    shiny::hideTab(inputId='DE_specific_list', target='Hierarchical clustering')
    shiny::hideTab(inputId='DE_specific_list', target='Two characteristics analysis')
  })
}) #shiny::observeEvent(input$DE_analysis_reset

shiny::observe({
  if(!is.null(variables$DE.processed.lipid.char.tab) & !is.null(input$DE_class_analysis_char)){
    if(input$DE_class_analysis_char %in% c('Total.C', 'Total.OH', 'Total.DB', 'FA.C', 'FA.OH', 'FA.DB')){
      shinyjs::enable('DE_class_split_char')
    }else{
      shinyjs::disable('DE_class_split_char')
    }
  }
})

#### control DE class split lipid characteristic select input ####
shiny::observeEvent(input$DE_class_analysis_char,{
  shiny::isolate({
    tryCatch(
      {
        if(input$DE_class_analysis_char %in% c('Total.C', 'Total.OH', 'Total.DB', 'FA.C', 'FA.OH', 'FA.DB')){
          shinyjs::enable('DE_class_split_char')
          variables$DE.class.sub_char <- variables$DE.class.char.twoWayAnova %>%
            dplyr::filter(characteristic != input$DE_class_analysis_char) %>%
            dplyr::select(aspect, characteristic) %>%
            tibble::add_row(aspect='none', characteristic='none') %>%
            dplyr::mutate(aspect=factor(aspect),
                          characteristic=factor(characteristic)) %>%
            dplyr::arrange(characteristic)
          sub_select <- ifelse('class' %in% variables$DE.class.sub_char$characteristic,'class',variables$DE.class.sub_char$characteristic[1])
          #### Output: DE.class.split.char ####
          output$DE.class.split.char <- renderUI({
            selectInput(
              "DE_class_split_char",
              "Subgroup of characteristics:",
              choices=split(as.list(levels(variables$DE.class.sub_char$characteristic)), variables$DE.class.sub_char$aspect),
              selected=sub_select, multiple=FALSE)
          })
        }else{
          shinyjs::disable('DE_class_split_char')
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics update subgroup of characteristics error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

#### control start button ####
shiny::observeEvent(input$DE_class_analysis_start, {
  shiny::isolate({
    tryCatch(
      {
        #### shiny show/hide tab ####
        shiny::showTab(inputId='DE_specific_list', target='Dimensionality reduction')
        shiny::showTab(inputId='DE_specific_list', target='Hierarchical clustering')
        #### shinyjs show/hide results ####
        shinyjs::show('DE_class_analysis_result_div')
        shinyjs::hide('DE_class_dim_redu_result_div')
        shinyjs::hide('DE_class_heatmap_result_div')
        shinyjs::hide('DE_class_twoCharHeatmap_result_div')
        if(is.numeric(input$DE_class_pval)){
          p_cutoff <- input$DE_class_pval
          if(input$DE_class_pval > 1){
            p_cutoff <- 1
            shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_pval', value=1)
          }else if(input$DE_class_pval < 0.001){
            p_cutoff <- 0.001
            shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_pval', value=0.001)
          }
        }else{
          p_cutoff <- 0.05
          shiny::showNotification("The p-value must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
          shiny::updateNumericInput(inputId='DE_class_pval', value=0.05)
        }
        if(input$DE_class_analysis_char %in%  c('Chains Ether/Ester linked ratio', 'Chains odd/even ratio', 'Ratio of Lysophospholipids to Phospholipids', 'Ratio of specific lipid class A to lipid class B')){
          transform <- 'log2'
          shiny::hideTab(inputId='DE_specific_list', target='Two characteristics analysis')
        }else{
          transform <- input$DE_transformation
          if(input$DE_class_analysis_char %in%  c("Total.FA","Total.C","Total.DB","Total.OH","FA","FA.C","FA.DB","FA.OH","FA.Unsaturation.Category1","FA.Unsaturation.Category2","FA.Chain.Length.Category1","FA.Chain.Length.Category2","FA.Chain.Length.Category3",'Chains Ether/Ester linked ratio', 'Chains odd/even ratio', 'Ratio of Lysophospholipids to Phospholipids', 'Ratio of specific lipid class A to lipid class B')){
            shiny::hideTab(inputId='DE_specific_list', target='Two characteristics analysis')
          }else{
            shiny::showTab(inputId='DE_specific_list', target='Two characteristics analysis')
          }
        }
        if(input$DE_transformation == 'none' & input$DE_stat_method %in% c('Wilcoxon test', 'Kruskal-Wallis test')){
          shinyWidgets::sendSweetAlert(
            session=session, title="Suggestion!",
            text="For parametric test, it is recommended that lipidomics data be transformed.",
            type="warning")
        }

        if(variables$DE.Ngroup == 'two'){
          if(is.numeric(input$DE_class_fc)){
            FC_cutoff <- input$DE_class_fc
            if(input$DE_class_fc > 8){
              FC_cutoff <- 8
              shiny::showNotification("The Fold change (FC) must be between 1 and 8, so it is calculated by replacing it with 8.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_fc', value=8)
            }else if(input$DE_class_fc < 1){
              FC_cutoff <- 1
              shiny::showNotification("The Fold change (FC) must be between 1 and 8, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_fc', value=1)
            }
          }else{
            FC_cutoff <- 1
            shiny::showNotification("The Fold change (FC) must be numeric, so it is calculated by replacing it with 0.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_fc', value=1)
          }
          variables$DE.class.char <- LipidSigR::deChar_twoGroup(
            processed_se=variables$DE.processed.SE, char=input$DE_class_analysis_char,
            ref_group=variables$DE.ref.group,
            test=input$DE_class_post_hoc_method,
            significant=input$DE_class_sig_p, p_cutoff=p_cutoff, FC_cutoff=FC_cutoff, transform=transform)
          variables$DE.class.char.plot <- LipidSigR::plot_deChar_twoGroup(deChar_se=variables$DE.class.char)
          variables$DE.class.char.tab <- S4Vectors::metadata(variables$DE.class.char)$all_deChar_result %>%
            dplyr::arrange(dplyr::desc(1))
          variables$DE.class.download.log <- 1
        }else{
          variables$DE.class.char <- LipidSigR::deChar_multiGroup(
            processed_se=variables$DE.processed.SE,
            char=input$DE_class_analysis_char,
            ref_group=variables$DE.ref.group,
            post_hoc=input$DE_class_post_hoc_method,
            post_hoc_sig=input$DE_class_sig_p, post_hoc_p_cutoff=p_cutoff, transform=transform)
          variables$DE.class.char.plot <- LipidSigR::plot_deChar_multiGroup(variables$DE.class.char)
          if(!is.null(variables$DE.class.char.plot$table_boxPlot)){
            variables$DE.class.char.plot$interactive_boxPlot <- variables$DE.class.char.plot$table_boxPlot %>%
              plotly::plot_ly(x=~group, y=~abund,
                              hovertext=~paste0('Sample: ', label_name, '<br />Group: ', group, '<br />Index: ', format(abund, digits=3, scientific=TRUE)),
                              type='box', color=~group, colors='Set2',
                              boxpoints='all', jitter=0.85, pointpos=0,
                              marker=list(size=5, opacity=0.8)) %>%
              plotly::layout(xaxis=list(title='Group', titlefont=list(size=16), tickfont=list(size=14)),
                             yaxis=list(title=~unique(feature), titlefont=list(size=16), tickfont=list(size=14)),
                             legend=list(font=list(size=14), y=0.5),
                             margin=list(l=70, r=70, b=80, t=60))
          }
          variables$DE.class.char.tab <- S4Vectors::metadata(variables$DE.class.char)$all_deChar_result %>%
            dplyr::arrange(dplyr::desc(1)) %>%
            dplyr::mutate(dplyr::across(dplyr::starts_with("mean_"), ~ format(., digits=2,scientific=TRUE))) %>%
            dplyr::mutate(dplyr::across(dplyr::starts_with("sd_"), ~ format(., digits=2,scientific=TRUE))) %>%
            dplyr::mutate(dplyr::across(dplyr::starts_with("pval_"), ~ format(., digits=2,scientific=TRUE))) %>%
            dplyr::mutate(dplyr::across(dplyr::starts_with("fval_"), ~ round(., 2))) %>%
            dplyr::mutate(dplyr::across(dplyr::ends_with("pval"), ~ format(., digits=2,scientific=TRUE))) %>%
            dplyr::mutate(dplyr::across(dplyr::ends_with("padj"), ~ format(., digits=2,scientific=TRUE))) %>%
            dplyr::mutate(dplyr::across(dplyr::ends_with("padj"), ~ format(., digits=2,scientific=TRUE))) %>%
            dplyr::mutate(dplyr::across(dplyr::ends_with("statistic"), ~ round(., 2)))
          variables$DE.class.download.log <- 1
        }

        if(!is.null(variables$DE.class.char)){
          VALUE <- ifelse(nrow(S4Vectors::metadata(variables$DE.class.char)$sig_deChar_result) < 10, nrow(S4Vectors::metadata(variables$DE.class.char)$sig_deChar_result), 10)
          shiny::updateSliderInput(session,
                                   inputId='DE_class_pca_variable_topN',
                                   value=VALUE,
                                   max=nrow(S4Vectors::metadata(variables$DE.class.char)$sig_deChar_result))
          variables$DE.class.hclustering.sig.check <- submit.check(abundance=variables$DE.processed.abundance,
                                                                   sig_result=S4Vectors::metadata(variables$DE.class.char)$sig_deSp_result,
                                                                   check.NA=TRUE, Nfeature=2, Nsig=2)
          variables$DE.class.hclustering.all.check <- submit.check(abundance=variables$DE.processed.abundance,
                                                                   sig_result=S4Vectors::metadata(variables$DE.class.char)$sig_deSp_result,
                                                                   check.NA=TRUE, Nfeature=2, Nsig=2)

        }
        if(input$DE_class_analysis_char %in% c('Total.C', 'Total.OH', 'Total.DB', 'FA.C', 'FA.OH', 'FA.DB')){
          variables$DE.class.split.char <- input$DE_class_split_char
        }else{
          variables$DE.class.split.char <- 'none'
        }
        if(variables$DE.class.split.char != 'none'){
          shinyjs::show('DE_class_split_result_div')
          variables$DE.class.sub.char.download.log <- 1
          if(variables$DE.Ngroup == 'two'){
            variables$DE.class.sub.char <- LipidSigR::subChar_twoGroup(
              processed_se=variables$DE.processed.SE,
              char=input$DE_class_analysis_char,
              subChar=input$DE_class_split_char, ref_group=variables$DE.ref.group,
              test=input$DE_class_post_hoc_method,
              significant=input$DE_class_sig_p, p_cutoff=p_cutoff,
              FC_cutoff=FC_cutoff, transform=transform)
          }else{
            variables$DE.class.sub.char <- LipidSigR::subChar_multiGroup(
              processed_se=variables$DE.processed.SE,
              char=input$DE_class_analysis_char,
              subChar=input$DE_class_split_char,
              ref_group=variables$DE.ref.group,
              post_hoc=input$DE_class_post_hoc_method,
              post_hoc_sig=input$DE_class_sig_p,
              post_hoc_p_cutoff=p_cutoff,
              transform=transform)
          }
          variables$DE.class.sub.char.table <- S4Vectors::metadata(variables$DE.class.sub.char)$all_deChar_result
          if(!is.null(variables$DE.class.sub.char)){
            variables$DE.sub.char.feature <- unique(
              LipidSigR::extract_summarized_experiment(variables$DE.class.sub.char)$all_deChar_result$sub_feature)
            shiny::updateSelectInput(session, "DE_class_split_class",
                                     choices= variables$DE.sub.char.feature,
                                     selected=variables$DE.sub.char.feature[1])
            if(variables$DE.Ngroup == 'two'){
              variables$DE.class.sub.char.plot <- LipidSigR::plot_subChar_twoGroup(subChar_se=variables$DE.class.sub.char,
                                                                                   subChar_feature=variables$DE.sub.char.feature[1])
            }else{
              variables$DE.class.sub.char.plot <- LipidSigR::plot_subChar_multiGroup(subChar_se=variables$DE.class.sub.char,
                                                                                     subChar_feature=variables$DE.sub.char.feature[1])
              if(!is.null(variables$DE.class.sub.char.plot$table_boxPlot)){
                variables$DE.class.sub.char.plot$interactive_boxPlot <- variables$DE.class.sub.char.plot$table_boxPlot %>%
                  plotly::plot_ly(x=~group, y=~abund,
                                  hovertext=~paste0('Sample: ', label_name, '<br />Group: ', group, '<br />Index: ', format(abund, digits=3, scientific=TRUE)),
                                  type='box', color=~group, colors='Set2',
                                  boxpoints='all', jitter=0.85, pointpos=0,
                                  marker=list(size=5, opacity=0.8)) %>%
                  plotly::layout(xaxis=list(title='Group', titlefont=list(size=16), tickfont=list(size=14)),
                                 yaxis=list(title=~unique(feature), titlefont=list(size=16), tickfont=list(size=14)),
                                 title=variables$DE.sub.char.feature[1],
                                 legend=list(font=list(size=14), y=0.5),
                                 margin=list(l=70, r=70, b=80, t=60))
              }
            }
          }

        }else{
          shinyjs::hide('DE_class_split_result_div')
          variables$DE.sub.char <- NULL
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics differential expression error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) #shiny::observeEvent(input$DE_class_analysis_start

#### Output: DE.class.tab.all ####
output$DE.class.tab.all <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$DE.class.char.tab), "Table not showing due to category charateristics."))
  DT::datatable(variables$DE.class.char.tab ,
                escape=FALSE, selection='none', rownames=TRUE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all")))) %>%
    DT::formatStyle(1, target='row', backgroundColor=DT::styleEqual(c("yes","no"), c('pink', '#EBECF0')))
}) #output$DE.class.tab.all <- renderDataTable

#### Output: DE.class.barplot ####
output$DE.class.barplot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.char.plot$interactive_barPlot), "Plot not showing due to category charateristics."))
  variables$DE.class.char.plot$interactive_barPlot
}) #output$DE.class.barplot <- renderPlotly

#### Output: DE.class.trendplot ####
output$DE.class.trendplot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.char.plot$interactive_linePlot), "Plot not showing due to category charateristics."))
  variables$DE.class.char.plot$interactive_linePlot
}) #output$DE.class.trendplot <- renderPlotly

#### Output: DE.class.boxplot ####
output$DE.class.boxplot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.char.plot$interactive_boxPlot), "Plot not showing due to category charateristics."))
  variables$DE.class.char.plot$interactive_boxPlot
}) #output$DE.class.boxplot <- renderPlotly

#### Output: DE.class.barplot.sqrt ####
output$DE.class.barplot.sqrt <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.char.plot$interactive_barPlot_sqrt), "Plot not showing due to category charateristics."))
  variables$DE.class.char.plot$interactive_barPlot_sqrt
}) #output$DE.class.barplot <- renderPlotly

#### Output: DE.class.trendplot.sqrt ####
output$DE.class.trendplot.sqrt <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.char.plot$interactive_linePlot_sqrt), "Plot not showing due to category charateristics."))
  variables$DE.class.char.plot$interactive_linePlot_sqrt
}) #output$DE.class.trendplot <- renderPlotly

#### Output: DE.class.boxplot.sqrt ####
output$DE.class.boxplot.sqrt <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.char.plot$interactive_boxPlot), "Plot not showing due to category charateristics."))
  variables$DE.class.char.plot$interactive_boxPlot
}) #output$DE.class.boxplot <- renderPlotly

shiny::observeEvent(input$DE.class.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.class.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.class.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.class.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.class.barplot.pdf'), width=8, height=6)
              print(variables$DE.class.char.plot$static_barPlot)
              grDevices::dev.off()
              grDevices::pdf(file.path(temp_directory,'DE.class.barplo.sqrt.pdf'), width=8, height=6)
              print(variables$DE.class.char.plot$static_barPlot_sqrt)
              grDevices::dev.off()
              if(!is.null(variables$DE.class.char.plot$static_linePlot)){
                grDevices::pdf(file.path(temp_directory,'DE.class.lineplot.pdf'), width=8, height=6)
                print(variables$DE.class.char.plot$static_linePlot)
                grDevices::dev.off()
              }
              if(!is.null(variables$DE.class.char.plot$static_linePlot_sqrt)){
                grDevices::pdf(file.path(temp_directory,'DE.class.lineplot.sqrt.pdf'), width=8, height=6)
                print(variables$DE.class.char.plot$static_linePlot_sqrt)
                grDevices::dev.off()
              }
              if(!is.null(variables$DE.class.char.plot$DE.class.boxPlot.pdf)){
                grDevices::pdf(file.path(temp_directory,'DE.class.boxPlot.pdf'), width=8, height=6)
                print(variables$DE.class.char.plot$static_boxPlot)
                grDevices::dev.off()
              }
              ## table  ##
              write.csv(variables$DE.class.char.plot$table_barPlot, file=file.path(temp_directory,'DE.class.barplot.csv'))
              if(!is.null(variables$DE.class.char.plot$table_linePlot)){
                write.csv(variables$DE.class.char.plot$table_linePlot, file=file.path(temp_directory,'DE.class.lineplot.csv'))
              }
              if(!is.null(variables$DE.class.char.plot$table_boxPlot)){
                write.csv(variables$DE.class.char.plot$table_boxPlot, file=file.path(temp_directory,'DE.class.boxPlot.csv'))
              }

              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.class.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.class.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.class.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics differential expression download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE_class_split_class, {
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$DE.class.char)){
          if(variables$DE.Ngroup == 'two'){
            variables$DE.class.sub.char.plot <- LipidSigR::plot_subChar_twoGroup(subChar_se=variables$DE.class.sub.char,
                                                                                 subChar_feature=input$DE_class_split_class)
          }else{
            variables$DE.class.sub.char.plot <- LipidSigR::plot_subChar_multiGroup(subChar_se=variables$DE.class.sub.char,
                                                                                   subChar_feature=input$DE_class_split_class)
            if(!is.null(variables$DE.class.sub.char.plot$table_boxPlot)){
              variables$DE.class.sub.char.plot$interactive_boxPlot <- variables$DE.class.sub.char.plot$table_boxPlot %>%
                plotly::plot_ly(x=~group, y=~abund,
                                hovertext=~paste0('Sample: ', label_name, '<br />Group: ', group, '<br />Index: ', format(abund, digits=3, scientific=TRUE)),
                                type='box', color=~group, colors='Set2',
                                boxpoints='all', jitter=0.85, pointpos=0,
                                marker=list(size=5, opacity=0.8)) %>%
                plotly::layout(xaxis=list(title='Group', titlefont=list(size=16), tickfont=list(size=14)),
                               yaxis=list(title=~unique(feature), titlefont=list(size=16), tickfont=list(size=14)),
                               title=input$DE_class_split_class,
                               legend=list(font=list(size=14), y=0.5),
                               margin=list(l=70, r=70, b=80, t=60))
            }
          }
          variables$DE.class.sub.char.download.log <- 1
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics subgroup analysis error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) #observeEvent(input$DE_class_split_class


#### Output: DE.class.sub.char.table ####
output$DE.class.sub.char.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.class.sub.char.table), "Table not showing due to category charateristics."))
  DT::datatable(variables$DE.class.sub.char.table %>%
                  dplyr::filter(sub_feature == input$DE_class_split_class) %>%
                  dplyr::arrange(dplyr::desc(1)) ,
                escape=FALSE, selection='none', rownames=TRUE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all")))) %>%
    DT::formatStyle(1, target='row', backgroundColor=DT::styleEqual(c("yes","no"), c('pink', '#EBECF0')))
}) #output$DE.class.split.tab.all <- renderDataTable

#### Output: DE.class.sub.char.barplot ####
output$DE.class.sub.char.barplot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.sub.char.plot$interactive_barPlot), "Plot not showing due to category charateristics."))
  variables$DE.class.sub.char.plot$interactive_barPlot
}) #output$DE.class.sub.char.plot.barplot <- renderPlotly

#### Output: DE.class.sub.char.barplot.lineplot ####
output$DE.class.sub.char.barplot.lineplot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.sub.char.plot$interactive_linePlot), "Plot not showing due to category charateristics."))
  variables$DE.class.sub.char.plot$interactive_linePlot
}) #output$DE.class.sub.char.barplot.lineplot <- renderPlotly

#### Output: DE.class.sub.char.barplot.boxplot ####
output$DE.class.sub.char.barplot.boxplot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.sub.char.plot$interactive_boxPlot), "Plot not showing due to category charateristics."))
  variables$DE.class.sub.char.plot$interactive_boxPlot
}) #output$DE.class.sub.char.barplot.boxplot <- renderPlotly

#### Output: DE.class.sub.char.barplot.sqrt ####
output$DE.class.sub.char.barplot.sqrt <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.sub.char.plot$interactive_barPlot_sqrt), "Plot not showing due to category charateristics."))
  variables$DE.class.sub.char.plot$interactive_barPlot_sqrt
}) #output$DE.class.sub.char.barplot.sqrt <- renderPlotly

#### Output: DE.class.sub.char.barplot.lineplot.sqrt ####
output$DE.class.sub.char.barplot.lineplot.sqrt <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.sub.char.plot$interactive_linePlot_sqrt), "Plot not showing due to category charateristics."))
  variables$DE.class.sub.char.plot$interactive_linePlot_sqrt
}) #output$DE.class.sub.char.barplot.lineplot.sqrt <- renderPlotly

#### Output: DE.class.sub.char.barplot.boxplot.sqrt ####
output$DE.class.sub.char.barplot.boxplot.sqrt <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.sub.char.plot$interactive_boxPlot), "Plot not showing due to category charateristics."))
  variables$DE.class.sub.char.plot$interactive_boxPlot
}) #output$DE.class.sub.char.barplot.boxplot.sqrt <- renderPlotly

shiny::observeEvent(input$DE.class.sub.char.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.class.sub.char.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.class.sub.char.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.class.sub.char.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.class.sub.char.barplot.pdf'), width=8, height=6)
              print(variables$DE.class.sub.char.plot$static_barPlot)
              grDevices::dev.off()
              grDevices::pdf(file.path(temp_directory,'DE.class.sub.char.barplo.sqrt.pdf'), width=8, height=6)
              print(variables$DE.class.sub.char.plot$static_barPlot_sqrt)
              grDevices::dev.off()
              grDevices::pdf(file.path(temp_directory,'DE.class.sub.char.lineplot.pdf'), width=8, height=6)
              print(variables$DE.class.sub.char.plot$static_linePlot)
              grDevices::dev.off()
              grDevices::pdf(file.path(temp_directory,'DE.class.sub.char.lineplot.sqrt.pdf'), width=8, height=6)
              print(variables$DE.class.sub.char.plot$static_linePlot_sqrt)
              grDevices::dev.off()
              grDevices::pdf(file.path(temp_directory,'DE.class.sub.char.boxPlot.pdf'), width=8, height=6)
              print(variables$DE.class.sub.char.plot$static_boxPlot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.class.sub.char.plot$table_barPlot, file=file.path(temp_directory,'DE.class.sub.char.barplot.csv'))
              write.csv(variables$DE.class.sub.char.plot$table_linePlot, file=file.path(temp_directory,'DE.class.sub.char.lineplot.csv'))
              write.csv(variables$DE.class.sub.char.plot$table_boxPlot, file=file.path(temp_directory,'DE.class.sub.char.boxPlot.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.class.sub.char.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.class.sub.char.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.class.sub.char.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics subgroup analysis download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

#############################################################
####  lipid category analysis: Dimensionality reduction  ####
#############################################################

#######################
#### Control panel ####
#######################

# #### Group assignment ####
#### number of samples ####
output$DE.class.group.count <- shiny::renderText({
  paste0(length(unique(variables$DE.group.info$group)), ' groups were detected in samples.')
})

#### dbscan ####
#### update numeric input minPts ####
shiny::observe({
  shiny::updateNumericInput(session,
                            inputId='DE_class_dbscan_minPts',
                            max=(nrow(variables$DE.group.info)-1)
  ) #updateNumericInput
})
#### control reset button ####
shiny::observeEvent(input$DE_class_dim_redu_reset, {
  shiny::isolate({
    #### shinyjs show/hide results ####
    shinyjs::hide('DE_class_dim_redu_result_div')
    #### shinyjs reset control panel ####
    shinyjs::reset("DE_class_dim_redu_reset_div")
  })
}) #shiny::observeEvent(input$DE_class_dim_redu_reset

#### control start button ####
shiny::observeEvent(input$DE_class_dim_redu_start,{
  shiny::isolate({
    tryCatch(
      {
        ## cluster method parameter ##
        cluster_num <- switch(input$DE_class_cluster_method,
                              group_info=length(variables$DE.all.group.name),
                              kmeans=input$DE_class_kmeans_group,
                              kmedoids=input$DE_class_pam_group,
                              hclustering=input$DE_class_hclust_group,
                              dbscan=NULL)
        kmedoids_metric <- if(input$DE_class_cluster_method == 'kmedoids'){ input$DE_class_pam_metric }else{ NULL }
        distfun <- if(input$DE_class_cluster_method == 'hclustering'){ input$DE_class_hclust_dist }else{ NULL }
        hclustfun <- if(input$DE_class_cluster_method == 'hclustering'){ input$DE_class_hclust_hclust }else{ NULL }
        eps <- if(input$DE_class_cluster_method == 'dbscan'){ input$DE_class_dbscan_eps }else{ NULL }
        minPts <- if(input$DE_class_cluster_method == 'dbscan'){ input$DE_class_dbscan_minPts }else{ NULL }
        n_PC <- if(input$DE_class_pca_contrib_PC == '1_2'){ c(1, 2) }else{ as.numeric(input$DE_class_pca_contrib_PC) }
        if(input$DE_class_cluster_method == 'dbscan'){
          if(is.numeric(eps)){
            if(eps < 0.1){
              eps <- 0.1
              shiny::showNotification("The epsilon must be greater than 0.1, so it is calculated by replacing it with 0.1.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_dbscan_eps', value=0.1)
            }
          }else{
            eps <- 0.5
            shiny::showNotification("The epsilon must be numeric, so it is calculated by replacing it with 0.5.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_dbscan_eps', value=0.5)
          }

          if(is.numeric(minPts)){
            if(minPts > 22){
              minPts <- 22
              shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 22.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_dbscan_minPts', value=22)
            }else if(minPts < 1){
              minPts <- 1
              shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_dbscan_minPts', value=1)
            }
          }else{
            minPts <- 1
            shiny::showNotification("The minPts must be numeric, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_dbscan_minPts',value=1)
          }
        }
        shinyjs::show('DE_class_dim_redu_result_div')
        if(input$DE_class_dim_redu_method == 'pca'){
          shinyjs::hide('DE_class_dim_redu_plsda_result_div')
          shinyjs::hide('DE_class_dim_redu_tsne_result_div')
          shinyjs::hide('DE_class_dim_redu_umap_result_div')
          variables$DE.class.dim.redu.pca.download.log <- 1
          variables$DE.class.dim.redu.pca.topN.download.log <- 1
          variables$DE.class.pca.result <- LipidSigR::dr_pca(
            processed_se=variables$DE.class.char, scaling=TRUE,
            centering=TRUE, clustering=input$DE_class_cluster_method,
            cluster_num=cluster_num, kmedoids_metric=kmedoids_metric, distfun=distfun,
            hclustfun=hclustfun, eps=eps, minPts=minPts, feature_contrib_pc=n_PC,
            plot_topN=input$DE_class_pca_variable_topN)
          if(!is.null(variables$DE.class.pca.result)){
            VALUE <- ifelse(nrow(variables$DE.class.pca.result$table_pca_contribution) < 10, nrow(variables$DE.class.pca.result$table_pca_contribution), 10)
            shiny::updateSliderInput(session,
                                     inputId='DE_class_pca_variable_topN',
                                     value=VALUE,
                                     max=nrow(variables$DE.class.pca.result$table_pca_contribution))
            shinyjs::show('DE_class_dim_redu_pca_result_div')
          }
        }else if(input$DE_class_dim_redu_method == 'plsda'){
          variables$DE.class.dim.redu.plsda.download.log <- 1
          variables$DE.class.plsda.result <- LipidSigR::dr_plsda(
            de_se=variables$DE.class.char, ncomp=2, scaling=TRUE,
            clustering=input$DE_class_cluster_method,
            cluster_num=cluster_num, kmedoids_metric=kmedoids_metric, distfun=distfun,
            hclustfun=hclustfun, eps=eps, minPts=minPts)
          shinyjs::hide('DE_class_dim_redu_pca_result_div')
          shinyjs::hide('DE_class_dim_redu_tsne_result_div')
          shinyjs::hide('DE_class_dim_redu_umap_result_div')
          shinyjs::show('DE_class_dim_redu_plsda_result_div')
        }else if(input$DE_class_dim_redu_method == 'tsne'){
          if(is.numeric(input$DE_class_tsne_perplexity)){
            perplexity <- input$DE_class_tsne_perplexity
            if(input$DE_class_tsne_perplexity > 7){
              perplexity <- 7
              shiny::showNotification("The perplexity must be between 3 and 7, so it is calculated by replacing it with 7.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_tsne_perplexity', value=7)
            }else if(input$DE_class_tsne_perplexity < 3){
              perplexity <- 3
              shiny::showNotification("The perplexity must be between 3 and 7, so it is calculated by replacing it with 3.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_tsne_perplexity', value=3)
            }
          }else{
            perplexity <- 5
            shiny::showNotification("The perplexity must be numeric, so it is calculated by replacing it with 5.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_tsne_perplexity', value=5)
          }
          if(is.numeric(input$DE_class_tsne_max_iter)){
            max_iter <- input$DE_class_tsne_max_iter
            if(input$DE_class_tsne_max_iter > 5000){
              max_iter <- 5000
              shiny::showNotification("The number of iterations must be between 100 and 5000, so it is calculated by replacing it with 5000.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_tsne_max_iter', value=5000)
            }else if(input$DE_class_tsne_max_iter < 100){
              max_iter <- 100
              shiny::showNotification("The number of iterations must be between 100 and 5000, so it is calculated by replacing it with 100.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_tsne_max_iter', value=100)
            }
          }else{
            max_iter <- 500
            shiny::showNotification("The number of iterations must be numeric, so it is calculated by replacing it with 500.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_tsne_max_iter', value=500)
          }
          variables$DE.class.dim.redu.tsne.download.log <- 1
          variables$DE.class.tsne.result <- LipidSigR::dr_tsne(
            processed_se=variables$DE.class.char,
            pca=TRUE, perplexity=perplexity, max_iter=max_iter,
            clustering=input$DE_class_cluster_method, cluster_num=cluster_num,
            kmedoids_metric=kmedoids_metric, distfun=distfun,
            hclustfun=hclustfun, eps=eps, minPts=minPts)
          shinyjs::hide('DE_class_dim_redu_pca_result_div')
          shinyjs::show('DE_class_dim_redu_tsne_result_div')
          shinyjs::hide('DE_class_dim_redu_umap_result_div')
          shinyjs::hide('DE_class_dim_redu_plsda_result_div')
        }else if(input$DE_class_dim_redu_method == 'umap'){
          variables$DE.class.dim.redu.umap.download.log <- 1
          if(is.numeric(input$DE_class_umap_n_neighbors)){
            n_neighbors <- input$DE_class_umap_n_neighbors
            if(input$DE_class_umap_n_neighbors > 23){
              n_neighbors <- 23
              shiny::showNotification("The number of neighbors must be between 2 and 23, so it is calculated by replacing it with 23.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_umap_n_neighbors', value=23)
            }else if(input$DE_class_umap_n_neighbors < 2){
              n_neighbors <- 2
              shiny::showNotification("The number of neighbors must be between 2 and 23, so it is calculated by replacing it with 2.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_umap_n_neighbors', value=2)
            }
          }else{
            n_neighbors <- 15
            shiny::showNotification("The number of neighbors must be numeric, so it is calculated by replacing it with 15.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_umap_n_neighbors', value=15)
          }
          variables$DE.class.umap.result <- LipidSigR::dr_umap(
            processed_se=variables$DE.class.char,
            n_neighbors=n_neighbors, scaling=TRUE,
            umap_metric=input$DE_class_umap_metric,
            clustering=input$DE_class_cluster_method, cluster_num=cluster_num,
            kmedoids_metric=kmedoids_metric, distfun=distfun,
            hclustfun=hclustfun, eps=eps, minPts=minPts)
          shinyjs::hide('DE_class_dim_redu_pca_result_div')
          shinyjs::hide('DE_class_dim_redu_tsne_result_div')
          shinyjs::show('DE_class_dim_redu_umap_result_div')
          shinyjs::hide('DE_class_dim_redu_plsda_result_div')
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics dimensionality reduction error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE_class_pca_variable_topN,{
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$DE.class.pca.result)){
          ## cluster method parameter ##
          cluster_num <- switch(input$DE_class_cluster_method,
                                group_info=length(variables$DE.all.group.name),
                                kmeans=input$DE_class_kmeans_group,
                                kmedoids=input$DE_class_pam_group,
                                hclustering=input$DE_class_hclust_group,
                                dbscan=NULL)
          kmedoids_metric <- if(input$DE_class_cluster_method == 'kmedoids'){ input$DE_class_pam_metric }else{ NULL }
          distfun <- if(input$DE_class_cluster_method == 'hclustering'){ input$DE_class_hclust_dist }else{ NULL }
          hclustfun <- if(input$DE_class_cluster_method == 'hclustering'){ input$DE_class_hclust_hclust }else{ NULL }
          eps <- if(input$DE_class_cluster_method == 'dbscan'){ input$DE_class_dbscan_eps }else{ NULL }
          minPts <- if(input$DE_class_cluster_method == 'dbscan'){ input$DE_class_dbscan_minPts }else{ NULL }
          n_PC <- if(input$DE_class_pca_contrib_PC == '1_2'){ c(1, 2) }else{ as.numeric(input$DE_class_pca_contrib_PC) }
          if(input$DE_class_cluster_method == 'dbscan'){
            if(is.numeric(eps)){
              if(eps < 0.1){
                eps <- 0.1
                shiny::showNotification("The epsilon must be greater than 0.1, so it is calculated by replacing it with 0.1.", type="warning")
                shiny::updateNumericInput(inputId='DE_class_dbscan_eps', value=0.1)
              }
            }else{
              eps <- 0.5
              shiny::showNotification("The epsilon must be numeric, so it is calculated by replacing it with 0.5.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_dbscan_eps', value=0.5)
            }

            if(is.numeric(minPts)){
              if(minPts > 22){
                minPts <- 22
                shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 22.", type="warning")
                shiny::updateNumericInput(inputId='DE_class_dbscan_minPts', value=22)
              }else if(minPts < 1){
                minPts <- 1
                shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='DE_class_dbscan_minPts', value=1)
              }
            }else{
              minPts <- 1
              shiny::showNotification("The minPts must be numeric, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_dbscan_minPts',value=1)
            }
          }
          shinyjs::hide('DE_class_dim_redu_plsda_result_div')
          shinyjs::hide('DE_class_dim_redu_tsne_result_div')
          shinyjs::hide('DE_class_dim_redu_umap_result_div')
          variables$DE.class.dim.redu.pca.download.log <- 1
          variables$DE.class.dim.redu.pca.topN.download.log <- 1
          variables$DE.class.pca.result <- LipidSigR::dr_pca(
            processed_se=variables$DE.class.char, scaling=TRUE,
            centering=TRUE, clustering=input$DE_class_cluster_method,
            cluster_num=cluster_num, kmedoids_metric=kmedoids_metric, distfun=distfun,
            hclustfun=hclustfun, eps=eps, minPts=minPts, feature_contrib_pc=n_PC,
            plot_topN=input$DE_class_pca_variable_topN)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics dimensionality reduction (PCA) error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE_class_pca_contrib_PC,{
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$DE.class.pca.result)){
          ## cluster method parameter ##
          cluster_num <- switch(input$DE_class_cluster_method,
                                group_info=length(variables$DE.all.group.name),
                                kmeans=input$DE_class_kmeans_group,
                                kmedoids=input$DE_class_pam_group,
                                hclustering=input$DE_class_hclust_group,
                                dbscan=NULL)
          kmedoids_metric <- if(input$DE_class_cluster_method == 'kmedoids'){ input$DE_class_pam_metric }else{ NULL }
          distfun <- if(input$DE_class_cluster_method == 'hclustering'){ input$DE_class_hclust_dist }else{ NULL }
          hclustfun <- if(input$DE_class_cluster_method == 'hclustering'){ input$DE_class_hclust_hclust }else{ NULL }
          eps <- if(input$DE_class_cluster_method == 'dbscan'){ input$DE_class_dbscan_eps }else{ NULL }
          minPts <- if(input$DE_class_cluster_method == 'dbscan'){ input$DE_class_dbscan_minPts }else{ NULL }
          n_PC <- if(input$DE_class_pca_contrib_PC == '1_2'){ c(1, 2) }else{ as.numeric(input$DE_class_pca_contrib_PC) }
          if(input$DE_class_cluster_method == 'dbscan'){
            if(is.numeric(eps)){
              if(eps < 0.1){
                eps <- 0.1
                shiny::showNotification("The epsilon must be greater than 0.1, so it is calculated by replacing it with 0.1.", type="warning")
                shiny::updateNumericInput(inputId='DE_class_dbscan_eps', value=0.1)
              }
            }else{
              eps <- 0.5
              shiny::showNotification("The epsilon must be numeric, so it is calculated by replacing it with 0.5.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_dbscan_eps', value=0.5)
            }

            if(is.numeric(minPts)){
              if(minPts > 22){
                minPts <- 22
                shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 22.", type="warning")
                shiny::updateNumericInput(inputId='DE_class_dbscan_minPts', value=22)
              }else if(minPts < 1){
                minPts <- 1
                shiny::showNotification("The minPts must be between 1 and 22, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='DE_class_dbscan_minPts', value=1)
              }
            }else{
              minPts <- 1
              shiny::showNotification("The minPts must be numeric, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_dbscan_minPts',value=1)
            }
          }
          shinyjs::hide('DE_class_dim_redu_plsda_result_div')
          shinyjs::hide('DE_class_dim_redu_tsne_result_div')
          shinyjs::hide('DE_class_dim_redu_umap_result_div')
          variables$DE.class.dim.redu.pca.download.log <- 1
          variables$DE.class.dim.redu.pca.topN.download.log <- 1
          variables$DE.class.pca.result <- LipidSigR::dr_pca(
            processed_se=variables$DE.class.char, scaling=TRUE,
            centering=TRUE, clustering=input$DE_class_cluster_method,
            cluster_num=cluster_num, kmedoids_metric=kmedoids_metric, distfun=distfun,
            hclustfun=hclustfun, eps=eps, minPts=minPts, feature_contrib_pc=n_PC,
            plot_topN=input$DE_class_pca_variable_topN)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics dimensionality reduction (PCA) error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

##### PCA ######
#### Output: DE.class.pca.biplot ####
output$DE.class.pca.biplot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.pca.result$interactive_pca), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.class.pca.result$interactive_pca
})

#### Output: DE.class.pca.screeplot ####
output$DE.class.pca.screeplot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.pca.result$interactive_screePlot), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.class.pca.result$interactive_screePlot
})

#### Output: DE.class.pca.rotated.data ####
output$DE.class.pca.rotated.data <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.class.pca.result$pca_rotated_data), "The table is not displayed due to an insufficient number of significant lipids."))
  DT::datatable(variables$DE.class.pca.result$pca_rotated_data %>% dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$DE.class.pca.rotated.data <- renderDataTable
#### Output: DE.class.pca.contrib.table ####
output$DE.class.pca.contrib.table <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$DE.class.pca.result$table_pca_contribution), "The table is not displayed due to an insufficient number of significant lipids."))
  DT::datatable(variables$DE.class.pca.result$table_pca_contribution %>%
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$DE.class.pca.contrib.table <- renderDataTable
#### Output: DE.class.pca.variable ####
output$DE.class.pca.variable <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.pca.result$interactive_variablePlot), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.class.pca.result$interactive_variablePlot
})
#### Output: DE.class.pca.contrib ####
output$DE.class.pca.contrib <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.pca.result$interactive_feature_contribution), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.class.pca.result$interactive_feature_contribution
})


shiny::observeEvent(input$DE.class.dim.redu.pca.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.class.dim.redu.pca.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.class.dim.redu.pca.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.class.dim.redu.pca.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.class.PCA.pdf'), width=8, height=6)
              print(variables$DE.class.pca.result$static_pca)
              grDevices::dev.off()
              grDevices::pdf(file.path(temp_directory,'DE.class.PCA.scree.pdf'), width=8, height=6)
              print(variables$DE.class.pca.result$static_screePlot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.class.pca.result$pca_rotated_data, file=file.path(temp_directory,'DE.class.PCA.rotated.csv'))
              write.csv(variables$DE.class.pca.result$table_pca_contribution, file=file.path(temp_directory,'DE.class.PCA.contribution.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.class.dim.redu.pca.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.class.dim.redu.pca.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.class.dim.redu.pca.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics dimensionality reduction (PCA) download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE.class.dim.redu.pca.topN.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.class.dim.redu.pca.topN.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.class.dim.redu.pca.topN.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.class.dim.redu.pca.topN.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.class.PCA.topN.variable.pdf'), width=8, height=6)
              print(variables$DE.class.pca.result$static_variablePlot)
              grDevices::dev.off()
              grDevices::pdf(file.path(temp_directory,'DE.class.PCA.topN.contribution.pdf'), width=8, height=6)
              print(variables$DE.class.pca.result$static_feature_contribution)
              grDevices::dev.off()
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.class.dim.redu.pca.topN.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.class.dim.redu.pca.topN.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.class.dim.redu.pca.topN.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics dimensionality reduction (PCA) download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

##### PLSDA #####
#### Output: DE.class.plsda.sample.plot ####
output$DE.class.plsda.sample.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.plsda.result$interacitve_plsda), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.class.plsda.result$interacitve_plsda
})
#### Output: DE.class.plsda.variable.plot ####
output$DE.class.plsda.variable.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.plsda.result$interactive_loadingPlot), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.class.plsda.result$interactive_loadingPlot
})

#### Output: DE.class.plsda.variate.table ####
output$DE.class.plsda.variate.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.class.plsda.result$plsda_result), "The table is not displayed due to an insufficient number of significant lipids."))

  DT::datatable(variables$DE.class.plsda.result$plsda_result %>%
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #DE.class.plsda.variate.table <- renderDataTable

#### Output: DE.class.plsda.loading.table ####
output$DE.class.plsda.loading.table <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$DE.class.plsda.result$table_plsda_loading), "The table is not displayed due to an insufficient number of significant lipids."))
  DT::datatable(variables$DE.class.plsda.result$table_plsda_loading %>%
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=TRUE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$DE.class.plsda.loading.table <- renderDataTable

shiny::observeEvent(input$DE.class.dim.redu.plsda.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.class.dim.redu.plsda.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.class.dim.redu.plsda.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.class.dim.redu.plsda.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.class.plsda.pdf'), width=8, height=6)
              print(variables$DE.class.plsda.result$static_plsda)
              grDevices::dev.off()
              grDevices::pdf(file.path(temp_directory,'DE.class.plsda.loadingplot.pdf'), width=8, height=6)
              print(variables$DE.class.plsda.result$static_loadingPlot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.class.plsda.result$plsda_result, file=file.path(temp_directory,'DE.class.plsda.csv'))
              write.csv(variables$DE.class.plsda.result$table_plsda_loading, file=file.path(temp_directory,'DE.class.plsda.loadingplot.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.class.dim.redu.plsda.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.class.dim.redu.plsda.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.class.dim.redu.plsda.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics dimensionality reduction (PLSDA) download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

##### tSNE #####
shiny::observe({
  VALUE1 <- ifelse(nrow(variables$DE.group.info)%%3 == 0, floor(nrow(variables$DE.group.info)/3)-1, floor(nrow(variables$DE.group.info)/3))
  VALUE2 <- ifelse(VALUE1 < 5, VALUE1, 5)
  shiny::updateNumericInput(session,
                            inputId='DE_class_tsne_perplexity',
                            value=VALUE2,
                            max=VALUE1)
})
#### Output: DE.class.tsne.plot ####
output$DE.class.tsne.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.tsne.result$interactive_tsne), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.class.tsne.result$interactive_tsne
})
#### Output: DE.class.tsne.table ####
output$DE.class.tsne.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.class.tsne.result$tsne_result), "The table is not displayed due to an insufficient number of significant lipids."))
  DT::datatable(variables$DE.class.tsne.result$tsne_result %>%
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$DE.class.tsne.table <- renderDataTable

shiny::observeEvent(input$DE.class.dim.redu.tsne.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.class.dim.redu.tsne.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.class.dim.redu.tsne.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.class.dim.redu.tsne.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.class.tsne.pdf'), width=8, height=6)
              print(variables$DE.class.tsne.result$static_tsne)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.class.tsne.result$tsne_result, file=file.path(temp_directory,'DE.class.tsne.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.class.dim.redu.tsne.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.class.dim.redu.tsne.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.class.dim.redu.tsne.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics dimensionality reduction (t-SNE) download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

##### UMAP ######
shiny::observe({
  VALUE <- ifelse(nrow(variables$DE.group.info) < 15, nrow(variables$DE.group.info), 15)
  shiny::updateNumericInput(session,
                            inputId='DE_class_umap_n_neighbors',
                            value=VALUE,
                            max=nrow(variables$DE.group.info))
})
#### Output: DE.class.umap.plot ####
output$DE.class.umap.plot <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$DE.class.umap.result$interactive_umap), "The plot is not displayed due to an insufficient number of significant lipids."))
  variables$DE.class.umap.result$interactive_umap
})

#### Output: DE.class.umap.table ####
output$DE.class.umap.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.class.umap.result$umap_result), "The table is not displayed due to an insufficient number of significant lipids."))
  DT::datatable(variables$DE.class.umap.result$umap_result %>%
                  dplyr::mutate_if(is.numeric, ~round(., 5)),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$DE.class.umap.table <- renderDataTable

shiny::observeEvent(input$DE.class.dim.redu.umap.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.class.dim.redu.umap.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.class.dim.redu.umap.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.class.dim.redu.umap.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.class.umap.pdf'), width=8, height=6)
              print(variables$DE.class.umap.result$static_umap)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.class.umap.result$umap_result, file=file.path(temp_directory,'DE.class.umap.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.class.dim.redu.umap.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.class.dim.redu.umap.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.class.dim.redu.umap.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics dimensionality reduction (UMAP) download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

##############################################################
#####  Lipid category analysis: Hierarchical clustering  #####
##############################################################

#### control reset button ####
shiny::observeEvent(input$DE_class_cluster_reset, {
  shiny::isolate({
    #### shinyjs show/hide results ####
    shinyjs::hide('DE_class_heatmap_div')
    #### shinyjs reset ####
    shinyjs::reset('DE_class_cluster_reset_div')
  })
})

#### control start button ####
shiny::observeEvent(input$DE_class_cluster_start, {
  shiny::isolate({
    tryCatch(
      {
        if(input$DE_class_cluster_by == 'all' & variables$DE.class.hclustering.all.check$logic){
          shinyjs::hide('DE_class_heatmap_div')
          shiny:::showModal(shiny::modalDialog(
            title="Important message",
            variables$DE.class.hclustering.all.check$message,
            easyClose = TRUE))
        }else if(input$DE_class_cluster_by == 'sig' & variables$DE.class.hclustering.sig.check$logic){
          shinyjs::hide('DE_class_heatmap_div')
          shiny:::showModal(shiny::modalDialog(
            title="Important message",
            variables$DE.class.hclustering.sig.check$message,
            easyClose=TRUE))
        }else{
          #### shinyjs show/hide results ####
          shinyjs::show('DE_class_heatmap_div')
          #### Function: Hclustering ####
          variables$DE.class.hclustering <- LipidSigR::heatmap_clustering(
            de_se=variables$DE.class.char, char=NULL,
            distfun=input$DE_class_dist,
            hclustfun=input$DE_class_hclust,
            type=input$DE_class_cluster_by)
          variables$DE.class.clustering.download.log <- 1
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics hierarchical clustering error!",
          text=as.character(e$message),
          type="error")
      }
    )
  }) #isolate
}) #shiny::observeEvent(input$DE_class_cluster_start

#### Output: DE.class.heatmap.text ####
output$DE.class.heatmap.text <- shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$DE.class.hclustering$interactive_heatmap), "The plot is not displayed due to an insufficient number of significant lipids."))
  NULL
}) #output$DE.class.heatmap.text <- renderPlotly

#### Output: DE.class.heatmap ####
output$DE.class.heatmap <- iheatmapr::renderIheatmap({
  variables$DE.class.hclustering$interactive_heatmap
}) #output$DE.class.heatmap <- renderIheatmap

shiny::observeEvent(input$DE.class.clustering.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.class.clustering.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.class.clustering.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.class.clustering.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.class.clustering.pdf'), width=8, height=6)
              print(variables$DE.class.hclustering$static_heatmap)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.class.hclustering$corr_coef_matrix, file=file.path(temp_directory,'DE.class.clustering.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.class.clustering.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.class.clustering.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.class.clustering.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics hierarchical clustering download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

##############################################################
#####  Lipid category analysis:  twoCharHeatmap          #####
##############################################################

#### control reset button ####
shiny::observeEvent(input$DE_class_twoCharHeatmap_cluster_reset, {
  shiny::isolate({
    #### shinyjs show/hide results ####
    shinyjs::hide('DE_class_twoCharHeatmap_result_div')
    #### shinyjs reset ####
    shinyjs::reset('DE_class_twoCharHeatmap_reset_div')
  })
})

shiny::observeEvent(input$DE_class_twoCharHeatmap_post_hoc_method,{
  shiny::isolate({
    if(input$DE_class_twoCharHeatmap_post_hoc_method == 'One-way ANOVA'){
      updateRadioButtons(session, "DE_class_twoCharHeatmap_postHoc_method",
                         choices="Tukey's HSD",
                         selected="Tukey's HSD")
    }else if(input$DE_class_twoCharHeatmap_post_hoc_method == 'Kruskal-Wallis test'){
      updateRadioButtons(session, "DE_class_twoCharHeatmap_postHoc_method",
                         choices="Dunn's Test",
                         selected="Dunn's Test")
    }
  })
})

#### control start button ####
shiny::observeEvent(input$DE_class_twoCharHeatmap_cluster_start, {
  shiny::isolate({
    tryCatch(
      {
        #### shinyjs show/hide results ####
        shinyjs::show('DE_class_twoCharHeatmap_result_div')
        if(is.numeric(input$DE_class_twoCharHeatmap_pval)){
          p_cutoff <- input$DE_class_twoCharHeatmap_pval
          if(input$DE_class_twoCharHeatmap_pval > 1){
            p_cutoff <- 1
            shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_pval', value=1)
          }else if(input$DE_class_twoCharHeatmap_pval < 0.001){
            p_cutoff <- 0.001
            shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_pval', value=0.001)
          }
        }else{
          p_cutoff <- 0.05
          shiny::showNotification("The p-value must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
          shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_pval', value=0.05)
        }

        if(is.numeric(input$DE_class_twoCharHeatmap_fc)){
          FC_cutoff <- input$DE_class_twoCharHeatmap_fc
          if(input$DE_class_twoCharHeatmap_fc > 8){
            FC_cutoff <- 8
            shiny::showNotification("The Fold change (FC) must be between 1 and 8, so it is calculated by replacing it with 8.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_fc', value=8)
          }else if(input$DE_class_twoCharHeatmap_fc < 1){
            FC_cutoff <- 1
            shiny::showNotification("The Fold change (FC) must be between 1 and 8, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_fc', value=1)
          }
        }else{
          FC_cutoff <- 1
          shiny::showNotification("The Fold change (FC) must be numeric, so it is calculated by replacing it with 1.", type="warning")
          shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_fc',value=1)
        }
        variables$DE.class.twoCharHeatmap <- LipidSigR::heatmap_chain_db(
          processed_se=variables$DE.processed.SE, char=input$DE_class_analysis_char,
          char_feature=NULL, ref_group=variables$DE.ref.group,
          test=input$DE_class_twoCharHeatmap_post_hoc_method,
          significant=input$DE_class_twoCharHeatmap_sig_p,
          p_cutoff=p_cutoff,
          FC_cutoff=FC_cutoff, transform=input$DE_transformation)
        char <- variables$DE.processed.lipid.char.tab %>%
          dplyr::pull(input$DE_class_analysis_char) %>%
          na.omit() %>%
          unique()
        shiny::updateSelectInput(session, "DE_class_twoCharHeatmap_total_charFeature",
                                 choices=char,
                                 selected=char[1])
        shiny::updateSelectInput(session, "DE_class_twoCharHeatmap_each_charFeature",
                                 choices=char,
                                 selected=char[1])
        variables$DE.class.twoChar.total.download.log <- 1
        variables$DE.class.twoChar.each.download.log <- 1
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics two characteristics analysis error!",
          text=as.character(e$message),
          type="error")
      }
    )
  }) #isolate
}) #shiny::observeEvent(input$DE_class_cluster_start

#### Output: DE.class.twoCharHeatmap.total.heatmap ####
output$DE.class.twoCharHeatmap.total.heatmap <- shiny::renderPlot({
  shiny::validate(shiny::need(!is.null(variables$DE.class.twoCharHeatmap$total_chain$static_heatmap), "Without DE class twoCharHeatmap result."))
  variables$DE.class.twoCharHeatmap$total_chain$static_heatmap
}) #output$DE.class.twoCharHeatmap.total.heatmap <- renderPlot

shiny::observeEvent(input$DE.class.twoChar.total.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.class.twoChar.total.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.class.twoChar.total.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.class.twoChar.total.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.class.twoChar.total.heatmap.pdf'), width=8, height=6)
              print(variables$DE.class.twoCharHeatmap$total_chain$static_heatmap)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.class.twoCharHeatmap$total_chain$table_heatmap, file=file.path(temp_directory,'DE.class.twoChar.total.heatmap.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.class.twoChar.total.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.class.twoChar.total.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.class.twoChar.total.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics two characteristics analysis (Total FA) download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

#### Output: DE.class.twoCharHeatmap.total.table ####
output$DE.class.twoCharHeatmap.total.table <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$DE.class.twoCharHeatmap$total_chain$table_heatmap), "Without DE class twoCharHeatmap result."))
  if(input$DE_class_twoCharHeatmap_sig_p == 'p'){
    DT::datatable(variables$DE.class.twoCharHeatmap$total_chain$table_heatmap %>%
                    dplyr::select(-c(`p.signif`,`p.adj.signif`,star,color)) %>%
                    dplyr::arrange(dplyr::desc(sig_pval)),
                  escape=FALSE, selection='none', rownames=TRUE,
                  class="nowrap row-border",
                  extensions=c('Buttons', 'Scroller'),
                  options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                               deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                               dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                               columnDefs=list(list(className='dt-center', targets="_all")))) %>%
      DT::formatStyle('sig_pval',target='row',backgroundColor=DT::styleEqual(c("yes","no"), c('pink', '#EBECF0')))
  }else{
    DT::datatable(variables$DE.class.twoCharHeatmap$total_chain$table_heatmap %>%
                    dplyr::select(-c(`p.signif`,`p.adj.signif`,star,color)) %>%
                    dplyr::arrange(dplyr::desc(sig_padj)),
                  escape=FALSE, selection='none', rownames=TRUE,
                  class="nowrap row-border",
                  extensions=c('Buttons', 'Scroller'),
                  options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                               deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                               dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                               columnDefs=list(list(className='dt-center', targets="_all")))) %>%
      DT::formatStyle('sig_padj',target='row',backgroundColor=DT::styleEqual(c("yes","no"), c('pink', '#EBECF0')))
  }

}) #output$DE.class.twoCharHeatmap.total.table <- renderDataTable

#### Output: DE.class.twoCharHeatmap.each.heatmap ####
output$DE.class.twoCharHeatmap.each.heatmap <- shiny::renderPlot({
  shiny::validate(shiny::need(!is.null(variables$DE.class.twoCharHeatmap$each_chain$static_heatmap), "Without DE class twoCharHeatmap result."))
  variables$DE.class.twoCharHeatmap$each_chain$static_heatmap
}) #output$DE.class.twoCharHeatmap.each.heatmap <- renderPlot

shiny::observeEvent(input$DE.class.twoChar.each.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.class.twoChar.each.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.class.twoChar.each.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.class.twoChar.each.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.class.twoChar.each.heatmap.pdf'), width=8, height=6)
              print(variables$DE.class.twoCharHeatmap$each_chain$static_heatmap)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.class.twoCharHeatmap$each_chain$table_heatmap, file=file.path(temp_directory,'DE.class.twoChar.each.heatmap.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.class.twoChar.each.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.class.twoChar.each.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.class.twoChar.each.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics two characteristics analysis (Each FA) download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

#### Output: DE.class.twoCharHeatmap.each.table ####
output$DE.class.twoCharHeatmap.each.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.class.twoCharHeatmap$each_chain$table_heatmap), "Without DE class twoCharHeatmap result."))
  if(input$DE_class_twoCharHeatmap_sig_p == 'p'){
    DT::datatable(variables$DE.class.twoCharHeatmap$each_chain$table_heatmap %>%
                    dplyr::select(-c(`p.signif`,`p.adj.signif`,star,color)) %>%
                    dplyr::arrange(dplyr::desc(sig_pval)),
                  escape=FALSE, selection='none', rownames=TRUE,
                  class="nowrap row-border",
                  extensions=c('Buttons', 'Scroller'),
                  options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                               deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                               dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                               columnDefs=list(list(className='dt-center', targets="_all")))) %>%
      DT::formatStyle('sig_pval',target='row',backgroundColor=DT::styleEqual(c("yes","no"), c('pink', '#EBECF0')))
  }else{
    DT::datatable(variables$DE.class.twoCharHeatmap$each_chain$table_heatmap %>%
                    dplyr::select(-c(`p.signif`,`p.adj.signif`,star,color)) %>%
                    dplyr::arrange(dplyr::desc(sig_padj)),
                  escape=FALSE, selection='none', rownames=TRUE,
                  class="nowrap row-border",
                  extensions=c('Buttons', 'Scroller'),
                  options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                               deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                               dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                               columnDefs=list(list(className='dt-center', targets="_all")))) %>%
      DT::formatStyle('sig_padj',target='row',backgroundColor=DT::styleEqual(c("yes","no"), c('pink', '#EBECF0')))
  }
}) #output$DE.class.twoCharHeatmap.each.table <- renderDataTable

shiny::observeEvent(input$DE_class_twoCharHeatmap_total_charFeature, {
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$DE.class.twoCharHeatmap)){
          if(is.numeric(input$DE_class_twoCharHeatmap_pval)){
            p_cutoff <- input$DE_class_twoCharHeatmap_pval
            if(input$DE_class_twoCharHeatmap_pval > 1){
              p_cutoff <- 1
              shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_pval', value=1)
            }else if(input$DE_class_twoCharHeatmap_pval < 0.001){
              p_cutoff <- 0.001
              shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_pval', value=0.001)
            }
          }else{
            p_cutoff <- 0.05
            shiny::showNotification("The p-value must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_pval', value=0.05)
          }
          if(is.numeric(input$DE_class_twoCharHeatmap_fc)){
            FC_cutoff <- input$DE_class_twoCharHeatmap_fc
            if(input$DE_class_twoCharHeatmap_fc > 8){
              FC_cutoff <- 8
              shiny::showNotification("The Fold change (FC) must be between 1 and 8, so it is calculated by replacing it with 8.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_fc', value=8)
            }else if(input$DE_class_twoCharHeatmap_fc < 1){
              FC_cutoff <- 1
              shiny::showNotification("The Fold change (FC) must be between 1 and 8, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_fc', value=1)
            }
          }else{
            FC_cutoff <- 1
            shiny::showNotification("The Fold change (FC) must be numeric, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_fc',value=1)
          }
          variables$DE.class.twoCharHeatmap.total.char <- LipidSigR::heatmap_chain_db(
            processed_se=variables$DE.processed.SE, char=input$DE_class_analysis_char,
            char_feature=input$DE_class_twoCharHeatmap_total_charFeature,
            ref_group=variables$DE.ref.group,
            test=input$DE_class_twoCharHeatmap_post_hoc_method,
            significant=input$DE_class_twoCharHeatmap_sig_p,
            p_cutoff=p_cutoff,
            FC_cutoff=FC_cutoff, transform=input$DE_transformation)
          if(!is.list(variables$DE.class.twoCharHeatmap.total.char$total_chain)){
            shinyjs::hide('DE_class_twoCharHeatmap_total_box_div')
            shinyjs::disable('DE.class.twoChar.total.charFeature.download.start')
          }else{
            shinyjs::show('DE_class_twoCharHeatmap_total_box_div')
            shinyjs::enable('DE.class.twoChar.total.charFeature.download.start')
            char.class <- LipidSigR::extract_summarized_experiment(variables$DE.class.twoCharHeatmap.total.char$total_chain$chain_db_se)$abundance %>%
              dplyr::select(feature) %>% dplyr::pull()
            shiny::updateSelectInput(session, "DE_class_twoCharHeatmap_total_box_char",
                                     choices= char.class,
                                     selected=char.class[1])
            variables$DE.class.twoChar.total.charFeature.download.log <- 1
          }
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics two characteristics analysis (charFeature) error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE_class_twoCharHeatmap_each_charFeature,{
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$DE.class.twoCharHeatmap)){
          if(is.numeric(input$DE_class_twoCharHeatmap_pval)){
            p_cutoff <- input$DE_class_twoCharHeatmap_pval
            if(input$DE_class_twoCharHeatmap_pval > 1){
              p_cutoff <- 1
              shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_pval', value=1)
            }else if(input$DE_class_twoCharHeatmap_pval < 0.001){
              p_cutoff <- 0.001
              shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_pval', value=0.001)
            }
          }else{
            p_cutoff <- 0.05
            shiny::showNotification("The p-value must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_pval', value=0.05)
          }
          if(is.numeric(input$DE_class_twoCharHeatmap_fc)){
            FC_cutoff <- input$DE_class_twoCharHeatmap_fc
            if(input$DE_class_twoCharHeatmap_fc > 8){
              FC_cutoff <- 8
              shiny::showNotification("The Fold change (FC) must be between 1 and 8, so it is calculated by replacing it with 8.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_fc', value=8)
            }else if(input$DE_class_twoCharHeatmap_fc < 1){
              FC_cutoff <- 1
              shiny::showNotification("The Fold change (FC) must be between 1 and 8, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_fc', value=1)
            }
          }else{
            FC_cutoff <- 1
            shiny::showNotification("The Fold change (FC) must be numeric, so it is calculated by replacing it with 1.", type="warning")
            shiny::updateNumericInput(inputId='DE_class_twoCharHeatmap_fc',value=1)
          }
          variables$DE.class.twoCharHeatmap.each.char <- LipidSigR::heatmap_chain_db(
            processed_se=variables$DE.processed.SE, char=input$DE_class_analysis_char,
            char_feature=input$DE_class_twoCharHeatmap_each_charFeature,
            ref_group=variables$DE.ref.group,
            test=input$DE_class_twoCharHeatmap_post_hoc_method,
            significant=input$DE_class_twoCharHeatmap_sig_p,
            p_cutoff=p_cutoff,
            FC_cutoff=FC_cutoff, transform=input$DE_transformation)
          if(!is.list(variables$DE.class.twoCharHeatmap.each.char$each_chain)){
            shinyjs::hide('DE_class_twoCharHeatmap_each_box_div')
            shinyjs::disable('DE.class.twoChar.each.charFeature.download.start')
          }else{
            shinyjs::show('DE_class_twoCharHeatmap_each_box_div')
            shinyjs::enable('DE.class.twoChar.each.charFeature.download.start')
            char.class <- LipidSigR::extract_summarized_experiment(variables$DE.class.twoCharHeatmap.each.char$each_chain$chain_db_se)$abundance %>%
              dplyr::select(feature) %>% dplyr::pull()
            shiny::updateSelectInput(session, "DE_class_twoCharHeatmap_each_box_char",
                                     choices= char.class,
                                     selected=char.class[1])
            variables$DE.class.twoChar.each.charFeature.download.log <- 1
          }
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics two characteristics analysis (charFeature) error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE.class.twoChar.total.charFeature.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.class.twoChar.total.charFeature.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.class.twoChar.total.charFeature.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.class.twoChar.total.charFeature.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.class.twoChar.total.charFeature.heatmap.pdf'), width=8, height=6)
              print(variables$DE.class.twoCharHeatmap.total.char$total_chain$static_heatmap)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.class.twoCharHeatmap.total.char$total_chain$table_heatmap, file=file.path(temp_directory,'DE.class.twoChar.total.charFeature.heatmap.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.class.twoChar.total.charFeature.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.class.twoChar.total.charFeature.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.class.twoChar.total.charFeature.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics two characteristics analysis (charFeature) download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

#### Output: DE.class.twoCharHeatmap.total.charFeature.heatmap ####
output$DE.class.twoCharHeatmap.total.charFeature.heatmap <- shiny::renderPlot({
  shiny::validate(shiny::need(is.list(variables$DE.class.twoCharHeatmap.total.char$total_chain), 'This lipid characteristic or characteristic feature does not include any lipids for fatty acid chain analysis.'))
  shiny::validate(shiny::need(!is.null(variables$DE.class.twoCharHeatmap.total.char$total_chain$static_heatmap), "Without DE class twoCharHeatmap result."))
  variables$DE.class.twoCharHeatmap.total.char$total_chain$static_heatmap
}) #output$DE.class.twoCharHeatmap.total.charFeature.heatmap <- renderPlot

#### Output: DE.class.twoCharHeatmap.total.charFeature.table ####
output$DE.class.twoCharHeatmap.total.charFeature.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(is.list(variables$DE.class.twoCharHeatmap.total.char$total_chain), 'This lipid characteristic or characteristic feature does not include any lipids for fatty acid chain analysis.'))
  shiny::validate(shiny::need(!is.null(variables$DE.class.twoCharHeatmap.total.char$total_chain$table_heatmap), "Without DE class twoCharHeatmap result."))
  DT::datatable(variables$DE.class.twoCharHeatmap.total.char$total_chain$table_heatmap %>%
                  dplyr::select(-c(`p.signif`,`p.adj.signif`,star,color)),
                escape=FALSE, selection='none', rownames=TRUE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))

}) #output$DE.class.twoCharHeatmap.total.charFeature.table <- renderDataTable

shiny::observeEvent(input$DE.class.twoChar.each.charFeature.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.class.twoChar.each.charFeature.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.class.twoChar.each.charFeature.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.class.twoChar.each.charFeature.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.class.twoChar.each.charFeature.heatmap.pdf'), width=8, height=6)
              print(variables$DE.class.twoCharHeatmap.each.char$each_chain$static_heatmap)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.class.twoCharHeatmap.each.char$each_chain$table_heatmap, file=file.path(temp_directory,'DE.class.twoChar.each.charFeature.heatmap.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.class.twoChar.each.charFeature.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.class.twoChar.each.charFeature.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.class.twoChar.each.charFeature.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics two characteristics analysis (charFeature) error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

#### Output: DE.class.twoCharHeatmap.each.charFeature.heatmap ####
output$DE.class.twoCharHeatmap.each.charFeature.heatmap <- shiny::renderPlot({
  shiny::validate(shiny::need(is.list(variables$DE.class.twoCharHeatmap.each.char$each_chain), 'This lipid characteristic or characteristic feature does not include any lipids for fatty acid chain analysis.'))
  shiny::validate(shiny::need(!is.null(variables$DE.class.twoCharHeatmap.each.char$each_chain$static_heatmap), "Without DE class twoCharHeatmap result."))
  variables$DE.class.twoCharHeatmap.each.char$each_chain$static_heatmap
}) #output$DE.class.twoCharHeatmap.each.charFeature.heatmap <- renderPlot

#### Output: DE.class.twoCharHeatmap.each.charFeature.table ####
output$DE.class.twoCharHeatmap.each.charFeature.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(is.list(variables$DE.class.twoCharHeatmap.each.char$each_chain), 'This lipid characteristic or characteristic feature does not include any lipids for fatty acid chain analysis.'))
  shiny::validate(shiny::need(!is.null(variables$DE.class.twoCharHeatmap.each.char$each_chain$table_heatmap), "Without DE class twoCharHeatmap result."))
  DT::datatable(variables$DE.class.twoCharHeatmap.each.char$each_chain$table_heatmap %>%
                  dplyr::select(-c(`p.signif`,`p.adj.signif`,star,color)),
                escape=FALSE, selection='none', rownames=TRUE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$DE.class.twoCharHeatmap.each.charFeature.table <- renderDataTable

shiny::observeEvent(input$DE_class_twoCharHeatmap_total_box_char,{
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$DE.class.twoCharHeatmap.total.char)){
          if(variables$DE.Ngroup == 'two'){
            variables$DE.class.twoCharHeatmap.total.box <- LipidSigR::boxPlot_feature_twoGroup(
              variables$DE.class.twoCharHeatmap.total.char$total_chain$chain_db_se,
              feature=input$DE_class_twoCharHeatmap_total_box_char,
              ref_group=variables$DE.ref.group, test=input$DE_class_twoCharHeatmap_post_hoc_method,
              transform=input$DE_transformation)
          }else{
            variables$DE.class.twoCharHeatmap.total.box <- LipidSigR::boxPlot_feature_multiGroup(
              processed_se=variables$DE.class.twoCharHeatmap.total.char$total_chain$chain_db_se,
              feature=input$DE_class_twoCharHeatmap_total_box_char,
              ref_group=variables$DE.ref.group,
              test=input$DE_class_twoCharHeatmap_post_hoc_method,
              post_hoc_sig=input$DE_class_twoCharHeatmap_sig_p,
              transform=input$DE_transformation)
          }
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics two characteristics analysis (boxPlot) error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE.class.twoChar.total.boxplot.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.class.twoChar.total.boxplot.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.class.twoChar.total.boxplot.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.class.twoChar.total.boxplot.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.class.twoChar.total.boxplot.heatmap.pdf'), width=8, height=6)
              print(variables$DE.class.twoCharHeatmap.total.box$static_boxPlot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.class.twoCharHeatmap.total.box$table_boxplot, file=file.path(temp_directory,'DE.class.twoChar.total.boxplot.heatmap.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.class.twoChar.total.boxplot.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.class.twoChar.total.boxplot.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.class.twoChar.total.boxplot.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics two characteristics analysis (boxplot) download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

#### Output: DE.class.twoCharHeatmap.total.sig.boxplot ####
output$DE.class.twoCharHeatmap.total.sig.boxplot <- shiny::renderPlot({
  shiny::validate(shiny::need(!is.null(variables$DE.class.twoCharHeatmap.total.box$static_boxPlot), "Without DE class twoCharHeatmap result."))
  variables$DE.class.twoCharHeatmap.total.box$static_boxPlot
}) #output$DE.class.twoCharHeatmap.total.sig.boxplot <- renderPlot
#### Output: DE.class.twoCharHeatmap.total.sig.table ####
output$DE.class.twoCharHeatmap.total.sig.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.class.twoCharHeatmap.total.box$table_stat), "Without DE class twoCharHeatmap result."))
  DT::datatable(variables$DE.class.twoCharHeatmap.total.box$table_stat,
                escape=FALSE, selection='none', rownames=TRUE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$DE.class.twoCharHeatmap.total.sig.table <- renderDataTable


shiny::observeEvent(input$DE_class_twoCharHeatmap_each_box_char,{
  shiny::isolate({
    tryCatch(
      {
        if(!is.null(variables$DE.class.twoCharHeatmap.each.char)){
          if(variables$DE.Ngroup == 'two'){
            variables$DE.class.twoCharHeatmap.each.box <- LipidSigR::boxPlot_feature_twoGroup(
              variables$DE.class.twoCharHeatmap.each.char$each_chain$chain_db_se,
              feature=input$DE_class_twoCharHeatmap_each_box_char,
              ref_group=variables$DE.ref.group, test=input$DE_class_twoCharHeatmap_post_hoc_method,
              transform=input$DE_transformation)
          }else{
            variables$DE.class.twoCharHeatmap.each.box <- LipidSigR::boxPlot_feature_multiGroup(
              processed_se=variables$DE.class.twoCharHeatmap.each.char$each_chain$chain_db_se,
              feature=input$DE_class_twoCharHeatmap_each_box_char,
              ref_group=variables$DE.ref.group,
              test=input$DE_class_twoCharHeatmap_post_hoc_method,
              post_hoc_sig=input$DE_class_twoCharHeatmap_sig_p,
              transform=input$DE_transformation)
          }
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics two characteristics analysis (boxplot) error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$DE.class.twoChar.each.boxplot.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$DE.class.twoChar.each.boxplot.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$DE.class.twoChar.each.boxplot.download <- shiny::downloadHandler(
            filename=function(){
              paste("DE.class.twoChar.each.boxplot.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## plot ##
              grDevices::pdf(file.path(temp_directory,'DE.class.twoChar.each.boxplot.heatmap.pdf'), width=8, height=6)
              print(variables$DE.class.twoCharHeatmap.each.box$static_boxPlot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$DE.class.twoCharHeatmap.each.box$table_boxplot, file=file.path(temp_directory,'DE.class.twoChar.each.boxplot.heatmap.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$DE.class.twoChar.each.boxplot.download.log <- 2
          shinyjs::runjs("document.getElementById('DE.class.twoChar.each.boxplot.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('DE.class.twoChar.each.boxplot.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid characteristics two characteristics analysis (boxplot) download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})
#### Output: DE.class.twoCharHeatmap.each.sig.boxplot ####
output$DE.class.twoCharHeatmap.each.sig.boxplot <- shiny::renderPlot({
  shiny::validate(shiny::need(!is.null(variables$DE.class.twoCharHeatmap.each.box$static_boxPlot), "Without DE class twoCharHeatmap result."))
  variables$DE.class.twoCharHeatmap.each.box$static_boxPlot
}) #output$DE.class.twoCharHeatmap.each.sig.boxplot <- renderPlot
#### Output: DE.class.twoCharHeatmap.each.sig.table ####
output$DE.class.twoCharHeatmap.each.sig.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$DE.class.twoCharHeatmap.each.box$table_stat), "Without DE class twoCharHeatmap result."))
  DT::datatable(variables$DE.class.twoCharHeatmap.each.box$table_stat,
                escape=FALSE, selection='none', rownames=TRUE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$DE.class.twoCharHeatmap.each.sig.table <- renderDataTable
