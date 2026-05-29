################################
################################
######                    ######
######   DataCheck Page   ######
######                    ######
################################
################################

shinyjs::hide('PRO_tabPanel_div')
## NOTE (integrated app): URL-based auto-load via PostgreSQL has been removed.
## In the single-app version, error messages in each analysis tab show a
## client-side link (switchTab) that brings the user directly to Data Check.
## The user simply re-uploads their data here for a detailed examination.

#### Preload from other tabs (triggered by .reupload() when errors are detected) ####
shiny::observeEvent(variables$Check.preload.type, {
  shiny::req(!is.null(variables$Check.preload.type),
             !is.null(variables$Check.abundance))
  shiny::isolate({
    tryCatch({
      shinyjs::show('data_check_successful')
      shinyjs::show('Check_upload_raw_div')
      variables$Check.web <- check_web(
        exp_data        = variables$Check.abundance,
        group_info      = variables$Check.group.info,
        condition_table = variables$Check.cond.tab,
        adjusted_table  = variables$Check.adj.tab,
        analysis_type   = variables$Check.type,
        nGroup          = variables$Check.Ngroups)
      if (!is.null(variables$Check.web$lipid_char_process)) {
        variables$Check.web$lipid_char_process <-
          char.tab.url(variables$Check.web$lipid_char_process)
      }
      output$Check.web.div <- shiny::renderUI({
        shiny::isolate(variables$Check.web$return_div)
      })
      variables$Data.check.progress <- variables$Check.web$check_sum_div
      shinyjs::show('data_process_table_div')
      ## Reset flag so re-uploading to the same type triggers again next time
      variables$Check.preload.type <- NULL
    }, error = function(e) {
      shinyWidgets::show_alert(
        title = 'Data Check error',
        text  = htmltools::HTML(paste0(
          "<h4>", as.character(e$message), "</h4>",
          "<h4>Please check your data format via the <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank'>FAQ</a>.</h4>")),
        html  = TRUE, type = 'error')
    })
  })
})

#### Output: Data.check.progress ####
output$Data.check.progress <- shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$Data.check.progress), ""))
  variables$Data.check.progress
})

#### Output: Check.demo.download ####
output$Check.demo.download <- shiny::downloadHandler(
   filename=function() {
      "DataCheck_example_dataset.zip"
   },
   content=function(file) {
      file.copy("www/download_demo_dataset/DataCheck_example_dataset.zip", file)
   },
   contentType="application/zip"
)
shiny::outputOptions(output, "Check.demo.download", suspendWhenHidden=FALSE)

shiny::observe({
   if(variables$Check.type == "Profiling"){
      shiny::hideTab(inputId='Check_process_table_tab', target='Group information')
      shiny::hideTab(inputId='Check_process_table_tab', target='Condition table')
      shiny::hideTab(inputId='Check_process_table_tab', target='Adjusted table')
   }else if(variables$Check.type == "DE"){
      shiny::showTab(inputId='Check_process_table_tab', target='Group information')
      shiny::hideTab(inputId='Check_process_table_tab', target='Condition table')
      shiny::hideTab(inputId='Check_process_table_tab', target='Adjusted table')
   }else if(variables$Check.type == "ML"){
      shiny::hideTab(inputId='Check_process_table_tab', target='Group information')
      shiny::showTab(inputId='Check_process_table_tab', target='Condition table')
      shiny::hideTab(inputId='Check_process_table_tab', target='Adjusted table')
   }else if(variables$Check.type == "Correlation"){
      shiny::hideTab(inputId='Check_process_table_tab', target='Group information')
      shiny::showTab(inputId='Check_process_table_tab', target='Condition table')
      shiny::showTab(inputId='Check_process_table_tab', target='Adjusted table')
   }
})

#### control user upload button ####
shiny::observe({
   if(input$Check_type == "Profiling"){
      if(is.null(input$Check_exp)){
         shinyjs::disable("Check_upload")
      }else{
         shinyjs::enable("Check_upload")
      }
   }else if(input$Check_type == "DE"){
      if(is.null(input$Check_exp) | is.null(input$Check_group)){
         shinyjs::disable("Check_upload")
      }else{
         shinyjs::enable("Check_upload")
      }
   }else if(input$Check_type == "ML"){
      if(is.null(input$Check_exp) | is.null(input$Check_cond)){
         shinyjs::disable("Check_upload")
      }else{
         shinyjs::enable("Check_upload")
      }
   }else if(input$Check_type == "Correlation"){
      if(is.null(input$Check_exp) | is.null(input$Check_cond)){
         shinyjs::disable("Check_upload")
      }else{
         shinyjs::enable("Check_upload")
      }
   }
})

#### control user reset button ####
shiny::observeEvent(input$Check_reset, {
   
   shinyjs::reset('Check_reset_div')
   shinyjs::disable("Check_upload")
   shinyjs::hide('data_process_table_div')
   shinyjs::hide('data_check_successful')
   shinyjs::hide('Check_upload_raw_div')

   #### clear variables ####
   variables$Check.type             <- "Profiling"
   variables$check_step1            <- NULL
   variables$Check.abundance        <- NULL
   variables$Check.group.info       <- NULL
   variables$Check.cond.tab         <- NULL
   variables$Check.adj.tab          <- NULL
   variables$Check.web              <- NULL
   variables$Check.preload.type     <- NULL
   variables$Data.check.progress    <- htmltools::HTML('<div style="font-size: 0px;"><h2>Upload progress</h2><h4>(1/2) Check upload data format.</h4></div>')
   
}) #shiny::observeEvent(input$Check_reset

#### PRO user dataset ####
shiny::observeEvent(input$Check_demo_upload, {
    shiny::withProgress(message='Upload data', style='notification', detail="Upload data", value=0, {
      shinyjs::show('data_check_successful')
      shinyjs::show('Check_upload_raw_div')
      variables$Check.type=input$Check_type
      variables$Check.group.info=NULL
      variables$Check.cond.tab=NULL
      variables$Check.adj.tab=NULL
      if(variables$Check.type == "Profiling"){
         variables$Check.SE <- readRDS('www/demo_dataset/Profiling.rds')
      }else if(variables$Check.type == "DE"){
         if(input$Check_demoe_ngroup == 'two'){
            variables$Check.SE <- readRDS('www/demo_dataset/Profiling.rds')
         }else{
            variables$Check.SE <- readRDS('www/demo_dataset/DE_3group_before_process.rds')
         }
        variables$Check.group.info <- variables$Check.SE %>% 
          SummarizedExperiment::colData() %>% as.data.frame() %>% tibble::remove_rownames()
        #variables$Check.group.info <- .extract_df(variables$Check.SE, type='group') %>% tibble::remove_rownames()
      }else if(variables$Check.type == "ML"){
         variables$Check.SE <- readRDS('www/demo_dataset/ML.rds')
         variables$Check.cond.tab <- variables$Check.SE %>% 
           SummarizedExperiment::colData() %>% as.data.frame() %>% tibble::remove_rownames()
         #variables$Check.cond.tab <- .extract_df(variables$Check.SE, type='group') %>% tibble::remove_rownames()
      }else if(variables$Check.type == "Correlation"){
         variables$Check.SE <- readRDS('www/demo_dataset/Corr.rds')
         variables$Check.cond.tab <- variables$Check.SE %>% 
           SummarizedExperiment::colData() %>% as.data.frame() %>% 
           dplyr::select(sample_name, FEV1_FVC, Emphysema, Exacerbations) %>% 
           tibble::remove_rownames()
         variables$Check.adj.tab <- variables$Check.SE %>% 
           SummarizedExperiment::colData() %>% as.data.frame() %>% 
           dplyr::select(sample_name, Age, Sex, Smoking, BMI, FEV1) %>% 
           tibble::remove_rownames()
         #variables$Check.cond.tab <- .extract_df(variables$Check.SE, type='group') %>% dplyr::select(sample_name, FEV1_FVC, Emphysema, Exacerbations) %>% tibble::remove_rownames()
         #variables$Check.adj.tab <- .extract_df(variables$Check.SE, type='group') %>% dplyr::select(sample_name, Age, Sex, Smoking, BMI, FEV1) %>% tibble::remove_rownames()
      }
      variables$Check.abundance <- variables$Check.SE %>%
        SummarizedExperiment::assay() %>% as.data.frame() %>%
        tibble::rownames_to_column("feature")
      #variables$Check.abundance <- .extract_df(variables$Check.SE, type='abundance') %>% tibble::remove_rownames()
      shiny::incProgress(0.33, detail='Check data format')
      variables$Check.web <- check_web(exp_data=variables$Check.abundance,
                                       group_info=variables$Check.group.info,
                                       condition_table=variables$Check.cond.tab,
                                       adjusted_table=variables$Check.adj.tab,
                                       analysis_type=input$Check_type,
                                       nGroup=input$Check_demoe_ngroup)
      if(!is.null(variables$Check.web$lipid_char_process)){
        variables$Check.web$lipid_char_process <- char.tab.url(variables$Check.web$lipid_char_process)
        #for(i in 1:nrow(variables$Check.web$lipid_char_process)){
        #  variables$Check.web$lipid_char_process$LION.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$LION.ID[i],type='LION')
        #  variables$Check.web$lipid_char_process$LIPID.MAPS.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$LIPID.MAPS.ID[i],type='LIPIDMAPS')
        #  variables$Check.web$lipid_char_process$SwissLipids.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$SwissLipids.ID[i],type='SwissLipids')
        #  variables$Check.web$lipid_char_process$HMDB.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$HMDB.ID[i],type='HMDB')
        #  variables$Check.web$lipid_char_process$ChEBI.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$ChEBI.ID[i],type='ChEBI')
        #  variables$Check.web$lipid_char_process$KEGG.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$KEGG.ID[i],type='KEGG')
        #  variables$Check.web$lipid_char_process$PubChem.CID[i] <- .url_creat(variables$Check.web$lipid_char_process$PubChem.CID[i],type='PubChem')
        #  variables$Check.web$lipid_char_process$MetaNetX.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$MetaNetX.ID[i],type='MetaNetX')
        #  variables$Check.web$lipid_char_process$PlantFA.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$PlantFA.ID[i],type='PlantFA')
        #}
      }
      output$Check.web.div <- renderUI({
         shiny::isolate({
            variables$Check.web$return_div
         })
      })
      incProgress(0.33, detail='ID Conversion')
      variables$Data.check.progress <- variables$Check.web$check_sum_div
      shinyjs::show('data_process_table_div')
      incProgress(0.34, detail='Data check finish')
   })
}) #shiny::observeEvent(input$PRO_user_upload



#### PRO user dataset ####
shiny::observeEvent(input$Check_upload, {
  shiny::withProgress(message='Upload data', style='notification', detail="Upload data", value=0, {
    shinyjs::show('data_check_successful')
    shinyjs::show('Check_upload_raw_div')
    shiny::showNotification("Start uploading file...", type="message")
    variables$Check.type=input$Check_type
    #### import user dataset ####
    tryCatch(
      {
        ## exp_data
        if(grepl('.xlsx', input$Check_exp$datapath)){
          variables$Check.abundance <- readxl::read_excel(input$Check_exp$datapath, na=c('', 'NA', 'na')) %>% as.data.frame()
        }else{
          variables$Check.abundance <- data.table::fread(
            input$Check_exp$datapath, header=TRUE, stringsAsFactors=FALSE, check.names=FALSE, data.table=FALSE, na.strings=c('', 'NA', 'na'))
        }
        if(!is.null(input$Check_group)){
          if(grepl('.xlsx', input$Check_group$datapath)){
            variables$Check.group.info <- readxl::read_excel(input$Check_group$datapath, na=c('', 'NA', 'na')) %>% as.data.frame()
          }else{
            variables$Check.group.info <- data.table::fread(
              input$Check_group$datapath, header=TRUE, stringsAsFactors=FALSE, check.names=FALSE, data.table=FALSE, na.strings=c('', 'NA', 'na'))
          }
        }else{
          variables$Check.group.info <- NULL
        }
        if(!is.null(input$Check_cond)){
          if(grepl('.xlsx',input$Check_cond$datapath)){
            variables$Check.cond.tab <- readxl::read_excel(input$Check_cond$datapath,na=c('', 'NA','na')) %>% as.data.frame()
          }else{
            variables$Check.cond.tab <- data.table::fread(
              input$Check_cond$datapath, header=TRUE, stringsAsFactors=FALSE, check.names=FALSE, data.table=FALSE, na.strings=c('', 'NA', 'na'))
          }
        }else{
          variables$Check.cond.tab <- NULL
        }
        if(!is.null(input$Check_adj)){
          if(grepl('.xlsx',input$Check_adj$datapath)){
            variables$Check.adj.tab <- readxl::read_excel(input$Check_adj$datapath,na=c('', 'NA','na')) %>% as.data.frame()
          }else{
            variables$Check.adj.tab <- data.table::fread(
              input$Check_adj$datapath, header=TRUE, stringsAsFactors=FALSE, check.names=FALSE, data.table=FALSE, na.strings=c('', 'NA', 'na'))
          }
        }else{
          variables$Check.adj.tab <- NULL
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
    print(variables$Check.abundance)
    variables$Check.web <- check_web(exp_data=variables$Check.abundance,
                                     group_info=variables$Check.group.info,
                                     condition_table=variables$Check.cond.tab,
                                     adjusted_table=variables$Check.adj.tab,
                                     analysis_type=input$Check_type,
                                     nGroup=input$Check_ngroup)
    print(variables$Check.web$exp_data)
    tryCatch(
      {
        shiny::incProgress(0.33, detail='Check data format')
        if(!is.null(variables$Check.web$lipid_char_process)){
          variables$Check.web$lipid_char_process <- char.tab.url(variables$Check.web$lipid_char_process)
          #for(i in 1:nrow(variables$Check.web$lipid_char_process)){
          #  variables$Check.web$lipid_char_process$LION.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$LION.ID[i],type='LION')
          #  variables$Check.web$lipid_char_process$LIPID.MAPS.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$LIPID.MAPS.ID[i],type='LIPIDMAPS')
          #  variables$Check.web$lipid_char_process$SwissLipids.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$SwissLipids.ID[i],type='SwissLipids')
          #  variables$Check.web$lipid_char_process$HMDB.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$HMDB.ID[i],type='HMDB')
          #  variables$Check.web$lipid_char_process$ChEBI.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$ChEBI.ID[i],type='ChEBI')
          #  variables$Check.web$lipid_char_process$KEGG.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$KEGG.ID[i],type='KEGG')
          #  variables$Check.web$lipid_char_process$PubChem.CID[i] <- .url_creat(variables$Check.web$lipid_char_process$PubChem.CID[i],type='PubChem')
          #  variables$Check.web$lipid_char_process$MetaNetX.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$MetaNetX.ID[i],type='MetaNetX')
          #  variables$Check.web$lipid_char_process$PlantFA.ID[i] <- .url_creat(variables$Check.web$lipid_char_process$PlantFA.ID[i],type='PlantFA')
          #}
        }
        output$Check.web.div <- shiny::renderUI({
          shiny::isolate({
            variables$Check.web$return_div
          })
        })## renderUI # Check.web.div
        shiny::incProgress(0.33, detail='ID Conversion')
        variables$Data.check.progress <- variables$Check.web$check_sum_div
        shinyjs::show('data_process_table_div')
        shiny::incProgress(0.34, detail='Data check finish')
      },
      error=function(e) {
        shinyWidgets::show_alert(
          title='Error',
          text=htmltools::HTML("<h4>Detect unknown input data format errors.</h4>
                               <h4>For the correct data format guidelines, please refer to the <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank'>FAQ</a></h4>.
                               <h4>If you need further assistance, please email us your data.(<a href='mailto:bioinfomics.web@gmail.com' target='_blank' style='color: darkblue;'>bioinfomics.web@gmail.com</a>)</h4>"),
          html=TRUE,type='error')
        return()
      }
    )
  })
}) #shiny::observeEvent(input$PRO_user_upload



shiny::observe({
  if(is.null(variables$Check.web$nonParseable_lipid)){
    shiny::hideTab(inputId='Check_process_table_tab', target='Unrecognized lipid')
  }else{
    shiny::showTab(inputId='Check_process_table_tab', target='Unrecognized lipid')
  }
})

shiny::observe({
  if(is.null(variables$Check.web$exp_naming_error)){
    shiny::hideTab(inputId='Check_process_table_tab', target='Error lipid')
  }else{
    shiny::showTab(inputId='Check_process_table_tab', target='Error lipid')
  }
})

#### Output: Check.exp.raw ####
output$Check.exp.raw <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$Check.web$exp_data), "Some error is in your expression data, please check your data and re-upload it."))
  DT::datatable(variables$Check.web$exp_data,
                escape=FALSE, selection='none', rownames=FALSE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller','FixedColumns'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                               deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                               dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                               fixedColumns=list(leftColumns=1),
                               columnDefs=list(list(className='dt-center', targets="_all"))))
})

#### Output: Check.exp.tran ####
output$Check.exp.tran <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$Check.web$exp_data_process), ""))
  DT::datatable(variables$Check.web$exp_data_process %>% 
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

#### Output: Check.nonParseable.lipid ####
output$Check.nonParseable.lipid <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$Check.web$nonParseable_lipid), ""))
  DT::datatable(variables$Check.web$nonParseable_lipid %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)), 
                escape=FALSE, selection='none', rownames=FALSE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                               deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                               dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                               columnDefs=list(list(className='dt-center', targets="_all"))))
})

#### Output: Check.naming.error.lipid ####
output$Check.naming.error.lipid <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$Check.web$exp_naming_error), "No error lipid."))
  DT::datatable(variables$Check.web$exp_naming_error %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)), 
                escape=FALSE, selection='none', rownames=FALSE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                               deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                               dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                               columnDefs=list(list(className='dt-center', targets="_all"))))
})

#### Output: Check.lipid.char.tran ####
output$Check.lipid.char.tran <- DT::renderDataTable(server=FALSE,{
  shiny::validate( shiny::need(!is.null(variables$Check.web$lipid_char_process), ""))
  DT::datatable(variables$Check.web$lipid_char_process %>% 
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

#### Output: Check.lipid.id ####
output$Check.lipid.id <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$Check.web$lipid_char_process), ""))
  DT::datatable(variables$Check.web$lipid_char_process %>% 
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

#### Output: Check.group.info.tran ####
output$Check.group.info.tran <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$Check.web$group_info_process), ""))
  DT::datatable(variables$Check.web$group_info_process %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)), 
                escape=FALSE, selection='none', rownames=FALSE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                               deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                               dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                               columnDefs=list(list(className='dt-center', targets="_all"))))
})

#### Output: Check.cond.tran ####
output$Check.cond.tran <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$Check.web$condition_table_process), ""))
  DT::datatable(variables$Check.web$condition_table_process %>% 
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

#### Output: Check.adj.tran ####
output$Check.adj.tran <- DT::renderDataTable(server=FALSE,{
  shiny::validate(shiny::need(!is.null(variables$Check.web$adjusted_table_process), ""))
  DT::datatable(variables$Check.web$adjusted_table_process %>% 
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