################################
################################
######                    ######
######   Enrichment Page  ######
######                    ######
################################
################################


#### Output: Enrichment.demo.download ####
output$Enrichment.demo.download <- shiny::downloadHandler(
   filename=function() {
      "Enrichment_example.zip"
   },
   content=function(file) {
      file.copy("www/download_demo_dataset/Enrichment.zip", file)
   },
   contentType="application/zip"
)
shiny::outputOptions(output, "Enrichment.demo.download", suspendWhenHidden=FALSE)

## NOTE (integrated app): URL-based preload via getQueryString() removed.
## DE → Enrichment handoff is handled directly in server_DE.R via reactiveValues.

#### control user reset button ####
shiny::observeEvent(input$Enrichment_reset, {

   shinyjs::reset('Enrichment_reset_div')
   shinyjs::hide('Enrichment_progress_div')
   shinyjs::hide('Enrichment_tabPanel_div')

   #### clear variables ####
   variables$deSp.se                       <- NULL
   variables$nGroups                       <- NULL
   variables$ORA.result                    <- NULL
   variables$ORA.each.result               <- NULL
   variables$Enrichment.lsea.all.result    <- NULL
   variables$Enrichment.lsea.each.result   <- NULL
   variables$Enrichment.ORA.all.download.log  <- 1
   variables$Enrichment.ORA.each.download.log <- 1
   variables$Enrichment.LSEA.all.download.log <- 1
   variables$Enrichment.LSEA.each.download.log <- 1

}) #shiny::observeEvent(input$Enrichment_reset

shiny::observeEvent(input$Enrichment_demo_start,{
  shiny::isolate({
    shinyjs::show('Enrichment_progress_div')
    if(input$Enrichment_source == "Enrichment_demo_data"){
      variables$deSp.se <- readRDS('www/demo_dataset/deSp_twoGroup.rds')
      variables$nGroups <- 'two'
    }else if(input$Enrichment_source == "Enrichment_demo_Multi_data"){
      variables$deSp.se <- readRDS('www/demo_dataset/deSp_multiGroup.rds')
      variables$nGroups <- 'multiple'
    }
    variables$Enrichment.checkEnrichmentInput <- check.despse(variables$deSp.se, variables$nGroups)
    if(!grepl('xmark icon',variables$Enrichment.checkEnrichmentInput)){
      shinyjs::show('Enrichment_start_div')
      variables$Enrichment.processed.abundance <- as.data.frame(SummarizedExperiment::assay(variables$deSp.se)) %>%
        tibble::rownames_to_column('feature')
      variables$Enrichment.processed.group.info <- as.data.frame(SummarizedExperiment::colData(variables$deSp.se))
      variables$Enrichment.processed.lipid.char <- as.data.frame(SummarizedExperiment::rowData(variables$deSp.se))
      variables$Enrichment.processed.lipid.char <- char.tab.url(variables$Enrichment.processed.lipid.char)
      variables$Enrichment.de.result <- S4Vectors::metadata(variables$deSp.se)$all_deSp_result
      variables$Enrichment.raw.abundance <- S4Vectors::metadata(variables$deSp.se)$processed_abundance %>%
        tibble::column_to_rownames('feature')
      
      variables$Enrichment.processed.SE <- SummarizedExperiment::SummarizedExperiment(
        assays=list(abundance=as.matrix(variables$Enrichment.raw.abundance)),
        rowData=S4Vectors::DataFrame(variables$Enrichment.processed.lipid.char, row.names=variables$Enrichment.processed.lipid.char$feature),
        colData=variables$Enrichment.processed.group.info)
      variables$Enrichment.lipid.char <- data.frame(aspect=names(LipidSigR::list_lipid_char(variables$Enrichment.processed.SE)$common_list),
                                             characteristic=LipidSigR::list_lipid_char(variables$Enrichment.processed.SE)$common_list) %>%
        dplyr::mutate(aspect=factor(aspect),
                      characteristic=factor(characteristic)) %>% 
        dplyr::arrange(characteristic)
    }else{
      shinyjs::hide('Enrichment_start_div')
    }
  })
})

shiny::observeEvent(input$Enrichment_url_start, {
  shiny::isolate({
    ## Integrated app: deSp_se is set directly by the DE tab handler;
    ## skip file read and proceed straight to the analysis setup.
    if(is.null(variables$deSp.se)){
      shinyWidgets::sendSweetAlert(
        session=session, title="No DE results found",
        text="Please run Differential Expression analysis first, then click the Enrichment button in the result download tab.",
        type="warning")
      return()
    }
    shinyjs::show('Enrichment_progress_div')
    tryCatch(
      {
        if('FC_cutoff' %in% names(S4Vectors::metadata(variables$deSp.se))){
          variables$nGroups <- 'two'
        }else{
          variables$nGroups <- 'multiple'
        }
        variables$Enrichment.checkEnrichmentInput <- check.despse(variables$deSp.se, variables$nGroups)
        if(!grepl('xmark icon',variables$Enrichment.checkEnrichmentInput)){
          shinyjs::show('Enrichment_start_div')
          variables$Enrichment.processed.abundance <- as.data.frame(SummarizedExperiment::assay(variables$deSp.se)) %>%
            tibble::rownames_to_column('feature')
          variables$Enrichment.processed.group.info <- as.data.frame(SummarizedExperiment::colData(variables$deSp.se))
          variables$Enrichment.processed.lipid.char <- as.data.frame(SummarizedExperiment::rowData(variables$deSp.se))
          variables$Enrichment.processed.lipid.char <- char.tab.url(variables$Enrichment.processed.lipid.char)
          variables$Enrichment.de.result <- S4Vectors::metadata(variables$deSp.se)$all_deSp_result
          variables$Enrichment.raw.abundance <- S4Vectors::metadata(variables$deSp.se)$processed_abundance %>%
            tibble::column_to_rownames('feature')
          variables$Enrichment.processed.SE <- SummarizedExperiment::SummarizedExperiment(
            assays=list(abundance=as.matrix(variables$Enrichment.raw.abundance)),
            rowData=S4Vectors::DataFrame(variables$Enrichment.processed.lipid.char, row.names=variables$Enrichment.processed.lipid.char$feature),
            colData=variables$Enrichment.processed.group.info)
          variables$Enrichment.lipid.char <- data.frame(aspect=names(LipidSigR::list_lipid_char(variables$Enrichment.processed.SE)$common_list),
                                                        characteristic=LipidSigR::list_lipid_char(variables$Enrichment.processed.SE)$common_list) %>%
            dplyr::mutate(aspect=factor(aspect),
                          characteristic=factor(characteristic)) %>% 
            dplyr::arrange(characteristic)
        }else{
          shinyjs::hide('Enrichment_start_div')
        }
      },
      error=function(e) {
        shinyWidgets::show_alert(
          title='Error',
          text=HTML("<h4>Detect unknown input data format errors.</h4>
                               <h4>For the correct data format guidelines, please refer to the <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank'>FAQ</a></h4>.
                               <h4>If you need further assistance, please email us your data.(<a href='mailto:bioinfomics.web@gmail.com' target='_blank' style='color: darkblue;'>bioinfomics.web@gmail.com</a>)</h4>"),
          html=TRUE,
          type ='error')
        return()
      }
    )
  })
})

shiny::observeEvent(input$Enrichment_upload,{
   shiny::isolate({
      shinyjs::show('Enrichment_progress_div')
      tryCatch(
         {
            #### import user dataset ####
           variables$deSp.se <- readRDS(input$Enrichment_deSpecies$datapath)
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
            if('FC_cutoff' %in% names(S4Vectors::metadata(variables$deSp.se))){
               variables$nGroups <- 'two'
            }else{
               variables$nGroups <- 'multiple'
            }
           variables$Enrichment.checkEnrichmentInput <- check.despse(variables$deSp.se, variables$nGroups)
           if(!grepl('xmark icon',variables$Enrichment.checkEnrichmentInput)){
             shinyjs::show('Enrichment_start_div')
             variables$Enrichment.processed.abundance <- as.data.frame(SummarizedExperiment::assay(variables$deSp.se)) %>%
               tibble::rownames_to_column('feature')
             variables$Enrichment.processed.group.info <- as.data.frame(SummarizedExperiment::colData(variables$deSp.se))
             variables$Enrichment.processed.lipid.char <- as.data.frame(SummarizedExperiment::rowData(variables$deSp.se))
             variables$Enrichment.processed.lipid.char <- char.tab.url(variables$Enrichment.processed.lipid.char)
             variables$Enrichment.de.result <- S4Vectors::metadata(variables$deSp.se)$all_deSp_result
             variables$Enrichment.raw.abundance <- S4Vectors::metadata(variables$deSp.se)$processed_abundance %>%
               tibble::column_to_rownames('feature')
             variables$Enrichment.processed.SE <- SummarizedExperiment::SummarizedExperiment(
               assays=list(abundance=as.matrix(variables$Enrichment.raw.abundance)),
               rowData=S4Vectors::DataFrame(variables$Enrichment.processed.lipid.char, row.names=variables$Enrichment.processed.lipid.char$feature),
               colData=variables$Enrichment.processed.group.info)
             variables$Enrichment.lipid.char <- data.frame(aspect=names(LipidSigR::list_lipid_char(variables$Enrichment.processed.SE)$common_list),
                                                           characteristic=LipidSigR::list_lipid_char(variables$Enrichment.processed.SE)$common_list) %>%
               dplyr::mutate(aspect=factor(aspect),
                             characteristic=factor(characteristic)) %>% 
               dplyr::arrange(characteristic)
           }else{
             shinyjs::hide('Enrichment_start_div')
           }
         },
         error=function(e) {
           shinyWidgets::show_alert(
               title='Error',
               text=HTML("<h4>Detect unknown input data format errors.</h4>
                               <h4>For the correct data format guidelines, please refer to the <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank'>FAQ</a></h4>.
                               <h4>If you need further assistance, please email us your data.(<a href='mailto:bioinfomics.web@gmail.com' target='_blank' style='color: darkblue;'>bioinfomics.web@gmail.com</a>)</h4>"),
               html=TRUE,
               type ='error')
            return()
         }
      )
   })
})

#### Output: Enrichment.checkEnrichmentInput ####
output$Enrichment.checkEnrichmentInput <- renderUI({
  validate(need(!is.null(variables$Enrichment.checkEnrichmentInput), ""))
  variables$Enrichment.checkEnrichmentInput
})

#### Output: Enrichment_ORA_char ####
output$Enrichment.ORA.char <- shiny::renderUI({
  shiny::selectInput(
    inputId='Enrichment_ORA_char', label='Select the lipid characteristics:',
    choices=split(as.list(levels(variables$Enrichment.lipid.char$characteristic)), variables$Enrichment.lipid.char$aspect),
    selected='class', multiple=FALSE)
})

#### Output: Enrichment.LSEA.char ####
output$Enrichment.LSEA.char <- shiny::renderUI({
  shiny::selectInput(
    inputId='Enrichment_LSEA_char', label='Select the lipid characteristics:',
    choices=split(as.list(levels(variables$Enrichment.lipid.char$characteristic)), variables$Enrichment.lipid.char$aspect),
    selected='class', multiple=FALSE)
})

#### Output: Enrichment_ORA_char ####
output$Enrichment.ORA.char <- shiny::renderUI({
  shiny::selectInput(
    inputId='Enrichment_ORA_char', label='Characteristics:',
    choices=split(as.list(levels(variables$Enrichment.lipid.char$characteristic)), variables$Enrichment.lipid.char$aspect),
    selected='class', multiple=FALSE)
})

shiny::observe({
  if(!is.null(variables$nGroups)){
    if(variables$nGroups == 'two'){
      shiny::updateRadioButtons(session, 'Enrichment_enrich_LSEA_rank_by',
                                choices=c('log2 fold change (FC)'='log2FC',
                                          'p-value'='pval',
                                          'adjusted p-value (padj)'='padj'),
                                selected='log2FC')
      shinyjs::show('Enrichment_ORA_sig_FC_div')
    }else if(variables$nGroups == 'multiple'){
      shiny::updateRadioButtons(session, 'Enrichment_enrich_LSEA_rank_by',
                                choices=c('p-value'='pval',
                                          'adjusted p-value (padj)'='padj'),
                                selected='pval')
      shinyjs::hide('Enrichment_ORA_sig_FC_div')
    }
  }
})

#### Output: Enrichment.processed.abundance ####
output$Enrichment.processed.abundance <- DT::renderDataTable(server=FALSE, {
   shiny::validate(shiny::need(!is.null(variables$Enrichment.processed.abundance), "Some error is in your deSpecies data, please check your data and re-upload it."))
   DT::datatable(variables$Enrichment.processed.abundance,
                 escape=FALSE, selection='none', rownames=FALSE,
                 class="nowrap row-border",
                 extensions=c('Buttons', 'Scroller'),
                 options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                                deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                                dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                                columnDefs=list(list(className='dt-center', targets="_all"))))
})

#### Output: Enrichment.processed.group.info ####
output$Enrichment.processed.group.info <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$Enrichment.processed.group.info), "Some error is in your deSpecies data, please check your data and re-upload it."))
  DT::datatable(variables$Enrichment.processed.group.info,
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})

#### Output: Enrichment.processed.lipid.char ####
output$Enrichment.processed.lipid.char <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$Enrichment.processed.lipid.char), "Some error is in your deSpecies data, please check your data and re-upload it."))
  DT::datatable(variables$Enrichment.processed.lipid.char %>% 
                  dplyr::select(-c(LION.ID,LIPID.MAPS.ID,SwissLipids.ID,HMDB.ID,ChEBI.ID,KEGG.ID,LipidBank.ID,PubChem.CID,MetaNetX.ID,PlantFA.ID)) %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 5)), 
                escape=FALSE, selection='none', rownames=FALSE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller','FixedColumns'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             fixedColumns=list(leftColumns=1)))
})

#### Output: Enrichment.lipid.id ####
output$Enrichment.lipid.id <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$Enrichment.processed.lipid.char), "Some error is in your deSpecies data, please check your data and re-upload it."))
  DT::datatable(variables$Enrichment.processed.lipid.char %>% 
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

#### Output: Enrichment.lipid.id ####
output$Enrichment.de.result <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$Enrichment.de.result), "Some error is in your deSpecies data, please check your data and re-upload it."))
  DT::datatable(variables$Enrichment.de.result,
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets="_all"))))
})

shiny::observeEvent(input$Enrichment_start,{
   isolate({
      shinyjs::show('Enrichment_tabPanel_div')
   })
})


#### control user reset button ####
shiny::observeEvent(input$Enrichment_ORA_reset, {

   shinyjs::reset('Enrichment_ORA_analysis_reset_div')
   shinyjs::hide('Enrichment_ORA_analysis_result_div')

   #### clear variables ####
   variables$Enrichment.ora.all.result <- NULL
   variables$Enrichment.ora.each.result <- NULL
   variables$Enrichment.ORA.all.download.log <- 1

}) #shiny::observeEvent(input$Enrichment_ORA_reset

shiny::observeEvent(input$Enrichment_ORA_start, {
   shiny::isolate({
     tryCatch(
       {
         enrich_pvalue <- input$Enrichment_ORA_enrich_pvalue
         if(is.numeric(input$Enrichment_ORA_enrich_pvalue)){
           if(input$Enrichment_ORA_enrich_pvalue > 1){
             enrich_pvalue <- 1
             shiny::showNotification("The p-value of significantly enriched lipid characteristics must be between 0.001 and 1, so it is calculated by replacing it with 0.05.", type="warning")
             shiny::updateNumericInput(inputId='Enrichment_ORA_enrich_pvalue', value=1)
           }else if(input$Enrichment_ORA_enrich_pvalue < 0.001){
             enrich_pvalue <- 0.001
             shiny::showNotification("The p-value of significantly enriched lipid characteristics must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
             shiny::updateNumericInput(inputId='Enrichment_ORA_enrich_pvalue', value=0.001)
           }
         }else{
           enrich_pvalue <- 0.05
           shiny::showNotification("The p-value of significantly enriched lipid characteristics must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
           shiny::updateNumericInput(inputId='Enrichment_ORA_enrich_pvalue', value=0.05)
         }
         
         variables$Enrichment.ora.all.result <- LipidSigR::enrichment_ora(
           deSp_se=variables$deSp.se, char=NULL, significant=input$Enrichment_ORA_enrich_stat, p_cutoff=enrich_pvalue)
         variables$Enrichment.ora.each.result <- LipidSigR::enrichment_ora(
           deSp_se=variables$deSp.se, char='class', significant=input$Enrichment_ORA_enrich_stat, p_cutoff=enrich_pvalue)
         variables$Enrichment.ORA.all.download.log <- 1
         variables$Enrichment.ORA.each.download.log <- 1
         shinyjs::show('Enrichment_ORA_analysis_result_div')
       },
       error=function(e) {
         shinyWidgets::sendSweetAlert(
           session=session, title="Over representation analysis error!",
           text=as.character(e$message),
           type="error")
         #### shinyjs show/hide results ####
         shinyjs::hide('Enrichment_ORA_analysis_result_div')
       }
     )
   })
})

shiny::observeEvent(input$Enrichment.ORA.all.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$Enrichment.ORA.all.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$Enrichment.ORA.all.download <- shiny::downloadHandler(
            filename=function(){
              paste("Enrichment.ORA.download.zip", sep="")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## UMAP plot ##
              grDevices::pdf(file.path(temp_directory,'barplot.pdf'), width=8, height=6)
              print(variables$Enrichment.ora.all.result$static_barPlot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$Enrichment.ora.all.result$table_barPlot, file=file.path(temp_directory,'barplot.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$Enrichment.ORA.all.download.log <- 2
          shinyjs::runjs("document.getElementById('Enrichment.ORA.all.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('Enrichment.ORA.all.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Over representation analysis download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) ## observeEvent #Enrichment.ORA.all.download.start


shiny::observeEvent(input$Enrichment_ORA_char,{
   shiny::isolate({
     tryCatch(
       {
         enrich_pvalue <- input$Enrichment_ORA_enrich_pvalue
         if(is.numeric(input$Enrichment_ORA_enrich_pvalue)){
           if(input$Enrichment_ORA_enrich_pvalue > 1){
             enrich_pvalue <- 1
             shiny::showNotification("The p-value of significantly enriched lipid characteristics must be between 0.001 and 1, so it is calculated by replacing it with 0.05.", type="warning")
             shiny::updateNumericInput(inputId='Enrichment_ORA_enrich_pvalue', value=1)
           }else if(input$Enrichment_ORA_enrich_pvalue < 0.001){
             enrich_pvalue <- 0.001
             shiny::showNotification("The p-value of significantly enriched lipid characteristics must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
             shiny::updateNumericInput(inputId='Enrichment_ORA_enrich_pvalue', value=0.001)
           }
         }else{
           enrich_pvalue <- 0.05
           shiny::showNotification("The p-value of significantly enriched lipid characteristics must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
           shiny::updateNumericInput(inputId='Enrichment_ORA_enrich_pvalue', value=0.05)
         }
         variables$Enrichment.ora.each.result <- LipidSigR::enrichment_ora(
           deSp_se=variables$deSp.se, 
           char=input$Enrichment_ORA_char, 
           significant=input$Enrichment_ORA_enrich_stat,
           p_cutoff=enrich_pvalue)
         variables$Enrichment.ORA.each.download.log <- 1
       },
       error=function(e) {
         shinyWidgets::sendSweetAlert(
           session=session, title="Over representation analysis characteristic error!",
           text=as.character(e$message),
           type="error")
         #### shinyjs show/hide results ####
         shinyjs::hide('PRO_dim_redu_result_div')
       }
     )
   })
})

shiny::observeEvent(input$Enrichment.ORA.each.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$Enrichment.ORA.each.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$Enrichment.ORA.each.download <- shiny::downloadHandler(
            filename=function(){
              paste("Enrichment.ORA.",input$Enrichment_ORA_char,".download.zip")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## UMAP plot ##
              grDevices::pdf(file.path(temp_directory, paste0(input$Enrichment_ORA_char,'.barplot.pdf')), width=8, height=6)
              print(variables$Enrichment.ora.each.result$static_barPlot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$Enrichment.ora.each.result$table_barPlot, file=file.path(temp_directory,paste0(input$Enrichment_ORA_char,'.barplot.csv')))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$Enrichment.ORA.each.download.log <- 2
          shinyjs::runjs("document.getElementById('Enrichment.ORA.each.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('Enrichment.ORA.each.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Over representation analysis characteristic download error!",
          text=as.character(e$message),
          type="error")
        #### shinyjs show/hide results ####
        shinyjs::hide('PRO_dim_redu_result_div')
      }
    )
  })
}) ## observeEvent #Enrichment.ORA.each.download.start

#### Output: Enrichment.all.bar ####
output$Enrichment.ORA.all.bar <- plotly::renderPlotly({
   shiny::validate(shiny::need(!is.null(variables$Enrichment.ora.all.result$interactive_barPlot), "Without bar plot"))
   variables$Enrichment.ora.all.result$interactive_barPlot
}) #output$Enrichment.all.bar <- renderPlotly

#### Output: Enrichment.each.bar ####
output$Enrichment.ORA.each.bar <- plotly::renderPlotly({
   shiny::validate(shiny::need(!is.null(variables$Enrichment.ora.each.result$interactive_barPlot), "Without bar plot"))
   variables$Enrichment.ora.each.result$interactive_barPlot
}) #output$Enrichment.each.bar <- renderPlotly

#### Output: Enrichment.all.tab ####
output$Enrichment.ORA.all.tab <- DT::renderDataTable(server=FALSE,{
   shiny::validate(shiny::need(!is.null(variables$Enrichment.ora.all.result$enrich_result), "Without table."))
   DT::datatable(variables$Enrichment.ora.all.result$enrich_result %>% 
                   dplyr::mutate_if(is.numeric, ~round(., 5)),
                 escape=FALSE, selection='none', rownames=FALSE,
                 class="nowrap row-border",
                 extensions=c('Buttons', 'Scroller'),
                 options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                                deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                                dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                                columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$Enrichment.all.tab <- renderDataTable

#### control user reset button ####
shiny::observeEvent(input$Enrichment_LSEA_reset, {

   shinyjs::reset('Enrichment_LSEA_analysis_reset_div')
   shinyjs::hide('Enrichment_LSEA_analysis_result_div')

   #### clear variables ####
   variables$LSEA.result <-NULL
}) #shiny::observeEvent(input$Enrichment_LSEA_reset

shiny::observeEvent(input$Enrichment_LSEA_start, {
   shiny::isolate({
     tryCatch(
       {
         enrich_pvalue <- input$Enrichment_LSEA_enrich_pvalue
         if(is.numeric(input$Enrichment_LSEA_enrich_pvalue)){
           if(input$Enrichment_LSEA_enrich_pvalue > 1){
             enrich_pvalue <- 1
             shiny::showNotification("The p-value of significantly enriched lipid characteristics lipids must be between 0.001 and 1, so it is calculated by replacing it with 0.05.", type="warning")
             shiny::updateNumericInput(inputId='Enrichment_LSEA_enrich_pvalue', value=1)
           }else if(input$Enrichment_LSEA_enrich_pvalue < 0.001){
             enrich_pvalue <- 0.001
             shiny::showNotification("The p-value of significantly enriched lipid characteristics must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
             shiny::updateNumericInput(inputId='Enrichment_LSEA_enrich_pvalue', value=0.001)
           }
         }else{
           enrich_pvalue <- 0.05
           shiny::showNotification("The p-value of significantly enriched lipid characteristics lipids must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
           shiny::updateNumericInput(inputId='Enrichment_LSEA_enrich_pvalue', value=0.05)
         }
         variables$Enrichment.lsea.all.result <- LipidSigR::enrichment_lsea(
           deSp_se=variables$deSp.se, 
           char=NULL, rank_by=input$Enrichment_enrich_LSEA_rank_by,
           significant=input$Enrichment_LSEA_enrich_stat,
           p_cutoff=enrich_pvalue)
         ## color map
         variables$asp.type <- data.frame(classes=c('Lipid classification', 'Cellular component', 
                                                    'Physical or chemical properties', 
                                                    'Fatty acid properties', 'Function'), 
                                          colour=c("#66C2A5", "#E78AC3", "#FC8D62", "#8DA0CB", "#FFD92F"), 
                                          stringsAsFactors=FALSE)
         variables$asp.colormap <- c(`Lipid classification`='#66C2A5', 
                                     `Cellular component`='#E78AC3', 
                                     `Physical or chemical properties`='#FC8D62', 
                                     `Fatty acid properties`='#8DA0CB', 
                                     `Function`='#FFD92F')
         variables$Enrichment.lsea.each.result <- LipidSigR::enrichment_lsea(
           deSp_se=variables$deSp.se, 
           char='class', rank_by=input$Enrichment_enrich_LSEA_rank_by,
           significant=input$Enrichment_LSEA_enrich_stat,
           p_cutoff=enrich_pvalue)
         variables$sig.type <- data.frame(classes=c('Up', 'Down', 'NS'),
                                          colour=c("#FF4500", "#4169E1", "#DDDDDD"),
                                          stringsAsFactors=FALSE)
         variables$sig.colormap <- c(Up='#FF4500', Down='#4169E1',NS='#DDDDDD')
         variables$Enrichment.LSEA.all.download.log <- 1
         variables$Enrichment.LSEA.each.download.log <- 1
         shinyjs::show('Enrichment_LSEA_analysis_result_div')
       },
       error=function(e) {
         shinyWidgets::sendSweetAlert(
           session=session, title="Lipid set enrichment analysis error!",
           text=as.character(e$message),
           type="error")
         #### shinyjs show/hide results ####
         shinyjs::hide('Enrichment_LSEA_analysis_result_div')
       }
     )
   })
})

#### Output: Enrichment.LSEA.all.bar ####
output$Enrichment.LSEA.all.bar <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$Enrichment.lsea.all.result), ""))
  plotly::plot_ly(variables$Enrichment.lsea.all.result$table_barPlot, x=~NES, y=~yText,
                  type='bar', color=~aspect,
                  colors=variables$asp.colormap[variables$asp.type$classes],
                  orientation='h', key=~yText, source="LSEA.all.bar", hovertext=~hover) %>%
    plotly::layout(title=list(font=list(size=20), text=''),
                   xaxis=list(title='NES', titlefont=list(size=16), tickfont=list(size=14)),
                   yaxis=list(title='', titlefont=list(size=16), tickfont=list(size=14)),
                   showlegend=TRUE,
                   legend=list(font=list(size=12), title=list(text='aspect'), y=-0.15, x=-0.2, orientation='h'),
                   margin=list(l=70, r=20, b=20, t =40))
}) #output$DE.species.maplot <- renderPlotly

#### Output: Enrichment.LSEA.all.enrichment ####
output$Enrichment.LSEA.all.enrichment <- shiny::renderPlot({
  eventdata <- plotly::event_data("plotly_click", source="LSEA.all.bar")
  shiny::validate(shiny::need(!is.null(eventdata), "Click the bar to show enrichment plot."))
  shiny::validate(shiny::need(gsub('<br />','\\\n', eventdata$key) %in% 
                                variables$Enrichment.lsea.all.result$table_barPlot$yText, "Click the bar to show enrichment plot."))
  
  barterm <- variables$Enrichment.lsea.all.result$table_barPlot %>% 
    dplyr::filter(yText == gsub('<br />','\\\n',eventdata$key)) #看使用者選到哪一個
  LipidSigR::plot_enrichment_lsea(lsea_res=variables$Enrichment.lsea.all.result, 
                                  char=barterm$characteristic, 
                                  char_feature=barterm$charFeature) +
    ggplot2::theme(text=ggplot2::element_text(size=9))
}) #output$DE.species.ma.box <- renderPlotly

shiny::observeEvent(input$Enrichment.LSEA.all.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$Enrichment.LSEA.all.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$Enrichment.LSEA.all.download <- shiny::downloadHandler(
            filename=function(){
              paste("Enrichment.LSEA.download.zip")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## UMAP plot ##
              grDevices::pdf(file.path(temp_directory, 'barplot.pdf'), width=8, height=6)
              print(variables$Enrichment.lsea.all.result$static_barPlot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$Enrichment.lsea.all.result$table_barPlot, file=file.path(temp_directory,'barplot.csv'))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$Enrichment.LSEA.all.download.log <- 2
          shinyjs::runjs("document.getElementById('Enrichment.LSEA.all.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('Enrichment.LSEA.all.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid set enrichment analysis download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) ## observeEvent #Enrichment.LSEA.all.download.start

shiny::observeEvent(input$Enrichment_LSEA_char, {
  shiny::isolate({
    tryCatch(
      {
        enrich_pvalue <- input$Enrichment_LSEA_enrich_pvalue
        if(is.numeric(input$Enrichment_LSEA_enrich_pvalue)){
          if(input$Enrichment_LSEA_enrich_pvalue > 1){
            enrich_pvalue <- 1
            shiny::showNotification("The p-value of significantly enriched lipid characteristics lipids must be between 0.001 and 1, so it is calculated by replacing it with 0.05.", type="warning")
            shiny::updateNumericInput(inputId='Enrichment_LSEA_enrich_pvalue', value=1)
          }else if(input$Enrichment_LSEA_enrich_pvalue < 0.001){
            enrich_pvalue <- 0.001
            shiny::showNotification("The p-value of significantly enriched lipid characteristics must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
            shiny::updateNumericInput(inputId='Enrichment_LSEA_enrich_pvalue', value=0.001)
          }
        }else{
          enrich_pvalue <- 0.05
          shiny::showNotification("The p-value of significantly enriched lipid characteristics lipids must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
          shiny::updateNumericInput(inputId='Enrichment_LSEA_enrich_pvalue', value=0.05)
        }
        variables$Enrichment.lsea.each.result <- LipidSigR::enrichment_lsea(
          deSp_se=variables$deSp.se, 
          char=input$Enrichment_LSEA_char, rank_by=input$Enrichment_enrich_LSEA_rank_by,
          significant=input$Enrichment_LSEA_enrich_stat,
          p_cutoff=enrich_pvalue)
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid set enrichment analysis characteristic error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
})

shiny::observeEvent(input$Enrichment.LSEA.each.download.start,{
  shiny::isolate({
    tryCatch(
      {
        if(variables$Enrichment.LSEA.each.download.log == 1){
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download", display_pct=TRUE, value=0)
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=33)
          output$Enrichment.LSEA.each.download <- shiny::downloadHandler(
            filename=function(){
              paste("Enrichment.LSEA.",input$Enrichment_LSEA_char,".download.zip")
            },
            content=function(file){
              temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
              dir.create(temp_directory)
              ## UMAP plot ##
              grDevices::pdf(file.path(temp_directory, paste0(input$Enrichment_LSEA_char,'.barplot.pdf')), width=8, height=6)
              print(variables$Enrichment.lsea.each.result$static_barPlot)
              grDevices::dev.off()
              ## table  ##
              write.csv(variables$Enrichment.lsea.each.result$table_barPlot, file=file.path(temp_directory,paste0(input$Enrichment_LSEA_char,'.barplot.csv')))
              ## zip all file ##
              zip::zip(zipfile=file, files=dir(temp_directory), root=temp_directory)
            },
            contentType="application/zip"
          )
          shinyWidgets::progressSweetAlert(
            session=session, id="data_progress",
            title="Download",display_pct=TRUE, value=66)
          variables$Enrichment.LSEA.each.download.log <- 2
          shinyjs::runjs("document.getElementById('Enrichment.LSEA.each.download.start').click();")
        }else{
          shinyjs::runjs("document.getElementById('Enrichment.LSEA.each.download').click();")
          shinyWidgets::updateProgressBar(
            session=session, id="data_progress", title="done", value=100)
          shinyWidgets::closeSweetAlert(session=session)
        }
      },
      error=function(e) {
        shinyWidgets::sendSweetAlert(
          session=session, title="Lipid set enrichment analysis characteristic download error!",
          text=as.character(e$message),
          type="error")
      }
    )
  })
}) ## observeEvent #Enrichment.LSEA.each.download.start

#### Output: Enrichment.LSEA.each.bar ####
output$Enrichment.LSEA.each.bar <- plotly::renderPlotly({
  shiny::validate(shiny::need(!is.null(variables$Enrichment.lsea.each.result), ""))
  plotly::plot_ly(variables$Enrichment.lsea.each.result$table_barPlot, x=~NES, y=~yText,
                  type='bar', color=~color,
                  colors=variables$sig.colormap[variables$sig.type$classes],
                  orientation='h', key=~yText, source="lsea.each.bar", hovertext=~hover) %>%
    plotly::layout(title=list(font=list(size=20), text=''),
                   xaxis=list(title='NES', titlefont=list(size=16), tickfont=list(size=14)),
                   yaxis=list(title='', titlefont=list(size=16), tickfont=list(size=14)),
                   showlegend=TRUE,
                   legend=list(font=list(size=12), title=list(text='aspect'), orientation='h'),
                   margin=list(l=70, r=20, b=20, t =40))
}) #output$DE.species.maplot <- renderPlotly

#### Output: Enrichment.LSEA.each.enrichment ####
output$Enrichment.LSEA.each.enrichment <- shiny::renderPlot({
   eventdata <- plotly::event_data("plotly_click", source="lsea.each.bar")
   shiny::validate(shiny::need(!is.null(variables$Enrichment.lsea.each.result), ""))
   shiny::validate(shiny::need(!is.null(eventdata), "Click the bar to show enrichment plot."))
   shiny::validate(shiny::need(gsub('<br />','\\\n',eventdata$key) %in% 
                                 variables$Enrichment.lsea.each.result$table_barPlot$yText, "Click the bar to show enrichment plot."))
   barterm <- variables$Enrichment.lsea.each.result$table_barPlot %>% 
     dplyr::filter(yText == gsub('<br />','\\\n',eventdata$key)) #看使用者選到哪一個
   LipidSigR::plot_enrichment_lsea(lsea_res=variables$Enrichment.lsea.each.result, 
                                   char=barterm$characteristic, 
                                   char_feature=barterm$charFeature) + 
     ggplot2::theme(text=ggplot2::element_text(size=9))
}) #output$DE.species.ma.box <- renderPlotly

#### Output: Enrichment.LSEA.all.tab ####
output$Enrichment.LSEA.all.tab <- DT::renderDataTable(server=FALSE, {
   shiny::validate(shiny::need(!is.null(variables$Enrichment.lsea.all.result$enrich_result), "Without table."))
   DT::datatable(variables$Enrichment.lsea.all.result$enrich_result %>% 
                   dplyr::mutate_if(is.numeric, ~round(., 5)),
                 escape=FALSE, selection='none', rownames=FALSE,
                 class="nowrap row-border",
                 extensions=c('Buttons', 'Scroller'),
                 options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE,
                                deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                                dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                                columnDefs=list(list(className='dt-center', targets="_all"))))
}) #output$Enrichment.all.tab <- renderDataTable
