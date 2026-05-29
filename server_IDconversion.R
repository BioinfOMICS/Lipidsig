####################################
####################################
######                        ######
######   ID conversion Page   ######
######                        ######
####################################
####################################

#### Output: IDconversion.demo.download ####
output$IDconversion.demo.download <- shiny::downloadHandler(
   filename=function() {
      "IDconversion_example_species.csv"
   },
   content=function(file) {
      file.copy("www/download_demo_dataset/Lipid_species.csv", file)
   },
   contentType="application/zip"
)
shiny::outputOptions(output, "IDconversion.demo.download", suspendWhenHidden=FALSE)

#### IDconversion demo dataset ####
shiny::observeEvent(input$IDconversion_demo_upload, {
  shiny::withProgress(message='ID Conversion', style='notification', detail="Upload data", value=0, {
    shinyjs::show('ID_conversion_upload_div')
    variables$IDconversion.progress <- htmltools::HTML('<div style="font-size: 0px;">
                                                        <h2>Upload progress</h2>
                                                        <h4>(1/2) Check upload data format.</h4>
                                                       </div>')
    variables$IDconversion.lipidList <- readRDS('www/demo_dataset/IDconversion_example.rds')
    shiny::incProgress(0.33, detail='Check data format')
    variables$parseableLipid <- rgoslin::parseLipidNames(variables$IDconversion.lipidList)
    variables$nonParseableLipid <- variables$parseableLipid$Original.Name[which(variables$parseableLipid$Grammar == 'NOT_PARSEABLE')]
    variables$recognized_lipid <- variables$parseableLipid$Original.Name[which(variables$parseableLipid$Grammar != 'NOT_PARSEABLE')]
    variables$goslin_annotation <- variables$parseableLipid %>%
      dplyr::filter(Original.Name %in% variables$recognized_lipid)
    variables$lipid_annotation_table <- LipidSigR::lipid_annotation(variables$goslin_annotation)
    variables$lipid_annotation_table <- char.tab.url(variables$lipid_annotation_table)
    shiny::incProgress(0.33, detail='ID Conversion')
    variables$IDconversion.progress <- htmltools::HTML('<div style="font-size: 0px;">
                                                        <h2>Upload progress</h2>
                                                        <h4>(2/2) ID conversion finish.</h4>
                                                       </div>')
    shinyjs::show('ID_conversion_table_div')
    shiny::incProgress(0.34, detail='ID Conversion finish')
  })
}) #observeEvent(input$IDconversion_demo_upload

#### control user reset button ####
shiny::observeEvent(input$IDconversion_reset, {
   
   shinyjs::reset('IDconversion_reset_div')
   shinyjs::hide('ID_conversion_upload_div')

   #### clear variables ####
   variables$IDconversion.abundance  <- NULL
   variables$IDconversion.lipidList  <- NULL
   variables$lipid_annotation_table  <- NULL
   variables$nonParseableLipid       <- NULL
   variables$IDconversion.progress   <- htmltools::HTML('<div style="font-size: 0px;"><h2>Upload progress</h2><h4>(1/2) Check upload data format.</h4></div>')
}) #observeEvent(input$Check_reset


#### IDconversion user dataset ####
shiny::observeEvent(input$IDconversion_upload, {
  shiny::withProgress(message='ID Conversion', style='notification', detail="Upload data", value=0, {
    shinyjs::show('ID_conversion_upload_div')
    shiny::showNotification("Start uploading file...", type="message")
    variables$IDconversion.progress <- htmltools::HTML('<div style="font-size: 0px;">
                                                        <h2>Upload progress</h2>
                                                        <h4>(1/2) Check upload data format.</h4>
                                                       </div>')
    if(input$IDconversion_species_type == "typing"){
      tryCatch(
        {
          variables$IDconversion.lipidList <- unlist(strsplit(input$IDconversion_species_typing, '\n'))
          if(length(variables$IDconversion.lipidList) == 0){
            variables$IDconversion.lipidList <- NULL
            shinyWidgets::closeSweetAlert(session=session)
            shinyWidgets::sendSweetAlert(
              session=session, 
              title="Input data warning!",
              text="Some error is in your typing, it maybe cause some problem we cannot expected.",
              type="warning")
          }
        },
        error=function(e) {
          shinyWidgets::closeSweetAlert(session=session)
          shinyWidgets::sendSweetAlert(
            session=session,
            title="Input data error!",
            text=as.character(message(e)),
            type="error")
          return()
        },
        warning=function(w) {
          shinyWidgets::closeSweetAlert(session=session)
          shinyWidgets::sendSweetAlert(
            session=session,
            title="Input data warning!",
            text="Some error is in your dataset, it maybe cause some problem we cannot expected.",
            type="warning")
          return()
        }
      )
    }else{
      tryCatch(
        {
          if(grepl('.xlsx', input$IDconversion_species_file$datapath)){
            variables$IDconversion.lipidList <- readxl::read_excel(
              input$IDconversion_species_file$datapath) %>% as.data.frame()
          }else if(grepl('.csv', input$IDconversion_species_file$datapath)){
            variables$IDconversion.lipidList <- readr::read_csv(
              input$IDconversion_species_file$datapath, na=c('', 'NA', 'na')) %>% as.data.frame()
          }else{
            variables$IDconversion.lipidList <- data.table::fread(
              input$IDconversion_species_file$datapath, header=TRUE, stringsAsFactors=FALSE,
              check.names=FALSE,   data.table=FALSE, na.strings=c('', 'NA', 'na'))
          }
        },
      error=function(e) {
        shinyWidgets::closeSweetAlert(session=session)
        shinyWidgets::sendSweetAlert(
          session=session,
          title="Input data error!",
          text=as.character(message(e)),
          type="error")
        return()
      },
      warning=function(w) {
        shinyWidgets::closeSweetAlert(session=session)
        shinyWidgets::sendSweetAlert(
          session=session,
          title="Input data warning!",
          text="Some error is in your dataset, it maybe cause some problem we cannot expected.",
          type="warning")
        return()
      }
    )
      if(!is.null(variables$IDconversion.lipidList)){
        variables$IDconversion.lipidList <- variables$IDconversion.lipidList[,1] %>% as.character()
      }
    }
    tryCatch(
      {
        shiny::incProgress(0.33, detail='Check data format')
        if(!is.null(variables$IDconversion.lipidList)){
          variables$parseableLipid <- rgoslin::parseLipidNames(variables$IDconversion.lipidList)
          variables$nonParseableLipid <- variables$parseableLipid$Original.Name[which(variables$parseableLipid$Grammar == 'NOT_PARSEABLE')]
          variables$recognized_lipid <- variables$parseableLipid$Original.Name[which(variables$parseableLipid$Grammar != 'NOT_PARSEABLE')]
          variables$goslin_annotation <- variables$parseableLipid %>%
            dplyr::filter(Original.Name %in% variables$recognized_lipid)
          variables$lipid_annotation_table <- LipidSigR::lipid_annotation(variables$goslin_annotation)
          variables$lipid_annotation_table <- char.tab.url(variables$lipid_annotation_table)
          shiny::incProgress(0.33, detail='ID Conversion')
          variables$IDconversion.progress <- htmltools::HTML('<div style="font-size: 0px;">
                                                              <h2>Upload progress</h2>
                                                              <h4>(2/2) ID conversion finish.</h4>
                                                              </div>')
          shinyjs::show('ID_conversion_upload_div')
          shinyjs::show('ID_conversion_table_div')
          shiny::incProgress(0.34, detail='ID Conversion finish')
        }else{
          variables$IDconversion.progress <- htmltools::HTML('<div style="font-size: 0px;">
                                                              <h2>Upload progress</h2>
                                                              <h4>(2/2) ID conversion finish.</h4>
                                                              </div>')
          shinyjs::hide('ID_conversion_upload_div')
          shinyjs::hide('ID_conversion_table_div')
          shiny::incProgress(0.67, detail='ID Conversion finish')
        }
      },
      error=function(e) {
        shinyWidgets::show_alert(
          title='Error',
          text=htmltools::HTML("<h4>Detect unknown input data format errors.</h4>
                                <h4>For the correct data format guidelines, please refer to the <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank'>FAQ</a></h4>.
                                <h4>If you need further assistance, please email us your data.(<a href='mailto:bioinfomics.web@gmail.com' target='_blank' style='color: darkblue;'>bioinfomics.web@gmail.com</a>)</h4>"),
          html=TRUE, type='error')
        return()
      }
    )
  })
}) #observeEvent(input$PRO_user_upload

#### Output: IDconversion.progress ####
output$IDconversion.progress <- shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$IDconversion.progress), ""))
  variables$IDconversion.progress
})

#### Output: IDconversion.exp.raw ####
output$IDconversion.exp.raw <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$IDconversion.lipidList), "Some error is in your data, please check your data and re-upload it."))
  DT::datatable(data.frame(feature=variables$IDconversion.lipidList), 
                escape=FALSE, selection='none', rownames=FALSE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                               deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                               dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                               columnDefs=list(list(className='dt-center', targets="_all"))))
})

#### Output: IDconversion.recognized.lipid ####
output$IDconversion.recognized.lipid <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$lipid_annotation_table), "Some error is in your data, please check your data and re-upload it."))
  DT::datatable(variables$lipid_annotation_table %>%
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

output$IDconversion.lipid.id <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$lipid_annotation_table), "Some error is in your data, please check your data and re-upload it."))
  DT::datatable(variables$lipid_annotation_table %>%
                   dplyr::select(feature,LION.ID,LIPID.MAPS.ID,SwissLipids.ID,HMDB.ID,ChEBI.ID,KEGG.ID,LipidBank.ID,PubChem.CID,MetaNetX.ID,PlantFA.ID), 
                escape=FALSE, selection='none', rownames=FALSE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller','FixedColumns'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                               deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                               dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                               fixedColumns=list(leftColumns=1)))
})

#### Output: IDconversion.unrecognized.lipid ####
output$IDconversion.unrecognized.lipid <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$nonParseableLipid), "Some error is in your data, please check your data and re-upload it."))
  DT::datatable(data.frame(feature=variables$nonParseableLipid), 
                escape=FALSE, selection='none', rownames=FALSE, 
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=5, autoWidth=FALSE, 
                               deferRender=TRUE, scrollY=200, scroller=TRUE, #Scroller
                               dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                               columnDefs=list(list(className='dt-center', targets="_all"))))
})
