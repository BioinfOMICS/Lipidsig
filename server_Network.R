library(magrittr)

##############################
##############################
######                  ######
######   Network Page   ######
######                  ######
##############################
##############################

## NOTE (integrated app): updateQueryString() calls removed from NET_analysis_tab
## observer to avoid polluting the navbarPage URL.

## NOTE (integrated app): URL-based preload via getQueryString() removed.
## DE → Network handoff is handled directly in server_DE.R via reactiveValues.

################################
####  Network analysis tab  ####
################################

#################################
####  pathwayActivity pathway  ####
#################################

#### control reset button ####
shiny::observeEvent(input$NET_pathwayActivity_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('NET_pathwayActivity_warning_div')
  shinyjs::hide('NET_pathwayActivity_result_div')
  shinyjs::hide('NET.pathwayActivity.table')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('NET_pathwayActivity_reset_div')
  
}) #shiny::observeEvent(input$NET_pathwayActivity_reset

shiny::observe({
  if(is.null(input$NET_pathwayActivity_deSpecies)){
    shinyjs::disable("NET_pathwayActivity_user_start")
  }else{
    shinyjs::enable("NET_pathwayActivity_user_start")
  }
}) #observe

#### Output: NET.pathwayActivity.demo.download ####
output$NET.pathwayActivity.demo.download <- shiny::downloadHandler(
  filename=function() {
    "deSp.se.rds"
  },
  content=function(file) {
    file.copy("www/demo_dataset/deSp_twoGroup.rds", file)
  },
  contentType="application/zip"
)
shiny::outputOptions(output, "NET.pathwayActivity.demo.download", suspendWhenHidden=FALSE)

#### control start button ####
shiny::observeEvent(input$NET_pathwayActivity_user_start, {
  shiny::isolate({
    shiny::withProgress(message='Pathway activity network', style='notification', detail="Upload data", value=0, {
      #### import user dataset ####
      tryCatch(
        {
          variables$NET.pathwayActivity.deSp.se <- readRDS(input$NET_pathwayActivity_deSpecies$datapath)
          shiny::showNotification("Received uploaded file.", type="message")
        },
        error=function(e) {
          shinyWidgets::sendSweetAlert(
            session=session, title="Input data error!",
            text=as.character(message(e)),
            type="error" )
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
          shiny::incProgress(0.33, detail='Check data format')
          variables$NET.pathwayActivity.check <- check.despse(variables$NET.pathwayActivity.deSp.se, 'two')
          shiny::incProgress(0.33, detail='Analysis in progress')
          if(!grepl('xmark icon',variables$NET.pathwayActivity.check)){
            shinyjs::show('NET_pathwayActivity_warning_div')
            shinyjs::show('NET_pathwayActivity_result_div')
            variables$NET.pathwayActivity <- 
              LipidSigR::nw_pathway_activity(deSp_se=variables$NET.pathwayActivity.deSp.se, organism=input$NET_pathwayActivity_organism)
            if(is.list(variables$NET.pathwayActivity)){
              variables$NET.pathwayActivity.plot <- visNetwork::visNetwork(
                nodes=variables$NET.pathwayActivity$table_node, edges=variables$NET.pathwayActivity$table_edge) %>%
                visNetwork::visLayout(randomSeed=500) %>%
                visNetwork::visPhysics(
                  solver='barnesHut', stabilization=TRUE, 
                  barnesHut=list(gravitationalConstant=-3000)) %>%
                visNetwork::visInteraction(navigationButtons=TRUE) %>%
                visNetwork::visEvents(
                  dragEnd="function () {this.setOptions( { physics: false } );}") %>%
                visNetwork::visEdges(color=list(color="#DDDDDD",highlight="#C62F4B")) %>%
                visNetwork::visOptions(
                  highlightNearest=list(enabled=TRUE, degree=1, hover=FALSE),
                  nodesIdSelection=TRUE)
            }else{
              shinyjs::hide('NET_pathwayActivity_warning_div')
              shinyjs::hide('NET_pathwayActivity_result_div')
              shinyWidgets::show_alert(
                title='Error',
                text=htmltools::HTML(variables$NET.pathwayActivity),
                html=TRUE, type ='error' )
            }
          }else{
            shinyjs::show('NET_pathwayActivity_warning_div')
            shinyjs::hide('NET_pathwayActivity_result_div')
          }
          shiny::incProgress(0.34, detail='Analysis finish')
        },
        error=function(e) {
          shinyWidgets::show_alert(
            title='Error',
            text=htmltools::HTML("<h4>Detect unknown input data format errors.</h4>
                               <h4>For the correct data format guidelines, please refer to the <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank'>FAQ</a></h4>.
                               <h4>If you need further assistance, please email us your data.(<a href='mailto:bioinfomics.web@gmail.com' target='_blank' style='color: darkblue;'>bioinfomics.web@gmail.com</a>)</h4>"),
            html=TRUE, type ='error' )
          return()
        }
      )
    })
  }) #isolate
}) #shiny::observeEvent(input$NET_pathwayActivity_start

#### control example button ####
shiny::observeEvent(input$NET_pathwayActivity_demo_start, {
  shiny::isolate({
    shiny::withProgress(message='Pathway activity network', style='notification', detail="Upload data", value=0, {
      shinyjs::show('NET_pathwayActivity_warning_div')
      shinyjs::show('NET_pathwayActivity_result_div')
      shinyjs::show('NET.pathwayActivity.table')
      variables$NET.pathwayActivity.deSp.se <- readRDS('www/demo_dataset/deSp_twoGroup.rds')
      variables$NET.pathwayActivity.check <- check.despse(variables$NET.pathwayActivity.deSp.se, 'two')
      shiny::incProgress(0.33, detail='Check data format')
      variables$NET.pathwayActivity <- 
        LipidSigR::nw_pathway_activity(deSp_se=variables$NET.pathwayActivity.deSp.se, organism=input$NET_pathwayActivity_organism)
      shiny::incProgress(0.33, detail='Analysis in progress')
      variables$NET.pathwayActivity.plot <- visNetwork::visNetwork(
        nodes=variables$NET.pathwayActivity$table_node, edges=variables$NET.pathwayActivity$table_edge) %>%
        visNetwork::visLayout(randomSeed=500) %>%
        visNetwork::visPhysics(
          solver='barnesHut', stabilization=TRUE, 
          barnesHut=list(gravitationalConstant=-3000)) %>%
        visNetwork::visInteraction(navigationButtons=TRUE) %>%
        visNetwork::visEvents(
          dragEnd="function () {this.setOptions( { physics: false } );}") %>%
        visNetwork::visEdges(color=list(color="#DDDDDD",highlight="#C62F4B")) %>%
        visNetwork::visOptions(
          highlightNearest=list(enabled=TRUE, degree=1, hover=FALSE),
          nodesIdSelection=TRUE)
      shiny::incProgress(0.34, detail='Analysis finish')
    })
  }) #isolate
}) #shiny::observeEvent(input$NET_pathwayActivity_start
#### control url start button ####
shiny::observeEvent(input$NET_pathwayActivity_url_start, {
  shiny::isolate({
    if(is.null(variables$NET.pathwayActivity.deSp.se)){
      shinyWidgets::sendSweetAlert(
        session=session, title="No DE results found",
        text="Please run Differential Expression analysis first, then click the Pathway Activity button in the DE result download tab.",
        type="warning")
      return()
    }
    shiny::withProgress(message='Pathway activity network', style='notification', detail="Upload data", value=0, {
      ## Integrated app: NET.pathwayActivity.deSp.se already set by server_DE.R
      tryCatch(
        {
          shiny::incProgress(0.33, detail='Check data format')
          variables$NET.pathwayActivity.check <- check.despse(variables$NET.pathwayActivity.deSp.se, 'two')
          shiny::incProgress(0.33, detail='Analysis in progress')
          if(!grepl('xmark icon',variables$NET.pathwayActivity.check)){
            shinyjs::show('NET_pathwayActivity_warning_div')
            shinyjs::show('NET_pathwayActivity_result_div')
            variables$NET.pathwayActivity <- 
              LipidSigR::nw_pathway_activity(deSp_se=variables$NET.pathwayActivity.deSp.se, organism=input$NET_pathwayActivity_organism)
            shiny::incProgress(0.33, detail='Analysis in progress')
            if(is.list(variables$NET.pathwayActivity)){
              variables$NET.pathwayActivity.plot <- visNetwork::visNetwork(
                nodes=variables$NET.pathwayActivity$table_node, edges=variables$NET.pathwayActivity$table_edge) %>%
                visNetwork::visLayout(randomSeed=500) %>%
                visNetwork::visPhysics(
                  solver='barnesHut', stabilization=TRUE, 
                  barnesHut=list(gravitationalConstant=-3000)) %>%
                visNetwork::visInteraction(navigationButtons=TRUE) %>%
                visNetwork::visEvents(
                  dragEnd="function () {this.setOptions( { physics: false } );}") %>%
                visNetwork::visEdges(color=list(color="#DDDDDD",highlight="#C62F4B")) %>%
                visNetwork::visOptions(
                  highlightNearest=list(enabled=TRUE, degree=1, hover=FALSE),
                  nodesIdSelection=TRUE)
            }else{
              shinyjs::hide('NET_pathwayActivity_warning_div')
              shinyjs::hide('NET_pathwayActivity_result_div')
              shinyWidgets::show_alert(
                title='Error',
                text=htmltools::HTML(variables$NET.pathwayActivity),
                html=TRUE, type ='error' )
            }
           
          }else{
            shinyjs::show('NET_pathwayActivity_warning_div')
            shinyjs::hide('NET_pathwayActivity_result_div')
          }
          shiny::incProgress(0.34, detail='Analysis finish')
        },
        error=function(e) {
          shinyWidgets::show_alert(
            title='Error',
            text=htmltools::HTML("<h4>Detect unknown input data format errors.</h4>
                               <h4>For the correct data format guidelines, please refer to the <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank'>FAQ</a></h4>.
                               <h4>If you need further assistance, please email us your data.(<a href='mailto:bioinfomics.web@gmail.com' target='_blank' style='color: darkblue;'>bioinfomics.web@gmail.com</a>)</h4>"),
            html=TRUE, type ='error' )
          return()
        }
      )
    })
  }) #isolate
}) #shiny::observeEvent(input$NET_pathwayActivity_url_start

output$NET.pathwayActivity.check.message <- renderUI({
  shiny::validate(shiny::need(!is.null(variables$NET.pathwayActivity.check), ""))
  variables$NET.pathwayActivity.check
})

#### Output: NET.pathwayActivity.table ####
output$NET.pathwayActivity.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$NET.pathwayActivity$table_pathway_score), "Without lipid class-related pathway!"))
  DT::datatable(variables$NET.pathwayActivity$table_pathway_score %>%
                  dplyr::mutate_if(is.numeric, ~round(., 3)) %>%
                  dplyr::ungroup() %>%
                  dplyr::select(-path_name) %>% 
                  dplyr::filter(abs(pathway_score) > 1.645),
                escape=FALSE, selection='none', rownames=FALSE,
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=FALSE, pageLength=10, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=400, scroller=FALSE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets='_all'))))
  
}) #output$NET.pathwayActivity.table <- renderDataTable

output$NET.pathwayActivity.network <- visNetwork::renderVisNetwork({
  shiny::validate(shiny::need(!is.null(variables$NET.pathwayActivity.plot), "No pathwayActivity pathway found!"))
  variables$NET.pathwayActivity.plot
}) #output$NET.pathwayActivity.network <- renderVisNetwork

shiny::observeEvent(input$NET_pathwayActivity_network_refresh, {
  shiny::isolate({
    output$NET.pathwayActivity.network <- visNetwork::renderVisNetwork({
      shiny::validate(shiny::need(!is.null(variables$NET.pathwayActivity.plot), "No pathwayActivity pathway found!"))
      variables$NET.pathwayActivity.plot
    }) #output$NET.pathwayActivity.network <- renderVisNetwork
  })
})

###########################
####  lipid Reaction   ####
###########################

#### Switch the default organism ####
observeEvent(input$NET_lipidReaction_source, {
  if(input$NET_lipidReaction_source == "NET_user_data") {
    updateRadioButtons(session, "NET_lipidReaction_organism", selected = "human")
  } else if(input$NET_lipidReaction_source == "NET_demo_data") {
    updateRadioButtons(session, "NET_lipidReaction_organism", selected = "mouse")
  }
})

#### control reset button ####
shiny::observeEvent(input$NET_lipidReaction_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('NET_lipidReaction_warning_div')
  shinyjs::hide('NET_lipidReaction_result_div')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('NET_lipidReaction_reset_div')
  
}) #shiny::observeEvent(input$NET_lipidReaction_reset

shiny::observe({
  
  if(is.null(input$NET_lipidReaction_deSpecies)){
    shinyjs::disable("NET_lipidReaction_start")
  }else{
    shinyjs::enable("NET_lipidReaction_start")
  }
  
}) #observe

#### control start button ####
shiny::observeEvent(input$NET_lipidReaction_start, {
  
  shiny::isolate({
    shiny::withProgress(message='Lipid reaction network', style='notification', detail="Upload data", value=0, {
      #### import user dataset ####
      tryCatch(
        {
          variables$NET.lipidReaction.deSp.se <- readRDS(input$NET_lipidReaction_deSpecies$datapath)
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
          shiny::incProgress(0.33, detail='Check data format')
          variables$NET.lipidReaction.check <- check.despse(variables$NET.lipidReaction.deSp.se, 'two')
          if(!grepl('xmark icon', variables$NET.lipidReaction.check)){
            shinyjs::show('NET_lipidReaction_warning_div')
            shinyjs::show('NET_lipidReaction_result_div')
            shiny::incProgress(0.33, detail='Check data format')
            shiny::incProgress(0.33, detail='Analysis in progress')
            if(is.numeric(input$NET_lipidReaction_lipidFoldChange)){
              sp_FC_cutoff <- input$NET_lipidReaction_lipidFoldChange
              if(input$NET_lipidReaction_lipidFoldChange > 8){
                sp_FC_cutoff <- 8
                shiny::showNotification("The Fold change (FC) of significantly lipids must be between 1 and 8, so it is calculated by replacing it with 8.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_lipidFoldChange', value=8)
              }else if(input$NET_lipidReaction_lipidFoldChange < 1){
                sp_FC_cutoff <- 1
                shiny::showNotification("The Fold change (FC) of significantly lipids must be between 1 and 8, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_lipidFoldChange', value=1)
              }
            }else{
              sp_FC_cutoff <- 1
              shiny::showNotification("The Fold change (FC) of significantly lipids must be numeric, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='NET_lipidReaction_lipidFoldChange', value=1)
            }
            if(is.numeric(input$NET_lipidReaction_lipidP)){
              sp_p_cutoff <- input$NET_lipidReaction_lipidP
              if(input$NET_lipidReaction_lipidP > 1){
                sp_p_cutoff <- 1
                shiny::showNotification("The p-value of significantly lipids must be between 0.001 and 1, so it is calculated by replacing it with 0.05.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_lipidP', value=1)
              }else if(input$NET_lipidReaction_lipidP < 0.001){
                sp_p_cutoff <- 0.001
                shiny::showNotification("The p-value of significantly lipids must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_lipidP', value=0.001)
              }
            }else{
              sp_p_cutoff <- 0.05
              shiny::showNotification("The p-value of significantly lipids must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
              shiny::updateNumericInput(inputId='NET_lipidReaction_lipidP', value=0.05)
            }
            if(is.numeric(input$NET_lipidReaction_classFoldChange)){
              class_FC_cutoff <- input$NET_lipidReaction_classFoldChange
              if(input$NET_lipidReaction_classFoldChange > 8){
                class_FC_cutoff <- 8
                shiny::showNotification("The Fold change (FC) of significantly class must be between 1 and 8, so it is calculated by replacing it with 8.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_classFoldChange', value=8)
              }else if(input$NET_lipidReaction_classFoldChange < 1){
                class_FC_cutoff <- 1
                shiny::showNotification("The Fold change (FC) of significantly class must be between 1 and 8, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_classFoldChange', value=1)
              }
            }else{
              class_FC_cutoff <- 1
              shiny::showNotification("The Fold change (FC) of significantly class must be numeric, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='NET_lipidReaction_classFoldChange', value=1)
            }
            if(is.numeric(input$NET_lipidReaction_classP)){
              class_p_cutoff <- input$NET_lipidReaction_classP
              if(input$NET_lipidReaction_classP > 1){
                class_p_cutoff <- 1
                shiny::showNotification("The p-value of significantly class must be between 0.001 and 1, so it is calculated by replacing it with 0.05.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_classP', value=1)
              }else if(input$NET_lipidReaction_classP < 0.001){
                class_p_cutoff <- 0.001
                shiny::showNotification("The p-value of significantly class must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_classP', value=0.001)
              }
            }else{
              class_p_cutoff <- 0.05
              shiny::showNotification("The p-value of significantly class must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
              shiny::updateNumericInput(inputId='NET_lipidReaction_classP', value=0.05)
            }
            variables$NET.lipidReaction <- LipidSigR::nw_lipid_reaction(
              deSp_se=variables$NET.lipidReaction.deSp.se, 
              organism=input$NET_lipidReaction_organism,
              show_sp=input$NET_lipidReaction_showSpecies, 
              show_all_reactions=input$NET_lipidReaction_showAllReaction,
              sp_significant=input$NET_lipidReaction_lipidSignif, sp_p_cutoff=sp_p_cutoff, sp_FC_cutoff=sp_FC_cutoff,
              class_significant=input$NET_lipidReaction_classSignif, class_p_cutoff=class_p_cutoff, class_FC_cutoff=class_FC_cutoff)
            shiny::incProgress(0.34, detail='Analysis finish')
            if(is.list(variables$NET.lipidReaction)){
              variables$NET.lipidReaction.plot <- visNetwork::visNetwork(nodes=variables$NET.lipidReaction$table_node, edges=variables$NET.lipidReaction$table_edge) %>%
                visNetwork::visLayout(randomSeed=500) %>%
                visNetwork::visPhysics(
                  solver='barnesHut', stabilization=TRUE, 
                  barnesHut=list(gravitationalConstant=-2500)) %>%
                visNetwork::visInteraction(navigationButtons=TRUE) %>%
                visNetwork::visEvents(dragEnd="function () {this.setOptions( { physics: false } );}") %>%
                visNetwork::visOptions(
                  highlightNearest=list(enabled=TRUE, degree=1, hover=FALSE),
                  selectedBy="group", nodesIdSelection=TRUE)
            }else{
              shinyjs::hide('NET_lipidReaction_warning_div')
              shinyjs::hide('NET_lipidReaction_result_div')
              shinyWidgets::show_alert(
                title='Error',
                text=shiny::HTML(variables$NET.lipidReaction),
                html=TRUE, type ='error')
            }
          }else{
            shinyjs::show('NET_lipidReaction_warning_div')
            shinyjs::hide('NET_lipidReaction_result_div')
            
          }
          shiny::incProgress(0.34, detail='Analysis finish')
        },
        error=function(e) {
          shinyWidgets::show_alert(
            title='Error',
            text=HTML("<h4>Detect unknown input data format errors.</h4>
                               <h4>For the correct data format guidelines, please refer to the <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank'>FAQ</a></h4>.
                               <h4>If you need further assistance, please email us your data.(<a href='mailto:bioinfomics.web@gmail.com' target='_blank' style='color: darkblue;'>bioinfomics.web@gmail.com</a>)</h4>"),
            html=TRUE, type ='error')
          return()
        }
      )
    })
  }) #isolate
}) #shiny::observeEvent(input$NET_lipidReactionNetwork_start

#### control example button ####
shiny::observeEvent(input$NET_lipidReaction_demo_start, {
  shiny::isolate({
    shiny::withProgress(message='Lipid reaction network', style='notification', detail="Upload data", value=0, {
      shinyjs::show('NET_lipidReaction_warning_div')
      shinyjs::show('NET_lipidReaction_result_div')
      variables$NET.lipidReaction.deSp.se <- readRDS('www/demo_dataset/deSp_twoGroup.rds')
      variables$NET.lipidReaction.check <- check.despse(variables$NET.lipidReaction.deSp.se, 'two')
      shiny::incProgress(0.33, detail='Check data format')
      shiny::incProgress(0.33, detail='Analysis in progress')
      if(is.numeric(input$NET_lipidReaction_lipidFoldChange)){
        sp_FC_cutoff <- input$NET_lipidReaction_lipidFoldChange
        if(input$NET_lipidReaction_lipidFoldChange > 8){
          sp_FC_cutoff <- 8
          shiny::showNotification("The Fold change (FC) of significantly lipids must be between 0 and 8, so it is calculated by replacing it with 8.", type="warning")
          shiny::updateNumericInput(inputId='NET_lipidReaction_lipidFoldChange', value=8)
        }else if(input$NET_lipidReaction_lipidFoldChange < 1){
          sp_FC_cutoff <- 1
          shiny::showNotification("The Fold change (FC) of significantly lipids must be between 0 and 8, so it is calculated by replacing it with 1.", type="warning")
          shiny::updateNumericInput(inputId='NET_lipidReaction_lipidFoldChange', value=1)
        }
      }else{
        sp_FC_cutoff <- 1
        shiny::showNotification("The Fold change (FC) of significantly lipids must be numeric, so it is calculated by replacing it with 1.", type="warning")
        shiny::updateNumericInput(inputId='NET_lipidReaction_lipidFoldChange', value=1)
      }
      if(is.numeric(input$NET_lipidReaction_lipidP)){
        sp_p_cutoff <- input$NET_lipidReaction_lipidP
        if(input$NET_lipidReaction_lipidP > 1){
          sp_p_cutoff <- 1
          shiny::showNotification("The p-value of significantly lipids must be between 0.001 and 1, so it is calculated by replacing it with 0.05.", type="warning")
          shiny::updateNumericInput(inputId='NET_lipidReaction_lipidP', value=1)
        }else if(input$NET_lipidReaction_lipidP < 0.001){
          sp_p_cutoff <- 0.001
          shiny::showNotification("The p-value of significantly lipids must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
          shiny::updateNumericInput(inputId='NET_lipidReaction_lipidP', value=0.001)
        }
      }else{
        sp_p_cutoff <- 0.05
        shiny::showNotification("The p-value of significantly lipids must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
        shiny::updateNumericInput(inputId='NET_lipidReaction_lipidP', value=0.05)
      }
      if(is.numeric(input$NET_lipidReaction_classFoldChange)){
        class_FC_cutoff <- input$NET_lipidReaction_classFoldChange
        if(input$NET_lipidReaction_classFoldChange > 8){
          class_FC_cutoff <- 8
          shiny::showNotification("The Fold change (FC) of significantly class must be between 1 and 8, so it is calculated by replacing it with 8.", type="warning")
          shiny::updateNumericInput(inputId='NET_lipidReaction_classFoldChange', value=8)
        }else if(input$NET_lipidReaction_classFoldChange < 1){
          class_FC_cutoff <- 1
          shiny::showNotification("The Fold change (FC) of significantly class must be between 1 and 8, so it is calculated by replacing it with 1.", type="warning")
          shiny::updateNumericInput(inputId='NET_lipidReaction_classFoldChange', value=1)
        }
      }else{
        class_FC_cutoff <- 1
        shiny::showNotification("The Fold change (FC) of significantly class must be numeric, so it is calculated by replacing it with 1.", type="warning")
        shiny::updateNumericInput(inputId='NET_lipidReaction_classFoldChange', value=1)
      }
      if(is.numeric(input$NET_lipidReaction_classP)){
        class_p_cutoff <- input$NET_lipidReaction_classP
        if(input$NET_lipidReaction_classP > 1){
          class_p_cutoff <- 1
          shiny::showNotification("The p-value of significantly class must be between 0.001 and 1, so it is calculated by replacing it with 0.05.", type="warning")
          shiny::updateNumericInput(inputId='NET_lipidReaction_classP', value=1)
        }else if(input$NET_lipidReaction_classP < 0.001){
          class_p_cutoff <- 0.001
          shiny::showNotification("The p-value of significantly class must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
          shiny::updateNumericInput(inputId='NET_lipidReaction_classP', value=0.001)
        }
      }else{
        class_p_cutoff <- 0.05
        shiny::showNotification("The p-value of significantly class must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
        shiny::updateNumericInput(inputId='NET_lipidReaction_classP', value=0.05)
      }
      
      variables$NET.lipidReaction <- LipidSigR::nw_lipid_reaction(
        deSp_se=variables$NET.lipidReaction.deSp.se, 
        organism=input$NET_lipidReaction_organism,
        show_sp=input$NET_lipidReaction_showSpecies, 
        show_all_reactions=input$NET_lipidReaction_showAllReaction,
        sp_significant=input$NET_lipidReaction_lipidSignif, sp_p_cutoff=sp_p_cutoff, sp_FC_cutoff=sp_FC_cutoff,
        class_significant=input$NET_lipidReaction_classSignif, class_p_cutoff=class_p_cutoff, class_FC_cutoff=class_FC_cutoff)
      shiny::incProgress(0.34, detail='Analysis finish')
      variables$NET.lipidReaction.plot <- visNetwork::visNetwork(nodes=variables$NET.lipidReaction$table_node, edges=variables$NET.lipidReaction$table_edge) %>%
        visNetwork::visLayout(randomSeed=500) %>%
        visNetwork::visPhysics(
          solver='barnesHut', stabilization=TRUE, 
          barnesHut=list(gravitationalConstant=-2500)) %>%
        visNetwork::visInteraction(navigationButtons=TRUE) %>%
        visNetwork::visEvents(dragEnd="function () {this.setOptions( { physics: false } );}") %>%
        visNetwork::visOptions(
          highlightNearest=list(enabled=TRUE, degree=1, hover=FALSE),
          selectedBy="group", nodesIdSelection=TRUE)
    })
  }) #isolate
  
}) #shiny::observeEvent(input$NET_lipidReaction_start

#### control url start button ####
shiny::observeEvent(input$NET_lipidReaction_url_start, {
  shiny::isolate({
    if(is.null(variables$NET.lipidReaction.deSp.se)){
      shinyWidgets::sendSweetAlert(
        session=session, title="No DE results found",
        text="Please run Differential Expression analysis first, then click the Lipid Reaction button in the DE result download tab.",
        type="warning")
      return()
    }
    shiny::withProgress(message='Lipid reaction network', style='notification', detail="Upload data", value=0, {
      ## Integrated app: NET.lipidReaction.deSp.se already set by server_DE.R
      tryCatch(
        {
          shiny::incProgress(0.33, detail='Check data format')
          variables$NET.lipidReaction.check <- check.despse(variables$NET.lipidReaction.deSp.se, 'two')
          if(!grepl('xmark icon', variables$NET.lipidReaction.check)){
            shinyjs::show('NET_lipidReaction_warning_div')
            shinyjs::show('NET_lipidReaction_result_div')
            shiny::incProgress(0.33, detail='Check data format')
            shiny::incProgress(0.33, detail='Analysis in progress')
            if(is.numeric(input$NET_lipidReaction_lipidFoldChange)){
              sp_FC_cutoff <- input$NET_lipidReaction_lipidFoldChange
              if(input$NET_lipidReaction_lipidFoldChange > 8){
                sp_FC_cutoff <- 8
                shiny::showNotification("The Fold change (FC) of significantly lipids must be between 1 and 8, so it is calculated by replacing it with 8.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_lipidFoldChange', value=8)
              }else if(input$NET_lipidReaction_lipidFoldChange < 1){
                sp_FC_cutoff <- 1
                shiny::showNotification("The Fold change (FC) of significantly lipids must be between 1 and 8, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_lipidFoldChange', value=1)
              }
            }else{
              sp_FC_cutoff <- 1
              shiny::showNotification("The Fold change (FC) of significantly lipids must be numeric, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='NET_lipidReaction_lipidFoldChange', value=1)
            }
            if(is.numeric(input$NET_lipidReaction_lipidP)){
              sp_p_cutoff <- input$NET_lipidReaction_lipidP
              if(input$NET_lipidReaction_lipidP > 1){
                sp_p_cutoff <- 1
                shiny::showNotification("The p-value of significantly lipids must be between 0.001 and 1, so it is calculated by replacing it with 0.05.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_lipidP', value=1)
              }else if(input$NET_lipidReaction_lipidP < 0.001){
                sp_p_cutoff <- 0.001
                shiny::showNotification("The p-value of significantly lipids must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_lipidP', value=0.001)
              }
            }else{
              sp_p_cutoff <- 0.05
              shiny::showNotification("The p-value of significantly lipids must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
              shiny::updateNumericInput(inputId='NET_lipidReaction_lipidP', value=0.05)
            }
            if(is.numeric(input$NET_lipidReaction_classFoldChange)){
              class_FC_cutoff <- input$NET_lipidReaction_classFoldChange
              if(input$NET_lipidReaction_classFoldChange > 8){
                class_FC_cutoff <- 8
                shiny::showNotification("The Fold change (FC) of significantly class must be between 1 and 8, so it is calculated by replacing it with 8.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_classFoldChange', value=8)
              }else if(input$NET_lipidReaction_classFoldChange < 1){
                class_FC_cutoff <- 1
                shiny::showNotification("The Fold change (FC) of significantly class must be between 1 and 8, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_classFoldChange', value=1)
              }
            }else{
              class_FC_cutoff <- 1
              shiny::showNotification("The Fold change (FC) of significantly class must be numeric, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='NET_lipidReaction_classFoldChange', value=1)
            }
            if(is.numeric(input$NET_lipidReaction_classP)){
              class_p_cutoff <- input$NET_lipidReaction_classP
              if(input$NET_lipidReaction_classP > 1){
                class_p_cutoff <- 1
                shiny::showNotification("The p-value of significantly class must be between 0.001 and 1, so it is calculated by replacing it with 0.05.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_classP', value=1)
              }else if(input$NET_lipidReaction_classP < 0.001){
                class_p_cutoff <- 0.001
                shiny::showNotification("The p-value of significantly class must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
                shiny::updateNumericInput(inputId='NET_lipidReaction_classP', value=0.001)
              }
            }else{
              class_p_cutoff <- 0.05
              shiny::showNotification("The p-value of significantly class must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
              shiny::updateNumericInput(inputId='NET_lipidReaction_classP', value=0.05)
            }
            variables$NET.lipidReaction <- LipidSigR::nw_lipid_reaction(
              deSp_se=variables$NET.lipidReaction.deSp.se, 
              organism=input$NET_lipidReaction_organism,
              show_sp=input$NET_lipidReaction_showSpecies, 
              show_all_reactions=input$NET_lipidReaction_showAllReaction,
              sp_significant=input$NET_lipidReaction_lipidSignif, sp_p_cutoff=sp_p_cutoff, sp_FC_cutoff=sp_FC_cutoff,
              class_significant=input$NET_lipidReaction_classSignif, class_p_cutoff=class_p_cutoff, class_FC_cutoff=class_FC_cutoff)
            shiny::incProgress(0.34, detail='Analysis finish')
            if(is.list(variables$NET.lipidReaction)){
              variables$NET.lipidReaction.plot <- visNetwork::visNetwork(nodes=variables$NET.lipidReaction$table_node, edges=variables$NET.lipidReaction$table_edge) %>%
                visNetwork::visLayout(randomSeed=500) %>%
                visNetwork::visPhysics(
                  solver='barnesHut', stabilization=TRUE, 
                  barnesHut=list(gravitationalConstant=-2500)) %>%
                visNetwork::visInteraction(navigationButtons=TRUE) %>%
                visNetwork::visEvents(dragEnd="function () {this.setOptions( { physics: false } );}") %>%
                visNetwork::visOptions(
                  highlightNearest=list(enabled=TRUE, degree=1, hover=FALSE),
                  selectedBy="group", nodesIdSelection=TRUE)
            }else{
              shinyjs::hide('NET_lipidReaction_warning_div')
              shinyjs::hide('NET_lipidReaction_result_div')
              shinyWidgets::show_alert(
                title='Error',
                text=shiny::HTML(variables$NET.lipidReaction),
                html=TRUE, type ='error')
            }
          }else{
            shinyjs::show('NET_lipidReaction_warning_div')
            shinyjs::hide('NET_lipidReaction_result_div')
            
          }
          shiny::incProgress(0.34, detail='Analysis finish')
        },
        error=function(e) {
          shinyWidgets::show_alert(
            title='Error',
            text=HTML("<h4>Detect unknown input data format errors.</h4>
                               <h4>For the correct data format guidelines, please refer to the <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank'>FAQ</a></h4>.
                               <h4>If you need further assistance, please email us your data.(<a href='mailto:bioinfomics.web@gmail.com' target='_blank' style='color: darkblue;'>bioinfomics.web@gmail.com</a>)</h4>"),
            html=TRUE, type ='error')
          return()
        }
      )
    })
  }) #isolate
}) #shiny::observeEvent(input$NET_lipidReaction_start

#### Output: NET.pathwayActivityNetwork.demo.download ####
output$NET.lipidReaction.demo.download <- shiny::downloadHandler(
  filename=function() {
    "deSp.se.rds"
  },
  content=function(file) {
    file.copy("www/demo_dataset/deSp_twoGroup.rds", file)
  },
  contentType="application/zip"
)
shiny::outputOptions(output, "NET.lipidReaction.demo.download", suspendWhenHidden=FALSE)

output$NET.lipidReaction.check.message <- shiny::renderUI({
  shiny::validate(shiny::need(!is.null(variables$NET.lipidReaction.check), ""))
  variables$NET.lipidReaction.check
})

#### Output: NET.lipidReaction.edge.table ####
output$NET.lipidReaction.edge.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$NET.lipidReaction$table_reaction), "Without lipid class-related pathway!"))
  DT::datatable(variables$NET.lipidReaction$table_reaction %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 3)),
                escape=FALSE, selection='none', rownames=FALSE,
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=FALSE, pageLength=10, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=400, scroller=FALSE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets='_all'))))
  
}) #output$NET.lipidReaction.edge.table <- renderDataTable

#### Output: NET.lipidReaction.node.table ####
output$NET.lipidReaction.node.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$NET.lipidReaction$table_stat), "Without lipid class-related pathway!"))
  DT::datatable(variables$NET.lipidReaction$table_stat %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 3)),
                escape=FALSE, selection='none', rownames=FALSE,
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=FALSE, pageLength=10, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=400, scroller=FALSE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets='_all'))))
}) #output$NET.lipidReaction.node.table <- renderDataTable

#### Output: NET.lipidReaction.network ####
output$NET.lipidReaction.network <- visNetwork::renderVisNetwork({
  shiny::validate(shiny::need(!is.null(variables$NET.lipidReaction.plot), "No lipidReactionNetwork pathway found!"))
  variables$NET.lipidReaction.plot
  
}) #output$NET.lipidReaction.network <- renderVisNetwork

shiny::observeEvent(input$NET_lipidReaction_network_refresh, {
  shiny::isolate({
    output$NET.lipidReaction.network <- visNetwork::renderVisNetwork({
      shiny::validate(shiny::need(!is.null(variables$NET.lipidReaction.plot), "No reactome pathway found!"))
      variables$NET.lipidReaction.plot
      
    }) #output$NET.lipidReaction.network <- renderVisNetwork
  })
})

#########################
####  gatomNetwork   ####
#########################

#### Switch the default organism ####
observeEvent(input$NET_gatom_source, {
  if(input$NET_gatom_source == "NET_user_data") {
    updateRadioButtons(session, "NET_gatom_organism", selected = "human")
  } else if(input$NET_gatom_source == "NET_demo_data") {
    updateRadioButtons(session, "NET_gatom_organism", selected = "mouse")
  }
})

#### control reset button ####
shiny::observeEvent(input$NET_gatom_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('NET_gatom_warning_div')
  shinyjs::hide('NET_gatom_result_div')
  shinyjs::hide('NET_gatom_table_div')
  #### shinyjs reset control panel ####
  shinyjs::reset('NET_gatom_reset_div')
  
}) #shiny::observeEvent(input$NET_gatom_reset

shiny::observe({
  
  if(is.null(input$NET_gatom_deSpecies)){
    shinyjs::disable("NET_gatom_start")
  }else{
    shinyjs::enable("NET_gatom_start")
  }
  
}) #observe

#### Output: NET.gatom.demo.download ####
output$NET.gatom.demo.download <- shiny::downloadHandler(
  filename=function() {
    "deSp.se.rds"
  },
  content=function(file) {
    file.copy("www/demo_dataset/deSp_twoGroup.rds", file)
  },
  contentType="application/zip"
)
shiny::outputOptions(output, "NET.gatom.demo.download", suspendWhenHidden=FALSE)

#### control start button ####
shiny::observeEvent(input$NET_gatom_start, {
  shiny::isolate({
    withProgress(message='GATOM network', style='notification', detail="Upload data", value=0, {
      tryCatch(
        {
          #### import user dataset ####
          variables$NET.gatom.deSp.se <- readRDS(input$NET_gatom_deSpecies$datapath)
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
          shiny::incProgress(0.33, detail='Check data format')
          variables$NET.gatom.check <- check.despse(variables$NET.gatom.deSp.se, 'two')
          shiny::incProgress(0.33, detail='Analysis in progress')
          if(!grepl('xmark icon', variables$NET.gatom.check)){
            shinyjs::show('NET_gatom_warning_div')
            shinyjs::show('NET_gatom_result_div')
            shinyjs::hide('NET_gatom_table_div')
            shiny::incProgress(0.33, detail='Analysis in progress')
            if(is.numeric(input$NET_gatom_lipidP)){
              sp_p_cutoff <- input$NET_gatom_lipidP
              if(input$NET_gatom_lipidP > 1){
                sp_p_cutoff <- 1
                shiny::showNotification("The p-value of significantly lipids must be between 0.001 and 1, so it is calculated by replacing it with 0.05.", type="warning")
                shiny::updateNumericInput(inputId='NET_gatom_lipidP', value=1)
              }else if(input$NET_gatom_lipidP < 0.001){
                sp_p_cutoff <- 0.001
                shiny::showNotification("The p-value of significantly lipids must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
                shiny::updateNumericInput(inputId='NET_gatom_lipidP', value=0.001)
              }
            }else{
              sp_p_cutoff <- 0.05
              shiny::showNotification("The p-value of significantly lipids must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
              shiny::updateNumericInput(inputId='NET_gatom_lipidP', value=0.05)
            }
            if(is.numeric(input$NET_gatom_lipidFoldChange)){
              sp_FC_cutoff=input$NET_gatom_lipidFoldChange
              if(input$NET_gatom_lipidFoldChange > 8){
                sp_FC_cutoff <- 8
                shiny::showNotification("The Fold change (FC) of significantly lipids must be between 1 and 8, so it is calculated by replacing it with 8.", type="warning")
                shiny::updateNumericInput(inputId='NET_gatom_lipidFoldChange', value=8)
              }else if(input$NET_gatom_lipidFoldChange < 1){
                sp_FC_cutoff <- 1
                shiny::showNotification("The Fold change (FC) of significantly lipids must be between 1 and 8, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='NET_gatom_lipidFoldChange', value=1)
              }
            }else{
              sp_FC_cutoff <- 1
              shiny::showNotification("The Fold change (FC) of significantly lipids must be numeric, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='NET_gatom_lipidFoldChange', value=1)
            }
            variables$NET.gatom <- LipidSigR::nw_gatom(
              deSp_se=variables$NET.gatom.deSp.se,
              organism=input$NET_gatom_organism,
              n_lipid=input$NET_gatom_numLipid,
              sp_significant=input$NET_gatom_lipidSignif,
              sp_p_cutoff=sp_p_cutoff, sp_FC_cutoff=sp_FC_cutoff)
            if(is.list(variables$NET.gatom)){
              variables$NET.gatom.plot <- visNetwork::visNetwork(nodes=variables$NET.gatom$table_node, edges=variables$NET.gatom$table_edge) %>%
                visNetwork::visLayout(randomSeed=500) %>%
                visNetwork::visPhysics(
                  solver='barnesHut', stabilization=TRUE, 
                  barnesHut=list(gravitationalConstant=-3000)) %>%
                visNetwork::visInteraction(navigationButtons=TRUE) %>%
                visNetwork::visEvents(
                  dragEnd="function () {this.setOptions( { physics: false } );}") %>%
                visNetwork::visEdges(color=list(color="#DDDDDD",highlight="#C62F4B")) %>%
                visNetwork::visOptions(
                  highlightNearest=list(enabled=TRUE, degree=1, hover=FALSE),
                  selectedBy="group", nodesIdSelection=TRUE)
              shiny::incProgress(0.34, detail='Analysis finish')
            }else{
              shinyjs::hide('NET_gatom_warning_div')
              shinyjs::hide('NET_gatom_result_div')
              shinyjs::hide('NET_gatom_table_div')
              shinyWidgets::show_alert(
                title='Error',
                text=htmltools::HTML(variables$NET.gatom),
                html=TRUE,type ='error')
            }
          }else{
            shinyjs::show('NET_gatom_warning_div')
            shinyjs::hide('NET_gatom_result_div')
            shinyjs::hide('NET_gatom_table_div')
          }
          shiny::incProgress(0.34, detail='Analysis finish')
        },
        error=function(e) {
          shinyWidgets::show_alert(
            title='Error',
            text=htmltools::HTML("<h4>Detect unknown input data format errors.</h4>
                               <h4>For the correct data format guidelines, please refer to the <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank'>FAQ</a></h4>.
                               <h4>If you need further assistance, please email us your data.(<a href='mailto:bioinfomics.web@gmail.com' target='_blank' style='color: darkblue;'>bioinfomics.web@gmail.com</a>)</h4>"),
            html=TRUE,type ='error')
          return()
        }
      )
    })
  }) #isolate
}) #shiny::observeEvent(input$NET_gatom_start

#### control example button ####
shiny::observeEvent(input$NET_gatom_demo_start, {
  shiny::isolate({
    shiny::withProgress(message='GATOM network', style='notification', detail="Upload data", value=0, {
      shinyjs::show('NET_gatom_warning_div')
      shinyjs::show('NET_gatom_result_div')
      shinyjs::show('NET_gatom_table_div')
      variables$NET.gatom.deSp.se <- readRDS('www/demo_dataset/deSp_twoGroup.rds')
      shiny::incProgress(0.33, detail='Check data format')
      variables$NET.gatom.check <- check.despse(variables$NET.gatom.deSp.se, 'two')
      shiny::incProgress(0.33, detail='Analysis in progress')
      if(is.numeric(input$NET_gatom_lipidP)){
        sp_p_cutoff <- input$NET_gatom_lipidP
        if(input$NET_gatom_lipidP > 1){
          sp_p_cutoff <- 1
          shiny::showNotification("The p-value of significantly lipids must be between 0.001 and 1, so it is calculated by replacing it with 0.05.", type="warning")
          shiny::updateNumericInput(inputId='NET_gatom_lipidP', value=1)
        }else if(input$NET_gatom_lipidP < 0.001){
          sp_p_cutoff <- 0.001
          shiny::showNotification("The p-value of significantly lipids must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
          shiny::updateNumericInput(inputId='NET_gatom_lipidP', value=0.001)
        }
      }else{
        sp_p_cutoff <- 0.05
        shiny::showNotification("The p-value of significantly lipids must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
        shiny::updateNumericInput(inputId='NET_gatom_lipidP', value=0.05)
      }
      if(is.numeric(input$NET_gatom_lipidFoldChange)){
        sp_FC_cutoff=input$NET_gatom_lipidFoldChange
        if(input$NET_gatom_lipidFoldChange > 8){
          sp_FC_cutoff <- 8
          shiny::showNotification("The Fold change (FC) of significantly lipids must be between 1 and 8, so it is calculated by replacing it with 8.", type="warning")
          shiny::updateNumericInput(inputId='NET_gatom_lipidFoldChange', value=8)
        }else if(input$NET_gatom_lipidFoldChange < 1){
          sp_FC_cutoff <- 1
          shiny::showNotification("The Fold change (FC) of significantly lipids must be between 1 and 8, so it is calculated by replacing it with 1.", type="warning")
          shiny::updateNumericInput(inputId='NET_gatom_lipidFoldChange', value=1)
        }
      }else{
        sp_FC_cutoff <- 1
        shiny::showNotification("The Fold change (FC) of significantly lipids must be numeric, so it is calculated by replacing it with 1.", type="warning")
        shiny::updateNumericInput(inputId='NET_gatom_lipidFoldChange', value=1)
      }
      variables$NET.gatom <- LipidSigR::nw_gatom(
        deSp_se=variables$NET.gatom.deSp.se,
        organism=input$NET_gatom_organism,
        n_lipid=input$NET_gatom_numLipid,
        sp_significant=input$NET_gatom_lipidSignif,
        sp_p_cutoff=sp_p_cutoff, sp_FC_cutoff=sp_FC_cutoff)
      variables$NET.gatom.plot <- visNetwork::visNetwork(nodes=variables$NET.gatom$table_node, edges=variables$NET.gatom$table_edge) %>%
        visNetwork::visLayout(randomSeed=500) %>%
        visNetwork::visPhysics(
          solver='barnesHut', stabilization=TRUE, 
          barnesHut=list(gravitationalConstant=-3000)) %>%
        visNetwork::visInteraction(navigationButtons=TRUE) %>%
        visNetwork::visEvents(
          dragEnd="function () {this.setOptions( { physics: false } );}") %>%
        visNetwork::visEdges(color=list(color="#DDDDDD",highlight="#C62F4B")) %>%
        visNetwork::visOptions(
          highlightNearest=list(enabled=TRUE, degree=1, hover=FALSE),
          selectedBy="group", nodesIdSelection=TRUE)
      shiny::incProgress(0.34, detail='Analysis finish')
    })
  }) #isolate
}) #shiny::observeEvent(input$NET_gatom_start

#### control url start button ####
shiny::observeEvent(input$NET_gatom_url_start, {
  shiny::isolate({
    if(is.null(variables$NET.gatom.deSp.se)){
      shinyWidgets::sendSweetAlert(
        session=session, title="No DE results found",
        text="Please run Differential Expression analysis first, then click the GATOM button in the DE result download tab.",
        type="warning")
      return()
    }
    withProgress(message='GATOM network', style='notification', detail="Upload data", value=0, {
      ## Integrated app: NET.gatom.deSp.se already set by server_DE.R
      tryCatch(
        {
          shiny::incProgress(0.33, detail='Check data format')
          variables$NET.gatom.check <- check.despse(variables$NET.gatom.deSp.se, 'two')
          shiny::incProgress(0.33, detail='Analysis in progress')
          if(!grepl('xmark icon', variables$NET.gatom.check)){
            shinyjs::show('NET_gatom_warning_div')
            shinyjs::show('NET_gatom_result_div')
            shinyjs::hide('NET_gatom_table_div')
            shiny::incProgress(0.33, detail='Analysis in progress')
            if(is.numeric(input$NET_gatom_lipidP)){
              sp_p_cutoff <- input$NET_gatom_lipidP
              if(input$NET_gatom_lipidP > 1){
                sp_p_cutoff <- 1
                shiny::showNotification("The p-value of significantly lipids must be between 0.001 and 1, so it is calculated by replacing it with 0.05.", type="warning")
                shiny::updateNumericInput(inputId='NET_gatom_lipidP', value=1)
              }else if(input$NET_gatom_lipidP < 0.001){
                sp_p_cutoff <- 0.001
                shiny::showNotification("The p-value of significantly lipids must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
                shiny::updateNumericInput(inputId='NET_gatom_lipidP', value=0.001)
              }
            }else{
              sp_p_cutoff <- 0.05
              shiny::showNotification("The p-value of significantly lipids must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
              shiny::updateNumericInput(inputId='NET_gatom_lipidP', value=0.05)
            }
            if(is.numeric(input$NET_gatom_lipidFoldChange)){
              sp_FC_cutoff=input$NET_gatom_lipidFoldChange
              if(input$NET_gatom_lipidFoldChange > 8){
                sp_FC_cutoff <- 8
                shiny::showNotification("The Fold change (FC) of significantly lipids must be between 1 and 8, so it is calculated by replacing it with 8.", type="warning")
                shiny::updateNumericInput(inputId='NET_gatom_lipidFoldChange', value=8)
              }else if(input$NET_gatom_lipidFoldChange < 1){
                sp_FC_cutoff <- 1
                shiny::showNotification("The Fold change (FC) of significantly lipids must be between 1 and 8, so it is calculated by replacing it with 1.", type="warning")
                shiny::updateNumericInput(inputId='NET_gatom_lipidFoldChange', value=1)
              }
            }else{
              sp_FC_cutoff <- 1
              shiny::showNotification("The Fold change (FC) of significantly lipids must be numeric, so it is calculated by replacing it with 1.", type="warning")
              shiny::updateNumericInput(inputId='NET_gatom_lipidFoldChange', value=1)
            }
            variables$NET.gatom <- LipidSigR::nw_gatom(
              deSp_se=variables$NET.gatom.deSp.se,
              organism=input$NET_gatom_organism,
              n_lipid=input$NET_gatom_numLipid,
              sp_significant=input$NET_gatom_lipidSignif,
              sp_p_cutoff=sp_p_cutoff, sp_FC_cutoff=sp_FC_cutoff)
            if(is.list(variables$NET.gatom)){
              variables$NET.gatom.plot <- visNetwork::visNetwork(nodes=variables$NET.gatom$table_node, edges=variables$NET.gatom$table_edge) %>%
                visNetwork::visLayout(randomSeed=500) %>%
                visNetwork::visPhysics(
                  solver='barnesHut', stabilization=TRUE, 
                  barnesHut=list(gravitationalConstant=-3000)) %>%
                visNetwork::visInteraction(navigationButtons=TRUE) %>%
                visNetwork::visEvents(
                  dragEnd="function () {this.setOptions( { physics: false } );}") %>%
                visNetwork::visEdges(color=list(color="#DDDDDD",highlight="#C62F4B")) %>%
                visNetwork::visOptions(
                  highlightNearest=list(enabled=TRUE, degree=1, hover=FALSE),
                  selectedBy="group", nodesIdSelection=TRUE)
              shiny::incProgress(0.34, detail='Analysis finish')
            }else{
              shinyjs::hide('NET_gatom_warning_div')
              shinyjs::hide('NET_gatom_result_div')
              shinyjs::hide('NET_gatom_table_div')
              shinyWidgets::show_alert(
                title='Error',
                text=htmltools::HTML(variables$NET.gatom),
                html=TRUE,type ='error')
            }
            shiny::incProgress(0.34, detail='Analysis finish')
          }else{
            shinyjs::show('NET_gatom_warning_div')
            shinyjs::hide('NET_gatom_result_div')
            shinyjs::hide('NET_gatom_table_div')
          }
          shiny::incProgress(0.34, detail='Analysis finish')
        },
        error=function(e) {
          shinyWidgets::show_alert(
            title='Error',
            text=htmltools::HTML("<h4>Detect unknown input data format errors.</h4>
                               <h4>For the correct data format guidelines, please refer to the <a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank'>FAQ</a></h4>.
                               <h4>If you need further assistance, please email us your data.(<a href='mailto:bioinfomics.web@gmail.com' target='_blank' style='color: darkblue;'>bioinfomics.web@gmail.com</a>)</h4>"),
            html=TRUE,type ='error')
          return()
        }
      )
    })
  }) #isolate
}) #shiny::observeEvent(input$NET_gatom_url_start

output$NET.gatom.check.message <- renderUI({
  shiny::validate(shiny::need(!is.null(variables$NET.gatom.check), ""))
  variables$NET.gatom.check
})

#### Output: NET.gatom.edge.table ####
output$NET.gatom.edge.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$NET.gatom$table_reaction), "Without lipid class-related pathway!"))
  DT::datatable(variables$NET.gatom$table_reaction %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 3)),
                escape=FALSE, selection='none', rownames=FALSE,
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=FALSE, pageLength=10, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=400, scroller=FALSE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets='_all'))))
}) #output$NET.gatom.edge.table <- renderDataTable

#### Output: NET.gatom.node.table ####
output$NET.gatom.node.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$NET.gatom$table_stat), "Without lipid class-related pathway!"))
  DT::datatable(variables$NET.gatom$table_stat %>% 
                  dplyr::mutate_if(is.numeric, ~round(., 3)),
                escape=FALSE, selection='none', rownames=FALSE,
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=FALSE, pageLength=10, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=400, scroller=FALSE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets='_all'))))
  
}) #output$NET.gatom.node.table <- renderDataTable

output$NET.gatom.network <- visNetwork::renderVisNetwork({
  shiny::validate(shiny::need(!is.null(variables$NET.gatom.plot), "No gatom pathway found!"))
  variables$NET.gatom.plot
}) #output$NET.gatom.network <- renderVisNetwork


shiny::observeEvent(input$NET_gatom_network_refresh, {
  shiny::isolate({
    output$NET.gatom.network <- visNetwork::renderVisNetwork({
      shiny::validate(shiny::need(!is.null(variables$NET.gatom.plot), "No reactome pathway found!"))
      variables$NET.gatom.plot
    }) #output$NET.gatom.network <- renderVisNetwork
  })
})

############################
####  Assign variables  ####
############################
variables$NET.lipid.gene.path <- readRDS('www/mapping_table/lipid_gene_path.rds')
variables$NET.all.interaction <- readRDS('www/mapping_table/reactome_all_interaction.rds')
variables$NET.all.node <- readRDS('www/mapping_table/reactome_all_node.rds')
variables$NET.complex <- readRDS('www/mapping_table/reactome_complex.rds')
variables$NET.edge <- readRDS('www/mapping_table/reactome_edge.rds')
variables$NET.node <- readRDS('www/mapping_table/reactome_node.rds')
variables$NET.node.id.for.complex <- readRDS('www/mapping_table/reactome_node_id_for_complex.rds')
variables$NET.uniprot <- readRDS('www/mapping_table/uniprot.rds')

############################
####  Reactome pathway  ####
############################

#### control reset button ####
shiny::observeEvent(input$NET_reactome_reset, {
  #### shinyjs show/hide results ####
  shinyjs::hide('NET_reactome_result_div')
  shinyjs::hide('NET.reactome.table')
  #### shinyjs reset control panel ####
  shinyjs::reset('NET_reactome_reset_div')
}) #shiny::observeEvent(input$NET_reactome_reset

#### control start button ####
shiny::observeEvent(input$NET_reactome_start, {
  shiny::isolate({
    shiny::withProgress(message='Reactome network', style='notification', detail="Upload data", value=0, {
      shinyjs::show('NET_reactome_result_div')
      shinyjs::show('NET.reactome.table')
      shiny::incProgress(0.33, detail='Analysis in progress')
      variables$NET.reactome.result <- get_path(input$NET_reactome_lipid_class,
                                                as.numeric(input$NET_reactome_num_path),
                                                as.numeric(input$NET_reactome_max_path),
                                                variables$NET.all.interaction,
                                                variables$NET.all.node,
                                                variables$NET.complex,
                                                variables$NET.edge,
                                                variables$NET.node,
                                                variables$NET.node.id.for.complex,
                                                variables$NET.uniprot,
                                                variables$NET.lipid.gene.path)
      shiny::incProgress(0.33, detail='Analysis in progress')
      
      if(!is.null(variables$NET.reactome.result$reactome_path_table)){
        variables$NET.reactome.visnetwork <- reactome_network(variables$NET.reactome.result$reactome_path_table,
                                                              variables$NET.reactome.result[[2]])
      }else{
        variables$NET.reactome.visnetwork <- NULL
      }
      
      shiny::incProgress(0.34, detail='Analysis finish')
    })
  }) #isolate
}) #shiny::observeEvent(input$NET_reactome_start

#### Output: NET.reactome.table ####
output$NET.reactome.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$NET.reactome.result[[2]]), "Without lipid class-related pathway!"))
  DT::datatable(variables$NET.reactome.result[[2]][, c(1:5)],
                #caption='Lipid expression data',
                colnames=c('Path', 'Path rank', 'Node order', 'Node name', 'Node type'),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=10, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=400, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets=c(0,1,2,4)))))
  
}) #output$NET.reactome.table <- renderDataTable

#### Output: NET.reactome.network ####
output$NET.reactome.network <- visNetwork::renderVisNetwork({
  shiny::validate(shiny::need(!is.null(variables$NET.reactome.visnetwork$network), "No reactome pathway found!"))
  variables$NET.reactome.visnetwork$network
}) #output$NET.reactome.network <- renderVisNetwork

shiny::observeEvent(input$NET_reactome_network_refresh, {
  shiny::isolate({
    output$NET.reactome.network <- visNetwork::renderVisNetwork({
      shiny::validate(shiny::need(!is.null(variables$NET.reactome.visnetwork$network), "No reactome pathway found!"))
      variables$NET.reactome.visnetwork$network
    }) #output$NET.reactome.network <- renderVisNetwork
  })
})

#### update select input NET_reactome_lipid_class####
shiny::observe({
  reactome_class <- variables$NET.lipid.gene.path %>%
    dplyr::filter(DB == 'REACTOME', !is.na(path_name)) %>%
    dplyr::distinct(key_name) %>%
    dplyr::arrange(key_name) %>%
    .$key_name
  shiny::updateSelectInput(session, 'NET_reactome_lipid_class',
                           choices=reactome_class,
                           selected=c('PC', 'PE', 'PG'))
})


#########################################
####  Lipid-related gene enrichment  ####
#########################################

#### control reset button ####
shiny::observeEvent(input$NET_enrichment_reset, {
  #### shinyjs show/hide results ####
  shinyjs::hide('NET_enrichment_result_div')
  shinyjs::hide('NET.enrichment.table')
  #### shinyjs reset control panel ####
  shinyjs::reset('NET_enrichment_reset_div')
}) #shiny::observeEvent(input$NET_enrichment_reset

#### control start button ####
shiny::observeEvent(input$NET_enrichment_start, {
  shiny::isolate({
    shiny::withProgress(message='Lipid-related gene enrichment', style='notification', detail="Upload data", value=0, {
      shinyjs::show('NET_enrichment_result_div')
      shinyjs::show('NET.enrichment.table')
      shiny::incProgress(0.33, detail='Analysis in progress')
      genelist <- variables$NET.lipid.gene.path %>%
        dplyr::filter(key_name %in% input$NET_enrichment_lipid_class) %>%
        dplyr::filter(!is.na(gene_name)) %>%
        dplyr::distinct(gene_name) %>%
        .$gene_name
      
      ref.genelist <- variables$NET.lipid.gene.path %>%
        dplyr::distinct(gene_name) %>%
        .$gene_name
      if(is.numeric(input$NET_enrichment_pval)){
        sig_pvalue <- input$NET_enrichment_pval
        if(input$NET_enrichment_pval > 1){
          sig_pvalue <- 1
          shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 0.05.", type="warning")
          shiny::updateNumericInput(inputId='NET_enrichment_pval',value=1)
        }else if(input$NET_enrichment_pval < 0.001){
          sig_pvalue <- 0.001
          shiny::showNotification("The p-value must be between 0.001 and 1, so it is calculated by replacing it with 0.001.", type="warning")
          shiny::updateNumericInput(inputId='NET_enrichment_pval',value=0.001)
        }
      }else{
        sig_pvalue <- 0.05
        shiny::showNotification("The p-value must be numeric, so it is calculated by replacing it with 0.05.", type="warning")
        shiny::updateNumericInput(inputId='NET_enrichment_pval',value=0.05)
      }
      variables$NET.enrichment.result <- annotation(genelist, ref.genelist,
                                                    database=input$NET_enrichment_database,
                                                    sig_pvalue=sig_pvalue,
                                                    environmentDir='www/environment/')
      
      shiny::incProgress(0.33, detail='Analysis in progress')
      #### update numeric input NET_enrichment_node ####
      shiny::observe({
        if(nrow(variables$NET.enrichment.result$sig_term) < 50){
          shiny::updateNumericInput(session, inputId='NET_enrichment_node',
                                    max=nrow(variables$NET.enrichment.result$sig_term),
                                    step=round((nrow(variables$NET.enrichment.result$sig_term)-10)/5))
        }else{
          shiny::updateNumericInput(session, inputId='NET_enrichment_node',
                                    max=50)
        }
      })
      
      variables$NET.enrichment.visnetwork <- lipid_related_gene_network(variables$NET.enrichment.result$plot_table,
                                                                        node=input$NET_enrichment_node,
                                                                        similarity_cutoff=input$NET_enrichment_edge)
      shiny::incProgress(0.34, detail='Analysis finish')
    })
    
  }) #isolate
}) #shiny::observeEvent(input$NET_enrichment_start

#### Output: NET.enrichment.table ####
output$NET.enrichment.table <- DT::renderDataTable(server=FALSE, {
  shiny::validate(shiny::need(!is.null(variables$NET.enrichment.result$all_term), "Without significant terms!")) 
  DT::datatable(variables$NET.enrichment.result$all_term[,-4],
                #caption='Lipid expression data',
                colnames=c('Pathway', 'Lipid gene number', 'Pathway gene number', '-Log10(p-value)', 'Lipid gene symbol'),
                escape=FALSE, selection='none', rownames=FALSE,
                class="nowrap row-border",
                extensions=c('Buttons', 'Scroller'),
                options=list(scrollX=TRUE, pageLength=10, autoWidth=FALSE,
                             deferRender=TRUE, scrollY=400, scroller=TRUE, #Scroller
                             dom='Bfrtip', buttons=list('csv', 'copy'), #Buttons
                             columnDefs=list(list(className='dt-center', targets=c(2:4)))))
  
}) #output$NET.enrichment.table <- renderDataTable

#### Output: NET.enrichment.network ####
output$NET.enrichment.network <- visNetwork::renderVisNetwork({
  
  variables$NET.enrichment.visnetwork$network
  
}) #output$NET.enrichment.network <- renderVisNetwork

shiny::observeEvent(input$NET_enrichment_network_refresh, {
  shiny::isolate({
    output$NET.enrichment.network <- visNetwork::renderVisNetwork({
      variables$NET.enrichment.visnetwork$network
    }) #output$NET.enrichment.network <- renderVisNetwork
  })
})

#### update select input NET_enrichment_lipid_class####
shiny::observe({
  enrichment_class <- variables$NET.lipid.gene.path %>%
    dplyr::distinct(key_name) %>%
    dplyr::arrange(key_name) %>%
    .$key_name
  shiny::updateSelectInput(session, 'NET_enrichment_lipid_class',
                           choices=enrichment_class,
                           selected=c('PC', 'PE', 'PG'))
})