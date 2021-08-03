
##############################
##############################
######                  ######
######   Network Page   ######
######                  ######
##############################
##############################

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

################################
####  Network analysis tab  ####
################################

############################
####  Reactome pathway  ####
############################

#### control reset button ####
observeEvent(input$NET_reactome_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('NET_reactome_result_div')
  shinyjs::hide('NET.reactome.table')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('NET_reactome_reset_div')
  
}) #observeEvent(input$NET_reactome_reset

#### control start button ####
observeEvent(input$NET_reactome_start, {
  
  #if(input$NET_reactome_start > 0){
    
    shinyjs::show('NET_reactome_result_div')
    shinyjs::show('NET.reactome.table')
    
    # variables$NET.all.interaction <- readRDS('www/mapping_table/reactome_all_interaction.rds')
    # variables$NET.all.node <- readRDS('www/mapping_table/reactome_all_node.rds')
    # variables$NET.complex <- readRDS('www/mapping_table/reactome_complex.rds')
    # variables$NET.edge <- readRDS('www/mapping_table/reactome_edge.rds')
    # variables$NET.node <- readRDS('www/mapping_table/reactome_node.rds')
    # variables$NET.node.id.for.complex <- readRDS('www/mapping_table/reactome_node_id_for_complex.rds')
    # variables$NET.uniprot <- readRDS('www/mapping_table/uniprot.rds')
    
    isolate({
      
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
      
      #### Output: NET.reactome.table ####
      output$NET.reactome.table <- renderDataTable(server = FALSE,{
        
        validate(need(!is.null(variables$NET.reactome.result[[2]]), "Without lipid class-related pathway!"))
        
        DT::datatable(variables$NET.reactome.result[[2]][, c(1:5)],
                      #caption = 'Lipid expression data',
                      colnames = c('Path', 'Path rank', 'Node order', 'Node name', 'Node type'),
                      escape = FALSE, selection = 'none', rownames = TRUE, 
                      class = "nowrap row-border",
                      extensions = c('Buttons', 'Scroller'),
                      options = list(scrollX = TRUE, pageLength = 10, autoWidth = FALSE, 
                                     deferRender = TRUE, scrollY = 400, scroller = TRUE, #Scroller
                                     dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                     columnDefs = list(list(className = 'dt-center', targets = c(1:3, 5)))))
        
      }) #output$NET.reactome.table <- renderDataTable
      
      if(!is.null(variables$NET.reactome.result$reactome_path_table)){
        variables$NET.reactome.visnetwork <- reactome_network(variables$NET.reactome.result$reactome_path_table, 
                                                              variables$NET.reactome.result[[2]])
      }else{
        variables$NET.reactome.visnetwork <- NULL
      }
      
      
      output$NET.reactome.network <- renderVisNetwork({
        
        validate(need(!is.null(variables$NET.reactome.visnetwork$network), "No reactome pathway found!"))
        variables$NET.reactome.visnetwork$network
        
      }) #output$NET.reactome.network <- renderVisNetwork
      
      
      observeEvent(input$NET_reactome_network_refresh, {
        
        # variables$NET.reactome.visnetwork <- reactome_network(variables$NET.reactome.result$reactome_path_table, 
        #                                                       input$NET_reactome_lipid_class)
        
        output$NET.reactome.network <- renderVisNetwork({
          
          validate(need(!is.null(variables$NET.reactome.visnetwork$network), "No reactome pathway found!"))
          variables$NET.reactome.visnetwork$network
          
        }) #output$NET.reactome.network <- renderVisNetwork
        
      })
      
    }) #isolate
    
  #}
  
}) #observeEvent(input$NET_reactome_start

#### update select input NET_reactome_lipid_class####
observe({
  reactome_class <- variables$NET.lipid.gene.path %>% 
    filter(DB == 'REACTOME', !is.na(path_name)) %>% 
    distinct(key_name) %>% 
    arrange(key_name) %>%
    .$key_name
  updateSelectInput(session, 'NET_reactome_lipid_class', 
                    choices = reactome_class, 
                    selected = c('PC', 'PE', 'PG'))
})


#########################################
####  Lipid-related gene enrichment  ####
#########################################

#### control reset button ####
observeEvent(input$NET_enrichment_reset, {
  
  #### shinyjs show/hide results ####
  shinyjs::hide('NET_enrichment_result_div')
  shinyjs::hide('NET.enrichment.table')
  
  #### shinyjs reset control panel ####
  shinyjs::reset('NET_enrichment_reset_div')
  
}) #observeEvent(input$NET_enrichment_reset

#### control start button ####
observeEvent(input$NET_enrichment_start, {
  
  if(input$NET_enrichment_start > 0){
    
    shinyjs::show('NET_enrichment_result_div')
    shinyjs::show('NET.enrichment.table')
    
    
    
    isolate({
      
      genelist <- variables$NET.lipid.gene.path %>% 
        filter(key_name %in% input$NET_enrichment_lipid_class) %>% 
        filter(!is.na(gene_name)) %>% 
        distinct(gene_name) %>% 
        .$gene_name
      
      ref.genelist <- variables$NET.lipid.gene.path %>% 
        distinct(gene_name) %>% 
        .$gene_name
      
      variables$NET.enrichment.result <- annotation(genelist, ref.genelist,
                                                    database = input$NET_enrichment_database, 
                                                    sig_pvalue = input$NET_enrichment_pval,
                                                    environmentDir='www/environment/')
      
      #### Output: NET.enrichment.table ####
      output$NET.enrichment.table <- renderDataTable(server = FALSE,{
        
        validate(need(!is.null(variables$NET.enrichment.result$all_term), "Without significant terms!"))
        
        DT::datatable(variables$NET.enrichment.result$all_term[,-4],
                      #caption = 'Lipid expression data',
                      colnames = c('Pathway', 'Lipid gene number', 'Pathway gene number', '-Log10(p-value)', 'Lipid gene symbol'),
                      escape = FALSE, selection = 'none', rownames = TRUE, 
                      class = "nowrap row-border",
                      extensions = c('Buttons', 'Scroller'),
                      options = list(scrollX = TRUE, pageLength = 10, autoWidth = FALSE, 
                                     deferRender = TRUE, scrollY = 400, scroller = TRUE, #Scroller
                                     dom = 'Bfrtip', buttons = list('csv', 'copy'), #Buttons
                                     columnDefs = list(list(className = 'dt-center', targets = c(2:4)))))
        
      }) #output$NET.enrichment.table <- renderDataTable
      
      #### update numeric input NET_enrichment_node ####
      observe({
        if(nrow(variables$NET.enrichment.result$sig_term) < 50){
          updateNumericInput(session, 
                             inputId = 'NET_enrichment_node', 
                             max = nrow(variables$NET.enrichment.result$sig_term))
        }else{
          updateNumericInput(session, 
                             inputId = 'NET_enrichment_node', 
                             max = 50)
        }
      })
      
    }) #isolate
    
    isolate({
      
      variables$NET.enrichment.visnetwork <- lipid_related_gene_network(variables$NET.enrichment.result$plot_table, 
                                                                        node = input$NET_enrichment_node, 
                                                                        similarity_cutoff = input$NET_enrichment_edge)
      
      output$NET.enrichment.network <- renderVisNetwork({
        
        variables$NET.enrichment.visnetwork$network
        
      }) #output$NET.enrichment.network <- renderVisNetwork
      
      
      observeEvent(input$NET_enrichment_network_refresh, {
        
        # variables$NET.enrichment.visnetwork <- lipid_related_gene_network(variables$NET.enrichment.result$plot_table, 
        #                                                                   node = input$NET_enrichment_node, 
        #                                                                   similarity_cutoff = input$NET_enrichment_edge)
        
        output$NET.enrichment.network <- renderVisNetwork({
          
          variables$NET.enrichment.visnetwork$network
          
        }) #output$NET.enrichment.network <- renderVisNetwork
        
      })
      
    }) #isolate
    
  }
  
}) #observeEvent(input$NET_enrichment_start


#### update select input NET_enrichment_lipid_class####
observe({
  enrichment_class <- variables$NET.lipid.gene.path %>% 
    distinct(key_name) %>% 
    arrange(key_name) %>%
    .$key_name
  updateSelectInput(session, 'NET_enrichment_lipid_class', 
                    choices = enrichment_class, 
                    selected = c('PC', 'PE', 'PG'))
})








