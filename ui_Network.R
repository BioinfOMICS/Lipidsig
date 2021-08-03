
tabPanel(title = h4('Network'), 
         value = 'Network',
         #### Network Header Panel ####
         h1('Network analysis'), 
         br(),
         fluidRow(column(width=12,
                         ####################################
                         ####  Network Page Description  ####
                         ####################################
                         div(id = 'NET_description_style_div', 
                             h3(p("Reactome pathway",style="text-align: left")),
                             h6('Our platform constructs two types of networks for users to investigate the lipid metabolism pathways and lipid-related gene enrichment.
                                The ‘Reactome Network’ contains the interaction of multiple lipids and genes and are summarized from the Reactome database,
                                which is a curated database encompassed pathways and reactions in human biology.
                                This function supports users to discover the metabolic networks between small molecules within Reactome Network topologies and their interactions with proteins.
                                We integrated the reactions in these pathways and explore the metabolic flow and regulatory proteins of the inputted lipids.
                                This information will be used to build an interactive network containing different nodes such as Biochemical Reaction, Small Molecule, Protein or Complex.'),
                             h3(p("Lipid-related gene enrichment",style="text-align: left")),
                             h6('By summarising the pathways from KEGG, Reactome, and GO databases, we present the functions networks of the user-defined lipid-related gene sets.
                                The mapped genes of the user-selected lipid class will be enriched and illustrated in the circular network diagram,
                                which allows users explore the significant functions and the relationship between lipid-related pathways and genes based on their lipid classes.'),
                             style="background-color: PowderBlue;border-left: 8px solid Teal;padding: 15px"
                         ), #div #NET_description_style_div
                         br(), 
                         ################################
                         ####  Network analysis tab  ####
                         ################################
                         tabsetPanel(id = 'NET_analysis_tab', 
                                     ############################
                                     ####  Reactome pathway  ####
                                     ############################
                                     tabPanel(title = 'Reactome network', 
                                              column(width = 12, 
                                                     h2('Reactome network'),
                                                     sidebarLayout(fluid = T, 
                                                                   position = 'right', 
                                                                   sidebarPanel(width = 3, 
                                                                                div(id = 'NET_reactome_reset_div', 
                                                                                    selectInput(inputId = 'NET_reactome_lipid_class', 
                                                                                                label = 'Lipid class:', 
                                                                                                choices = c('PC', 'PE', 'PG', 'TAG', 'DAG'), 
                                                                                                selected = c('PC', 'PE', 'PG'),
                                                                                                multiple = T
                                                                                    ), #selectInput #NET_reactome_lipid_class
                                                                                    selectInput(inputId = 'NET_reactome_num_path', 
                                                                                                label = 'Number of path:', 
                                                                                                choices = c(1:5), 
                                                                                                selected = 3,
                                                                                                multiple = F) %>%
                                                                                      helper(type = "inline",
                                                                                             title = "Number of path",
                                                                                             content = c("‘Number of path’ means the number of pathway between selected lipid classes.")),
                                                                                    selectInput(inputId = 'NET_reactome_max_path', 
                                                                                                label = 'Maximum path length:', 
                                                                                                choices = c(1:10), 
                                                                                                selected = 5,
                                                                                                multiple = F) %>%
                                                                                      helper(type = "inline",
                                                                                             title = "Maximum path length",
                                                                                             content = c("‘Maximum path length’ means the maximum links between nodes.")),
                                                                                ), #div #NET_reactome_reset_div
                                                                                actionButton(inputId = 'NET_reactome_reset', 
                                                                                             label = 'Reset', icon = icon('redo')
                                                                                ), #actionButton #NET_reactome_reset
                                                                                actionButton(inputId = 'NET_reactome_start', 
                                                                                             label = 'Submit', icon = icon('play')
                                                                                ) #actionButton #NET_reactome_start
                                                                   ), #sidebarPanel
                                                                   mainPanel(width = 9, 
                                                                             conditionalPanel(condition = 'input.NET_reactome_start',
                                                                                              div(id = 'NET_reactome_result_div', 
                                                                                                  visNetworkOutput(outputId = 'NET.reactome.network', height = '600px') %>% withSpinner(), 
                                                                                                  column(width = 5), 
                                                                                                  column(width = 7, 
                                                                                                         actionButton(inputId = 'NET_reactome_network_refresh', 
                                                                                                                      label = 'Refresh', 
                                                                                                                      icon = icon('sync-alt')
                                                                                                         ) #actionButton #NET_reactome_network_refresh
                                                                                                  ) #column
                                                                                              ) #div #NET_reactome_result_div
                                                                             ) #conditionalPanel 
                                                                   ) #mainPanel
                                                     ), #sidebarLayout
                                                     br(),
                                                     conditionalPanel(condition = 'input.NET_reactome_start',
                                                                      dataTableOutput(outputId = 'NET.reactome.table', width = '100%') %>% withSpinner()
                                                     ), #conditionalPanel 
                                                     br(), 
                                                     br()
                                              ) #column
                                     ), #tabPanel
                                     #########################################
                                     ####  Lipid-related gene enrichment  ####
                                     #########################################
                                     tabPanel(title = 'Lipid-related gene enrichment', 
                                              column(width = 12, 
                                                     h2('Lipid-related gene enrichment'),
                                                     sidebarLayout(fluid = T, 
                                                                   position = 'right', 
                                                                   sidebarPanel(width = 3, 
                                                                                div(id = 'NET_enrichment_reset_div',
                                                                                    selectInput(inputId = 'NET_enrichment_lipid_class', 
                                                                                                label = 'Lipid class:', 
                                                                                                choices = c('PC', 'PE', 'PG', 'TAG', 'DAG'), 
                                                                                                selected = c('PC', 'PE', 'PG'),
                                                                                                multiple = T
                                                                                                ), #selectInput #NET_enrich_lipid_class
                                                                                    radioButtons(inputId = 'NET_enrichment_database', 
                                                                                                 label = 'Database:', 
                                                                                                 choices = c('KEGG' = 'KEGG', 
                                                                                                             'Reactome' = 'REACTOME'), 
                                                                                                             #'GO BP' = 'BP', 
                                                                                                             #'GO CC' = 'CC', 
                                                                                                             #'GO MF' = 'MF'), 
                                                                                                 selected = 'KEGG', 
                                                                                                 inline = F
                                                                                                 ), #radioButtons #NET_enrich_database
                                                                                    radioButtons(inputId = 'NET_enrichment_method', 
                                                                                                 label = 'Method:', 
                                                                                                 choices = c("Fisher's exact test" = 'fisher'), 
                                                                                                 selected = 'fisher', 
                                                                                                 inline = F
                                                                                                 ), #radioButtons #NET_enrich_method
                                                                                    numericInput(inputId = 'NET_enrichment_pval', 
                                                                                                 label = 'p-value:', 
                                                                                                 value = 0.05, 
                                                                                                 min = 0.001, 
                                                                                                 max = 0.05
                                                                                                 ), #numericInput #NET_enrichment_pval
                                                                                    sliderInput(inputId = 'NET_enrichment_node', 
                                                                                                label = 'Top N significant terms:', 
                                                                                                min = 10, 
                                                                                                max = 50, 
                                                                                                value = 30, 
                                                                                                step = 10
                                                                                                ), #sliderInput #NET_enrichment_node
                                                                                    sliderInput(inputId = 'NET_enrichment_edge', 
                                                                                                label = 'Gene similarity:', 
                                                                                                min = 0, 
                                                                                                max = 1, 
                                                                                                value = 0.5, 
                                                                                                step = 0.1
                                                                                                ) %>% #sliderInput #NET_enrichment_edge
                                                                                      helper(type = "inline",
                                                                                             title = "Gene similarity",
                                                                                             content = c("‘Gene similarity’ calculates the intersection of genes in term1 and genes in term2,
                                                                                                          then diveded by the genes number of term1 or term2."))
                                                                                ), #div #NET_enrichment_reset_div
                                                                                actionButton(inputId = 'NET_enrichment_reset', 
                                                                                             label = 'Reset', icon = icon('redo')
                                                                                ), #actionButton #NET_enrichment_reset
                                                                                actionButton(inputId = 'NET_enrichment_start', 
                                                                                             label = 'Submit', icon = icon('play')
                                                                                ) #actionButton #NET_enrichment_start
                                                                   ), #sidebarPanel
                                                                   mainPanel(width =9, 
                                                                             conditionalPanel(condition = 'input.NET_enrichment_start',
                                                                                              div(id = 'NET_enrichment_result_div', 
                                                                                                  visNetworkOutput(outputId = 'NET.enrichment.network', height = '600px') %>% withSpinner(), 
                                                                                                  column(width = 5), 
                                                                                                  column(width = 7, 
                                                                                                         actionButton(inputId = 'NET_enrichment_network_refresh', 
                                                                                                                      label = 'Refresh', 
                                                                                                                      icon = icon('sync-alt')
                                                                                                         ) #actionButton #NET_enrichment_network_refresh
                                                                                                  ) #column
                                                                                              ) #div #NET_enrichment_result_div
                                                                             ) #conditionalPanel 
                                                                   ) #mainPanel
                                                     ), #sidebarLayout
                                                     br(),
                                                     conditionalPanel(condition = 'input.NET_enrichment_start',
                                                                      dataTableOutput(outputId = 'NET.enrichment.table', width = '100%') %>% withSpinner()
                                                     ), #conditionalPanel 
                                                     br(), 
                                                     br()
                                              ) #column
                                     ) #tabPanel #Lipid-related gene enrichment
                         ) #tabsetPanel #NET_analysis_tab
         ) #column
         ) #fluidRow
)