shiny::tabPanel(title=htmltools::HTML("<h4 style='font-size:18px;padding-top:20.5px;padding-bottom:20.5px;'>Network</h4>"),
                value='Network',
                #### Network Header Panel ####
                htmltools::h1('Network analysis'),
                htmltools::br(),
                shiny::fluidRow(
                  shiny::column(width=12,
                                ####################################
                                ####  Network Page Description  ####
                                ####################################
                                htmltools::div(style="background-color: PowderBlue;border-left: 8px solid Teal;padding: 15px",
                                               htmltools::HTML("<h6>In the network analysis section, five distinct types of networks are provided.</h6>
                                                               <h6> Results from 'Differential Expression' are further examined with the <strong>'Pathway Activity Network'</strong>, which computes flux changes in the lipid reaction network and helps identify active or suppressed pathways.
                                                               The <strong>'Lipid Reaction Network'</strong> graphically represents significant lipid classes or species within lipid biosynthetic pathways. 
                                                               Lastly, the <strong>'GATOM Network'</strong> isolates significant subnetworks within the constructed metabolite-level network.
                                                               </h6>
                                                               
                                                               <em style='font-size:17px; text-align:left; line-height: 25px; color:#CC6600;'>
                                                               <i class='fas fa-caret-right' role='presentation' aria-label='caret-right icon'></i>
                                                                 Note: The <strong>'Pathway activity network'</strong>, <strong>'Lipid reaction network'</strong>, and <strong>'GATOM network'</strong> are currently available <strong>only for two-group comparisons</strong>. Multi-group analysis support for these networks is under development.
                                                               </em>
                                                               
                                                               <h6>In the investigated lipid classes, the <strong>'Reactome network'</strong> aggregates reactions pertinent to these classes as documented in Reactome.
                                                               The <strong>'Lipid-related gene enrichment'</strong> analysis identifies genes associated with the lipid classes of interest, followed by an enrichment analysis to discern significant pathways.
                                                               </h6>
                                                               <br>
                                                               <em style='font-size:17px; text-align:left; line-height: 25px;'>
                                                               <i class='fas fa-caret-right' role='presentation' aria-label='caret-right icon'></i>
                                                               Demo dataset source: <a href='https://pubmed.ncbi.nlm.nih.gov/31742247/' target='_blank'>A nutritional memory effect counteracts benefits of dietary restriction in old mice (Nat Metab. 2019)</a>
                                                               </em>")
                                ), #div 
                                htmltools::br(),
                                ################################
                                ####  Network analysis tab  ####
                                ################################
                                shiny::tabsetPanel(id='NET_analysis_tab',
                                                   shiny::tabPanel(title='Pathway activity network',
                                                                   style='height:1900px;',
                                                                   shiny::column(width=12,
                                                                                 htmltools::h2('Pathway activity network'),
                                                                                 htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue; padding: 15px">',
                                                                                                 '<h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                                 The Pathway activity network module implements a flux‐based pathway analysis algorithm to quantify activity changes across lipid metabolic pathways. In this visualization, each node represents a lipid class and each directed edge a biochemical reaction; edge color encodes the computed pathway activity score (red for up-regulated/positive, blue for down-regulated/negative).
                                                                                                 </h4>
                                                                                                 <h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                                 An accompanying table details every reaction step, its activity score, and the genes involved.
                                                                                                 </h4>
                                                                                                 <h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                                 It is critical to acknowledge that lipid biochemical pathways exhibit significant variability across mammalian species, contrary to common assumptions. Thus, caution should be used when generalizing findings across species, recognizing that each may present unique lipidomic landscapes that could impact the interpretation and applicability of research outcomes.
                                                                                                 </h4> 
                                                                                                 <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                 <ul>
                                                                                                 
                                                                                                 <li> The "Pathway activity network" module is currently available <strong>only for two-group comparisons</strong>. Support for multi-group analysis is under development. </li>
                                                                                                 
                                                                                                 <li> For input data preparation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ6" target="_blank" style="color: darkblue;">FAQ</a>.
                                                                                                 You can download inputs directly from the <a href="https://lipidsig.bioinfomics.org/DE/" target="_blank" style="color: darkblue;">Differential expression analysis</a> to ensure format accuracy.</li>
                                                                                                 <li> Manipulate the network by clicking on the toolbar at the bottom right and left. </li>
                                                                                                 <li> View the corresponding information of a lipid or reaction by hovering the mouse on a specific node or line. </li>
                                                                                                 </ul></h4></div>'),
                                                                                 htmltools::br(),
                                                                                 shiny::sidebarLayout(fluid=TRUE,
                                                                                                      position='right',
                                                                                                      shiny::sidebarPanel(width=3,
                                                                                                                          shiny::radioButtons(inputId='NET_pathwayActivity_source', label=htmltools::h4('Data source'),
                                                                                                                                              choices=c('Example dataset'='NET_demo_data',
                                                                                                                                                        'Upload your data!'='NET_user_data'),
                                                                                                                                              selected='NET_demo_data'),
                                                                                                                          shiny::conditionalPanel(condition='input.NET_pathwayActivity_source == "NET_user_data"',
                                                                                                                                                  shiny::helpText("Only applicable for two-group comparisons."),
                                                                                                                                                  shiny::fileInput(inputId='NET_pathwayActivity_deSpecies', label='SummarizedExperiment object (.rds format):',
                                                                                                                                                                   accept=c(".rds"), multiple=FALSE)#fileInput #NET_pathwayActivity_exp_data
                                                                                                                                                  ),#conditionalPanel
                                                                                                                          htmltools::div(id='NET_pathwayActivity_reset_div',
                                                                                                                                         shiny::radioButtons(inputId='NET_pathwayActivity_organism', label='Organism:',
                                                                                                                                                             choices=c('Human'='human', 'Mouse'='mouse'),
                                                                                                                                                             selected='mouse')
                                                                                                                                         ),
                                                                                                                          shiny::conditionalPanel(condition='input.NET_pathwayActivity_source == "NET_demo_data"',
                                                                                                                                                  shiny::downloadButton(outputId='NET.pathwayActivity.demo.download', label='Download example'),
                                                                                                                                                  shiny::column(width=12,htmltools::br()),
                                                                                                                                                  shiny::actionButton(inputId='NET_pathwayActivity_demo_start', label='Submit', icon=shiny::icon('play')) #actionButton #NET_pathwayActivity_start
                                                                                                                                                  ),#conditionalPanel
                                                                                                                          shiny::conditionalPanel(condition='input.NET_pathwayActivity_source == "NET_user_data"',
                                                                                                                                                  shiny::actionButton(inputId='NET_pathwayActivity_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #DE_demo_upload
                                                                                                                                                  shiny::actionButton(inputId='NET_pathwayActivity_user_start', label='Submit', icon=shiny::icon('upload')) #actionButton #NET_pathwayActivity_start
                                                                                                                                                  ),#conditionalPanel
                                                                                                                          shiny::conditionalPanel(condition='input.NET_pathwayActivity_source == "DE"',
                                                                                                                                                  shiny::downloadButton(outputId='NET.pathwayActivity.url.download', label='Download DE .zip'),
                                                                                                                                                  shiny::column(width=12,htmltools::br()),
                                                                                                                                                  shiny::actionButton(inputId='NET_pathwayActivity_url_start', label='Submit', icon=shiny::icon('upload'))
                                                                                                                                                  ),#conditionalPanel
                                                                                                      ), #shiny::sidebarPanel
                                                                                                      shiny::mainPanel(width =9,
                                                                                                                       htmltools::div(id='NET_pathwayActivity_warning_div', style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px;display: none;",
                                                                                                                                      shiny::htmlOutput('NET.pathwayActivity.check.message') %>% shinycssloaders::withSpinner()
                                                                                                                       )
                                                                                                      ) #mainPanel
                                                                                 ), #sidebarLayout
                                                                                 htmltools::br(),
                                                                                 htmltools::div(id='NET_pathwayActivity_result_div', style='height:700px;display: none;',
                                                                                                visNetwork::visNetworkOutput(outputId='NET.pathwayActivity.network', height='600px') %>% shinycssloaders::withSpinner(),
                                                                                                shiny::column(width=5),
                                                                                                shiny::column(width=7, shiny::actionButton(inputId='NET_pathwayActivity_network_refresh', label='Refresh', icon=shiny::icon('sync-alt'))), #actionButton #NET_pathwayActivity_network_refresh
                                                                                                htmltools::br(),
                                                                                                htmltools::h3('Pathway activity network table'),
                                                                                                DT::dataTableOutput(outputId='NET.pathwayActivity.table', width='100%') %>% shinycssloaders::withSpinner()
                                                                                 ), #div #NET_pathwayActivity_result_div
                                                                                 htmltools::br(),
                                                                                 htmltools::br()
                                                                   ) #column
                                                   ),
                                                   shiny::tabPanel(title='Lipid reaction network',
                                                                   shiny::column(width=12,
                                                                                 htmltools::h2('Lipid reaction network'),
                                                                                 htmltools::HTML(
                                                                                   '<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue; padding: 15px">',
                                                                                   '<h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                   The Lipid reaction network renders differential lipid metabolism as a reaction-centric graph in which square nodes represent lipid classes (sized by significance: larger squares for smaller p-values) and circular nodes represent individual lipid species. Yellow squares and red circles denote up-regulated classes and species, respectively; purple squares and blue circles denote down-regulated ones; grey indicates non-significant or missing data.
                                                                                   </h4>
                                                                                   <h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                   Two distinct edge types appear: bold orange arrows trace actual biochemical reactions from one lipid entity to another, while thin grey lines simply link each class to its member species.
                                                                                   </h4>
                                                                                   <h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                   Beneath the graph are two interactive tables: the edge table details every reaction path with its enzyme EC number and associated genes, and the node table reports log2 fold-change and p-value for each class and species.
                                                                                   </h4>
                                                                                   <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                   <ul>
                                                                                   <li> The "Lipid reaction network" module is currently available <strong>only for two-group comparisons</strong>. Support for multi-group analysis is under development. </li>
                                                                                   <li> For input data preparation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ6" target="_blank" style="color: darkblue;">FAQ</a>.
                                                                                   You can download inputs directly from the <a href="https://lipidsig.bioinfomics.org/DE/" target="_blank" style="color: darkblue;">Differential expression analysis</a> to ensure format accuracy.</li>
                                                                                   <li> Manipulate the network by clicking on the toolbar at the bottom right and left. </li>
                                                                                   <li> View the corresponding information of a lipid or reaction by hovering the mouse on a specific node or line. </li>  
                                                                                   </h4>',
                                                                                   '</div>'
                                                                                 ),
                                                                                 htmltools::br(),
                                                                                 shiny::sidebarLayout(fluid=TRUE,
                                                                                                      position='right',
                                                                                                      shiny::sidebarPanel(width=12,
                                                                                                                          htmltools::div(id='NET_lipidReaction_reset_div',style='height: 410px;',
                                                                                                                                         shiny::column(width=12,
                                                                                                                                                       shiny::column(width=4,
                                                                                                                                                                     shiny::radioButtons(inputId='NET_lipidReaction_source', label=htmltools::h4('Data source'),
                                                                                                                                                                                         choices=c('Example dataset'='NET_demo_data',
                                                                                                                                                                                                   'Upload your data!'='NET_user_data'),
                                                                                                                                                                                         selected='NET_demo_data'),
                                                                                                                                                                     shiny::conditionalPanel(condition='input.NET_lipidReaction_source == "NET_user_data"',
                                                                                                                                                                                             shiny::helpText("Only applicable for two-group comparisons."),
                                                                                                                                                                                             shiny::fileInput(inputId='NET_lipidReaction_deSpecies', label='SummarizedExperiment object (.rds format):',
                                                                                                                                                                                                              accept=c(".rds"), multiple=FALSE)#fileInput #NET_lipidReaction_deLipid
                                                                                                                                                                     ),#conditionalPanel
                                                                                                                                                       ),
                                                                                                                                                       shiny::column(width=8,
                                                                                                                                                                     shiny::column(width=5,
                                                                                                                                                                                   shiny::selectInput(inputId='NET_lipidReaction_showSpecies', label='Species display mode:',
                                                                                                                                                                                                      choices=c('All lipid species'='all',
                                                                                                                                                                                                                'Species of significant classes only'='sigClass',
                                                                                                                                                                                                                'Hide all species'='none'),
                                                                                                                                                                                                      selected=c('sigClass'), multiple=FALSE), #selectInput #NET_lipidReaction_showSpecies
                                                                                                                                                                     ),
                                                                                                                                                                     shiny::column(width=5,
                                                                                                                                                                                   shiny::radioButtons(inputId='NET_lipidReaction_showAllReaction', label='Reaction display mode:',
                                                                                                                                                                                                       choices=c('All reactions of significant classes'='TRUE', 
                                                                                                                                                                                                                 'Reactions between significant classes only'='FALSE'), 
                                                                                                                                                                                                       selected='FALSE'), #radioButtons #NET_lipidReaction_showAllReaction
                                                                                                                                                                     ),
                                                                                                                                                                     shiny::column(width=2,
                                                                                                                                                                                   shiny::radioButtons(inputId='NET_lipidReaction_organism',
                                                                                                                                                                                                       label='Organism:',
                                                                                                                                                                                                       choices=c('Human'='human',
                                                                                                                                                                                                                 'Mouse'='mouse'))
                                                                                                                                                                     ),
                                                                                                                                                                     shiny::column(width=6,
                                                                                                                                                                                   shiny::selectInput(inputId='NET_lipidReaction_lipidSignif', label='Identify significant lipids:',
                                                                                                                                                                                                      choices=c('p-value'='pval',
                                                                                                                                                                                                                'adjusted p-value (padj)'='padj'),
                                                                                                                                                                                                      selected='pval', multiple=FALSE), #selectInput #NET_lipidReaction_lipidSignif
                                                                                                                                                                                   shiny::numericInput(inputId='NET_lipidReaction_lipidP',label='p-value of significantly lipids:',
                                                                                                                                                                                                       value=0.05, min=0.001, max=1, step=0.001),#numericInput #NET_lipidReaction_lipidP
                                                                                                                                                                                   shiny::numericInput(inputId='NET_lipidReaction_lipidFoldChange',label='Fold change (FC) of significantly lipids:',
                                                                                                                                                                                                       value=1, min=1, max=8) #numericInput #NET_lipidReaction_lipidFoldChange
                                                                                                                                                                     ),
                                                                                                                                                                     shiny::column(width=6,
                                                                                                                                                                                   shiny::selectInput(inputId='NET_lipidReaction_classSignif', label='Identify significant class:',
                                                                                                                                                                                                      choices=c('p-value'='pval',
                                                                                                                                                                                                                'adjusted p-value (padj)'='padj'),
                                                                                                                                                                                                      selected='pval', multiple=FALSE), #selectInput #NET_lipidReaction_classSignif
                                                                                                                                                                                   shiny::numericInput(inputId='NET_lipidReaction_classP',label='p-value of significantly class:',
                                                                                                                                                                                                       value=0.05, min=0.001, max=1, step=0.001),#numericInput #NET_lipidReaction_classP
                                                                                                                                                                                   shiny::numericInput(inputId='NET_lipidReaction_classFoldChange',label='Fold change (FC) of significantly class:',
                                                                                                                                                                                                       value=1, min=1, max=8) #numericInput #NET_lipidReaction_classFoldChange
                                                                                                                                                                     )
                                                                                                                                                       )
                                                                                                                                         ),
                                                                                                                                         shiny::column(width=12,
                                                                                                                                                       shiny::conditionalPanel(condition='input.NET_lipidReaction_source == "NET_user_data"',
                                                                                                                                                                               shiny::column(width=8),
                                                                                                                                                                               shiny::column(width=2, style='padding:0px;', shiny::actionButton(inputId='NET_lipidReaction_reset', label='Reset', icon=shiny::icon('redo'))), #actionButton #NET_lipidReaction_reset
                                                                                                                                                                               shiny::column(width=2, shiny::actionButton(inputId='NET_lipidReaction_start', label='Submit', icon=shiny::icon('upload'))) #actionButton #NET_lipidReaction_start
                                                                                                                                                                               
                                                                                                                                                       ),#conditionalPanel
                                                                                                                                                       shiny::conditionalPanel(condition='input.NET_lipidReaction_source == "NET_demo_data"',
                                                                                                                                                                               shiny::column(width=8),
                                                                                                                                                                               shiny::column(width=2, style='padding:0px;', shiny::downloadButton(outputId='NET.lipidReaction.demo.download', label='Download example')),
                                                                                                                                                                               shiny::column(width=2, shiny::actionButton(inputId='NET_lipidReaction_demo_start', label='Submit', icon=shiny::icon('play'))) #actionButton #NET_lipidReaction_start
                                                                                                                                                       ),#conditionalPanel
                                                                                                                                                       shiny::conditionalPanel(condition='input.NET_lipidReaction_source == "DE"',
                                                                                                                                                                               shiny::column(width=8),
                                                                                                                                                                               shiny::column(width=2, style='padding:0px;', shiny::downloadButton(outputId='NET.lipidReaction.url.download', label='Download DE .zip')),
                                                                                                                                                                               shiny::column(width=2, shiny::actionButton(inputId='NET_lipidReaction_url_start', label='Submit', icon=shiny::icon('upload'))) #actionButton #NET_lipidReaction_start
                                                                                                                                                       )#conditionalPanel
                                                                                                                                         )
                                                                                                                                         
                                                                                                                          ), #div #NET_lipidReaction_reset_div
                                                                                                      ), #shiny::sidebarPanel
                                                                                                      shiny::mainPanel(width =0) #mainPanel
                                                                                 ), #sidebarLayout
                                                                                 htmltools::br(),
                                                                                 htmltools::div(id='NET_lipidReaction_warning_div', style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px;display: none;",
                                                                                                shiny::htmlOutput('NET.lipidReaction.check.message') %>% shinycssloaders::withSpinner()
                                                                                 ),
                                                                                 htmltools::br(),
                                                                                 htmltools::div(id='NET_lipidReaction_result_div', style='height:700px;display: none;',
                                                                                                shiny::column(width=12,visNetwork::visNetworkOutput(outputId='NET.lipidReaction.network', height='600px') %>% shinycssloaders::withSpinner()),
                                                                                                shiny::column(width=12,
                                                                                                              shiny::column(width=5),
                                                                                                              shiny::column(width=7, shiny::actionButton(inputId='NET_lipidReaction_network_refresh', label='Refresh', icon=shiny::icon('sync-alt')))
                                                                                                ),
                                                                                                shiny::column(width=12,htmltools::br()),
                                                                                                shiny::column(width=12,
                                                                                                              htmltools::h3('Lipid reaction network edge table'),
                                                                                                              DT::dataTableOutput(outputId='NET.lipidReaction.edge.table', width='100%') %>% shinycssloaders::withSpinner(),
                                                                                                              htmltools::h3('Lipid reaction network node table'),
                                                                                                              DT::dataTableOutput(outputId='NET.lipidReaction.node.table', width='100%') %>% shinycssloaders::withSpinner()),
                                                                                                shiny::column(width=12,htmltools::br()),
                                                                                                shiny::column(width=12,htmltools::br())
                                                                                 ), #div #NET_lipidReaction_result_div
                                                                   ) #column
                                                   ),
                                                   shiny::tabPanel(title='GATOM network',
                                                                   shiny::column(width=12,
                                                                                 htmltools::h2('GATOM network'),
                                                                                 htmltools::HTML(
                                                                                   '<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue; padding: 15px">',
                                                                                   '<h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                   The GATOM network highlights key reactions involving differentially expressed lipid species. These lipids, identified by distinct abundance patterns, are mapped onto a comprehensive metabolic reaction network to construct an experiment-specific framework.
                                                                                   </h4>
                                                                                   <h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                   Each node in the network represents a differentially expressed lipid, with color indicating the direction and magnitude of fold change—red for upregulation and blue for downregulation. The size of each node corresponds to its statistical significance, with larger nodes representing smaller p-values. Edges in the network denote biochemical reactions connecting these lipid species.
                                                                                   </h4>
                                                                                   <h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                   Below the network, two accompanying tables provide detailed annotation. The edge table summarizes the metabolic paths and reactions connecting lipid species, along with their associated enzymes and genes. The node table lists differentially expressed lipids included in the network, displaying their fold change (Log2FC) and statistical significance (p-value). This integrated framework allows users to interpret lipid metabolic alterations at both the molecular and pathway levels.
                                                                                   </h4>
                                                                                   <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                   <ul>
                                                                                   <li> The "GATOM network" module is currently available <strong>only for two-group comparisons</strong>. Support for multi-group analysis is under development. </li>
                                                                                   <li> For input data preparation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ6" target="_blank" style="color: darkblue;">FAQ</a>.
                                                                                   You can download inputs directly from the <a href="https://lipidsig.bioinfomics.org/DE/" target="_blank" style="color: darkblue;">Differential expression analysis</a> to ensure format accuracy.</li>
                                                                                   <li> Manipulate the network by clicking on the toolbar at the bottom right and left. </li>
                                                                                   <li> View the corresponding information of a lipid or reaction by hovering the mouse on a specific node or line. </li>
                                                                                   </ul>
                                                                                   </h4>',
                                                                                   '</div>'),
                                                                                 htmltools::br(),
                                                                                 shiny::sidebarLayout(fluid=TRUE,
                                                                                                      position='right',
                                                                                                      shiny::sidebarPanel(width=3,
                                                                                                                          shiny::radioButtons(inputId='NET_gatom_source', label=htmltools::h4('Data source'),
                                                                                                                                              choices=c('Example dataset'='NET_demo_data',
                                                                                                                                                        'Upload your data!'='NET_user_data'),
                                                                                                                                              selected='NET_demo_data'),
                                                                                                                          htmltools::div(id='NET_gatom_reset_div',
                                                                                                                                         shiny::conditionalPanel(condition='input.NET_gatom_source == "NET_user_data"',
                                                                                                                                                                 shiny::helpText("Only applicable for two-group comparisons."),
                                                                                                                                                                 shiny::fileInput(inputId='NET_gatom_deSpecies', label='SummarizedExperiment object (.rds format):',
                                                                                                                                                                                  accept=c(".rds"), multiple=FALSE)
                                                                                                                                         ),#conditionalPanel
                                                                                                                                         shiny::sliderInput(inputId='NET_gatom_numLipid', label='Number of lipids:',
                                                                                                                                                            min=10, max=100, value=50, step=10), #sliderInput #NET_gatom_numLipid
                                                                                                                                         shiny::selectInput(inputId='NET_gatom_lipidSignif',label='Identify significant lipids:',
                                                                                                                                                            choices=c('p-value'='pval',
                                                                                                                                                                      'adjusted p-value (padj)'='padj'),
                                                                                                                                                            selected='pval', multiple=FALSE), #selectInput #NET_gatom_lipidSignif
                                                                                                                                         shiny::numericInput(inputId='NET_gatom_lipidP',label='p-value of significantly lipids:',
                                                                                                                                                             value=0.05, min=0.001, max=1, step=0.001),#numericInput #NET_gatom_lipidP
                                                                                                                                         shiny::numericInput(inputId='NET_gatom_lipidFoldChange',label='Fold change (FC) of significantly lipids:',
                                                                                                                                                             value=1, min=1, max=8),
                                                                                                                                         shiny::radioButtons(inputId='NET_gatom_organism',label='Organism:',
                                                                                                                                                             choices=c('Human'='human',
                                                                                                                                                                       'Mouse'='mouse'))#radioButtons #NET_gatom_reset_div
                                                                                                                          ), #div #NET_gatom_reset_div
                                                                                                                          shiny::conditionalPanel(condition='input.NET_gatom_source == "NET_demo_data"',
                                                                                                                                                  shiny::downloadButton(outputId='NET.gatom.demo.download', label='Download example'),
                                                                                                                                                  shiny::column(width=12,htmltools::br()),
                                                                                                                                                  shiny::actionButton(inputId='NET_gatom_demo_start', label='Submit', icon=shiny::icon('play')) #actionButton #NET_pathwayActivity_start
                                                                                                                          ),
                                                                                                                          shiny::conditionalPanel(condition='input.NET_gatom_source == "NET_user_data"',
                                                                                                                                                  shiny::actionButton(inputId='NET_gatom_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #NET_gatom_reset
                                                                                                                                                  shiny::actionButton(inputId='NET_gatom_start', label='Submit', icon=shiny::icon('upload')) #actionButton #NET_gatom_start
                                                                                                                          ),
                                                                                                                          shiny::conditionalPanel(condition='input.NET_gatom_source == "DE"',
                                                                                                                                                  shiny::downloadButton(outputId='NET.gatom.url.download', label='Download DE .zip'),
                                                                                                                                                  shiny::column(width=12,htmltools::br()),
                                                                                                                                                  shiny::actionButton(inputId='NET_gatom_url_start', label='Submit', icon=shiny::icon('upload'))
                                                                                                                          )
                                                                                                                          
                                                                                                      ), #shiny::sidebarPanel
                                                                                                      shiny::mainPanel(width =9,
                                                                                                                       htmltools::div(id='NET_gatom_warning_div', style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px;display: none;",
                                                                                                                                      shiny::htmlOutput('NET.gatom.check.message') %>% shinycssloaders::withSpinner()
                                                                                                                       ),
                                                                                                                       htmltools::br(),
                                                                                                                       htmltools::div(id='NET_gatom_result_div', style='height:700px;display: none;',
                                                                                                                                      shiny::column(width=12,visNetwork::visNetworkOutput(outputId='NET.gatom.network', height='600px') %>% shinycssloaders::withSpinner()),
                                                                                                                                      shiny::column(width=12,
                                                                                                                                                    shiny::column(width=5),
                                                                                                                                                    shiny::column(width=7, shiny::actionButton(inputId='NET_gatom_network_refresh', label='Refresh', icon=shiny::icon('sync-alt'))) #actionButton #NET_gatom_network_refresh
                                                                                                                                      ),
                                                                                                                                      shiny::column(width=12, htmltools::br())
                                                                                                                       )#div #NET_gatom_result_div
                                                                                                      ) #mainPanel
                                                                                 ), #sidebarLayout
                                                                                 div(id = 'NET_gatom_table_div',style='display: none;',
                                                                                     h3('GATOM network edge table'),
                                                                                     DT::dataTableOutput(outputId = 'NET.gatom.edge.table', width = '100%') %>% shinycssloaders::withSpinner(),
                                                                                     h3('GATOM network node table'),
                                                                                     DT::dataTableOutput(outputId = 'NET.gatom.node.table', width = '100%') %>% shinycssloaders::withSpinner(),
                                                                                     htmltools::br()
                                                                                 ) #div
                                                                   ) #column
                                                   ),
                                                   ############################
                                                   ####  Reactome pathway  ####
                                                   ############################
                                                   shiny::tabPanel(title='Reactome network',
                                                                   shiny::column(width=12,
                                                                                 htmltools::h2('Reactome network'),
                                                                                 htmltools::HTML(
                                                                                   '<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue; padding: 15px">',
                                                                                   '<h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                        The \'Reactome Network\' contains the interaction of multiple lipids and genes and is summarized from the Reactome database, a curated database encompassing pathways and reactions in human biology.
                                                        This function supports users to discover the metabolic networks between small molecules within Reactome Network topologies and their interactions with proteins.
                                                        We integrated the reactions in these pathways and explored the inputted lipids\' metabolic flow and regulatory proteins.
                                                        This information will be used to build an interactive network containing different nodes, including biochemical reactions (square), small molecules (diamond), proteins (round), or complexes (triangle).
                                                        <br>
                                                        The table below the network provides the corresponding information of the network, such as path, path rank, node order, and node name.
                                                        </h4>
                                                        <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                        <ul>
                                                        <li> Nodes representing the selected lipid classes are highlighted in larger size and red. </li>
                                                        <li> Manipulate the network by clicking on the toolbar at the bottom right and left. </li>
                                                        <li> Display or hide a specific node or line by clicking on it. </li>
                                                        </ul>
                                                        </h4>',
                                                                                   '</div>'
                                                                                 ),
                                                                                 htmltools::br(),
                                                                                 shiny::sidebarLayout(fluid=TRUE,
                                                                                                      position='right',
                                                                                                      shiny::sidebarPanel(width=3,
                                                                                                                          htmltools::div(id='NET_reactome_reset_div',
                                                                                                                                         shiny::selectInput(inputId='NET_reactome_lipid_class', label='Lipid class:',
                                                                                                                                                            choices=c('PC', 'PE', 'PG', 'TAG', 'DAG'),
                                                                                                                                                            selected=c('PC', 'PE', 'PG'), multiple=TRUE
                                                                                                                                         ), #selectInput #NET_reactome_lipid_class
                                                                                                                                         shiny::selectInput(inputId='NET_reactome_num_path',label='Number of path:',
                                                                                                                                                            choices=c(1:5),
                                                                                                                                                            selected=3, multiple=FALSE) %>%
                                                                                                                                           shinyhelper::helper(type="inline", title="Number of path", content=c("‘Number of path’ means the number of pathway between selected lipid classes.")),
                                                                                                                                         shiny::selectInput(inputId='NET_reactome_max_path', label='Maximum path length:',
                                                                                                                                                            choices=c(1:10),
                                                                                                                                                            selected=5, multiple=FALSE) %>%
                                                                                                                                           shinyhelper::helper(type="inline", title="Maximum path length",content=c("‘Maximum path length’ means the maximum links between nodes.")),
                                                                                                                          ), #div #NET_reactome_reset_div
                                                                                                                          shiny::actionButton(inputId='NET_reactome_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #NET_reactome_reset
                                                                                                                          shiny::actionButton(inputId='NET_reactome_start', label='Submit', icon=shiny::icon('play')) #actionButton #NET_reactome_start
                                                                                                      ), #shiny::sidebarPanel
                                                                                                      shiny::mainPanel(width=9,
                                                                                                                       shiny::conditionalPanel(condition='input.NET_reactome_start',
                                                                                                                                               htmltools::div(id='NET_reactome_result_div',
                                                                                                                                                              visNetwork::visNetworkOutput(outputId='NET.reactome.network', height='600px') %>% shinycssloaders::withSpinner(),
                                                                                                                                                              shiny::column(width=5),
                                                                                                                                                              shiny::column(width=7,
                                                                                                                                                                            shiny::actionButton(inputId='NET_reactome_network_refresh', label='Refresh', icon=shiny::icon('sync-alt'))) #actionButton #NET_reactome_network_refresh
                                                                                                                                               ) #div #NET_reactome_result_div
                                                                                                                       ) #conditionalPanel
                                                                                                      ) #mainPanel
                                                                                 ), #sidebarLayout
                                                                                 htmltools::br(),
                                                                                 shiny::conditionalPanel(condition='input.NET_reactome_start',
                                                                                                         htmltools::h3('Reactome network table'),
                                                                                                         DT::dataTableOutput(outputId='NET.reactome.table', width='100%') %>% shinycssloaders::withSpinner()
                                                                                 ), #conditionalPanel
                                                                                 htmltools::br(),
                                                                                 htmltools::br()
                                                                   ) #column
                                                   ), #tabPanel
                                                   #########################################
                                                   ####  Lipid-related gene enrichment  ####
                                                   #########################################
                                                   shiny::tabPanel(title='Lipid-related gene enrichment',
                                                                   shiny::column(width=12,
                                                                                 htmltools::h2('Lipid-related gene enrichment'),
                                                                                 htmltools::HTML(
                                                                                   '<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue; padding: 15px">',
                                                                                   '<h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                        This function network of the user-defined lipid-related gene sets is presented by summarising the pathways from KEGG and Reactome databases.
                                                        The mapped genes of the user-selected lipid class will be enriched and illustrated in the circular network diagram, which allows users to explore the significant functions and the relationship between lipid-related pathways and genes based on their lipid classes. The larger node size refers to more gene numbers.
                                                        <br>
                                                        The table below the network provides the corresponding information of the network, such as pathway, lipid gene number, pathway gene number, and -log10 p-value.
                                                        </h4>
                                                        <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                        <ul>
                                                        <li> Manipulate the network by clicking on the toolbar at the bottom right and left. </li>
                                                        <li> View the corresponding information of a node or line by hovering the mouse on it. </li>
                                                        </ul>
                                                        </h4>',
                                                                                   '</div>'
                                                                                 ),
                                                                                 htmltools::br(),
                                                                                 shiny::sidebarLayout(fluid=T,
                                                                                                      position='right',
                                                                                                      shiny::sidebarPanel(width=3,
                                                                                                                          htmltools::div(id='NET_enrichment_reset_div',
                                                                                                                                         shiny::selectInput(inputId='NET_enrichment_lipid_class', label='Lipid class:',
                                                                                                                                                            choices=c('PC', 'PE', 'PG', 'TAG', 'DAG'),
                                                                                                                                                            selected=c('PC', 'PE', 'PG'), multiple=TRUE), #selectInput #NET_enrich_lipid_class
                                                                                                                                         shiny::radioButtons(inputId='NET_enrichment_database', label='Database:',
                                                                                                                                                             choices=c('KEGG'='KEGG',
                                                                                                                                                                       'Reactome'='REACTOME'),
                                                                                                                                                             selected='KEGG', inline=FALSE), #radioButtons #NET_enrich_database
                                                                                                                                         shiny::radioButtons(inputId='NET_enrichment_method', label='Method:',
                                                                                                                                                             choices=c("Fisher's exact test"='fisher'),
                                                                                                                                                             selected='fisher', inline=FALSE), #radioButtons #NET_enrich_method
                                                                                                                                         shiny::numericInput(inputId='NET_enrichment_pval',label='p-value:',
                                                                                                                                                             value=0.05, min=0.001, max=1, step=0.001), #numericInput #NET_enrichment_pval
                                                                                                                                         shiny::sliderInput(inputId='NET_enrichment_node',label='Top N significant terms:',
                                                                                                                                                            min=10, max=50, value=30, step=10), #sliderInput #NET_enrichment_node
                                                                                                                                         shiny::sliderInput(inputId='NET_enrichment_edge',label='Gene similarity:',
                                                                                                                                                            min=0, max=1, value=0.5, step=0.1) %>% #sliderInput #NET_enrichment_edge
                                                                                                                                           shinyhelper::helper(type="inline", title="Gene similarity",
                                                                                                                                                               content=c("‘Gene similarity’ calculates the intersection of genes in term1 and genes in term2,
                                                                                                          then diveded by the genes number of term1 or term2."))
                                                                                                                          ), #div #NET_enrichment_reset_div
                                                                                                                          shiny::actionButton(inputId='NET_enrichment_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #NET_enrichment_reset
                                                                                                                          shiny::actionButton(inputId='NET_enrichment_start', label='Submit', icon=shiny::icon('play')) #actionButton #NET_enrichment_start
                                                                                                      ), #shiny::sidebarPanel
                                                                                                      shiny::mainPanel(width =9,
                                                                                                                       shiny::conditionalPanel(condition='input.NET_enrichment_start',
                                                                                                                                               htmltools::div(id='NET_enrichment_result_div',
                                                                                                                                                              visNetwork::visNetworkOutput(outputId='NET.enrichment.network', height='600px') %>% shinycssloaders::withSpinner(),
                                                                                                                                                              shiny::column(width=5),
                                                                                                                                                              shiny::column(width=7,
                                                                                                                                                                            shiny::actionButton(inputId='NET_enrichment_network_refresh', label='Refresh', icon=shiny::icon('sync-alt')
                                                                                                                                                                            ) #actionButton #NET_enrichment_network_refresh
                                                                                                                                                              ) #column
                                                                                                                                               ) #div #NET_enrichment_result_div
                                                                                                                       ) #conditionalPanel
                                                                                                      ) #mainPanel
                                                                                 ), #sidebarLayout
                                                                                 htmltools::br(),
                                                                                 shiny::conditionalPanel(condition='input.NET_enrichment_start',
                                                                                                         htmltools::h3('Lipid-related gene enrichment table'),
                                                                                                         DT::dataTableOutput(outputId='NET.enrichment.table', width='100%') %>% shinycssloaders::withSpinner()
                                                                                 ), #conditionalPanel
                                                                                 htmltools::br(),
                                                                                 htmltools::br()
                                                                   ) #column
                                                   ), #tabPanel #Lipid-related gene enrichment
                                ) #tabsetPanel #NET_analysis_tab
                  ) #column
                ) #fluidRow
)
