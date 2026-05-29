shiny::tabPanel(title=htmltools::HTML("<h4 style='font-size:18px;padding-top:18.5px;padding-bottom:20.5px;'>Enrichment</h4>"),

         value='Enrichment',
         #### Profiling Header Panel ####
         htmltools::h1('Enrichment'),
         shiny::fluidRow(
           shiny::column(width=12,
                         ###############################
                         ####  DE Page Description  ####
                         ###############################
                         htmltools::div(style="background-color: PowderBlue;border-left: 8px solid Teal;padding: 15px",
                                        htmltools::HTML('<h6>
                                                         Our platform supports two enrichment approaches: <strong>Over Representation Analysis (ORA)</strong> and <strong>Lipid Set Enrichment Analysis (LSEA)</strong>.
                                                         </h6>
                                                         <h6>
                                                         <strong>ORA</strong> identifies lipid categories (e.g., classes or subclasses) that are overrepresented among significantly changed lipid species. 
                                                         It is suitable when you have a defined list of differentially expressed lipids.
                                                         </h6>
                                                         <h6>
                                                         <strong>LSEA</strong> evaluates whether predefined lipid sets are enriched at the top or bottom of a ranked lipid list, without applying a strict significance cutoff. 
                                                         This method is useful for detecting subtle but coordinated changes in lipid characteristics.
                                                         </h6><br>
                                                         <em style="font-size:17px; text-align:left; line-height: 25px;">
                                                         <i class="fas fa-caret-right" role="presentation" aria-label="caret-right icon"></i>
                                                         Demo dataset source (two-group): <a href="https://pubmed.ncbi.nlm.nih.gov/29320510/" target="_blank">Adipose tissue ATGL modifies the cardiac lipidome in pressure-overload-induced left Ventricular failure (PLoS Genet. 2018)</a>
                                                        </em>
                                                        <br>
                                                        <em style="font-size:17px; text-align:left; line-height: 25px;">
                                                        <i class="fas fa-caret-right" role="presentation" aria-label="caret-right icon"></i>
                                                        Demo dataset source (multi-group): <a href="https://pubmed.ncbi.nlm.nih.gov/32165635/" target="_blank">Lipidomic and biophysical homeostasis of mammalian membranes counteracts dietary lipid perturbations to maintain cellular fitness (Nat Commun 11, 1339. 2020)</a>
                                                        </em>')
                                        ), ## div 
                         htmltools::br(),
                         #################################
                         ###   Enrichment Data Source  ###
                         #################################
                         htmltools::h2('Data Source'),
                         shiny::sidebarLayout(fluid=TRUE,
                                              shiny::sidebarPanel(width=4,
                                                                  shiny::radioButtons(inputId='Enrichment_source', label=htmltools::h4('Data source'),
                                                                                      choices=c('Example dataset(two group)'='Enrichment_demo_data',
                                                                                                'Example dataset(Multi-groups)'='Enrichment_demo_Multi_data',
                                                                                                'Upload your data!'='Enrichment_user_data'),
                                                                                      selected='Enrichment_demo_data'),
                                                                  htmltools::HTML('<script src="disable.js"></script>'),
                                                                  shiny::conditionalPanel(condition='input.Enrichment_source == "Enrichment_demo_data" | input.Enrichment_source == "Enrichment_demo_Multi_data"',
                                                                                          shiny::actionButton(inputId='Enrichment_demo_start', label='Submit', icon=shiny::icon('play')), #actionButton #NET_pathwayActivityNetwork_start,
                                                                                          shiny::downloadButton(outputId='Enrichment.demo.download', label='Download example'),
                                                                                          br()),## conditionalPanel
                                                                  shiny::conditionalPanel(condition='input.Enrichment_source == "Enrichment_user_data"',
                                                                                          htmltools::div(id='Enrichment_reset_div',
                                                                                                         shiny::fileInput(inputId='Enrichment_deSpecies', label='SummarizedExperiment object (.rds format):', accept=c(".rds"), multiple=FALSE) %>% #fileInput #Enrichment_deSpecies
                                                                                                           shinyhelper::helper(type="inline", title="Upload Differential Expression (.rds) File", size ="l",
                                                                                                                               content=c('<ol style="font-size: 0px;">',
                                                                                                                                         '<li style="font-size: 16px;">
                                                                                                                                         Upload the Diffrential Expression analysis result (<code>.rds</code> format).<br>
                                                                                                                                         The <code>.rds</code> file contains a <em>SummarizedExperiment</em> object that includes:
                                                                                                                                         <ul>
                                                                                                                                          <li>Lipid abundance matrix</li>
                                                                                                                                          <li>Group/sample metadata</li>
                                                                                                                                          <li>Lipid characteristics table</li>
                                                                                                                                          <li>Differential expression results</li>
                                                                                                                                         </ul>
                                                                                                                                         <p></p>
                                                                                                                                         To ensure compatibility and minimize format errors, we strongly recommend using the <code>.rds</code> output generated by our platform’s Differential Expression module. <br>
                                                                                                                                         <p></p>
                                                                                                                                         For details on preparing and downloading Differential Expression results, please refer to the <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ6" target="_blank" style="color: darkblue;">FAQ</a>.
                                                                                                                                         <p></p>
                                                                                                                                         </li>
                                                                                                                                         <li style="font-size: 16px;">
                                                                                                                                         Once the file is chosen and shown ‘Upload complete’ then press ‘Upload’.
                                                                                                                                         </li>
                                                                                                                                         </ol>')),
                                                                                                         htmltools::br(),
                                                                                                         shiny::actionButton(inputId='Enrichment_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #Enrichment_user_reset
                                                                                                         shiny::actionButton(inputId='Enrichment_upload', label='Upload', icon=shiny::icon('upload')) #actionButton #Enrichment_user_upload
                                                                                                         ),## div # Enrichment_reset_div
                                                                                          shiny::helpText("Upload your data table in .csv/.tsv/.xlsx") #helpText
                                                                                          ), ## conditionalPanel
                                                                  shiny::conditionalPanel(condition='input.Enrichment_source == "DE"',
                                                                                          shiny::actionButton(inputId='Enrichment_url_start', label='Submit', icon=shiny::icon('play')),
                                                                                          shiny::downloadButton(outputId='Enrichment.url.download', label="Download DE .zip"),
                                                                                          br()
                                                                                          ),
                                                                  htmltools::br(),
                                                                  htmltools::HTML("<a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ6' target='_blank' style='color: darkblue;'>How to prepare your dataset?</a>"),
                                                                  htmltools::br(),
                                                                  htmltools::HTML("<a href='https://lipidsig.bioinfomics.org/Tutorial/?Enrichment' target='_blank' style='color: darkblue;'>How to use this function?</a>")
                                                                  ), #sidebarPanel
                                              shiny::mainPanel(width=8,
                                                              htmltools::div(id='Enrichment_progress_div',style='display: none;',
                                                                             htmltools::div(
                                                                               style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                                               shiny::htmlOutput("Enrichment.checkEnrichmentInput")
                                                                               ), ## div
                                                                             shiny::helpText(htmltools::tags$p(shiny::icon("check"),": Successfully uploaded.", style="font-size: 16px;", htmltools::HTML('&nbsp;'), shiny::icon("times"), ": Error happaned. Please check your dataset.", htmltools::HTML('&nbsp;'), shiny::icon("exclamation"), ": Warning message.", style="font-size: 16px;")),
                                                                             htmltools::br()
                                                                             )## div Enrichment_progress_div
                                                               ) #mainPanel
                                              ), #sidebarLayout
                         htmltools::div(id='Enrichment_start_div', 
                                        style='display: none;height: 400px;',
                                        htmltools::h4(htmltools::strong('Uploaded data')),
                                        htmltools::br(),
                                        shiny::tabsetPanel(id='Enrichment_table_tab',
                                                    shiny::tabPanel(title="Processed abundance data", DT::dataTableOutput(outputId = 'Enrichment.processed.abundance') %>% shinycssloaders::withSpinner()), #dataTableOutput #Enrichment.processed.abundance
                                                    shiny::tabPanel(title="Group information", DT::dataTableOutput(outputId = 'Enrichment.processed.group.info') %>% shinycssloaders::withSpinner()), #dataTableOutput #Enrichment.processed.group.info
                                                    shiny::tabPanel(title="Lipid characteristics", DT::dataTableOutput(outputId = 'Enrichment.processed.lipid.char') %>% shinycssloaders::withSpinner()), #dataTableOutput #Enrichment.processed.lipid.char
                                                    shiny::tabPanel(title="Lipid id", DT::dataTableOutput(outputId = 'Enrichment.lipid.id') %>% shinycssloaders::withSpinner()), #dataTableOutput #Enrichment.lipid.id
                                                    shiny::tabPanel(title="Differential expression analysis result", DT::dataTableOutput(outputId = 'Enrichment.de.result') %>% shinycssloaders::withSpinner()) #dataTableOutput #Enrichment.de.result
                                                    ),
                                        htmltools::br(),
                                        shiny::column(width=12,
                                                      shiny::column(width=5),
                                                      shiny::column(width=2, shiny::actionButton(inputId='Enrichment_start', label='Start!', icon=shiny::icon('play'))), #actionButton #DE_user_start
                                                      shiny::column(width=5) 
                                                      ) ## column 12
                                        ),
                         htmltools::br(),
                         htmltools::div(id='Enrichment_tabPanel_div',style='display: none;',
                                        htmltools::h2('Result'),
                                        shiny::tabsetPanel(id='Enrichment_analysis_tab',
                                                           shiny::tabPanel(title='Over Representation Analysis',
                                                                           htmltools::h3('Over Representation Analysis'),
                                                                           htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue; padding: 15px">',
                                                                                           '<h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                           The Over Representation Analysis (ORA) identifies whether significantly altered lipid species are enriched in specific lipid-related categories, such as classes, chain lengths, or physicochemical properties.
                                                                                           This helps identify biological patterns within the differentially expressed lipids.
                                                                                           </h4>
                                                                                           <h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                           The analysis output includes:
                                                                                           <ul>
                                                                                           <h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                           <strong>Summary Bar Plot: </strong>This plot displays the top 10 significant up- and down-regulated terms for comparisons involving two groups, and the top 20 terms for comparisons involving multiple groups. 
                                                                                           Terms are ranked by statistical significance (−log10 p-value), and colored by lipid aspect (e.g., class, function, cellular component).
                                                                                           </h4>
                                                                                           <h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                           <strong>Characteristic-Specific Bar Plot: </strong>Based on the selected lipid characteristic from the dropdown menu (e.g., class, saturation, chain length), this plot shows the enrichment results for all terms under that characteristic.
                                                                                           Red bars represent up-regulated terms, blue bars indicate down-regulated terms, and grey bars denote non-significant terms.
                                                                                           </h4>
                                                                                           <h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                           <strong>Enrichment Table: </strong>A detailed, downloadable table listing all terms, p-values, adjusted p-values, and the associated lipid characteristics.
                                                                                           </h4>
                                                                                           </ul>
                                                                                           </h4>
                                                                                           
                                                                                           <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                           <ul>
                                                                                           <li> Configure your conditions by the control panel below, then click the \'Submit\' button. </li>
                                                                                           <li> For input data preparation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ6" target="_blank" style="color: darkblue;">FAQ</a>.
                                                                                           You can download inputs directly from the <a href="https://lipidsig.bioinfomics.org/DE/" target="_blank" style="color: darkblue;">Differential expression analysis</a> to ensure format accuracy.</li>
                                                                                           </ul></h4></div>'),
                                                                           htmltools::br(),
                                                                           htmltools::div(id='Enrichment_ORA_analysis_style_div',
                                                                                          style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:200px",
                                                                                          htmltools::div(id='Enrichment_ORA_analysis_reset_div',
                                                                                                         shiny::column(width=12,
                                                                                                                       shiny::column(width=4, style='padding-left: 0px;padding-right: 0px;',
                                                                                                                                     shiny::selectInput(inputId='Enrichment_ORA_enrich_stat', label='Identify significantly enriched lipid characteristics:',
                                                                                                                                                        choices=c('p-value'='pval',
                                                                                                                                                                  'adjusted p-value (padj)'='padj'),
                                                                                                                                                        selected='p', multiple=F, width='80%') #selectInput #Enrichment_ORA_enrich_stat
                                                                                                                                     ), ## column 4
                                                                                                                       shiny::column(width=4, style='padding-left: 0px;padding-right: 0px;',
                                                                                                                                     shiny::numericInput(inputId='Enrichment_ORA_enrich_pvalue', label='p-value of significantly enriched lipid characteristics:',
                                                                                                                                                         value=0.05, min=0.001, max=1, step=0.001, width='80%') #numericInput #Enrichment_ORA_enrich_pvalue
                                                                                                                                     ), ## column 4
                                                                                                                       shiny::column(width=4, style='padding-left: 0px;padding-right: 0px;',
                                                                                                                                     shiny::radioButtons(inputId='Enrichment_ORA_enrich_padj_method', label='Multiple testing correction:',
                                                                                                                                                         choices=c('Benjamini & Hochberg'='BH'), selected='BH', inline=TRUE) #radioButtons #Enrichment_enrich_padj_method
                                                                                                                                     ) ## column 4
                                                                                                                       ), ## column 12
                                                                                                         shiny::column(width=12,
                                                                                                                       shiny::column(width=9),
                                                                                                                       shiny::column(width=3,
                                                                                                                                     shiny::actionButton(inputId='Enrichment_ORA_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #Enrichment_ORA_reset
                                                                                                                                     shiny::actionButton(inputId='Enrichment_ORA_start', label='Submit', icon=shiny::icon('play')) #actionButton #Enrichment_ORA_start
                                                                                                                                     ) #column
                                                                                                                       )
                                                                                                         ) #div #DE_analysis_reset_div
                                                                                          ), #div #Enrichment_ORA_analysis_style_div
                                                                           htmltools::div(id='Enrichment_ORA_analysis_result_div', style='display: none;',
                                                                                          shiny::column(width=12, style='border: #ccc 2px solid;padding: 0px;',
                                                                                                        htmltools::br(),
                                                                                                        shiny::column(width=4,
                                                                                                                      htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue; padding: 15px">',
                                                                                                                                       '<h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                                                                       The bar plot shows the top 10 significant up-regulated and down-regulated terms for datasets involving two groups and the top 20 for multiple groups.
                                                                                                                                       </h4>
                                                                                                                                       <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                       <ul>
                                                                                                                                       <li> Hover the mouse on a specific bar to view the corresponding detailed information. </li>
                                                                                                                                       <li> For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>. </li>
                                                                                                                                       </ul></h4></div>')
                                                                                                                      ), ## column 4
                                                                                                        shiny::column(width=8, style='padding:0px;',
                                                                                                                      shiny::column(width=12,
                                                                                                                                    shiny::column(width=4),
                                                                                                                                    shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("Enrichment.ORA.all.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                    shiny::column(width=4, shiny::downloadButton("Enrichment.ORA.all.download", "Download", style="visibility:hidden;"))
                                                                                                                                    ), ## column 12
                                                                                                                      shiny::column(width=12,plotly::plotlyOutput(outputId='Enrichment.ORA.all.bar', height='600px') %>% shinycssloaders::withSpinner()) #plotlyOutput #Enrichment.all.bar
                                                                                                                      
                                                                                                                      ) ## column 8
                                                                                                        ),## column 12
                                                                                          shiny::column(width=12, htmltools::br()),
                                                                                          shiny::column(width=12, style='border: #ccc 2px solid;padding: 0px;',
                                                                                                        htmltools::br(),
                                                                                                        shiny::column(width=4,
                                                                                                                      htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue; padding: 15px">',
                                                                                                                                      '<h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                                                                       The bar plot classifies significant lipid species into \'up-regulated\' or \'down-regulated\' categories based on their log2 fold change, according to a selected characteristic.
                                                                                                                                       Red bars indicate up-regulated, blue bars represent down-regulated, and grey bars signify non-significant.
                                                                                                                                       </h4>
                                                                                                                                       <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                       <ul>
                                                                                                                                       <li> Select a characteristic of interest from the drop-down list above the plot. </li>
                                                                                                                                       <li> Hover the mouse on a specific bar to view the corresponding detailed information. </li>
                                                                                                                                       <li> For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>. </li>
                                                                                                                                       </ul></h4></div>')
                                                                                                                     ), ## column 4
                                                                                                        shiny::column(width=8,
                                                                                                                      shiny::column(width=12,
                                                                                                                                    shiny::column(width=4),
                                                                                                                                    shiny::column(width=4,shiny::uiOutput("Enrichment.ORA.char")),
                                                                                                                                    shiny::column(width=4)
                                                                                                                             ),
                                                                                                                      shiny::column(width=12,
                                                                                                                                    shiny::column(width=4),
                                                                                                                                    shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("Enrichment.ORA.each.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                    shiny::column(width=4, shiny::downloadButton("Enrichment.ORA.each.download", "Download", style="visibility:hidden;"))
                                                                                                                      ), ## column 12
                                                                                                                      shiny::column(width=12,plotly::plotlyOutput(outputId='Enrichment.ORA.each.bar', height='600px') %>% shinycssloaders::withSpinner()) #plotlyOutput #Enrichment.each.bar
                                                                                                                      )## column 8
                                                                                                        ), ## column 12
                                                                                          shiny::column(width=12,htmltools::br()),
                                                                                          shiny::column(width=12,DT::dataTableOutput(outputId='Enrichment.ORA.all.tab', height='100%') %>% shinycssloaders::withSpinner()) #plotlyOutput #Enrichment.all.tab
                                                                                          )## div Enrichment_ORA_analysis_result_div
                                                                           ), ## tabPanel Over Representation Analysis 
                                                           shiny::tabPanel(title='Lipid set enrichment analysis',
                                                                           htmltools::h3('Lipid set enrichment analysis'),
                                                                           htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue; padding: 15px">',
                                                                                           '<h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                           Lipid Set Enrichment Analysis (LSEA) evaluates whether predefined sets of lipids show statistically significant and coordinated differences between biological states (e.g., phenotypes). 
                                                                                           Unlike ORA, LSEA uses the full ranked list of lipid species and calculates enrichment based on Normalized Enrichment Score (NES).
                                                                                           </h4>
                                                                                           <h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                           The analysis output includes:
                                                                                           <ul>
                                                                                           <h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                           <strong>Summary Bar Plot: </strong>This plot displays the top 10 significant up- and down-regulated terms for comparisons involving two groups, and the top 20 terms for comparisons involving multiple groups. 
                                                                                           Terms are colored based on lipid aspect and ranked by NES.
                                                                                           </h4>
                                                                                           <h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                           <strong>Characteristic-Specific Bar Plot: </strong>Based on the selected lipid characteristic from the dropdown menu (e.g., class, saturation, chain length), this plot shows the enrichment results for all terms under that characteristic.
                                                                                           Red bars represent up-regulated terms, blue bars indicate down-regulated terms, and grey bars denote non-significant terms.
                                                                                           </h4>
                                                                                           <h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                           <strong>Enrichment Plot: </strong>Shows the enrichment score curve of a selected term, highlighting how the lipid set is distributed across the ranked list.
                                                                                           </h4>
                                                                                           <h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                           <strong>Enrichment Table: </strong>A detailed, downloadable table listing lipid set name, p-values, adjusted p-values, and enrichment scores.
                                                                                           </h4>
                                                                                           </ul>
                                                                                           </h4>
                                                                                           
                                                                                           <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                           <ul>
                                                                                           <li> Configure your conditions by the control panel below, then click the \'Submit\' button. </li>
                                                                                           <li> For input data preparation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ6" target="_blank" style="color: darkblue;">FAQ</a>.
                                                                                           You can download inputs directly from the <a href="https://lipidsig.bioinfomics.org/DE/" target="_blank" style="color: darkblue;">Differential expression analysis</a> to ensure format accuracy.</li>
                                                                                           </ul></h4></div>'),
                                                                           htmltools::br(),
                                                                           htmltools::div(id='Enrichment_LSEA_analysis_style_div',
                                                                                          style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:190px",
                                                                                          htmltools::div(id='Enrichment_LSEA_analysis_reset_div',
                                                                                                         shiny::column(width=12,
                                                                                                                       shiny::column(width=2, style='padding-left: 0px;padding-right: 0px;',
                                                                                                                                     shiny::radioButtons(inputId='Enrichment_enrich_LSEA_rank_by',label='Rank by :',
                                                                                                                                                         choices=c('log2 fold change (FC)'='log2FC',
                                                                                                                                                                   'p-value'='p',
                                                                                                                                                                   'adjusted p-value (padj)'='p.adj'),
                                                                                                                                                         selected='log2FC') #radioButtons #Enrichment_enrich_padj_method
                                                                                                                                     ), ## column 2
                                                                                                                       shiny::column(width=5, style='padding-left: 0px;padding-right: 0px;',
                                                                                                                                     shiny::selectInput(inputId='Enrichment_LSEA_enrich_stat', label='Identify significantly enriched lipid characteristics:',
                                                                                                                                                        choices=c('p-value'='pval',
                                                                                                                                                                  'adjusted p-value (padj)'='padj'),
                                                                                                                                                        selected='p', multiple=FALSE, width='80%') #selectInput #Enrichment_LSEA_enrich_stat
                                                                                                                                     ), ## column 5
                                                                                                                       shiny::column(width=5, style='padding-left: 0px;padding-right: 0px;',
                                                                                                                                     shiny::numericInput(inputId='Enrichment_LSEA_enrich_pvalue', label='p-value of significantly enriched lipid characteristics:',
                                                                                                                                                         value=0.05, min=0.001, max=1, step=0.001, width='80%') #numericInput #Enrichment_LSEA_enrich_pvalue
                                                                                                                                     ) ## column 5
                                                                                                                       ) ## column 12
                                                                                                         ), #div #DE_analysis_reset_div
                                                                                          shiny::column(width=12,
                                                                                                        shiny::column(width=9),
                                                                                                        shiny::column(width=3,
                                                                                                                      shiny::actionButton(inputId='Enrichment_LSEA_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #Enrichment_LSEA_reset
                                                                                                                      shiny::actionButton(inputId='Enrichment_LSEA_start', label='Submit', icon=shiny::icon('play')) #actionButton #Enrichment_LSEA_start
                                                                                                                      ) ## column 3
                                                                                                        )## column 12
                                                                                          ), #div #Enrichment_LSEA_analysis_style_div
                                                                           htmltools::div(id='Enrichment_LSEA_analysis_result_div', style='display: none;',
                                                                                          shiny::column(width=12, htmltools::br()),
                                                                                          shiny::column(width=12, style='border: #ccc 2px solid;margin: 5px;height: 800px;',
                                                                                                        shiny::column(width=12,htmltools::br()),
                                                                                                        shiny::column(width=12,
                                                                                                                      column(width=8, style='padding: 0px;', 
                                                                                                                             shiny::column(width=12,
                                                                                                                                           shiny::column(width=4),
                                                                                                                                           shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("Enrichment.LSEA.all.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                           shiny::column(width=4, shiny::downloadButton("Enrichment.LSEA.all.download", "Download", style="visibility:hidden;"))
                                                                                                                             ), ## column 12
                                                                                                                             shiny::column(width=12, plotly::plotlyOutput("Enrichment.LSEA.all.bar", height='700px') %>% shinycssloaders::withSpinner())),
                                                                                                                      column(width=4, style='padding: 0px;padding-right: 5px;',
                                                                                                                             htmltools::div(style='text-align:justify;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;height:350px; padding: 15px',
                                                                                                                             htmltools::HTML('<h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                                                                             The bar plot shows the top 10 significant up-regulated and down-regulated terms for datasets involving two groups and the top 20 for multiple groups.
                                                                                                                                             </h4>
                                                                                                                                             <h4 style="font-style:italic; font-weight: bolder; font-size:15px; line-height: 30px; color: #CD5C5C">
                                                                                                                                             <ul>
                                                                                                                                             <li> Click on a specific bar to display the corresponding enrichment plot. </li>
                                                                                                                                             <li> Hover the mouse on a specific bar to view the corresponding detailed information. </li>
                                                                                                                                             <li> For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" onclick="show()" style="color: darkblue;">FAQ</a>. </li>
                                                                                                                                             </ul>
                                                                                                                                             </h4>')),
                                                                                                                             htmltools::br(),
                                                                                                                             shiny::plotOutput("Enrichment.LSEA.all.enrichment",width='350px', height='350px') %>% shinycssloaders::withSpinner())
                                                                                                                       )## column 12
                                                                                                        ),
                                                                                          shiny::column(width=12, htmltools::br()),
                                                                                          shiny::column(width=12, style='border: #ccc 2px solid;margin: 5px;height: 880px;',
                                                                                                        shiny::column(width=12, htmltools::br()),
                                                                                                        shiny::column(width=12,
                                                                                                                      shiny::column(width=8,style='padding: 0px;', 
                                                                                                                                    shiny::column(width=12,
                                                                                                                                                  shiny::column(width=4),
                                                                                                                                                  shiny::column(width=4,shiny::uiOutput("Enrichment.LSEA.char")),
                                                                                                                                                  shiny::column(width=4)),
                                                                                                                                    shiny::column(width=12,
                                                                                                                                                  shiny::column(width=4),
                                                                                                                                                  shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("Enrichment.LSEA.each.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                                  shiny::column(width=4, shiny::downloadButton("Enrichment.LSEA.each.download", "Download", style="visibility:hidden;"))
                                                                                                                                    ), ## column 12
                                                                                                                                    shiny::column(width=12,plotly::plotlyOutput("Enrichment.LSEA.each.bar", height='700px') %>% shinycssloaders::withSpinner())),
                                                                                                                      shiny::column(width=4,style='padding: 0px;padding-right: 5px;', 
                                                                                                                                    htmltools::div(style='text-align:justify;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;height:475px; padding: 15px',
                                                                                                                                                   htmltools::HTML('<h4 style="font-size: 20px; text-align:left; line-height: 30px; color:black;">
                                                                                                                                                                   The bar plot classifies significant lipid species into \'up-regulated\' or \'down-regulated\' categories based on their log2 fold change, according to a selected characteristic.
                                                                                                                                                                   Red bars indicate up-regulated, blue bars represent down-regulated, and grey bars signify non-significant.
                                                                                                                                                                   </h4>
                                                                                                                                                                   <h4 style="font-style:italic; font-weight: bolder; font-size:15px; line-height: 27px; color: #CD5C5C">
                                                                                                                                                                   <ul>
                                                                                                                                                                   <li> Select a characteristic of interest from the drop-down list above the plot. </li>
                                                                                                                                                                   <li> Click on a specific bar to display the corresponding enrichment plot. </li>
                                                                                                                                                                   <li> Hover the mouse on a specific bar to view the corresponding detailed information. </li>
                                                                                                                                                                   <li> For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" onclick="show()" style="color: darkblue;">FAQ</a>. </li>
                                                                                                                                                                   </ul>
                                                                                                                                                                   </h4>')),
                                                                                                                                    htmltools::br(),
                                                                                                                                    shiny::plotOutput("Enrichment.LSEA.each.enrichment",width='350px', height='350px') %>% shinycssloaders::withSpinner())
                                                                                                                      )## column 12
                                                                                                        ),
                                                                                          shiny::column(width=12, htmltools::br()),
                                                                                          shiny::column(width=12, DT::dataTableOutput(outputId='Enrichment.LSEA.all.tab', height='100%') %>% shinycssloaders::withSpinner()) #plotlyOutput #Enrichment.LSEA.all.tab
                                                                                          ) ## div Enrichment_LSEA_analysis_result_div
                                                                           ) ## tabPanel Lipid set enrichment analysis
                                                           ) ## tabsetPanel Enrichment_analysis_tab
                                        )## div Enrichment_tabPanel_div
                         ) ## column 12
         ) ## fluidRow
) ## tabPanel
