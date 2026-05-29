tabPanel(title=htmltools::HTML("<h4 style='font-size:18px;padding-top:20.5px;padding-bottom:20.5px;'>Correlation</h4>"),

         value='Correlation',
         #### Correlation Header Panel ####
         htmltools::h1('Correlation analysis'),
         htmltools::br(),
         shiny::fluidRow(
           shiny::column(width=12,
                         ########################################
                         ####  Correlation Page Description  ####
                         ########################################
                         htmltools::div(id='CORR_description_style_div',
                                        htmltools::h6('In this section, we provide a comprehensive correlation analysis to assist researchers to interrogate the clinical features that connect to lipids species and other mechanistically relevant lipid characteristics.
                                           Correlation analysis between lipids and clinical features is broadly used in many fields of study, such as Bowler RP et al. discovering that sphingomyelins are strongly associated with emphysema and glycosphingolipids are associated with COPD exacerbations.
                                           Hence, continuous clinical data can be uploaded here, and diverse correlation analyses are offered. For instance, the Correlation Coefficient and Linear Regression are supported for continuous clinical data. Moreover,
                                           lipids can be classified either by lipid species or by lipid categories when conducting these correlation analyses.',style="text-align: left"),
                                        htmltools::br(),
                                        htmltools::HTML('<em style="font-size:17px; text-align:left; line-height: 25px;">
                                              <i class="fas fa-caret-right" role="presentation" aria-label="caret-right icon"></i>
                                              Demo dataset source: <a href="https://pubmed.ncbi.nlm.nih.gov/25494452/" target="_blank">Plasma sphingolipids associated with chronic obstructive pulmonary disease phenotypes (Am J Respir Crit Care Med. 2015)</a>
                                             </em>'),
                                        style="background-color: PowderBlue;border-left: 8px solid Teal;padding: 15px"
                         ), #div #CORR_description_style_div
                         htmltools::br(),
                         ###################################
                         ####  Correlation Data Source  ####
                         ###################################
                         htmltools::h2('Data Source'),
                         shiny::sidebarLayout(fluid=TRUE,
                                              shiny::sidebarPanel(width=4,
                                                                  shiny::radioButtons(inputId='CORR_data_source', label=h4('Data source'),
                                                                                      choices=c('Example dataset (Am J Respir Crit Care Med. 2015)'='CORR_demo_data',
                                                                                                'Upload your data!'='CORR_user_data'),
                                                                                      selected='CORR_demo_data') %>% #radioButtons #CORR_data_source
                                                                    shinyhelper::helper(type="inline", title="Data source", size ="l",
                                                                                        content=c('<ol style="font-size: 0px;">',
                                                                                                  '<li style="font-size: 16px;">Lipid dataset can be uploaded by users or using example datasets. This information, namely Lipid abundance data, Condition table and Adjusted table (optional). Three tables must assign to a vector  all data needs to be uploaded in
                                                                                                  <mark style="background-color: white;color: red;">CSV</mark>, <mark style="background-color: white;color: red;">TSV</mark>, or <mark style="background-color: white;color: red;">XLSX</mark> format.
                                                                                                  The maximum file size is 30MB. <br>
                                                                                                  <ul style="font-size: 16px; color:red;">
                                                                                                  <li>NOTE: When uploading in XLSX format, ensure the data frame is on the first sheet. </li>
                                                                                                  </ul>
                                                                                                  </li>
                                                                                                  <br>',
                                                                                                  '<li style="font-size: 16px;">Once the files are chosen and shown ‘Upload complete’ then press ‘Upload’.</li>',
                                                                                                  '</ol>')),
                                                                  shiny::conditionalPanel(condition='input.CORR_data_source == "CORR_demo_data"',
                                                                                          shiny::actionButton(inputId='CORR_demo_cont_upload', label='Submit', icon=shiny::icon('upload')), #actionButton #CORR_demo_cont_upload
                                                                                          shiny::downloadButton(outputId='CORR.demo.download', label='Download example')
                                                                  ), #conditionalPanel
                                                                  shiny::conditionalPanel(condition='input.CORR_data_source == "CORR_user_data"',
                                                                                          htmltools::div(id='CORR_user_reset_div',
                                                                                                         shiny::fileInput(inputId='CORR_user_exp', label='Lipid expression data:', accept=c('.csv', '.tsv', '.xlsx'), multiple=FALSE) %>% #fileInput #CORR_user_exp
                                                                                                           shinyhelper::helper(type="inline", title="Lipid abundance data", size ="l",
                                                                                                                               content=c('<ol style="font-size: 0px;">',
                                                                                                                                         '<li style="font-size: 16px;">The first column must contain a list of unique lipids name(features). </li>',
                                                                                                                                         '<li style="font-size: 16px;">Other columns encompass the expressed values of groups under different conditions that you want to compare.</li>',
                                                                                                                                         '<li style="font-size: 16px;">An example of ‘Lipid abundance data’</li>',
                                                                                                                                         '</ol>',
                                                                                                                                         '<img src="Description/CORR_Lipid expression data.webp" style="border:2px #ccc solid;padding:20px;" loading="lazy" width="100%"/>')),
                                                                                                         shiny::fileInput(inputId='CORR_user_cond', label='Condition table:', accept=c('.csv', '.tsv', '.xlsx'), multiple=FALSE) %>% #fileInput #CORR_user_cond
                                                                                                           shinyhelper::helper(type="inline", title="Condition table", size ="l",
                                                                                                                               content=c('<ol style="font-size: 0px;">',
                                                                                                                                         '<li style="font-size: 16px;">The condition table encompasses sample names and clinical conditions (disease status, gene dependence score etc.), which assigned each sample to a specific condition for further association analysis.</li>',
                                                                                                                                         '<li style="font-size: 16px;">The first column must contain a list of samples name, MUST SAME AS THE SAMPLE NAME (COLUM NAMES) OF ‘Lipid abundance data’ !</li>',
                                                                                                                                         '<li style="font-size: 16px;">Other columns are clinical conditions, such as Emphysema, Exacerbations. NOTE: ALL VALUES MUST BE NUMERIC</li>',
                                                                                                                                         '<li style="font-size: 16px;">An example of ‘Condition table’</li>',
                                                                                                                                         '</ol>',
                                                                                                                                         '<img src="Description/CORR_Condition table.webp" style="border:2px #ccc solid;padding:20px;" loading="lazy" width="100%"/>')),
                                                                                                         shiny::fileInput(inputId='CORR_user_adj', label='Adjusted table (optional):', accept=c('.csv', '.tsv', '.xlsx'), multiple=FALSE) %>% #fileInput #CORR_user_adj
                                                                                                           shinyhelper::helper(type="inline", title="Adjusted table", size ="l",
                                                                                                                               content=c('<ol style="font-size: 0px;">',
                                                                                                                                         '<li style="font-size: 16px;">‘Adjusted table’ represents the user-defined variables that will be corrected in linear regression or logistic regression analysis, which can be the cancer types or the clinical information, like gender, age, or BMI.</li>',
                                                                                                                                         '<li style="font-size: 16px;">The first column must contain a list of samples name, MUST SAME AS THE SAMPLE NAME (COLUM NAMES) OF ‘Lipid abundance data’ !</li>',
                                                                                                                                         '<li style="font-size: 16px;">An example of ‘Adjusted table’:</li>',
                                                                                                                                         '</ol>',
                                                                                                                                         '<img src="Description/CORR_Adjusted table.webp" style="border:2px #ccc solid;padding:20px;" loading="lazy" width="100%"/>')),
                                                                                                         shiny::helpText("Upload your data table in .csv/.tsv/.xlsx")), #helpText
                                                                                          shiny::actionButton(inputId='CORR_user_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #CORR_user_reset
                                                                                          shiny::actionButton(inputId='CORR_user_upload', label='Upload', icon=shiny::icon('upload')) #actionButton #CORR_user_upload
                                                                  ), #conditionalPanel
                                                                  htmltools::br(),
                                                                  htmltools::HTML("<a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' onclick='show()' style='color: darkblue;'>How to prepare your dataset?</a>"),
                                                                  htmltools::br(),
                                                                  htmltools::HTML("<a href='https://lipidsig.bioinfomics.org/Tutorial/?Correlation' onclick='show()' style='color: darkblue;'>How to use this function?</a>")
                                              ), #sidebarPanel
                                              shiny::mainPanel(width=8,
                                                               htmltools::div(id='CORR_data_check_progress', style='display:none;',
                                                                              style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                                              shiny::htmlOutput("CORR_data_check_progress")
                                                               ) ## div CORR_data_check_progress
                                              ) #mainPanel
                         ), #sidebarLayout
                         htmltools::div(id='CORR_data_check_successful',style='display:none;',
                                        htmltools::div(id='CORR_data_Uploaded',
                                                       htmltools::h4(htmltools::strong('Uploaded data')),
                                                       shiny::tabsetPanel(id='CORR_user_lipid_exp_tab',
                                                                          shiny::tabPanel(title="Lipid abundance data", DT::dataTableOutput(outputId='CORR.raw.abundance') %>% shinycssloaders::withSpinner()), #tabPanel # Lipid abundance data
                                                                          shiny::tabPanel(title="Condition table (clinical factor)",DT::dataTableOutput(outputId='CORR.cond.raw') %>% shinycssloaders::withSpinner()), #dataTableOutput #CORR.cond.raw
                                                                          shiny::tabPanel(title="Adjusted table", DT::dataTableOutput(outputId='CORR.adj.raw') %>% shinycssloaders::withSpinner()) #dataTableOutput #CORR.adj.raw
                                                                          ) ## tabsetPanel # CORR_user_lipid_exp_tab
                                                       ),
                                        htmltools::br(),
                                        htmltools::div(id='CORR_data_warning_div',
                                                       style='display:none;text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px',
                                                       shiny::htmlOutput("CORR.Check.SE"),
                                                       shiny::helpText(htmltools::tags$p(shiny::icon("check"),": Successfully uploaded.", style="font-size:16px;", htmltools::HTML('&nbsp;'), shiny::icon("times"), ": Error happaned. Please check your dataset.", htmltools::HTML('&nbsp;'), shiny::icon("exclamation"), ": Warning message.", style="font-size: 16px;")),
                                                       htmltools::br()
                                        ),
                                        htmltools::br(),
                                        htmltools::div(id='CORR_data_processing_div',
                                                       style='display: none;background: #ecf0f1;border: 1px solid transparent;border-radius: 4px;height: 350px;',
                                                       shiny::column(width=12,
                                                                     htmltools::h3('Data processing')%>%
                                                                       shinyhelper::helper(type="inline", title="Data processing", size ="l",
                                                                                           content=c('<h4 style="font-size: 18px; text-align:left; line-height: 25px; color:darkblue;">
                                                                                           <ul>
                                                                                           <li>For detailed descriptions of missing values, sample normalization, and data transformation methods, please refer to the <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ13" target="_blank" style="color: red;">FAQ</a>.</li>
                                                                                           </ul>
                                                                                           </h4>',
                                                                                                     '<ol style="font-size: 0px;">',
                                                                                                     '<li style="font-size: 16px;">If you clicking on ‘Remove features with many missing values’, the threshold of the percentage of blank in the dataset that will be deleted can be defined by users.</li>',
                                                                                                     '<img src="Description/Data processing_1.webp" loading="lazy" />',
                                                                                                     '<li style="font-size: 16px;">The ‘Missing values imputation’ is for users to choose minimum, mean, and median to replace the missing values in the dataset.
                                                                                                                           If users select minimum, minimum will be multiplied by the value of user inputted. After uploading data, three datasets will show on the right-hand side.
                                                                                                                           When finishing checking data, click ‘Start’ for further analysis.</li>',
                                                                                                     '<img src="Description/Data processing_2.webp" loading="lazy" />',
                                                                                                     '<li style="font-size: 16px;">A variety of data transformation and normalization methods are available for selection. Use the drop-down menu to choose your preferred method.</li>',
                                                                                                     '<img src="Description/Data processing_3.webp" loading="lazy" />',
                                                                                                     '</ol>'))
                                                       ), ## column 12
                                                       column(width=12,
                                                              shiny::column(width=4,
                                                                            shiny::checkboxInput(inputId="CORR_rm_NA", label="Remove features with many missing values", value=TRUE), #checkboxInput #CORR_rm_NA
                                                                            shiny::conditionalPanel(condition='input.CORR_rm_NA',
                                                                                                    shiny::numericInput(inputId='CORR_filtration_param', label='More than % missing values',
                                                                                                                        value=70, min=5, max=100, step=5)
                                                                            ) #conditionalPanel
                                                              ),## column 4
                                                              shiny::column(width=4,
                                                                            shiny::selectInput(inputId='CORR_fill_NA', label='Fill missing value with:',
                                                                                               choices=c('Mean'='mean', 'Median'='median', 'Minimum'='min',
                                                                                                         'Quantile regression imputation of left-censored data'='QRILC',
                                                                                                         'Singular value decomposition'='SVD',
                                                                                                         'K Nearest Neighbors'='KNN',
                                                                                                         'International Risk Management Institute'='IRMI',
                                                                                                         'Probabilistic principal component analysis'='PPCA',
                                                                                                         'Bayesian. Principal Component Analysis'='BPCA'),
                                                                                               selected='min', multiple=FALSE), ## selectInput CORR_fill_NA
                                                                            shiny::conditionalPanel(condition='input.CORR_fill_NA == "min"',
                                                                                                    shiny::numericInput(inputId='CORR_fill_min', label='Multiply by minimum',
                                                                                                                        value=0.5, min=0.1, max=0.5, step=0.1)
                                                                            ), ## conditionalPanel (If the user chooses fill missing value with Minimum)
                                                                            shiny::conditionalPanel(condition='input.CORR_fill_NA == "QRILC"',
                                                                                                    shiny::numericInput(inputId='CORR_fill_QRILC', label='Tune sigma',
                                                                                                                        value=1, min=0.1, max=1, step=0.1)
                                                                            ), ## conditionalPanel (If the user chooses fill missing value with QRILC)
                                                                            shiny::conditionalPanel(condition="['SVD', 'PPCA', 'BPCA'].includes(input.CORR_fill_NA)",
                                                                                                    shiny::numericInput(inputId='CORR_fill_param', label='nPCs',
                                                                                                                        value=3, min=1, max=10, step=1)
                                                                            ), ## conditionalPanel (If the user chooses fill missing value with SVD/PPCA/BPCA)
                                                                            shiny::conditionalPanel(condition='input.CORR_fill_NA == "KNN"',
                                                                                                    shiny::numericInput(inputId='CORR_fill_KNN', label='The number of neighbors',
                                                                                                                        value=3, min=1, max=10, step=1)
                                                                            ) ## conditionalPanel (If the user chooses fill missing value with KNN)
                                                              ),## column 4
                                                              shiny::column(width=4,
                                                                            htmltools::h4('Data Normalization'),
                                                                            shiny::selectInput(inputId='CORR_normalization', label='Normalization with:', 
                                                                                               choices=c('None'='none', 
                                                                                                         'Percentage'='Percentage',
                                                                                                         'Probabilistic Quotient Normalization'='PQN',
                                                                                                         'Quantile normalization'='Quantile',
                                                                                                         'Normalization by sum'='Sum',
                                                                                                         'Normalization by median'='Median'),
                                                                                               selected='Percentage', multiple=FALSE),
                                                                            htmltools::h4('Data Transformation'),
                                                                            shiny::selectInput(inputId='CORR_transformation',
                                                                                               label='Transformation with:',
                                                                                               choices=c('None'='none',
                                                                                                         'log10'='log10',
                                                                                                         'Square root'='square',
                                                                                                         'Cube root'='cube'),
                                                                                               selected='log10', multiple=FALSE)
                                                              ),## column 4
                                                       ), ## column 12
                                                       shiny::column(width=12,
                                                                     shiny::column(width=7),
                                                                     shiny::column(width=3, shiny::actionButton(inputId='CORR_processing_reset', label='Reset processing method', icon=shiny::icon('redo'))), #actionButton #CORR_user_reset
                                                                     shiny::column(width=2, shiny::actionButton(inputId='CORR_processing_start', label='Processing', icon=shiny::icon('play'))) #actionButton #CORR_user_upload
                                                       ) ## column 12
                                        ), ## div # CORR_data_processing_div
                                        htmltools::br(),
                                        htmltools::div(id='CORR_data_summary_div',style='display:none;',
                                                       style="display:none;text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                       shiny::htmlOutput("CORR.data.summary")
                                        ), ## div CORR_data_summary_div
                                        htmltools::br(),
                                        htmltools::div(id='CORR_data_process_table_div',style='display:none;',
                                                       htmltools::h3(strong('Processed data')),
                                                       htmltools::div(style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                                      htmltools::HTML('<h4 style="font-size: 18px; text-align:left; line-height: 30px; color:black;">
                                                                                       <ul>
                                                                                       <li><strong>Processed abundance data</strong>: User-uploaded abundance data after data processing.</li> 
                                                                                       <li><strong>Condition table (clinical factor)</strong>: User-uploaded condition table.</li>
                                                                                       <li><strong>Adjusted table (if provided)</strong>: User-uploaded adjusted table.</li>
                                                                                       <li><strong>Lipid characteristics</strong>: Lipid characteristics converted according to the uploaded lipids in the abundance data. Detailed information about the converted characteristics can be found in the <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ11" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                       <li><strong>Lipid id</strong>: Links to the LION ID, LIPID MAPS ID, and other resource IDs for the uploaded lipids.</li>
                                                                                       <li><strong>Data quality</strong>: Box and density plots of the abundance data before and after data processing.</li>
                                                                                       </ul></h4>')
                                                       ),## div 
                                                       htmltools::br(),
                                                       shiny::tabsetPanel(id='CORR_user_process_table_tab',
                                                                          shiny::tabPanel(title="Processed abundance data", DT::dataTableOutput(outputId='CORR.processed.abundance') %>% shinycssloaders::withSpinner()), #dataTableOutput #CORR.processed.abundance
                                                                          shiny::tabPanel(title="Condition table (clinical factor)", DT::dataTableOutput(outputId='CORR.processed.cond') %>% shinycssloaders::withSpinner()), #dataTableOutput #CORR.processed.cond
                                                                          shiny::tabPanel(title="Adjusted table", DT::dataTableOutput(outputId='CORR.processed.adj') %>% shinycssloaders::withSpinner()), #dataTableOutput #CORR.adj.process
                                                                          shiny::tabPanel(title="Lipid characteristics", DT::dataTableOutput(outputId='CORR.processed.lipid.char') %>% shinycssloaders::withSpinner()), #dataTableOutput #CORR.user.exp
                                                                          shiny::tabPanel(title="Lipid id", DT::dataTableOutput(outputId='CORR.lipid.id') %>% shinycssloaders::withSpinner()), #dataTableOutput #CORR.user.exp
                                                                          shiny::tabPanel(title="Data quality",
                                                                                          style='height:900px;',
                                                                                          shiny::column(width=12,htmltools::br()),
                                                                                          shiny::column(width=12,
                                                                                                        shiny::column(width=5),
                                                                                                        shiny::column(width=2, style='padding: 0px;',
                                                                                                                      shiny::actionButton("CORR.processed.download.start", "Download PDF", icon=shiny::icon("download"))),
                                                                                                        shiny::column(width=5,downloadButton("CORR.processed.download", "Download", style="visibility: hidden;"))
                                                                                          ), ## column 12
                                                                                          shiny::column(width=12, htmltools::br()),
                                                                                          shiny::column(width=12,
                                                                                                        shiny::column(width=6, plotly::plotlyOutput(outputId='CORR.before.processed.boxplot') %>% shinycssloaders::withSpinner()),
                                                                                                        shiny::column(width=6, plotly::plotlyOutput(outputId='CORR.after.processed.boxplot') %>% shinycssloaders::withSpinner())
                                                                                          ), ## column 12
                                                                                          shiny::column(width=12, htmltools::br()),
                                                                                          shiny::column(width=12,
                                                                                                        shiny::column(width=6, plotly::plotlyOutput(outputId='CORR.before.processed.density') %>% shinycssloaders::withSpinner()),
                                                                                                        shiny::column(width=6, plotly::plotlyOutput(outputId='CORR.after.processed.density') %>% shinycssloaders::withSpinner())
                                                                                          ) ## column 12
                                                                          ) ## tabPanel # Data quality
                                                       ) #tabsetPanel
                                        )## div # CORR_data_process_table_div
                         ), ## div # CORR_data_check_successful
                         htmltools::br(),
                         htmltools::div(id='CORR_start_div',style='display:none;height:50px;',
                                        shiny::column(width=5), #column
                                        shiny::column(width=2, shiny::actionButton(inputId='CORR_start', label='Start!', icon=shiny::icon('play'))), #actionButton #CORR_user_start
                                        shiny::column(width=5) #column
                         ), ## div CORR_start_div
                         htmltools::hr(),
                         ####################################
                         ####  Correlation analysis tab  ####
                         ####################################
                         htmltools::div(id='CORR_tabPanel_div',style='display:none;',
                                        htmltools::h2('Result'),
                                        shiny::tabsetPanel(id='CORR_analysis_tab',
                                                           shiny::tabPanel(title='Lipid species analysis',
                                                                           htmltools::br(),
                                                                           shiny::navlistPanel(widths=c(3, 9),
                                                                                               id='CORR_species_list',
                                                                                               #######################
                                                                                               ####  Correlation  ####
                                                                                               #######################
                                                                                               shiny::tabPanel(title='Correlation',
                                                                                                               htmltools::h3('Correlation'),
                                                                                                               shiny::column(width=12,
                                                                                                                             htmltools::div(htmltools::h6('The Correlation Coefficient gives a summary view that tells researchers whether a relationship exists between clinical features and lipid species,
                                                                                                               how strong that relationship is and whether the relationship is positive or negative. Here we provide three types of correlations, Pearson, Spearman, and Kendall,
                                                                                                                                            and adjusted by Benjamini & Hochberg methods. The cut-offs for correlation coefficient and the p-value can be decided by users.'),
                                                                                                                                            htmltools::h6('A heatmap will show after users inputting cut-offs and choosing a value for clustering/methods for clustering.
                                                                                                                                                          Users can use either correlation coefficient between clinical features (e.g. genes) and lipid species or choose their statistic instead.'),
                                                                                                                                            style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px"),
                                                                                                                             htmltools::br(),
                                                                                                                             htmltools::div(id='CORR_species_corr_style_div',
                                                                                                                                            style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:320px",
                                                                                                                                            htmltools::div(id='CORR_species_corr_reset_div',
                                                                                                                                                           shiny::column(width=12,
                                                                                                                                                                         shiny::column(width=4,
                                                                                                                                                                                       shiny::selectInput(inputId='CORR_species_corr_method', label='Correlation method:',
                                                                                                                                                                                                          choices=c('Pearson'='pearson',
                                                                                                                                                                                                                    'Spearman'='spearman'),
                                                                                                                                                                                                          selected='pearson', multiple=FALSE), #selectInput #CORR_species_corr_method
                                                                                                                                                                                       shiny::radioButtons(inputId='CORR_species_corr_adj_stat_method', label='Multiple testing correction:',
                                                                                                                                                                                                           choices=c('Benjamini & Hochberg'='BH'),
                                                                                                                                                                                                           selected='BH', inline=FALSE), #radioButtons #CORR_species_corr_adj_stat_method
                                                                                                                                                                                       shiny::uiOutput("CORR.species.corr.sidecolor")
                                                                                                                                                                         ), ## column 4
                                                                                                                                                                         shiny::column(width=4,
                                                                                                                                                                                       shiny::selectInput(inputId='CORR_species_corr_sig_p', label='Identify significant lipids:',
                                                                                                                                                                                                          choices=c('p-value'='pval',
                                                                                                                                                                                                                    'adjusted p-value'='padj'),
                                                                                                                                                                                                          selected='padj', multiple=FALSE), #selectInput #CORR_species_corr_sig_p
                                                                                                                                                                                       shiny::numericInput(inputId='CORR_species_corr_pval', label='p-value:',
                                                                                                                                                                                                           value=1, min=0.001, max=1, step=0.001), #numericInput #CORR_species_corr_pval
                                                                                                                                                                                       shiny::numericInput(inputId='CORR_species_corr_coef', label='Correlation coefficient cutoff:',
                                                                                                                                                                                                           value=0, min=0, max=1, step=0.1 ) %>% #numericInput #CORR_species_corr_coef
                                                                                                                                                                                         shinyhelper::helper(type="inline", title="Cutoff of correlation coefficient Help",
                                                                                                                                                                                                             content=c("A coefficient of <0.1 indicates a negligible and >0.9 a very strong relationship, values in-between are disputable.")),
                                                                                                                                                                         ), ## column 4
                                                                                                                                                                         shiny::column(width=4,
                                                                                                                                                                                       shiny::radioButtons(inputId='CORR_species_corr_color', label='Value for clustering:',
                                                                                                                                                                                                           choices=c('correlation coefficient'='cor_coef',
                                                                                                                                                                                                                     'statistics'='statistic'),
                                                                                                                                                                                                           selected='cor_coef', inline=FALSE), #radioButtons #CORR_species_corr_color
                                                                                                                                                                                       shiny::selectInput(inputId='CORR_species_corr_dist', label='Distance measure:',
                                                                                                                                                                                                          choices=c('Pearson'='pearson',
                                                                                                                                                                                                                    'Spearman'='spearman',
                                                                                                                                                                                                                    'Kendall'='kendall'),
                                                                                                                                                                                                                    #"Euclidean"="euclidean",
                                                                                                                                                                                                                    #"Maximum"="maximum",
                                                                                                                                                                                                                    #"Manhattan"="manhattan",
                                                                                                                                                                                                                    #"Canberra"="canberra",
                                                                                                                                                                                                                    #"Binary"="binary",
                                                                                                                                                                                                                    #"Minkowski"="minkowski"),
                                                                                                                                                                                                          selected='spearman', multiple=FALSE), #selectInput #CORR_species_corr_dist
                                                                                                                                                                                       shiny::selectInput(inputId='CORR_species_corr_hclust', label='Clustering method:',
                                                                                                                                                                                                          choices=c('Complete'='complete',
                                                                                                                                                                                                                    'Single'='single',
                                                                                                                                                                                                                    'Median'='median',
                                                                                                                                                                                                                    'Average'='average',
                                                                                                                                                                                                                    "Ward.D"="ward.D",
                                                                                                                                                                                                                    "Ward.D2"="ward.D2",
                                                                                                                                                                                                                    "WPGMA"="mcquitty",
                                                                                                                                                                                                                    "WOGMC"="median",
                                                                                                                                                                                                                    "UPGMC"="centroid"),
                                                                                                                                                                                                          selected='average', multiple=FALSE) #selectInput #CORR_species_corr_hclust
                                                                                                                                                                         ) ## column 4
                                                                                                                                                           ) ## column 12
                                                                                                                                            ), #div #CORR_species_corr_reset_divs
                                                                                                                                            shiny::column(width=12),
                                                                                                                                            shiny::column(width=12,
                                                                                                                                                          shiny::column(width=4),
                                                                                                                                                          shiny::column(width=8, 
                                                                                                                                                                        shiny::actionButton(inputId='CORR_species_corr_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #CORR_species_corr_reset
                                                                                                                                                                        shiny::actionButton(inputId='CORR_species_corr_start', label='Submit', icon=shiny::icon('play')) #actionButton #CORR_species_corr_start
                                                                                                                                                          ) ## column 8
                                                                                                                                            ) ## column 12
                                                                                                                             ), #div #CORR_species_corr_style_div
                                                                                                                             htmltools::br()
                                                                                                               ) ## column 12
                                                                                               ), #tabPanel #Correlation
                                                                                               #############################
                                                                                               ####  Linear regression  ####
                                                                                               #############################
                                                                                               shiny::tabPanel(title='Linear regression',
                                                                                                               htmltools::h3('Linear regression'),
                                                                                                               shiny::column(width=12,
                                                                                                                             htmltools::h6('Linear regression is a statistical technique that uses several explanatory variables to predict the outcome of a continuous response variable,
                                                                                                                             allowing researchers to estimate the associations between lipid levels and clinical features.
                                                                                                                             For multiple linear regression analysis, additional variables in ‘adjusted table’ will be added into the algorithm and used to adjust the confounding effect.
                                                                                                                                           Once calculation completes, each lipid species will be assigned a beta coefficient and t statistic (p-value), which can be chosen for clustering. ',
                                                                                                                                           style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px"),
                                                                                                                             htmltools::br(),
                                                                                                                             htmltools::div(id='CORR_species_linear_style_div',
                                                                                                                                            style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:290px",
                                                                                                                                            htmltools::div(id='CORR_species_linear_reset_div',
                                                                                                                                                           shiny::column(width=12,
                                                                                                                                                                         shiny::column(width=4,
                                                                                                                                                                                       shiny::radioButtons(inputId='CORR_species_linear_adj_stat_method', label='Multiple testing correction:',
                                                                                                                                                                                                           choices=c('Benjamini & Hochberg'='BH'),
                                                                                                                                                                                                           selected='BH',inline=TRUE), #radioButtons #CORR_species_linear_adj_stat_method
                                                                                                                                                                                       shiny::uiOutput("CORR.species.linear.sidecolor")
                                                                                                                                                                         ), ## column 4
                                                                                                                                                                         shiny::column(width=4,
                                                                                                                                                                                       shiny::selectInput(inputId='CORR_species_linear_sig_p',label='Identify significant lipids:',
                                                                                                                                                                                                          choices=c('p-value'='pval',
                                                                                                                                                                                                                    'adjusted p-value'='padj'),
                                                                                                                                                                                                          selected='p', multiple=FALSE), #selectInput #CORR_species_linear_sig_p
                                                                                                                                                                                       shiny::numericInput(inputId='CORR_species_linear_pval',label='p-value:',
                                                                                                                                                                                                           value=1, min=0.001, max=1, step=0.001) #numericInput #CORR_species_linear_pval
                                                                                                                                                                         ), ## column 4
                                                                                                                                                                         shiny::column(width=4,
                                                                                                                                                                                       shiny::radioButtons(inputId='CORR_species_linear_color', label='Value for clustering:',
                                                                                                                                                                                                           choices=c('beta coefficient'='beta_coef',
                                                                                                                                                                                                                     't statistics'='t_statistic'),
                                                                                                                                                                                                           selected='beta_coef', inline=TRUE), #radioButtons #CORR_species_linear_color
                                                                                                                                                                                       shiny::selectInput(inputId='CORR_species_linear_dist', label='Distance measure:',
                                                                                                                                                                                                          choices=c('Pearson'='pearson',
                                                                                                                                                                                                                    'Spearman'='spearman',
                                                                                                                                                                                                                    'Kendall'='kendall',
                                                                                                                                                                                                                    "Euclidean"="euclidean",
                                                                                                                                                                                                                    "Maximum"="maximum",
                                                                                                                                                                                                                    "Manhattan"="manhattan",
                                                                                                                                                                                                                    "Canberra"="canberra",
                                                                                                                                                                                                                    "Binary"="binary",
                                                                                                                                                                                                                    "Minkowski"="minkowski"),
                                                                                                                                                                                                          selected='pearson', multiple=FALSE), #selectInput #CORR_species_linear_dist
                                                                                                                                                                                       shiny::selectInput(inputId='CORR_species_linear_hclust', label='Clustering method:',
                                                                                                                                                                                                          choices=c('Complete'='complete',
                                                                                                                                                                                                                    'Single'='single',
                                                                                                                                                                                                                    'Median'='median',
                                                                                                                                                                                                                    'Average'='average',
                                                                                                                                                                                                                    "Ward.D"="ward.D",
                                                                                                                                                                                                                    "Ward.D2"="ward.D2",
                                                                                                                                                                                                                    "WPGMA"="mcquitty",
                                                                                                                                                                                                                    "WOGMC"="median",
                                                                                                                                                                                                                    "UPGMC"="centroid"),
                                                                                                                                                                                                          selected='centroid', multiple=FALSE) #selectInput #CORR_species_linear_hclust
                                                                                                                                                                         ) ## column 4
                                                                                                                                                           ) ## column 12
                                                                                                                                            ), #div #CORR_species_linear_reset_div
                                                                                                                                            shiny::column(width=12),
                                                                                                                                            shiny::column(width=12,
                                                                                                                                                          shiny::column(width=4),
                                                                                                                                                          shiny::column(width=8,
                                                                                                                                                                        shiny::actionButton(inputId='CORR_species_linear_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #CORR_species_linear_reset
                                                                                                                                                                        shiny::actionButton(inputId='CORR_species_linear_start', label='Submit', icon=shiny::icon('play')) #actionButton #CORR_species_linear_start
                                                                                                                                                          ) #column
                                                                                                                                            )## column 12
                                                                                                                             ), #div #CORR_species_linear_style_div
                                                                                                                             htmltools::br()
                                                                                                               ) ## column 12
                                                                                               ) #tabPanel #Linear regression
                                                                           ), #navlistPanel
                                                                           htmltools::div(id='CORR_species_corr_result_div', style='display:none;',
                                                                                          shiny::column(width=3,
                                                                                                        htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                        '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                         The heatmap demonstrates a correlation between clinical features and lipid species.
                                                                                                                         Clinical features are displayed along the heatmap rows, while the columns represent various lipid species.
                                                                                                                         The color of each cell corresponds to the coefficient value, ranging from positive (red) to negative (blue) in a gradient.
                                                                                                                         </h4>
                                                                                                                         <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                         <ul>
                                                                                                                         <li>If the lipid/sample number exceeds 50, their names will not be displayed on the heatmap.</li>
                                                                                                                         <li>Hover on a specific heatmap cell to view more corresponding information.</li>
                                                                                                                         </ul></h4></div>')
                                                                                          ), ## column 
                                                                                          shiny::column(width=9,
                                                                                                        htmltools::h3('Lipid species-clinical features correlation heatmap',style='text-align:center;'),
                                                                                                        plotly::plotlyOutput(outputId='CORR.species.corr.heatmap', height='100%') %>% shinycssloaders::withSpinner(), #plotlyOutput #CORR.species.corr.heatmap
                                                                                                        htmltools::br(),
                                                                                                        shiny::column(12,
                                                                                                                      shiny::column(width=5),
                                                                                                                      shiny::column(width=2, style='padding:0px;text-align:center;', shiny::actionButton("CORR.species.corr.download.start", "Download PDF", icon=shiny::icon("download"))),
                                                                                                                      shiny::column(width=5, shiny::downloadButton("CORR.species.corr.download", "Download", style="visibility:hidden;"))
                                                                                                        ) ## column 12
                                                                                          ) ## column 9
                                                                           ), ## div # CORR_species_corr_result_div
                                                                           htmltools::div(id='CORR_species_linear_result_div', style='display: none;',
                                                                                          shiny::column(width=3,
                                                                                                        htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                        '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                         The heatmap demonstrates a correlation between clinical features and lipid species.
                                                                                                                         Clinical features are displayed along the heatmap rows, while the columns represent various lipid species.
                                                                                                                         The color of each cell corresponds to the coefficient value, ranging from positive (red) to negative (blue) in a gradient.
                                                                                                                         </h4>
                                                                                                                         <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                         <ul>
                                                                                                                         <li>If the lipid/sample number exceeds 50, their names will not be displayed on the heatmap.</li>
                                                                                                                         <li>Hover on a specific heatmap cell to view more corresponding information.</li>
                                                                                                                         </ul></h4></div>')
                                                                                          ), ## column 3
                                                                                          shiny::column(width=9,
                                                                                                        htmltools::h3('Lipid species-clinical features correlation heatmap',style='text-align: center;'),
                                                                                                        plotly::plotlyOutput(outputId='CORR.species.linear.heatmap', height='100%') %>% shinycssloaders::withSpinner(), #plotlyOutput #CORR.species.linear.heatmap
                                                                                                        htmltools::br(),
                                                                                                        shiny::column(12,
                                                                                                                      shiny::column(width=5),
                                                                                                                      shiny::column(width=2, style='padding:0px;text-align:center;', shiny::actionButton("CORR.species.linear.download.start", "Download PDF", icon=shiny::icon("download"))),
                                                                                                                      shiny::column(width=5, shiny::downloadButton("CORR.species.linear.download", "Download", style="visibility:hidden;"))
                                                                                                        ) ## column 12
                                                                                          ) ## column 8
                                                                           ) ## div # CORR_species_linear_result_divc
                                                           ), #tabPanel #Lipid species analysis
                                                           shiny::tabPanel(title='Lipid characteristics analysis',
                                                                           htmltools::br(),
                                                                           shiny::navlistPanel(widths=c(3, 9),
                                                                                               id='CORR_class_list',
                                                                                               #######################
                                                                                               ####  Correlation  ####
                                                                                               #######################
                                                                                               shiny::tabPanel(title='Correlation', 
                                                                                                               htmltools::h3('Correlation'),
                                                                                                               shiny::column(width=12,
                                                                                                                             htmltools::div(htmltools::h6('The Correlation Coefficient gives a summary view that tells researchers whether a relationship exists between clinical features and user-defined lipid characteristics,
                                                                                                                       how strong that relationship is and whether the relationship is positive or negative. Here we provide three types of correlations, Pearson, Spearman, and Kendall,
                                                                                                                       and adjusted by Benjamini & Hochberg methods. The cut-offs for correlation coefficient and the p-value can be decided by users. '),
                                                                                                                                            htmltools::h6('A heatmap will show after users inputting cut-offs and choosing a value for clustering/methods for clustering.
                                                                                                                       Users can use either correlation coefficient between clinical features (e.g. genes) and lipid characteristics or choose their statistic instead.'),
                                                                                                                                            style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px"),
                                                                                                                             htmltools::br(),
                                                                                                                             htmltools::div(id='CORR_class_corr_style_div',
                                                                                                                                            style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:320px",
                                                                                                                                            htmltools::div(id='CORR_class_corr_reset_div',
                                                                                                                                                           shiny::column(width=12,
                                                                                                                                                                         shiny::column(width=4,
                                                                                                                                                                                       shiny::uiOutput("CORR.class.corr.lipid.char"), #uiOutput #CORR_class_corr_lipid_char
                                                                                                                                                                                       shiny::selectInput(inputId='CORR_class_corr_method',
                                                                                                                                                                                                          label='Correlation method:',
                                                                                                                                                                                                          choices=c('Pearson'='pearson',
                                                                                                                                                                                                                    'Spearman'='spearman'),
                                                                                                                                                                                                          selected='pearson', multiple=FALSE), #selectInput #CORR_class_corr_method
                                                                                                                                                                                       shiny::radioButtons(inputId='CORR_class_corr_adj_stat_method',
                                                                                                                                                                                                           label='Multiple testing correction:',
                                                                                                                                                                                                           choices=c('Benjamini & Hochberg'='BH'),
                                                                                                                                                                                                           selected='BH', inline=TRUE) #radioButtons #CORR_class_corr_adj_stat_method 
                                                                                                                                                                         ), ## column 4
                                                                                                                                                                         shiny::column(width=4,
                                                                                                                                                                                       shiny::selectInput(inputId='CORR_class_corr_sig_p', label='Identify significant lipids:',
                                                                                                                                                                                                          choices=c('p-value'='pval',
                                                                                                                                                                                                                    'adjusted p-value'='padj'),
                                                                                                                                                                                                          selected='padj', multiple=FALSE), #selectInput #CORR_class_corr_sig_p
                                                                                                                                                                                       shiny::numericInput(inputId='CORR_class_corr_pval', label='p-value:',
                                                                                                                                                                                                           value=1, min=0.001, max=1, step=0.001), #numericInput #CORR_class_corr_pval
                                                                                                                                                                                       shiny::numericInput(inputId='CORR_class_corr_coef', label='Correlation coefficient cutoff:',
                                                                                                                                                                                                           value=0, min=0, max=1, step=0.1) %>% #numericInput #CORR_class_corr_coef
                                                                                                                                                                                         shinyhelper::helper(type="inline", title="Cutoff of correlation coefficientg Help",
                                                                                                                                                                                                             content=c("A coefficient of <0.1 indicates a negligible and >0.9 a very strong relationship, values in-between are disputable."))
                                                                                                                                                                         ), ## column 4
                                                                                                                                                                         shiny::column(width=4,
                                                                                                                                                                                       shiny::radioButtons(inputId='CORR_class_corr_color', label='Value for clustering:',
                                                                                                                                                                                                           choices=c('correlation coefficient'='cor_coef',
                                                                                                                                                                                                                     'statistics'='statistic'),
                                                                                                                                                                                                           selected='cor_coef', inline=FALSE), #radioButtons #CORR_class_corr_color
                                                                                                                                                                                       shiny::selectInput(inputId='CORR_class_corr_dist', label='Distance measure:',
                                                                                                                                                                                                          choices=c('Pearson'='pearson',
                                                                                                                                                                                                                    'Spearman'='spearman',
                                                                                                                                                                                                                    'Kendall'='kendall'),
                                                                                                                                                                                                                    #"Euclidean"="euclidean",
                                                                                                                                                                                                                    #"Maximum"="maximum",
                                                                                                                                                                                                                    #"Manhattan"="manhattan",
                                                                                                                                                                                                                    #"Canberra"="canberra",
                                                                                                                                                                                                                    #"Binary"="binary",
                                                                                                                                                                                                                    #"Minkowski"="minkowski"),
                                                                                                                                                                                                          selected='spearman', multiple=FALSE), #selectInput #CORR_class_corr_dist
                                                                                                                                                                                       shiny::selectInput(inputId='CORR_class_corr_hclust', label='Clustering method:',
                                                                                                                                                                                                          choices=c('Complete'='complete',
                                                                                                                                                                                                                    'Single'='single',
                                                                                                                                                                                                                    'Median'='median',
                                                                                                                                                                                                                    'Average'='average',
                                                                                                                                                                                                                    "Ward.D"="ward.D",
                                                                                                                                                                                                                    "Ward.D2"="ward.D2",
                                                                                                                                                                                                                    "WPGMA"="mcquitty",
                                                                                                                                                                                                                    "WOGMC"="median",
                                                                                                                                                                                                                    "UPGMC"="centroid"),
                                                                                                                                                                                                          selected='average', multiple=FALSE ) #selectInput #CORR_class_corr_hclust
                                                                                                                                                                         ) ## column 4
                                                                                                                                                           )## column 12
                                                                                                                                            ), #div #CORR_class_corr_reset_div
                                                                                                                                            shiny::column(width=12,
                                                                                                                                                          shiny::column(width=4),
                                                                                                                                                          shiny::column(width=8,
                                                                                                                                                                        shiny::actionButton(inputId='CORR_class_corr_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #CORR_class_corr_reset
                                                                                                                                                                        shiny::actionButton(inputId='CORR_class_corr_start', label='Submit', icon=shiny::icon('play')) #actionButton #CORR_class_corr_start
                                                                                                                                                          ) ## column 8
                                                                                                                                            )
                                                                                                                             ), #div #CORR_class_corr_style_div
                                                                                                                             htmltools::br()
                                                                                                               ) ## column 12
                                                                                               ), #tabPanel #Correlation
                                                                                               #############################
                                                                                               ####  Linear regression  ####
                                                                                               #############################
                                                                                               tabPanel(title='Linear regression',
                                                                                                        htmltools::h3('Linear regression'),
                                                                                                        shiny::column(width=12,
                                                                                                                      htmltools::h6(p("Multiple linear regression is a statistical technique that uses several explanatory variables to predict the outcome of a response variable,
                                                                                                 allowing researchers to estimate the associations between lipid levels and clinical features (i.e., genetic polymorphisms).
                                                                                                 In this page, the lipids will be classified by the user-selected lipid characteristics (e.g. class), then implementing multiple linear regression analysis.
                                                                                                 Each variable (the pair of lipid characteristics and clinical features) will be assigned a beta coefficient and t statistic (p-value), which can be chosen for clustering.",
                                                                                                                                      style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                                      htmltools::br(),
                                                                                                                      htmltools::div(id='CORR_class_linear_style_div',
                                                                                                                                     style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:290px",
                                                                                                                                     htmltools::div(id='CORR_class_linear_reset_div',
                                                                                                                                                    shiny::column(width=12,
                                                                                                                                                                  shiny::column(width=4,
                                                                                                                                                                                shiny::uiOutput("CORR.class.linear.lipid.char"), #uiOutput #CORR_class_linear_lipid_char
                                                                                                                                                                                shiny::radioButtons(inputId='CORR_class_linear_adj_stat_method', label='Multiple testing correction:',
                                                                                                                                                                                                    choices=c('Benjamini & Hochberg'='BH'),
                                                                                                                                                                                                    selected='BH', inline=TRUE) #radioButtons #CORR_class_linear_adj_stat_method
                                                                                                                                                                                ), ## column 4
                                                                                                                                                                  shiny::column(width=4,
                                                                                                                                                                                shiny::selectInput(inputId='CORR_class_linear_sig_p', label='Identify significant lipids:',
                                                                                                                                                                                                   choices=c('p-value'='pval',
                                                                                                                                                                                                             'adjusted p-value'='padj'),
                                                                                                                                                                                                   selected='p', multiple=FALSE), #selectInput #CORR_class_linear_sig_p
                                                                                                                                                                                shiny::numericInput(inputId='CORR_class_linear_pval', label='p-value:',
                                                                                                                                                                                                    value=1, min=0.001, max=1, step=0.001) #numericInput #CORR_class_linear_pval
                                                                                                                                                                                ), ## column 4
                                                                                                                                                                  shiny::column(width=4,
                                                                                                                                                                                shiny::radioButtons(inputId='CORR_class_linear_color', label='Value for clustering:',
                                                                                                                                                                                                    choices=c('beta coefficient'='beta_coef',
                                                                                                                                                                                                              't statistics'='t_statistic'),
                                                                                                                                                                                                    selected='beta_coef', inline=TRUE), #radioButtons #CORR_class_linear_color
                                                                                                                                                                                shiny::selectInput(inputId='CORR_class_linear_dist', label='Distance measure:',
                                                                                                                                                                                                   choices=c('Pearson'='pearson',
                                                                                                                                                                                                             'Spearman'='spearman',
                                                                                                                                                                                                             'Kendall'='kendall',
                                                                                                                                                                                                             "Euclidean"="euclidean",
                                                                                                                                                                                                             "Maximum"="maximum",
                                                                                                                                                                                                             "Manhattan"="manhattan",
                                                                                                                                                                                                             "Canberra"="canberra",
                                                                                                                                                                                                             "Binary"="binary",
                                                                                                                                                                                                             "Minkowski"="minkowski"),
                                                                                                                                                                                                   selected='pearson', multiple=FALSE), #selectInput #CORR_class_linear_dist
                                                                                                                                                                                shiny::selectInput(inputId='CORR_class_linear_hclust', label='Clustering method:',
                                                                                                                                                                                                   choices=c('Complete'='complete',
                                                                                                                                                                                                             'Single'='single',
                                                                                                                                                                                                             'Median'='median',
                                                                                                                                                                                                             'Average'='average',
                                                                                                                                                                                                             "Ward.D"="ward.D",
                                                                                                                                                                                                             "Ward.D2"="ward.D2",
                                                                                                                                                                                                             "WPGMA"="mcquitty",
                                                                                                                                                                                                             "WOGMC"="median",
                                                                                                                                                                                                             "UPGMC"="centroid"),
                                                                                                                                                                                                   selected='centroid', multiple=FALSE) #selectInput #CORR_class_linear_hclust
                                                                                                                                                                                ) ## column 4
                                                                                                                                                                  ) ## column 12
                                                                                                                                                    ), #div #CORR_class_linear_reset_div
                                                                                                                                     shiny::column(width=12,
                                                                                                                                                   shiny::column(width=4),
                                                                                                                                                   shiny::column(width=8,
                                                                                                                                                                 shiny::actionButton(inputId='CORR_class_linear_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #CORR_class_linear_reset
                                                                                                                                                                 shiny::actionButton(inputId='CORR_class_linear_start', label='Submit', icon=shiny::icon('play')) #actionButton #CORR_class_linear_start
                                                                                                                                                                 ) ## column 8
                                                                                                                                                   )## column 12
                                                                                                                                     ), #div #CORR_class_linear_style_div
                                                                                                                      htmltools::br()
                                                                                                                      ) ## column 12
                                                                                                        ) #tabPanel #Linear regression
                                                                                               ), #navlistPanel
                                                                           htmltools::div(id='CORR_class_corr_result_div', style='display: none;',
                                                                                          shiny::column(width=3,
                                                                                                        htmltools::HTML(
                                                                                                          '<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                          '<h4 style="text-align:left; line-height: 25px;">
                                                                                                           The heatmap demonstrates a correlation between clinical features user-defined lipid characteristics.
                                                                                                           Clinical features are displayed along the heatmap rows, while the columns represent lipid characteristics.
                                                                                                           The color of each cell corresponds to the coefficient value, ranging from positive (red) to negative (blue) in a gradient.
                                                                                                           </h4>
                                                                                                           <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                           <ul>
                                                                                                           <li>If the lipid/sample number exceeds 50, their names will not be displayed on the heatmap.</li>
                                                                                                           <li>Hover on a specific heatmap cell to view more corresponding information.</li>
                                                                                                           </ul></h4></div>' )
                                                                                                        ),## column 3
                                                                                          shiny::column(width=9,
                                                                                                        htmltools::h3('Lipid characteristics-clinical features correlation heatmap',style='text-align: center;'),
                                                                                                        plotly::plotlyOutput(outputId='CORR.class.corr.heatmap', height='100%') %>% shinycssloaders::withSpinner(), #plotlyOutput #CORR.class.corr.heatmap
                                                                                                        htmltools::br(),
                                                                                                        shiny::column(12,
                                                                                                                      shiny::column(width=5),
                                                                                                                      shiny::column(width=2, style='padding:0px;text-align:center;', shiny::actionButton("CORR.class.corr.download.start", "Download PDF", icon=shiny::icon("download"))),
                                                                                                                      shiny::column(width=5, shiny::downloadButton("CORR.class.corr.download", "Download", style="visibility:hidden;"))
                                                                                                                      ) ## column 12
                                                                                                        ) ## column 9
                                                                                          ), ## div # CORR_class_corr_result_div
                                                                           htmltools::div(id='CORR_class_linear_result_div', style='display:none;',
                                                                                          shiny::column(width=3,
                                                                                                        htmltools::HTML(
                                                                                                          '<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                          '<h4 style="text-align:left; line-height: 25px;">
                                                                                                           The heatmap demonstrates a correlation between clinical features user-defined lipid characteristics.
                                                                                                           Clinical features are displayed along the heatmap rows, while the columns represent lipid characteristics.
                                                                                                           The color of each cell corresponds to the coefficient value, ranging from positive (red) to negative (blue) in a gradient.
                                                                                                           </h4>
                                                                                                           <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                           <ul>
                                                                                                           <li>If the lipid/sample number exceeds 50, their names will not be displayed on the heatmap.</li>
                                                                                                           <li>Hover on a specific heatmap cell to view more corresponding information.</li>
                                                                                                           </ul></h4></div>')
                                                                                                        ), ## column 3
                                                                                          shiny::column(width=9,
                                                                                                        htmltools::h3('Lipid characteristics-clinical features correlation heatmap',style='text-align: center;'),
                                                                                                        plotly::plotlyOutput(outputId='CORR.class.linear.heatmap', height='100%') %>% shinycssloaders::withSpinner(), #plotlyOutput #CORR.class.linear.heatmap
                                                                                                        htmltools::br(),
                                                                                                        shiny::column(12,
                                                                                                                      shiny::column(width=5),
                                                                                                                      shiny::column(width=2, style='padding:0px;text-align:center;', shiny::actionButton("CORR.class.linear.download.start", "Download PDF", icon=shiny::icon("download"))),
                                                                                                                      shiny::column(width=5, shiny::downloadButton("CORR.class.linear.download", "Download", style="visibility:hidden;"))
                                                                                                                      ) ## column 12
                                                                                                        ) ## column 9
                                                                                          ) ## div # CORR_class_linear_result_div
                                                           ) #tabPanel #Lipid category analysis
                                        ) #tabsetPanel
                         ) ## div # CORR_tabPanel_div
           ) ## column 12
         ) ## fluidRow
) ## tabPanel