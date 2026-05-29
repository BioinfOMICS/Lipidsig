shiny::tabPanel(title=htmltools::HTML("<h4 style='font-size:18px;padding-top:9.5px;padding-bottom:11.5px;text-align:center;'>Differential<br>Expression</h4>"),

                value='Differential expression',
                #### DE Header Panel ####
                htmltools::h1('Differential expression analysis'),
                htmltools::br(),
                shiny::fluidRow(
                  shiny::column(width=12,
                                ###############################
                                ####  DE Page Description  ####
                                ###############################
                                htmltools::div(id='DE_description_div',
                                               style="background-color: PowderBlue;border-left: 8px solid Teal;padding: 15px",
                                               htmltools::h6("In Differential Expression Page, significant lipid species or lipid characteristics can be explored through two main customised analysis, by ",
                                                             htmltools::strong("‘Lipid species’"), "or by ", htmltools::strong("‘Lipid characteristics’"),
                                                             ", with user-uploaded data. Subsequently, further analysis and visualisation methods, including dimensionality reduction, hierarchical clustering, characteristics analysis, and two characteristic analysis,
                                                      can be implemented based on the results of differential expressed analysis by utilising user-defined methods and characteristics."),
                                               htmltools::br(),
                                               htmltools::HTML('<em style="font-size:17px; text-align:left; line-height: 25px;">
                                                         <i class="fas fa-caret-right" role="presentation" aria-label="caret-right icon"></i>
                                                         Demo dataset source (two-group): <a href="https://pubmed.ncbi.nlm.nih.gov/29320510/" target="_blank">Adipose tissue ATGL modifies the cardiac lipidome in pressure-overload-induced left Ventricular failure (PLoS Genet. 2018)</a></em>'),
                                               htmltools::br(),
                                               htmltools::HTML('<em style="font-size:17px; text-align:left; line-height: 25px;">
                                                         <i class="fas fa-caret-right" role="presentation" aria-label="caret-right icon"></i>
                                                         Demo dataset source (multi-group): <a href="https://pubmed.ncbi.nlm.nih.gov/32165635/" target="_blank">Lipidomic and biophysical homeostasis of mammalian membranes counteracts dietary lipid perturbations to maintain cellular fitness (Nat Commun 11, 1339. 2020)</a></em>'),
                                ), ## div # DE_description_div
                                htmltools::br(),
                                ##########################
                                ####  DE Data Source  ####
                                ##########################
                                htmltools::h2('Data Source'),
                                shiny::sidebarLayout(fluid=TRUE,
                                                     shiny::sidebarPanel(width=4,
                                                                         shiny::radioButtons(inputId='DE_data_source',
                                                                                             label=htmltools::h4('Data source'),
                                                                                             choices=c('Example dataset (two group)'='DE_demo_data',
                                                                                                       'Example dataset (Multiple group)'='DE_demo_multiple_data',
                                                                                                       'Upload your data!'='DE_user_data'),
                                                                                             selected='DE_demo_data') %>% #radioButtons #DE_data_source
                                                                           shinyhelper::helper(type="inline", title="Data source", size ="l",
                                                                                               content=c('<ol style="font-size: 0px;">',
                                                                                                         '<li style="font-size: 16px;">Lipid dataset can be uploaded by users or using example datasets. This information, namely Lipid abundance data and Group information, all data needs to be uploaded in
                                                                                                  <mark style="background-color: white;color: red;">CSV</mark>, <mark style="background-color: white;color: red;">TSV</mark>, or <mark style="background-color: white;color: red;">XLSX</mark> format. The maximum file size is 30MB.<br>
                                                                                                  <ul style="font-size: 16px; color:red;">
                                                                                                  <li>NOTE: When uploading in XLSX format, ensure the data frame is on the first sheet. </li>
                                                                                                  </ul></li>',
                                                                                                         '<li style="font-size: 16px;">Once two files are chosen and shown ‘Upload complete’ then press ‘Upload’.</li>',
                                                                                                         '</ol>')),
                                                                         shiny::conditionalPanel(condition='input.DE_data_source == "DE_demo_data" | input.DE_data_source == "DE_demo_multiple_data"',
                                                                                                 shiny::actionButton(inputId='DE_demo_upload', label='Submit', icon=shiny::icon('upload')), #actionButton #DE_demo_upload
                                                                                                 shiny::downloadButton(outputId='DE.demo.download', label='Download example')), #conditionalPanel
                                                                         shiny::conditionalPanel(condition='input.DE_data_source == "DE_user_data"',
                                                                                                 htmltools::div(id='DE_user_reset_div',
                                                                                                                shiny::fileInput(inputId='DE_user_abundance', label='Lipid abundance data:', accept=c(".csv", ".tsv", '.xlsx'), multiple=FALSE) %>% #fileInput #DE_user_exp
                                                                                                                  shinyhelper::helper(type="inline", title="Lipid abundance data", size ="l",
                                                                                                                                      content=c('<ol style="font-size: 0px;">',
                                                                                                                                                '<li style="font-size: 16px;">The first column must contain a list of unique lipids name(features). </li>',
                                                                                                                                                '<li style="font-size: 16px;">Other columns encompass the expressed values of groups under different conditions that you want to compare.</li>',
                                                                                                                                                '<li style="font-size: 16px;">An example of ‘Lipid abundance data’</li>',
                                                                                                                                                '</ol>',
                                                                                                                                                '<img src="Description/DE_Lipid expression data.webp" style="border:2px #ccc solid;padding:20px;" loading="lazy" width="100%"/>')),
                                                                                                                shiny::fileInput(inputId='DE_user_group', label='Group information:', accept=c(".csv", ".tsv", '.xlsx'), multiple=FALSE)%>% #fileInput #DE_user_group
                                                                                                                  shinyhelper::helper(type="inline", title="Group information", size ="l",
                                                                                                                                      content=c('<ol style="font-size: 0px;">',
                                                                                                                                                '<li style="font-size: 16px;">Columns must be 4 or 3 columns, arranging in order of sample_name, label_name, group, pair(optional). Note that the first 3 columns cannot contain NA or blanks.</li>',
                                                                                                                                                '<li style="font-size: 16px;">‘sample_name’ must same as the name of samples of ‘Lipid abundance data’!</li>',
                                                                                                                                                '<li style="font-size: 16px;">‘lable_name’ give each group labels, such as ctrl1, treatment1.</li>',
                                                                                                                                                '<li style="font-size: 16px;">‘group’ separates samples into groups. The name of reference group will be input by users.</li>',
                                                                                                                                                '<li style="font-size: 16px;">The ‘pair’ column (optional) can be added if your sample wants to be calculated in pair, you must assign each pair to a specific number, starting from 1 to N, cannot have NA, blank, or skip numbers. i.e., if OVCAR8_ctrl1 & OVCAR8_sgAGPS1 are a pair, input 1 in column ‘pair’ for both rows.
                                                                                                                                         <br>
                                                                                                                                         NOTE: Multi-group data should contatin "pair" column.
                                                                                                                                         </li>',
                                                                                                                                                '<li style="font-size: 16px;">At least 1 sample must be included for each sample</li>',
                                                                                                                                                '<li style="font-size: 16px;">An example of ‘Group Information’</li>',
                                                                                                                                                '<img src="Description/DE_Group Information.webp" style="border:2px #ccc solid;padding:20px;" loading="lazy" width="80%"/>',
                                                                                                                                                '</ol>')),
                                                                                                                shiny::helpText("Upload your data table in .csv/.tsv/.xlsx"), #helpText
                                                                                                                shiny::selectInput(inputId='DE_user_nGroup',
                                                                                                                                   label='Number of groups',
                                                                                                                                   choices=c('Two'='two', 'Multiple'='multiple'),
                                                                                                                                   selected='two', multiple=FALSE)
                                                                                                 ), ## div # DE_user_reset_div
                                                                                                 shiny::actionButton(inputId='DE_user_reset', label='Reset uploaded data', icon=shiny::icon('redo')), #actionButton #DE_user_reset
                                                                                                 shiny::actionButton(inputId='DE_user_upload', label='Upload', icon=shiny::icon('upload')), #actionButton #DE_user_upload
                                                                                                 htmltools::div(id='DE_user_ref_group_div', style='display: none;',
                                                                                                                htmltools::br(),
                                                                                                                shiny::selectInput(inputId='DE_user_ref_group',
                                                                                                                                   label='Reference group (Control):',
                                                                                                                                   choices=c('', 'ctrl', 'exp'),
                                                                                                                                   selected='', multiple=FALSE) %>%
                                                                                                                  shinyhelper::helper(type="inline", title="Reference group (Control):", size ="l",
                                                                                                                                      content=c('<ul style="font-size: 0px;">',
                                                                                                                                                '<li style="font-size: 16px;">
                                                                                                                                         The reference group must be selected from the "group" column in the group information table.
                                                                                                                                         The drop-down list automatically extracts all the group names from this column. Select one from the drop-down menu as the reference group.
                                                                                                                                         </li>',
                                                                                                                                                '<img src="Description/DE_Reference_group.webp" style="border:2px #ccc solid;padding:20px;" loading="lazy" width="60%"/>',
                                                                                                                                                '</ul>')),
                                                                                                                shiny::actionButton(inputId='DE_user_submit', label='Submit', icon=shiny::icon('upload'))
                                                                                                 )## div # DE_user_ref_group_div
                                                                         ), ## conditionalPanel
                                                                         htmltools::br(),
                                                                         htmltools::HTML("<a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank' style='color: darkblue;'>How to prepare your dataset?</a>"),
                                                                         htmltools::br(),
                                                                         htmltools::HTML("<a href='https://lipidsig.bioinfomics.org/Tutorial/?DE' target='_blank' style='color: darkblue;'>How to use this function?</a>")
                                                     ), #sidebarPanel
                                                     shiny::mainPanel(width=8,
                                                                      shiny::conditionalPanel(condition='input.DE_user_upload | input.DE_demo_upload',
                                                                                              htmltools::div(
                                                                                                style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                                                                shiny::htmlOutput("DE.data.check.progress")
                                                                                              ) ## div
                                                                      ) ## conditionalPanel
                                                     ) ## mainPanel
                                ), #sidebarLayout
                                div(id='DE_data_check_successful',style='display: none;',
                                    htmltools::div(id='DE_data_Uploaded',
                                                   htmltools::h4(htmltools::strong('Uploaded data')),
                                                   shiny::tabsetPanel(id = 'DE_user_raw_table_tab',
                                                                      shiny::tabPanel(title = "Lipid abundance data", DT::dataTableOutput(outputId = 'DE.raw.abundance') %>% shinycssloaders::withSpinner()), #dataTableOutput #DE.user.exp.raw
                                                                      shiny::tabPanel(title = "Group information", DT::dataTableOutput(outputId = 'DE.group.info') %>% shinycssloaders::withSpinner()) #dataTableOutput #DE.user.exp
                                                                      ) #tabsetPanel
                                                   ),
                                    htmltools::br(),
                                    htmltools::div(id='DE_data_warning',style='display: none;',
                                                   htmltools::div(
                                                     style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                     shiny::htmlOutput("DE.Check.SE")),
                                                   shiny::helpText(htmltools::tags$p(shiny::icon("check"),": Successfully uploaded.", style="font-size: 16px;", htmltools::HTML('&nbsp;'), shiny::icon("times"), ": Error happaned. Please check your dataset.", htmltools::HTML('&nbsp;'), shiny::icon("exclamation"), ": Warning message.", style="font-size: 16px;")),
                                                   htmltools::br()
                                    ), ## div data_warning
                                    htmltools::div(id='DE_data_processing_div',style='display:none;',
                                                   style='background: #ecf0f1;border: 1px solid transparent;border-radius: 4px;height: 350px;',
                                                   shiny::column(width=12,
                                                                 htmltools::h3('Data processing')%>%
                                                                   shinyhelper::helper(type="inline", title="Data processing", size ="l",
                                                                                       content=c('<h4 style="font-size: 18px; text-align:left; line-height: 25px; color:darkblue;">
                                                                       <ul>
                                                                       <li>
                                                                       For detailed descriptions of missing values, sample normalization, and data transformation methods, please refer to the <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ13" target="_blank" style="color: red;">FAQ</a>.
                                                                       </li>
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
                                                                                                 '</ol>')) ## helper (for h3 Data processing)
                                                   ),## column 12
                                                   shiny::column(width=4,
                                                                 shiny::checkboxInput(inputId="DE_rm_NA", label="Remove features with many missing values", value=TRUE), #checkboxInput #DE_rm_NA
                                                                 shiny::conditionalPanel(condition='input.DE_rm_NA',
                                                                                         shiny::numericInput(inputId='DE_filtration_param', label='More than % missing values',
                                                                                                             value=70, min=5, max=100, step=5) ## numericInput DE_rm_NA_pct
                                                                 )## conditionalPanel (If the user chooses to remove features)
                                                   ),## column 4
                                                   shiny::column(width=4,
                                                                 shiny::selectInput(inputId='DE_fill_NA', label='Fill missing value with:',
                                                                                    choices=c('Mean'='mean', 'Median'='median', 'Minimum'='min',
                                                                                              'Quantile regression imputation of left-censored data'='QRILC',
                                                                                              'Singular value decomposition'='SVD',
                                                                                              'K Nearest Neighbors'='KNN',
                                                                                              'International Risk Management Institute'='IRMI',
                                                                                              'Probabilistic principal component analysis'='PPCA',
                                                                                              'Bayesian. Principal Component Analysis'='BPCA'),
                                                                                    selected='min', multiple=FALSE), ## selectInput DE_fill_NA
                                                                 shiny::conditionalPanel(condition='input.DE_fill_NA == "min"',
                                                                                         shiny::numericInput(inputId='DE_fill_min', label='Multiply by minimum',
                                                                                                             value=0.5, min=0.1, max=0.5, step=0.1)
                                                                 ), ## conditionalPanel (If the user chooses fill missing value with Minimum)
                                                                 shiny::conditionalPanel(condition='input.DE_fill_NA == "QRILC"',
                                                                                         shiny::numericInput(inputId='DE_fill_QRILC', label='Tune sigma',
                                                                                                             value=1, min=0.1, max=1, step=0.1)
                                                                 ), ## conditionalPanel (If the user chooses fill missing value with QRILC)
                                                                 shiny::conditionalPanel(condition="['SVD', 'PPCA', 'BPCA'].includes(input.DE_fill_NA)",
                                                                                         shiny::numericInput(inputId='DE_fill_param', label='nPCs',
                                                                                                             value=3, min=1, max=10, step=1)
                                                                 ), ## conditionalPanel (If the user chooses fill missing value with SVD/PPCA/BPCA)
                                                                 shiny::conditionalPanel(condition='input.DE_fill_NA == "KNN"',
                                                                                         shiny::numericInput(inputId='DE_fill_KNN', label='The number of neighbors',
                                                                                                             value=3, min=1, max=10, step=1)
                                                                 ) ## conditionalPanel (If the user chooses fill missing value with KNN)
                                                   ),## column 4
                                                   shiny::column(width=4,
                                                                 htmltools::h4('Data Normalization'),
                                                                 shiny::selectInput(inputId='DE_normalization', label='Normalization with:',
                                                                                    choices=c('None'='none', 'Percentage'='Percentage',
                                                                                              'Probabilistic Quotient Normalization'='PQN',
                                                                                              'Quantile normalization'='Quantile',
                                                                                              'Normalization by sum'='Sum',
                                                                                              'Normalization by median'='Median'),
                                                                                    selected='Percentage', multiple=FALSE),
                                                                 htmltools::h4('Data Transformation'),
                                                                 shiny::selectInput(inputId='DE_transformation',
                                                                                    label='Transformation with:',
                                                                                    choices=c('None'='none',
                                                                                              'log10'='log10',
                                                                                              'Square root'='square',
                                                                                              'Cube root'='cube'),
                                                                                    selected='log10', multiple=FALSE)
                                                   ),## column 4
                                                   shiny::column(width=7),
                                                   shiny::column(width=3, shiny::actionButton(inputId='DE_processing_reset', label='Reset processing method', icon=shiny::icon('redo'))), #actionButton #DE_processing_reset
                                                   shiny::column(width=2, shiny::actionButton(inputId='DE_processing_start', label='Processing', icon=shiny::icon('play'))) #actionButton #DE_processing_start
                                    ), ## div # data_processing_div
                                    htmltools::br(),
                                    htmltools::div(id='DE_data_summary_div',
                                                   style="display:none;text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                   shiny::htmlOutput("DE.data.summary")
                                    ), ## div DE_data_summary_div
                                    htmltools::br(),
                                    htmltools::div(id='DE_data_process_table_div', style='display:none;height: 440px;',
                                                   shiny::column(width=12,
                                                                 htmltools::h3(htmltools::strong('Processed data')),
                                                                 htmltools::div(style="display: none;text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                                                htmltools::HTML('<h4 style="font-size: 18px; text-align:left; line-height: 30px; color:black;">
                                                                                      <ul>
                                                                                          <li><strong>Processed abundance data</strong>: User-uploaded abundance data after data processing.</li>
                                                                                          <li><strong>Group information</strong>: The user-uploaded group information table includes an added column listing the control (ctrl) and experiment (exp) groups for two-group data and listing control (ctrl) groups for multiple-group data.</li>
                                                                                          <li><strong>Lipid characteristics</strong>: Lipid characteristics converted according to the uploaded lipids in the abundance data. Detailed information about the converted characteristics can be found in the <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ11" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                          <li><strong>Lipid id</strong>: Links to the LION ID, LIPID MAPS ID, and other resource IDs for the uploaded lipids.</li>
                                                                                          <li><strong>Data quality</strong>: Box and density plots of the abundance data before and after data processing.</li>
                                                                                      </ul></h4>')
                                                                 ), ## div
                                                                 htmltools::br(),
                                                                 shiny::tabsetPanel(id='DE_user_process_table_tab',
                                                                                    shiny::tabPanel(title="Processed data", DT::dataTableOutput(outputId='DE.processed.abundance') %>% shinycssloaders::withSpinner()), #dataTableOutput #DE.exp.process
                                                                                    shiny::tabPanel(title="Group information", DT::dataTableOutput(outputId='DE.processed.group.info') %>% shinycssloaders::withSpinner()), ## tabPanel # Group information
                                                                                    shiny::tabPanel(title="Lipid characteristics", DT::dataTableOutput(outputId='DE.processed.lipid.char.tab') %>% shinycssloaders::withSpinner()), ## tabPanel # Group information
                                                                                    shiny::tabPanel(title="Lipid id", DT::dataTableOutput(outputId='DE.lipid.id') %>% shinycssloaders::withSpinner()), ## tabPanel # Group information
                                                                                    shiny::tabPanel(title="Data quality", style='height:900px;',
                                                                                                    shiny::column(width=12,htmltools::br()),
                                                                                                    shiny::column(width=12,
                                                                                                                  shiny::column(width=5),
                                                                                                                  shiny::column(width=2, style='padding: 0px;', shiny::actionButton("DE.processed.download.start", "Download PDF", icon=shiny::icon("download"))),
                                                                                                                  shiny::column(width=5, shiny::downloadButton("DE.processed.download", "Download", style="visibility: hidden;"),style='height:34px')
                                                                                                    ), ## column 12
                                                                                                    shiny::column(width=12,htmltools::br()),
                                                                                                    shiny::column(width=12,
                                                                                                                  shiny::column(width=6, plotly::plotlyOutput(outputId='DE.before.processed.boxplot') %>% shinycssloaders::withSpinner()),
                                                                                                                  shiny::column(width=6, plotly::plotlyOutput(outputId='DE.after.processed.boxplot') %>% shinycssloaders::withSpinner())
                                                                                                    ), ## column 12
                                                                                                    shiny::column(width=12,htmltools::br()),
                                                                                                    shiny::column(width=12,
                                                                                                                  shiny::column(width=6, plotly::plotlyOutput(outputId='DE.before.processed.density') %>% shinycssloaders::withSpinner()),
                                                                                                                  shiny::column(width=6, plotly::plotlyOutput(outputId='DE.after.processed.density') %>% shinycssloaders::withSpinner())
                                                                                                    ) ## column 12
                                                                                    ) ## tabPanel # Data quality
                                                                 ) ## tabsetPanel # DE_user_process_table_tab
                                                   ) ## column 12
                                    ) ## div # data_process_table_div
                                    ),
                  ),## column 12
                  htmltools::div(id='DE_start_div',style='display:none;height:50px;',
                                 shiny::column(width=5), #column
                                 shiny::column(width=2, shiny::actionButton(inputId='DE_start', label='Start!', icon=shiny::icon('play'))), #actionButton #DE_user_start
                                 shiny::column(width=5) #column
                  ), ## div DE_start_div
                  shiny::column(width=12,htmltools::hr()),
                  ###########################
                  ####  DE analysis tab  ####
                  ###########################
                  shiny::column(width=12,
                                htmltools::div(id='DE_tabPanel_div',style='display:none;',
                                               htmltools::h2('Result'),
                                               shiny::tabsetPanel(id='DE_analysis_tab',
                                                                  shiny::tabPanel(title='Lipid species analysis',
                                                                                  htmltools::br(),
                                                                                  shiny::navlistPanel(widths=c(3, 9), id='DE_species_list',
                                                                                                      'Step 1:',
                                                                                                      ###########################################
                                                                                                      ####  Differential expressed analysis  ####
                                                                                                      ###########################################
                                                                                                      shiny::tabPanel(title='Differential expression',
                                                                                                                      htmltools::h3('Differential expression'),
                                                                                                                      shiny::column(width=12,
                                                                                                                                    htmltools::h6(htmltools::p(
                                                                                                                                      style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px",
                                                                                                                                      "In lipid species analysis section, differentially expressed analysis is performed to find significant lipid species.
                                                                                                                                                                      In short, samples will be divided into two/multiple groups (independent) based on the Group Information of input.
                                                                                                                                                                      Two statistical methods are provided for different data types: the t-test and the Wilcoxon test (Wilcoxon rank-sum test)for two-group data, along with one-way ANOVA and the Kruskal-Wallis test for multi-group data.
                                                                                                                                                                      Additionally, the p-value will be adjusted using the Benjamini-Hochberg procedure.
                                                                                                                                                                      The condition and cut-offs for significant lipid species are also users selected. ")),
                                                                                                                                    htmltools::br(),
                                                                                                                                    htmltools::div(id='DE_analysis_style_div',
                                                                                                                                                   style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:360px",
                                                                                                                                                   htmltools::div(id='DE_analysis_reset_div',
                                                                                                                                                                  shiny::column(width=12,
                                                                                                                                                                                shiny::column(width=6,
                                                                                                                                                                                              shiny::selectInput(inputId='DE_stat_method', label='Method:',
                                                                                                                                                                                                                 choices=c('t-test'='t-test', 'Wilcoxon test'='Wilcoxon test'),
                                                                                                                                                                                                                 selected='t-test', multiple=FALSE, width='80%'), #selectInput #DE_stat_method
                                                                                                                                                                                              htmltools::div(id='DE_postHoc_div',
                                                                                                                                                                                                             shiny::radioButtons(inputId='DE_postHoc_method', label='Post Hoc method:',
                                                                                                                                                                                                                                 choices=c("Tukey's HSD"), selected="Tukey's HSD", inline=TRUE)),
                                                                                                                                                                                              shiny::radioButtons(inputId='DE_adj_stat_method', label='Multiple testing correction:',
                                                                                                                                                                                                                  choices=c('Benjamini & Hochberg'='BH'), selected='BH', inline=TRUE), #radioButtons #DE_adj_stat_method
                                                                                                                                                                                              shiny::uiOutput('DE.species.groupInfo.compare.message')
                                                                                                                                                                                ), ## column 6
                                                                                                                                                                                shiny::column(width=6,
                                                                                                                                                                                              shiny::selectInput(inputId='DE_sig_p', label='Identify significant lipids:',
                                                                                                                                                                                                                 choices=c('p-value'='pval',
                                                                                                                                                                                                                           'adjusted p-value (padj)'='padj'),
                                                                                                                                                                                                                 selected='pval', multiple=FALSE, width='80%'), #selectInput #DE_sig_p
                                                                                                                                                                                              shiny::numericInput(inputId='DE_pval', label='p-value:', value=0.05,
                                                                                                                                                                                                                  min=0.001, max=1, step=0.001, width='80%'), #numericInput #DE_pval
                                                                                                                                                                                              htmltools::div(id='DE_fc_div',
                                                                                                                                                                                                             shiny::numericInput(inputId='DE_fc', label='Fold change (FC):',
                                                                                                                                                                                                                                 value=1, min=1, max=8, width='80%') #numericInput #DE_fc
                                                                                                                                                                                              )## div # DE_fc_div
                                                                                                                                                                                ) ## column 6
                                                                                                                                                                  )
                                                                                                                                                   ), ## div # DE_analysis_reset_div
                                                                                                                                                   shiny::column(width=12),
                                                                                                                                                   shiny::column(width=12,
                                                                                                                                                                 shiny::column(width=7),
                                                                                                                                                                 shiny::column(width=5,
                                                                                                                                                                               shiny::actionButton(inputId='DE_analysis_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #DE_analysis_reset
                                                                                                                                                                               shiny::actionButton(inputId='DE_analysis_start', label='Submit', icon=shiny::icon('play')) #actionButton #DE_analysis_start
                                                                                                                                                                 )## column 5
                                                                                                                                                   ) ## column 12
                                                                                                                                    ), #div #DE_analysis_style_div
                                                                                                                                    htmltools::br()
                                                                                                                      ) ## column 12
                                                                                                      ),#tabPanel # Differential expressed analysis
                                                                                                      shiny::tabPanel(title='Result download',
                                                                                                                      htmltools::h3('Download results for Enrichment Analysis'),
                                                                                                                      htmltools::p(),
                                                                                                                      htmltools::h6(
                                                                                                                        htmltools::div(
                                                                                                                          style = "text-align: left; background-color: AliceBlue; border-left: 8px solid LightSteelBlue; padding: 15px;",
                                                                                                                          htmltools::HTML(
                                                                                                                            "Click the <strong>Get Files for Enrichment Module</strong> button to obtain a <code>.zip</code> file containing two folders:<br>
                                                                                                                            <ul>
                                                                                                                            <li><strong><em>analysis_results</em></strong> – includes CSV files summarizing the results of differential expression analysis.<br></li>
                                                                                                                            <li><strong><em>web_assets</em></strong> – contains a <code>.rds</code> file (in <em>SummarizedExperiment</em> format) ready for upload to the enrichment module.<br></li>
                                                                                                                            </ul>
                                                                                                                            Clicking the <strong>Go to Enrichment analysis</strong> button will navigate you to the enrichment analysis page and <strong>automatically preload</strong> the <code>.rds</code> file, so you can proceed directly without manual upload."
                                                                                                                          )
                                                                                                                        )
                                                                                                                      ),
                                                                                                                      shiny::column(width=12, style='padding: 0px;',
                                                                                                                                    shiny::column(width=3, shiny::actionButton("DE.species.enrichment.download.start", HTML("Get Files for<br>Enrichment Module"), icon=shiny::icon("download"), style='width:100%')),
                                                                                                                                    shiny::column(width=3, style='padding:0px;',shiny::actionButton("DE.species.enrichment.url", HTML("Go to<br>Enrichment analysis"), icon=shiny::icon("arrow-up-right-from-square"), style='width:95%')),
                                                                                                                                    shiny::column(width=6, shiny::downloadButton("DE.species.enrichment.download", "Download", style="visibility:hidden;"))),
                                                                                                                      shiny::column(width=12,htmltools::br()),
                                                                                                                      shiny::column(width=12,htmltools::br()),
                                                                                                                      htmltools::h3('Download results for Network Analysis'),
                                                                                                                      htmltools::p(),
                                                                                                                      shiny::column(width=12, style='padding: 0px;',
                                                                                                                                    htmltools::div(id='Network_download_div',
                                                                                                                                                   htmltools::h6(
                                                                                                                                                     htmltools::div(
                                                                                                                                                       style = "text-align: left; background-color: AliceBlue; border-left: 8px solid LightSteelBlue; padding: 15px;",
                                                                                                                                                       htmltools::HTML(
                                                                                                                                                         "Click the <strong>Get Files for Network Modules</strong> button to obtain a <code>.zip</code> file that also includes two folders:<br>
                                                                                                                                                         <ul>
                                                                                                                                                         <li><strong><em>analysis_results</em></strong> – includes CSV files summarizing the results of differential expression analysis.<br></li>
                                                                                                                                                         <li><strong><em>web_assets</em></strong> – contains a single <code>.rds</code> file, formatted as a <em>SummarizedExperiment</em>, which serves as the shared input for all three network analysis modules.<br></li>
                                                                                                                                                         </ul>
                                                                                                                                                         Clicking any <strong>Go to Network analysis button</strong> (Pathway Activity, Lipid Reaction, or GATOM) will direct you to the selected module and <strong>automatically preload</strong> the <code>.rds</code> file, ready for immediate use.<br>

                                                                                                                                                         <em style='font-size:17px; text-align:left; line-height: 25px; color:#CC6600;'>
                                                                                                                                                         <i class='fas fa-caret-right' role='presentation' aria-label='caret-right icon'></i>
                                                                                                                                                         Note: These three modules are currently available only for two-group comparisons. Support for multi-group analysis is under development.
                                                                                                                                                         </em>
                                                                                                                                                         "
                                                                                                                                                       )
                                                                                                                                                     )
                                                                                                                                                    ),
                                                                                                                                                   shiny::column(width=12, style='padding: 0px;',
                                                                                                                                                                 shiny::column(width=3, shiny::actionButton("DE.species.network.download.start", HTML("Get Files for<br>Network Modules"), icon=shiny::icon("download"), style='width:100%')),
                                                                                                                                                                 shiny::column(width=3, style='padding:0px;', shiny::actionButton("DE.species.network.activity.url", HTML("Go to <br>Pathway Activity Network"), icon=shiny::icon("arrow-up-right-from-square"))),
                                                                                                                                                                 shiny::column(width=3, shiny::actionButton("DE.species.network.reaction.url", HTML("Go to <br>Lipid Reaction Network"), icon=shiny::icon("arrow-up-right-from-square"))),
                                                                                                                                                                 shiny::column(width=3, shiny::actionButton("DE.species.network.gatom.url", HTML("Go to <br>GATOM Network"), icon=shiny::icon("arrow-up-right-from-square"))),
                                                                                                                                                                 shiny::column(width=12, shiny::downloadButton("DE.species.network.download", "Download", style="visibility:hidden;"))
                                                                                                                                                                 )
                                                                                                                                                   )## div # Network_download_div
                                                                                                                                    ),
                                                                                                                      shiny::column(width=12,htmltools::br())
                                                                                                      ),#tabPanel # Result download
                                                                                                      'Step 2:',
                                                                                                      ####################################
                                                                                                      ####  Dimensionality reduction  ####
                                                                                                      ####################################
                                                                                                      shiny::tabPanel(title='Dimensionality reduction',
                                                                                                                      htmltools::h3('Dimensionality reduction'),
                                                                                                                      shiny::column(width=12,
                                                                                                                                    htmltools::h6(
                                                                                                                                      htmltools::p(
                                                                                                                                        style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px",
                                                                                                                                        "Dimensionality reduction is common when dealing with large numbers of observations and/or large numbers of variables in lipids analysis.
                                                                                                                                                 It transforms data from a high-dimensional space into a low-dimensional space so that to retain vital properties of the original data and close to its intrinsic dimension.")),
                                                                                                                                    htmltools::br(),
                                                                                                                                    htmltools::div(id='DE_species_dim_redu_reset_div',
                                                                                                                                                   shiny::column(width=5,
                                                                                                                                                                 htmltools::div(id='DE_species_dim_redu_style1_div',
                                                                                                                                                                                style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:340px",
                                                                                                                                                                                shiny::selectInput(inputId='DE_species_dim_redu_method', label='Dimensionality reduction method:',
                                                                                                                                                                                                   choices=c('PCA'='pca',
                                                                                                                                                                                                             'PLS-DA'='plsda',
                                                                                                                                                                                                             't-SNE'='tsne',
                                                                                                                                                                                                             'UMAP'='umap'),
                                                                                                                                                                                                   selected='pca', multiple=FALSE, width='100%'), #selectInput #DE_species_dim_redu_method
                                                                                                                                                                                shiny::conditionalPanel(condition='input.DE_species_dim_redu_method == "tsne"',
                                                                                                                                                                                                        #shinyWidgets::materialSwitch(inputId='DE_species_tsne_pca', label='PCA:',
                                                                                                                                                                                                        #                             value=TRUE, status="primary", right=FALSE), #materialSwitch #DE_class_tsne_pca
                                                                                                                                                                                                        shiny::numericInput(inputId='DE_species_tsne_perplexity', label='Perplexity:',
                                                                                                                                                                                                                            value=5, min=3, max=7, step=1, width='100%') %>%
                                                                                                                                                                                                          shinyhelper::helper(type="inline", title="Perplexity",
                                                                                                                                                                                                                              content=c("The perplexity may be considered as a knob that sets the number of effective nearest neighbours. The typical perplexity range between 5 and 50.")),
                                                                                                                                                                                                        shiny::numericInput(inputId='DE_species_tsne_max_iter', label='Number of iterations:',
                                                                                                                                                                                                                            value=500, min=100, max=5000, step=100, width='100%') %>%
                                                                                                                                                                                                          shinyhelper::helper(type="inline", title="Number of iterations", content=c("The number of iterations is the maximum number of iterations to perform."))
                                                                                                                                                                                ), #conditionalPanel
                                                                                                                                                                                shiny::conditionalPanel(condition='input.DE_species_dim_redu_method == "umap"',
                                                                                                                                                                                                        shiny::numericInput(inputId='DE_species_umap_n_neighbors', label='Number of neighbors:',
                                                                                                                                                                                                                            value=15, min=2, max=23, step=1, width='100%') %>%
                                                                                                                                                                                                          shinyhelper::helper(type="inline", title="Number of neighbors",
                                                                                                                                                                                                                              content=c("The number of neighbors (the number of neighbouring sample points), which is used for manifold approximation.
                                                                                                                                                                                                                     Larger values lead to more global views of the manifold, whilst smaller values result in more local data being preserved.
                                                                                                                                                                                                                               In general values should be in the range 2 to 100.")),
                                                                                                                                                                                                        shiny::selectInput(inputId='DE_species_umap_metric', label='Distance metric:',
                                                                                                                                                                                                                           choices=c('Euclidean'='euclidean',
                                                                                                                                                                                                                                     'Cosine'='cosine',
                                                                                                                                                                                                                                     'Manhattan'='manhattan',
                                                                                                                                                                                                                                     'Hamming'='hamming'),
                                                                                                                                                                                                                           selected='euclidean', multiple=FALSE, width='100%') %>%
                                                                                                                                                                                                          shinyhelper::helper(type="inline", title="Distance metric",
                                                                                                                                                                                                                              content=c("The distance metric is use to find nearest neighbors"))
                                                                                                                                                                                ) #conditionalPanel
                                                                                                                                                                 ) #div #DE_species_dim_redu_style1_div
                                                                                                                                                   ), ## column 6
                                                                                                                                                   shiny::column(width=5,
                                                                                                                                                                 htmltools::div(id='DE_species_dim_redu_style2_div',
                                                                                                                                                                                style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:340px",
                                                                                                                                                                                shiny::selectInput(inputId='DE_species_cluster_method', label='Clustering method:',
                                                                                                                                                                                                   choices=c('Group information'='group_info',
                                                                                                                                                                                                             'K-means'='kmeans',
                                                                                                                                                                                                             'Partitioning around medoids (PAM)'='kmedoids',
                                                                                                                                                                                                             'Hierarchical clustering'='hclustering',
                                                                                                                                                                                                             'DBSCAN'='dbscan'),
                                                                                                                                                                                                   selected='group_info', multiple=FALSE, width='100%'
                                                                                                                                                                                ), #selectInput #DE_species_cluster_method
                                                                                                                                                                                shiny::conditionalPanel(condition='input.DE_species_cluster_method == "group_info"',
                                                                                                                                                                                                        shiny::textOutput(outputId='DE.species.group.count'),
                                                                                                                                                                                                        shiny::uiOutput('DE.species.PCA.groupInfo.compare.message'),
                                                                                                                                                                                                        htmltools::br()
                                                                                                                                                                                ), #conditionalPanel
                                                                                                                                                                                shiny::conditionalPanel(condition='input.DE_species_cluster_method == "kmeans"',
                                                                                                                                                                                                        shiny::sliderInput(inputId='DE_species_kmeans_group', label='Number of groups:',
                                                                                                                                                                                                                           value=2, min=2, max=10, step=1, width='100%')
                                                                                                                                                                                ), #conditionalPanel
                                                                                                                                                                                shiny::conditionalPanel(condition='input.DE_species_cluster_method == "kmedoids"',
                                                                                                                                                                                                        shiny::sliderInput(inputId='DE_species_pam_group', label='Number of groups:',
                                                                                                                                                                                                                           value=2, min=2, max=10, step=1, width='100%'),
                                                                                                                                                                                                        shiny::selectInput(inputId='DE_species_pam_metric', label='Distance metrics:',
                                                                                                                                                                                                                           choices=c('Euclidean'='euclidean',
                                                                                                                                                                                                                                     'Manhattan'='manhattan'),
                                                                                                                                                                                                                           selected='euclidean', multiple=FALSE, width='100%') #selectInput #DE_species_pca_metric
                                                                                                                                                                                ), #conditionalPanel
                                                                                                                                                                                shiny::conditionalPanel(condition='input.DE_species_cluster_method == "hclustering"',
                                                                                                                                                                                                        shiny::sliderInput(inputId='DE_species_hclust_group', label='Number of groups:',
                                                                                                                                                                                                                           value=2, min=2, max=10, step=1, width='100%'),
                                                                                                                                                                                                        shiny::selectInput(inputId='DE_species_hclust_dist', label='Distance measure:',
                                                                                                                                                                                                                           choices=c('Pearson'='pearson',
                                                                                                                                                                                                                                     'Spearman'='spearman',
                                                                                                                                                                                                                                     'Kendall'='kendall',
                                                                                                                                                                                                                                     "Euclidean"="euclidean",
                                                                                                                                                                                                                                     "Maximum"="maximum",
                                                                                                                                                                                                                                     "Manhattan"="manhattan",
                                                                                                                                                                                                                                     "Canberra"="canberra",
                                                                                                                                                                                                                                     "Binary"="binary",
                                                                                                                                                                                                                                     "Minkowski"="minkowski"),
                                                                                                                                                                                                                           selected='pearson', multiple=FALSE, width='100%'), #selectInput #DE_species_pca_dist
                                                                                                                                                                                                        shiny::selectInput(inputId='DE_species_hclust_hclust', label='Clustering method:',
                                                                                                                                                                                                                           choices=c('Complete'='complete',
                                                                                                                                                                                                                                     'Single'='single',
                                                                                                                                                                                                                                     'Median'='median',
                                                                                                                                                                                                                                     'Average'='average',
                                                                                                                                                                                                                                     "Ward.D"="ward.D",
                                                                                                                                                                                                                                     "Ward.D2"="ward.D2",
                                                                                                                                                                                                                                     "WPGMA"="mcquitty",
                                                                                                                                                                                                                                     "WOGMC"="median",
                                                                                                                                                                                                                                     "UPGMC"="centroid"),
                                                                                                                                                                                                                           selected='complete', multiple=FALSE, width='100%') #selectInput #DE_species_pca_hclust
                                                                                                                                                                                ), #conditionalPanel
                                                                                                                                                                                shiny::conditionalPanel(condition='input.DE_species_cluster_method == "dbscan"',
                                                                                                                                                                                                        shiny::numericInput(inputId='DE_species_dbscan_eps', label='Epsilon:',
                                                                                                                                                                                                                            min=0, value=0.5, step=0.1, width='100%'), #numericInput #DE_species_pca_eps
                                                                                                                                                                                                        shiny::numericInput(inputId='DE_species_dbscan_minPts', label='minPts:',
                                                                                                                                                                                                                            value=1, min=1, max=22, width='100%') #numericInput #DE_species_pca_minPts
                                                                                                                                                                                ) #conditionalPanel
                                                                                                                                                                 ) #div #DE_species_dim_redu_style2_div
                                                                                                                                                   ) ## column 5
                                                                                                                                    ), #div #DE_species_dim_redu_reset_div
                                                                                                                                    shiny::column(width=2,
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  shiny::actionButton(inputId='DE_species_dim_redu_start', label='Submit', icon=shiny::icon('play'), width='100%'), #DE_species_dim_redu_start
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  shiny::actionButton(inputId='DE_species_dim_redu_reset', label='Reset', icon=shiny::icon('redo'), width='100%') #actionButton #DE_species_dim_redu_reset
                                                                                                                                    ), #column
                                                                                                                                    shiny::column(width=12, htmltools::br())
                                                                                                                      ) ##column 12
                                                                                                      ), ## tabPanel # Dimensionality reduction
                                                                                                      ######################
                                                                                                      ####  Clustering  ####
                                                                                                      ######################
                                                                                                      shiny::tabPanel(title='Hierarchical clustering',
                                                                                                                      htmltools::h3('Hierarchical clustering'),
                                                                                                                      shiny::column(width=12,
                                                                                                                                    htmltools::h6(
                                                                                                                                      htmltools::p(
                                                                                                                                        style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px",
                                                                                                                                        "Lipid species that derived from two/multiple groups will be clustered and visualised on heatmap using hierarchical clustering.
                                                                                                                                              Through heatmap, users may discover the difference between the two/multiple groups by observing the distribution of lipid species.
                                                                                                                                              This analysis provides an overview of lipid species differences between the control group and the experimental group.")),
                                                                                                                                    htmltools::br(),
                                                                                                                                    htmltools::div(id='DE_species_cluster_style_div',
                                                                                                                                                   style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:230px",
                                                                                                                                                   htmltools::div(id='DE_species_cluster_reset_div',
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::selectInput(inputId='DE_species_cluster_by', label='Select feature for clustering:',
                                                                                                                                                                                                   choices=c('By all lipid species'='all',
                                                                                                                                                                                                             'By significant lipid species'='sig'),
                                                                                                                                                                                                   selected='sig_lipid', multiple=FALSE) #selectInput #DE_species_cluster_by
                                                                                                                                                                  ), ## column 6
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::uiOutput('DE.species.sidecolor')
                                                                                                                                                                  ), ## column 6
                                                                                                                                                                  shiny::column(width=12), #column
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::selectInput(inputId='DE_species_dist', label='Distance measure:',
                                                                                                                                                                                                   choices=c('Pearson'='pearson',
                                                                                                                                                                                                             'Spearman'='spearman',
                                                                                                                                                                                                             'Kendall'='kendall'),
                                                                                                                                                                                                   selected='pearson', multiple=FALSE) #selectInput #DE_species_dist
                                                                                                                                                                  ), ## column 6
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::selectInput(inputId='DE_species_hclust', label='Clustering method:',
                                                                                                                                                                                                   choices=c('Complete'='complete',
                                                                                                                                                                                                             'Single'='single',
                                                                                                                                                                                                             'Median'='median',
                                                                                                                                                                                                             'Average'='average',
                                                                                                                                                                                                             "Ward.D"="ward.D",
                                                                                                                                                                                                             "Ward.D2"="ward.D2",
                                                                                                                                                                                                             "WPGMA"="mcquitty",
                                                                                                                                                                                                             "WOGMC"="median",
                                                                                                                                                                                                             "UPGMC"="centroid"),
                                                                                                                                                                                                   selected='complete', multiple=FALSE) #selectInput #DE_species_hclust
                                                                                                                                                                  ), ## column 6
                                                                                                                                                   ), #div #DE_species_cluster_reset_div
                                                                                                                                                   shiny::column(width=12,
                                                                                                                                                                 shiny::column(width=7), #column
                                                                                                                                                                 shiny::column(width=5,
                                                                                                                                                                               shiny::actionButton(inputId='DE_species_cluster_reset',label='Reset', icon=shiny::icon('redo')), #actionButton #DE_species_cluster_reset
                                                                                                                                                                               shiny::actionButton(inputId='DE_species_cluster_start', label='Submit', icon=shiny::icon('play')) #actionButton #DE_species_cluster_start
                                                                                                                                                                 ), ## column 5
                                                                                                                                                   ),## column 12
                                                                                                                                                   shiny::column(width=12,htmltools::br())
                                                                                                                                    ) #div #DE_species_cluster_style_div
                                                                                                                      ) ## column 12
                                                                                                      ), #tabPanel #Clustering
                                                                                                      #################################
                                                                                                      ####  Lipid characteristics  ####
                                                                                                      #################################
                                                                                                      shiny::tabPanel(title='Characteristics association',
                                                                                                                      htmltools::h3(htmltools::p('Characteristics association')),
                                                                                                                      shiny::column(width=12,
                                                                                                                                    htmltools::h6(htmltools::p(
                                                                                                                                      style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px",
                                                                                                                                      "In this part, we categorize significant lipid species based on different lipid characteristics and visualise the difference between control and experimental groups by applying log2 Fold Change."))
                                                                                                                      ) ## column 12
                                                                                                      ) #tabPanel #Lipid characteristics
                                                                                  ), #navlistPanel #DE_species_list
                                                                                  htmltools::div(id='DE_species_analysis_result_div',
                                                                                                 shiny::column(width=12,
                                                                                                               htmltools::h3('Result table of differiential expression analysis',style='text-align:center;'),
                                                                                                               DT::dataTableOutput(outputId='DE.species.tab.all') %>% shinycssloaders::withSpinner() #dataTableOutput #DE.species.tab.all
                                                                                                 ), #column
                                                                                                 shiny::column(width=12,htmltools::br()),
                                                                                                 shiny::column(width=12,
                                                                                                               shiny::column(width=4),
                                                                                                               shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.species.dotchart.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                               shiny::column(width=4, shiny::downloadButton("DE.species.dotchart.download", "Download", style="visibility:hidden;"))),
                                                                                                 shiny::column(width=12,htmltools::br()),
                                                                                                 shiny::column(width=8,
                                                                                                               htmltools::h3('Lollipop chart of significant lipid species',style='text-align:center;'),
                                                                                                               plotly::plotlyOutput(outputId='DE.species.dotchart.sig', height='100%') %>% shinycssloaders::withSpinner() #plotlyOutput #DE.species.dotchart.sig
                                                                                                 ),#column
                                                                                                 shiny::column(width=4,
                                                                                                               htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                               '<h4 style="text-align:left; line-height: 25px;">The lollipop chart presents lipid species that meet the predefined cut-off criteria.
                                                                                                                      <br>
                                                                                                                      The x-axis indicates the log2 fold change for two-group data and the -log10 p-value for multi-group data.
                                                                                                                      Lipid species are listed along the y-axis.
                                                                                                                      The color of each point on the chart corresponds to the -log10 adjusted p-value or p-value.
                                                                                                                      </h4>
                                                                                                                      <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                      <ul>
                                                                                                                      <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                                      <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                      </ul></h4>
                                                                                                                      </div>')
                                                                                                 ), ## column 4
                                                                                                 shiny::column(width=12,htmltools::br()),
                                                                                                 shiny::column(width=12,style='border: solid 2px grey;',
                                                                                                               htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                               '<h4 style="text-align:left; line-height: 25px;">For two-group data, the MA plot categorizes lipid species into three groups: up-regulated (red), down-regulated (blue), and non-significant (grey).
                                                                                                                                      Similarly, the volcano plot follows the same color scheme to highlight the most biologically significant lipid species visually.
                                                                                                                                      <br>
                                                                                                                                      For multiple-group data, the scatter plot of significant expressed lipid species in each class. The dot color is corresponding to the lipid class.
                                                                                                                                      </h4>
                                                                                                                                      <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                      <ul>
                                                                                                                                      <li>Hover the mouse on a specific dot (lipid species) to view the corresponding detailed information.</li>
                                                                                                                                      <li>Hover the mouse on a specific dot (lipid species) to display the boxplot on the right-hand side with their abundance by groups.</li>
                                                                                                                                      <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                                      </ul></h4></div>'
                                                                                                               ),
                                                                                                               shiny::column(width=12,htmltools::br()),
                                                                                                               shiny::column(width=12,
                                                                                                                             shiny::column(width=4),
                                                                                                                             shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.species.maplot.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                             shiny::column(width=4, shiny::downloadButton("DE.species.maplot.download", "Download", style="visibility:hidden;"))),

                                                                                                               shiny::column(width=12,htmltools::br()),
                                                                                                               shiny::column(width=12,
                                                                                                                             shiny::column(width=8, plotly::plotlyOutput("DE.species.maplot", height='500px') %>% shinycssloaders::withSpinner()),
                                                                                                                             shiny::column(width=4, plotly::plotlyOutput("DE.species.ma.box", height='440px') %>% shinycssloaders::withSpinner())
                                                                                                               ),## column 12
                                                                                                               shiny::column(width=12,htmltools::br()),
                                                                                                               shiny::column(width=12,
                                                                                                                             htmltools::div(id='DE_species_volcano_div', style='display: none;',
                                                                                                                                            shiny::column(width=8, plotly::plotlyOutput("DE.species.volcano", height='500px') %>% shinycssloaders::withSpinner()),
                                                                                                                                            shiny::column(width=4, plotly::plotlyOutput("DE.species.vol.box", height='440px') %>% shinycssloaders::withSpinner())
                                                                                                                             ) ## div # DE_species_volcano_div
                                                                                                               ),## column 12
                                                                                                               shiny::column(width=12,htmltools::br()),
                                                                                                 ), ## column 12
                                                                                                 shiny::column(width=12,htmltools::br()),
                                                                                                 shiny::column(width=12,style='border: solid 2px grey;',
                                                                                                               htmltools::br(),
                                                                                                               htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                               '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                      This section displays the abundance boxplot and table of a lipid species. Use the drop-down menu to select a significantly expressed lipid species.
                                                                                                                                      Once a specific lipid species is chosen, its abundance boxplot and corresponding table will be displayed.
                                                                                                                                      </h4></div>'
                                                                                                               ),
                                                                                                               htmltools::br(),
                                                                                                               shiny::column(width=12,
                                                                                                                             shiny::column(width=4),
                                                                                                                             shiny::column(width=4, shiny::selectInput(inputId='DE_species_boxplot_download_lipid', label='Select a lipid:',
                                                                                                                                                                       choices=c('TAG 48:0;0'),
                                                                                                                                                                       selected='TAG 48:0;0')), ## column 4
                                                                                                                             shiny::column(width=4)
                                                                                                               ), ## column 12
                                                                                                               shiny::column(width=12,
                                                                                                                             shiny::column(width=4),
                                                                                                                             shiny::column(width=4, style='padding:0px;text-align:center;',
                                                                                                                                           actionButton("DE.species.boxplot.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                             shiny::column(width=4, shiny::downloadButton("DE.species.boxplot.download", "Download", style="visibility:hidden;"),style='height:55px')
                                                                                                               ), ## column 12
                                                                                                               shiny::column(width=6, style='padding: 0px;',
                                                                                                                             htmltools::h3('Lipid species abundance boxplot', style='text-align: center;'),
                                                                                                                             shiny::plotOutput('DE.species.boxplot.download.plot', width='550px', height='412.5px') %>% shinycssloaders::withSpinner()
                                                                                                               ), ## column 6
                                                                                                               shiny::column(width=6, style='text-align:center;',
                                                                                                                             htmltools::h3('Differential expression analysis result (lipid species)'),
                                                                                                                             DT::dataTableOutput(outputId='DE.species.boxplot.table') %>% shinycssloaders::withSpinner() #dataTableOutput #DE.species.boxplot.table
                                                                                                               ) ## column 6
                                                                                                 ) ## column 12
                                                                                  ), #div #DE_species_analysis_result_div
                                                                                  htmltools::div(id='DE_species_dim_redu_result_div',style='display: none;',
                                                                                                 htmltools::div(id='DE_species_dim_redu_pca_result_div',style='display: none;',
                                                                                                                shiny::column(width=12, style='border: solid 2px grey;',
                                                                                                                              htmltools::br(),
                                                                                                                              htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                              '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                     The PCA plot visually simplifies and discerns patterns within complex lipidomic data, effectively reducing multidimensional variables to principal components.
                                                                                                                                                     The distinct separation or overlap of the groups reflects the underlying differences or similarities.
                                                                                                                                                     <br>
                                                                                                                                                     The scree plot is a common method for determining the number of PCs to be retained.
                                                                                                                                                     The "elbow" of the graph indicates all components to the left of this point can explain most variability of the samples.
                                                                                                                                                     </h4>
                                                                                                                                                     <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                     <ul>
                                                                                                                                                     <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                                                                     <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                                                     </ul></h4></div>'
                                                                                                                              ),
                                                                                                                              htmltools::br(),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            shiny::column(width=4),
                                                                                                                                            shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.species.dim.redu.pca.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                            shiny::column(width=4, shiny::downloadButton("DE.species.dim.redu.pca.download", "Download", style="visibility:hidden;"))),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::h3('PCA plot',style='text-align: center;'),
                                                                                                                                            plotly::plotlyOutput(outputId='DE.species.pca.biplot', height='320px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.species.pca.biplot
                                                                                                                              ), ## column 6
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::h3('PCA scree plot',style='text-align:center;'),
                                                                                                                                            plotly::plotlyOutput(outputId='DE.species.pca.screeplot', height='320px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.species.pca.screeplot
                                                                                                                              ), ## column 6
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::br(),
                                                                                                                                            htmltools::h3('Rotation table',style='text-align:center;'),
                                                                                                                                            DT::dataTableOutput(outputId='DE.species.pca.rotated.data') %>% shinycssloaders::withSpinner(),
                                                                                                                                            htmltools::br()
                                                                                                                              ),
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::br(),
                                                                                                                                            htmltools::h3('Table of PCA contribution table', style='text-align: center;'),
                                                                                                                                            DT::dataTableOutput(outputId='DE.species.pca.contrib.table') %>% shinycssloaders::withSpinner(),
                                                                                                                                            htmltools::br()
                                                                                                                              )
                                                                                                                ),## column 12
                                                                                                                shiny::column(width=12,htmltools::br()),
                                                                                                                shiny::column(12, style='border: solid 2px grey;',
                                                                                                                              htmltools::br(),
                                                                                                                              htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                              '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                     The correlation circle plot illustrates the relationship between top N individual features (lipid species) and principal components (PCs).
                                                                                                                                                     It displays how all the variables are interrelated, with those positively correlated positioned in the same quadrant and negatively correlated ones located diametrically across the origin of the plot.
                                                                                                                                                     <br>
                                                                                                                                                     The feature contribution histogram offers an in-depth view of how individual features (lipid species) contribute to a user-selected principal component, such as PC1, PC2, or a combination thereof (PC1+PC2).
                                                                                                                                                     It allows users to identify which features influence the chosen principal component more.
                                                                                                                                                     </h4>
                                                                                                                                                     <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                     <ul>
                                                                                                                                                     <li>Adjust the slider above each plot to choose the desired number of top features to display.</li>
                                                                                                                                                     <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                                                                     <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                                                     </ul></h4></div>'
                                                                                                                              ),
                                                                                                                              htmltools::br(),
                                                                                                                              htmltools::div(style='padding: 19px;margin-bottom: 20px;background-color: #ecf0f1;border: 1px solid transparent;border-radius: 4px;height: 125px;',
                                                                                                                                             shiny::column(width=6, shiny::sliderInput(inputId='DE_species_pca_variable_topN', label='top N feature:', min=1, max=30, value=10, step=1)), ## column 6
                                                                                                                                             shiny::column(width=6, shiny::selectInput(inputId='DE_species_pca_contrib_PC', label='Contribution of features to principal component:',
                                                                                                                                                                                       choices=c('Component 1'=1,
                                                                                                                                                                                                 'Component 2'=2,
                                                                                                                                                                                                 'Component 1 & Component 2'= '1_2'),
                                                                                                                                                                                       selected='PC1_PC2', multiple=FALSE)) ## column 6
                                                                                                                              ), ## column 6
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            shiny::column(width=5),
                                                                                                                                            shiny::column(width=4, style='padding:0px;', shiny::actionButton("DE.species.dim.redu.pca.topN.download.start", "Download PDF", icon=shiny::icon("download"))),
                                                                                                                                            shiny::column(width=3, shiny::downloadButton("DE.species.dim.redu.pca.topN.download", "Download", style="visibility:hidden;"))),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::h3('PCA correlation circle plot', style='text-align: center;'),
                                                                                                                                            plotly::plotlyOutput(outputId='DE.species.pca.variable') %>% shinycssloaders::withSpinner() #plotlyOutput #DE.species.pca.variable
                                                                                                                              ), ## column 6
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::h3('Feature contribution histogram', style='text-align: center;'),
                                                                                                                                            plotly::plotlyOutput(outputId='DE.species.pca.contrib') %>% shinycssloaders::withSpinner() #plotlyOutput #DE.species.pca.contrib
                                                                                                                              ), ## column 6
                                                                                                                              shiny::column(width=12,htmltools::br())
                                                                                                                ) ## column 12
                                                                                                 ), ## div # DE_species_dim_redu_pca_result_div
                                                                                                 htmltools::div(id='DE_species_dim_redu_plsda_result_div',style='display: none;',
                                                                                                                shiny::column(12, style='border: solid 2px grey;',
                                                                                                                              htmltools::br(),
                                                                                                                              htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                              '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                   The PLS-DA plot visually simplifies and discerns patterns within complex lipidomic data, effectively reducing multidimensional variables to principal components.
                                                                                                                                                   The distinct separation or overlap of the groups reflects the underlying differences or similarities.
                                                                                                                                                   <br>
                                                                                                                                                   In the PLS-DA varialble loading plot, the distance to the variables\' center indicates the variable\'s contribution.
                                                                                                                                                   The value of the x-axis reveals the contribution of the variable to PLS-DA-1, whereas the value of the y-axis discloses the contribution of the variable to PLS-DA-2.
                                                                                                                                                   </h4>
                                                                                                                                                   <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                   <ul>
                                                                                                                                                   <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                                                                   <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                                                   </ul></h4></div>'),
                                                                                                                              htmltools::br(),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            shiny::column(width=4),
                                                                                                                                            shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.species.dim.redu.plsda.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                            shiny::column(width=4, shiny::downloadButton("DE.species.dim.redu.plsda.download", "Download", style="visibility:hidden;"))),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=6, plotly::plotlyOutput(outputId='DE.species.plsda.sample.plot') %>% shinycssloaders::withSpinner()), #plotlyOutput #DE.species.plsda.sample.plot
                                                                                                                              shiny::column(width=6, plotly::plotlyOutput(outputId='DE.species.plsda.variable.plot') %>% shinycssloaders::withSpinner()), #plotlyOutput #DE.species.plsda.variable.plot
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::h3('Table of sample variate', style='text-align: center;'),
                                                                                                                                            DT::dataTableOutput(outputId='DE.species.plsda.variate.table') %>% shinycssloaders::withSpinner(),
                                                                                                                              ), ## column 6
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::h3('Table of sample loading', style='text-align: center;'),
                                                                                                                                            DT::dataTableOutput(outputId='DE.species.plsda.loading.table') %>% shinycssloaders::withSpinner()
                                                                                                                              ), ## column 6
                                                                                                                              shiny::column(width=12,htmltools::br())
                                                                                                                ) ## column 12
                                                                                                 ),## div # DE_species_dim_redu_plsda_result_div
                                                                                                 htmltools::div(id='DE_species_dim_redu_tsne_result_div',style='display: none;',
                                                                                                                shiny::column(12,style='border: solid 2px grey;',
                                                                                                                              htmltools::br(),
                                                                                                                              htmltools:: HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                               '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                      The t-SNE plot visually simplifies and discerns patterns within complex lipidomic data, effectively reducing multidimensional variables to principal components.
                                                                                                                                                      The distinct separation or overlap of the groups reflects the underlying differences or similarities.
                                                                                                                                                      </h4>
                                                                                                                                                      <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                      <ul>
                                                                                                                                                      <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                                                                      <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                                                      </ul></h4></div>'),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            shiny::column(width=4),
                                                                                                                                            shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.species.dim.redu.tsne.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                            shiny::column(width=4, shiny::downloadButton("DE.species.dim.redu.tsne.download", "Download", style="visibility:hidden;"))),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::h3('t-SNE plot', style='text-align: center;'),
                                                                                                                                            plotly::plotlyOutput(outputId='DE.species.tsne.plot', height='400px') %>% shinycssloaders::withSpinner(),
                                                                                                                              ), ## column 6
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::h3('Table of t-SNE data', style='text-align: center;'),
                                                                                                                                            DT::dataTableOutput(outputId='DE.species.tsne.table') %>% shinycssloaders::withSpinner()
                                                                                                                              ), ## column 6
                                                                                                                              shiny::column(width=12,htmltools::br())
                                                                                                                ) ## column 12
                                                                                                 ), ## div # DE_species_dim_redu_tsne_result_div
                                                                                                 htmltools::div(id='DE_species_dim_redu_umap_result_div', style='display: none;',
                                                                                                                shiny::column(12,style='border: solid 2px grey;',
                                                                                                                              htmltools::br(),
                                                                                                                              htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                              '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                     The UMAP plot visually simplifies and discerns patterns within complex lipidomic data, effectively reducing multidimensional variables to principal components.
                                                                                                                                                     The distinct separation or overlap of the groups reflects the underlying differences or similarities.
                                                                                                                                                     </h4>
                                                                                                                                                     <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                     <ul>
                                                                                                                                                     <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                                                                     <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                                                     </ul></h4></div>'),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            shiny::column(width=4),
                                                                                                                                            shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.species.dim.redu.umap.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                            shiny::column(width=4, shiny::downloadButton("DE.species.dim.redu.umap.download", "Download", style="visibility:hidden;"))),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::h3('UMAP plot', style='text-align: center;'),
                                                                                                                                            plotly::plotlyOutput(outputId='DE.species.umap.plot', height='400px') %>% shinycssloaders::withSpinner() ## plotlyOutput # DE.species.umap.plot
                                                                                                                              ), ## column 6
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::h3('Table of UMAP data', style='text-align: center;'),
                                                                                                                                            DT::dataTableOutput(outputId='DE.species.umap.table') %>% shinycssloaders::withSpinner() ## plotlyOutput # DE.species.umap.table
                                                                                                                              ), ## column 6
                                                                                                                              shiny::column(width=12,htmltools::br())
                                                                                                                ) ## column 12
                                                                                                 ) ## div # DE_species_dim_redu_tsne_result_div
                                                                                  ), #div #DE_species_dim_redu_result_div
                                                                                  htmltools::div(id='DE_species_heatmap_result_div', style='display: none;',
                                                                                                 htmltools::div(id='DE_species_heatmap_div', style='display: none;',
                                                                                                                shiny::column(width=3,
                                                                                                                              HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                   '<h4 style="text-align:left; line-height: 25px;">
                                                                                                               The heatmap\'s top annotation categorizes data by sample group, while the side annotation—selectable from a drop-down menu—allows for organization according to various lipid characteristics.
                                                                                                               Cell colors reveal the correlation values, with red indicating positive correlation and blue signifying negative correlation.
                                                                                                               </h4>
                                                                                                               <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                               <ul>
                                                                                                               <li>If the lipid/sample number exceeds 50, their names will not be displayed on the heatmap.</li>
                                                                                                               <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                               <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                               </ul></h4></div>')
                                                                                                                ), ## column 3
                                                                                                                shiny::column(width=9,
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            shiny::column(width=4),
                                                                                                                                            shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.species.clustering.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                            shiny::column(width=4, shiny::downloadButton("DE.species.clustering.download", "Download", style="visibility:hidden;"))),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              htmltools::h3('Lipid species hierarchical clustering heatmap', style='text-align: center;'),
                                                                                                                              shiny::uiOutput('DE.species.heatmap.text'),
                                                                                                                              iheatmapr::iheatmaprOutput(outputId='DE.species.heatmap', height='800px') %>% shinycssloaders::withSpinner() #iheatmaprOutput #DE.species.heatmap
                                                                                                                ) ## column 9
                                                                                                 )## div DE_species_heatmap_div
                                                                                  ), #div DE_species_heatmap_result_div
                                                                                  htmltools::div(id='DE_species_lipid_char_association_result_div',style='display: none;',
                                                                                                 htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                 '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                        The relevant results will appear upon choosing a lipid characteristic from the drop-down menu provided.
                                                                                                                        <br>
                                                                                                                        The lollipop chart compares all significant groups within the selected characteristic by log2(fold change) for two-group data and -log10(p-value) for multiple-group data.
                                                                                                                        <br>
                                                                                                                        The word cloud visualizes the frequency of each group (value) associated with the chosen characteristic.
                                                                                                                        <br>
                                                                                                                        The bar chart distinguishes significant groups (values) exhibiting a mean fold change greater than 2, with orange indicating significance and black denoting non-significance.
                                                                                                                        </h4>
                                                                                                                        <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                        <ul>
                                                                                                                        <li>The bar chart is will not generate for multiple-group data.</li>
                                                                                                                        <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                                        <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                        </ul></h4></div>'
                                                                                                 ),
                                                                                                 htmltools::br(),
                                                                                                 shiny::column(width=4),
                                                                                                 shiny::column(width=4, shiny::uiOutput('DE.species.lipid.char')), ## uiOutput # DE.species.lipid.char
                                                                                                 shiny::column(width=4),
                                                                                                 shiny::column(width=12,htmltools::br()),
                                                                                                 shiny::column(width=12,
                                                                                                               shiny::column(width=4),
                                                                                                               shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.species.lipid.char.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                               shiny::column(width=4, shiny::downloadButton("DE.species.lipid.char.download", "Download", style="visibility:hidden;"))),
                                                                                                 shiny::column(width=12,htmltools::br()),
                                                                                                 shiny::column(width=6,
                                                                                                               htmltools::h3('Lollipop chart of all significant groups', style='text-align: center;'),
                                                                                                               plotly::plotlyOutput(outputId='DE.species.lipid.char.dot') %>% shinycssloaders::withSpinner() #plotlyOutput #DE.species.lipid.char.dot
                                                                                                 ), ## column 6
                                                                                                 shiny::column(width=6,
                                                                                                               htmltools::h3('Word cloud with the count of each group', style='text-align: center;'),
                                                                                                               hwordcloud::hwordcloudOutput(outputId='DE.species.lipid.char.word') %>% shinycssloaders::withSpinner() #hwordcloudOutput #DE.species.lipid.char.word
                                                                                                 ), ## column 6
                                                                                                 shiny::column(width=12, htmltools::br()),
                                                                                                 shiny::column(width=12,
                                                                                                               shiny::column(width=3),
                                                                                                               shiny::column(width=6,
                                                                                                                             htmltools::div(id='DE_species_lipidchar_bar_div',
                                                                                                                                            htmltools::h3('Bar chart of significant groups', style='text-align: center;'),
                                                                                                                                            plotly::plotlyOutput(outputId='DE.species.lipid.char.bar') %>% shinycssloaders::withSpinner() #plotlyOutput #DE.species.lipid.char.bar
                                                                                                                             ) ## div DE_species_lipidchar_bar_div
                                                                                                               ), ## column 6
                                                                                                               shiny::column(width=3)
                                                                                                 )## column 12
                                                                                  )#div DE_species_lipid_char_association_result_div
                                                                  ), #tabPanel #Lipid species analysis
                                                                  shiny::tabPanel(title='Lipid characteristics analysis',
                                                                                  htmltools::br(),
                                                                                  shiny::navlistPanel(widths=c(3, 9),
                                                                                                      id='DE_specific_list',
                                                                                                      'Step 1:',
                                                                                                      ###########################################
                                                                                                      ####  Differential expressed analysis  ####
                                                                                                      ###########################################
                                                                                                      shiny::tabPanel(title='Differential expression',
                                                                                                                      htmltools::h3(htmltools::p('Differential expression')),
                                                                                                                      shiny::column(width=12,
                                                                                                                                    htmltools::h6(style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px",
                                                                                                                                                  'In ', htmltools::strong('Lipid Characteristics Analysis'), ' section, lipid species are categorised and summarised into new lipid abundance table according to two selected lipid characteristic,
                                                                                                                                                         then conducted differential expressed analysis. Samples will be divided into two/multiple groups based on the ', htmltools::strong('Group Information'), ' of input data.
                                                                                                                                                         Two-way ANOVA and appropriate post hoc tests for different data types are utilized. For two-group data, the t-test and the Wilcoxon test (Wilcoxon rank-sum test) are applied, while for multi-group data, one-way ANOVA and the Kruskal-Wallis test are used. This ',
                                                                                                                                                  htmltools::strong('Differentially Expressed Analysis'),' section separates into 2 sections, analysing based on first ‘Characteristics’ and adding ', htmltools::strong('‘Subgroup of characteristics’'),
                                                                                                                                                  ' to the analysis. The first section is analysed based on the first selected ', htmltools::strong('‘characteristics’'),
                                                                                                                                                  '. The second section is the subgroup analysis of the first section.'),
                                                                                                                                    htmltools::br(),
                                                                                                                                    htmltools::h3('Lipid characteristics table', style='text-align: center;'),
                                                                                                                                    htmltools::br(),
                                                                                                                                    shiny::column(width=4),
                                                                                                                                    shiny::column(width=4,
                                                                                                                                                  shiny::selectInput(inputId='DE_class_twoWayAnova_select', label='Select a aspect:',
                                                                                                                                                                     choices=c('All'), selected='All') ## selectInput DE_class_twoWayAnova_select
                                                                                                                                    ),## column 4
                                                                                                                                    shiny::column(width=4),
                                                                                                                                    DT::dataTableOutput(outputId='DE.class.twoWayAnova.tab') %>% shinycssloaders::withSpinner(), #dataTableOutput #DE.class.tab.all
                                                                                                                                    htmltools::br(),
                                                                                                                                    htmltools::div(id='DE_class_analysis_style_div',
                                                                                                                                                   style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:480px",
                                                                                                                                                   htmltools::div(id='DE_class_analysis_reset_div',
                                                                                                                                                                  shiny::column(width=6, shiny::uiOutput('DE.class.analysis.char')), #column
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::uiOutput('DE.class.split.char'), #selectInput #DE.class.split.char
                                                                                                                                                                                shiny::helpText("NOTE: two selected characteristics should be both continuous data or one categorical data with one continuous data.")
                                                                                                                                                                  ), ## column 6
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::radioButtons(inputId='DE_class_stat_method', label='Method:',
                                                                                                                                                                                                    choices=c('Two-way ANOVA'), selected='Two-way ANOVA', inline=TRUE) #radioButtons #DE_class_stat_method
                                                                                                                                                                  ), ## column 6
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::selectInput(inputId='DE_class_sig_p', label='Identify significant lipids:',
                                                                                                                                                                                                   choices=c('p-value'='pval',
                                                                                                                                                                                                             'adjusted p-value (padj)'='padj'),
                                                                                                                                                                                                   selected='pval', multiple=FALSE, width='80%'), #selectInput #DE_class_sig_p
                                                                                                                                                                  ), ## column 6
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::selectInput(inputId='DE_class_post_hoc_method', label='Post hoc tests:',
                                                                                                                                                                                                   choices=c('t-test','Wilcoxon test'),
                                                                                                                                                                                                   selected='t-test') #selectInput #DE_class_adj_stat_method
                                                                                                                                                                  ), ## column 6
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::numericInput(inputId='DE_class_pval', label='p-value:',
                                                                                                                                                                                                    value=0.05, min=0.001, max=1, step=0.001) #numericInput #DE_class_pval
                                                                                                                                                                  ), ## column 6
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::radioButtons(inputId='DE_class_adj_stat_method', label='Multiple testing correction:',
                                                                                                                                                                                                    choices=c('Benjamini & Hochberg'='BH'), selected='BH', inline=TRUE) #radioButtons #DE_adj_stat_method
                                                                                                                                                                  ), ## column 6
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                htmltools::div(id='DE_class_fc_div',
                                                                                                                                                                                               shiny::numericInput(inputId='DE_class_fc', label='Fold change (FC):',
                                                                                                                                                                                                                   value=1, min=1, max=8) #numericInput #DE_class_fc
                                                                                                                                                                                ) ## div # DE_class_fc_div
                                                                                                                                                                  ) #column
                                                                                                                                                   ), #div #DE_analysis_reset_div
                                                                                                                                                   shiny::column(width=12),
                                                                                                                                                   shiny::column(width=7, shiny::uiOutput('DE.class.groupInfo.compare.message')),
                                                                                                                                                   shiny::column(width=5,
                                                                                                                                                                 shiny::actionButton(inputId='DE_class_analysis_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #DE_class_analysis_reset
                                                                                                                                                                 shiny::actionButton(inputId='DE_class_analysis_start', label='Submit', icon=shiny::icon('play')) #actionButton #DE_class_analysis_start
                                                                                                                                                   ), ## column 5

                                                                                                                                    ), ## div # DE_analysis_style_div
                                                                                                                                    htmltools::br()
                                                                                                                      ) ## column 12
                                                                                                      ), # #tabPanel #Differential expressed analysis
                                                                                                      'Step 2:',
                                                                                                      ####################################
                                                                                                      ####  Dimensionality reduction  ####
                                                                                                      ####################################
                                                                                                      shiny::tabPanel(title='Dimensionality reduction',
                                                                                                                      shiny::h3('Dimensionality reduction'),
                                                                                                                      shiny::column(width=12,
                                                                                                                                    htmltools::h6(htmltools::p("Dimensionality reduction in this section assists users to tackle with large numbers of variables in lipids analysis.
                                                                                                                                                                      The high-dimensional space is transformed into a low-dimensional space. Hence, the crucial properties of the lipid data are revealed and still close to its intrinsic characteristics.
                                                                                                                                                                      Here, we provide four types of dimensionality reduction approaches, PCA, PLS-DA, t-SNE, UMAP, and four clustering methods, K-means, partitioning around medoids (PAM), Hierarchical clustering, and DBSCAN.",
                                                                                                                                                               style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                                                    htmltools::br(),
                                                                                                                                    htmltools::div(id='DE_class_dim_redu_reset_div',
                                                                                                                                                   shiny::column(width=5,
                                                                                                                                                                 htmltools::div(id='DE_class_dim_redu_style1_div',
                                                                                                                                                                                style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:340px",
                                                                                                                                                                                shiny::selectInput(inputId='DE_class_dim_redu_method', label='Dimensionality reduction method:',
                                                                                                                                                                                                   choices=c('PCA'='pca',
                                                                                                                                                                                                             'PLS-DA'='plsda',
                                                                                                                                                                                                             't-SNE'='tsne',
                                                                                                                                                                                                             'UMAP'='umap'),
                                                                                                                                                                                                   selected='pca', multiple=FALSE, width='100%'), #selectInput #DE_class_dim_redu_method
                                                                                                                                                                                shiny::conditionalPanel(condition='input.DE_class_dim_redu_method == "tsne"',
                                                                                                                                                                                                        #shinyWidgets::materialSwitch(inputId='DE_class_tsne_pca', label='PCA:',
                                                                                                                                                                                                        #                             value=TRUE, status="primary", right=FALSE), #materialSwitch #DE_class_tsne_pca
                                                                                                                                                                                                        shiny::numericInput(inputId='DE_class_tsne_perplexity', label='Perplexity:',
                                                                                                                                                                                                                            value=5, min=3, max=7, step=1, width='100%') %>%
                                                                                                                                                                                                          shinyhelper::helper(type="inline", title="Perplexity",
                                                                                                                                                                                                                              content=c("The perplexity may be considered as a knob that sets the number of effective nearest neighbours. The typical perplexity range between 5 and 50.")),
                                                                                                                                                                                                        shiny::numericInput(inputId='DE_class_tsne_max_iter', label='Number of iterations:',
                                                                                                                                                                                                                            value=500, min=100,max=5000, step=100, width='100%') %>%
                                                                                                                                                                                                          shinyhelper::helper(type="inline", title="Number of iterations",
                                                                                                                                                                                                                              content=c("The number of iterations is the maximum number of iterations to perform."))
                                                                                                                                                                                ), #conditionalPanel
                                                                                                                                                                                shiny::conditionalPanel(condition='input.DE_class_dim_redu_method == "umap"',
                                                                                                                                                                                                        shiny::numericInput(inputId='DE_class_umap_n_neighbors', label='Number of neighbors:',
                                                                                                                                                                                                                            value=15, min=2, max=23, step=1, width='100%') %>%
                                                                                                                                                                                                          shinyhelper::helper(type="inline", title="Number of neighbors",
                                                                                                                                                                                                                              content=c("The number of neighbors (the number of neighbouring sample points), which is used for manifold approximation.
                                                                                                                                                                                                                 Larger values lead to more global views of the manifold, whilst smaller values result in more local data being preserved.
                                                                                                                                                                                                                           In general values should be in the range 2 to 100.")),
                                                                                                                                                                                                        shiny::selectInput(inputId='DE_class_umap_metric', label='Distance metric:',
                                                                                                                                                                                                                           choices=c('Euclidean'='euclidean',
                                                                                                                                                                                                                                     'Cosine'='cosine',
                                                                                                                                                                                                                                     'Manhattan'='manhattan',
                                                                                                                                                                                                                                     'Hamming'='hamming'),
                                                                                                                                                                                                                           selected='euclidean', multiple=FALSE, width='100%') %>%
                                                                                                                                                                                                          shinyhelper::helper(type="inline", title="Distance metric",
                                                                                                                                                                                                                              content=c("The distance metric is use to find nearest neighbors"))
                                                                                                                                                                                ) #conditionalPanel
                                                                                                                                                                 ) #div #DE_class_dim_redu_style1_div
                                                                                                                                                   ), ## column 5
                                                                                                                                                   shiny::column(width=5,
                                                                                                                                                                 htmltools::div(id='DE_class_dim_redu_style2_div',
                                                                                                                                                                                style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:340px",
                                                                                                                                                                                shiny::selectInput(inputId='DE_class_cluster_method', label='Clustering method:',
                                                                                                                                                                                                   choices=c('Group information'='group_info',
                                                                                                                                                                                                             'K-means'='kmeans',
                                                                                                                                                                                                             'Partitioning around medoids (PAM)'='kmedoids',
                                                                                                                                                                                                             'Hierarchical clustering'='hclustering',
                                                                                                                                                                                                             'DBSCAN'='dbscan'),
                                                                                                                                                                                                   selected='group_info', multiple=FALSE, width='100%'), #selectInput #DE_class_cluster_method
                                                                                                                                                                                shiny::conditionalPanel(condition='input.DE_class_cluster_method == "group_info"',
                                                                                                                                                                                                        htmltools::strong(shiny::textOutput(outputId='DE.class.group.count')),
                                                                                                                                                                                                        shiny::uiOutput('DE.class.PCA.groupInfo.compare.message'),
                                                                                                                                                                                                        htmltools::br()
                                                                                                                                                                                ), #conditionalPanel
                                                                                                                                                                                shiny::conditionalPanel(condition='input.DE_class_cluster_method == "kmeans"',
                                                                                                                                                                                                        shiny::sliderInput(inputId='DE_class_kmeans_group', label='Number of groups:',
                                                                                                                                                                                                                           value=2, min=2, max=10, step=1, width='100%')
                                                                                                                                                                                ), #conditionalPanel
                                                                                                                                                                                shiny::conditionalPanel(condition='input.DE_class_cluster_method == "kmedoids"',
                                                                                                                                                                                                        shiny::sliderInput(inputId='DE_class_pam_group', label='Number of groups:',
                                                                                                                                                                                                                           value=2, min=2, max=10, step=1, width='100%'),
                                                                                                                                                                                                        shiny::selectInput(inputId='DE_class_pam_metric', label='Distance metrics:',
                                                                                                                                                                                                                           choices=c('Euclidean'='euclidean',
                                                                                                                                                                                                                                     'Manhattan'='manhattan'),
                                                                                                                                                                                                                           selected='euclidean', multiple=FALSE, width='100%') #selectInput #DE_class_pca_metric
                                                                                                                                                                                ), #conditionalPanel
                                                                                                                                                                                shiny::conditionalPanel(condition='input.DE_class_cluster_method == "hclustering"',
                                                                                                                                                                                                        shiny::sliderInput(inputId='DE_class_hclust_group',
                                                                                                                                                                                                                           label='Number of groups:',
                                                                                                                                                                                                                           value=2, min=2, max=10, step=1, width='100%'),
                                                                                                                                                                                                        shiny::selectInput(inputId='DE_class_hclust_dist', label='Distance measure:',
                                                                                                                                                                                                                           choices=c('Pearson'='pearson',
                                                                                                                                                                                                                                     'Spearman'='spearman',
                                                                                                                                                                                                                                     'Kendall'='kendall'),
                                                                                                                                                                                                                           selected='pearson', multiple=FALSE, width='100%'), #selectInput #DE_class_pca_dist
                                                                                                                                                                                                        shiny::selectInput(inputId='DE_class_hclust_hclust', label='Clustering method:',
                                                                                                                                                                                                                           choices=c('Complete'='complete',
                                                                                                                                                                                                                                     'Single'='single',
                                                                                                                                                                                                                                     'Median'='median',
                                                                                                                                                                                                                                     'Average'='average',
                                                                                                                                                                                                                                     "Ward.D"="ward.D",
                                                                                                                                                                                                                                     "Ward.D2"="ward.D2",
                                                                                                                                                                                                                                     "WPGMA"="mcquitty",
                                                                                                                                                                                                                                     "WOGMC"="median",
                                                                                                                                                                                                                                     "UPGMC"="centroid"),
                                                                                                                                                                                                                           selected='complete', multiple=FALSE, width='100%') #selectInput #DE_class_pca_hclust
                                                                                                                                                                                ), #conditionalPanel
                                                                                                                                                                                shiny::conditionalPanel(condition='input.DE_class_cluster_method == "dbscan"',
                                                                                                                                                                                                        shiny::numericInput(inputId='DE_class_dbscan_eps', label='Epsilon:',
                                                                                                                                                                                                                            min=0, value=0.5, step=0.1, width='100%'), #numericInput #DE_class_pca_eps
                                                                                                                                                                                                        shiny::numericInput(inputId='DE_class_dbscan_minPts', label='minPts:',
                                                                                                                                                                                                                            value=1, min=1, max=22, width='100%' ) #numericInput #DE_class_pca_minPts
                                                                                                                                                                                ) #conditionalPanel
                                                                                                                                                                 ) #div #DE_class_dim_redu_style2_div
                                                                                                                                                   ) #column
                                                                                                                                    ), #div #DE_class_dim_redu_reset_div
                                                                                                                                    shiny::column(width=2,
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  shiny::actionButton(inputId='DE_class_dim_redu_start', label='Submit', icon=shiny::icon('play'), width='100%'), #DE_class_dim_redu_start
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  shiny::actionButton(inputId='DE_class_dim_redu_reset', label='Reset', icon=shiny::icon('redo'), width='100%') #actionButton #DE_class_dim_redu_reset
                                                                                                                                    ) ## column 2
                                                                                                                      ) ## column 12
                                                                                                      ), #tabPanel #Dimensionality reduction
                                                                                                      ######################
                                                                                                      ####  Clustering  ####
                                                                                                      ######################
                                                                                                      shiny::tabPanel(title='Hierarchical clustering',
                                                                                                                      htmltools::h3('Hierarchical clustering'),
                                                                                                                      shiny::column(width=12,
                                                                                                                                    htmltools::h6(htmltools::p("New lipid abundance table summed up from species will be clustered and shown on the heatmap using hierarchical clustering.
                                                                                                                                                                                      Through heatmap, users may discover the difference between the two/multiple groups by observing the distribution of lipid characteristic abundance.
                                                                                                                                                                                      This analysis provides an overview of lipid characteristic abundance differences between two/multiple groups.
                                                                                                                                                                                      Four distance measures can be chosen, Person, Spearman, or Kendall, and eight clustering methods can be selected by pulling down the menu. ",
                                                                                                                                                               style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                                                    htmltools::br(),
                                                                                                                                    htmltools::div(id='DE_class_cluster_style_div',
                                                                                                                                                   style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:230px",
                                                                                                                                                   htmltools::div(id='DE_class_cluster_reset_div',
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::selectInput(inputId='DE_class_cluster_by', label='Select feature for clustering:',
                                                                                                                                                                                                   choices=c('By all lipid categories'='all',
                                                                                                                                                                                                             'By significant lipid categories'='sig'),
                                                                                                                                                                                                   selected='sig_lipid', multiple=FALSE) #selectInput #DE_class_cluster_by
                                                                                                                                                                  ), ## column 6
                                                                                                                                                                  shiny::column(width=6), #column
                                                                                                                                                                  shiny::column(width=12), #column
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::selectInput(inputId='DE_class_dist', label='Distance measure:',
                                                                                                                                                                                                   choices=c('Pearson'='pearson',
                                                                                                                                                                                                             'Spearman'='spearman',
                                                                                                                                                                                                             'Kendall'='kendall',
                                                                                                                                                                                                             "Euclidean"="euclidean",
                                                                                                                                                                                                             "Maximum"="maximum",
                                                                                                                                                                                                             "Manhattan"="manhattan",
                                                                                                                                                                                                             "Canberra"="canberra",
                                                                                                                                                                                                             "Binary"="binary",
                                                                                                                                                                                                             "Minkowski"="minkowski"),
                                                                                                                                                                                                   selected='pearson', multiple=FALSE) #selectInput #DE_class_dist
                                                                                                                                                                  ), ## column 6
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::selectInput(inputId='DE_class_hclust', label='Clustering method:',
                                                                                                                                                                                                   choices=c('Complete'='complete',
                                                                                                                                                                                                             'Single'='single',
                                                                                                                                                                                                             'Median'='median',
                                                                                                                                                                                                             'Average'='average',
                                                                                                                                                                                                             "Ward.D"="ward.D",
                                                                                                                                                                                                             "Ward.D2"="ward.D2",
                                                                                                                                                                                                             "WPGMA"="mcquitty",
                                                                                                                                                                                                             "WOGMC"="median",
                                                                                                                                                                                                             "UPGMC"="centroid"),
                                                                                                                                                                                                   selected='complete', multiple=FALSE) #selectInput #DE_class_hclust
                                                                                                                                                                  ), ## column 6
                                                                                                                                                   ), #div #DE_class_cluster_reset_div
                                                                                                                                                   shiny::column(width=7), ## column 7
                                                                                                                                                   shiny::column(width=5,
                                                                                                                                                                 shiny::actionButton(inputId='DE_class_cluster_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #DE_class_cluster_reset
                                                                                                                                                                 shiny::actionButton(inputId='DE_class_cluster_start', label='Submit', icon=shiny::icon('play')) #actionButton #DE_class_cluster_start
                                                                                                                                                   ), ## column 5
                                                                                                                                    ) #div #DE_class_cluster_style_div
                                                                                                                      ) ## column 12
                                                                                                      ), #tabPanel #Clustering
                                                                                                      shiny::tabPanel(title='Two characteristics analysis',
                                                                                                                      htmltools::h3('Two characteristics analysis'),
                                                                                                                      shiny::column(width=12,
                                                                                                                                    htmltools::h6(htmltools::p("The heatmaps provide the correlation between the double bond and chain length of lipid species.
                                                                                                                                                                      The color in the heatmaps is gradient according to different data types (log2FC for two group data and p-value for multi-group data).",
                                                                                                                                                               style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                                                    htmltools::br(),
                                                                                                                                    htmltools::div(id='DE_class_twoCharHeatmap_style_div',
                                                                                                                                                   style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:400px",
                                                                                                                                                   htmltools::div(id='DE_class_twoCharHeatmap_reset_div',
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::selectInput(inputId='DE_class_twoCharHeatmap_post_hoc_method', label='Method:',
                                                                                                                                                                                                   choices=c('t-test','Wilcoxon test'),
                                                                                                                                                                                                   selected='t-test') #selectInput #DE_class_twoCharHeatmap_post_hoc_method
                                                                                                                                                                  ), ## column 6
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::selectInput(inputId='DE_class_twoCharHeatmap_sig_p', label='Identify significant lipids:',
                                                                                                                                                                                                   choices=c('p-value'='pval',
                                                                                                                                                                                                             'adjusted p-value (padj)'='padj'),
                                                                                                                                                                                                   selected='pval', multiple=FALSE, width='80%') #selectInput #DE_class_twoCharHeatmap_sig_p
                                                                                                                                                                  ), ## column 6
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                htmltools::div(id='DE_class_twoCharHeatmap_postHoc_div',
                                                                                                                                                                                               shiny::radioButtons(inputId='DE_class_twoCharHeatmap_postHoc_method', label='Post Hoc method:',
                                                                                                                                                                                                                   choices=c("Tukey's HSD"), selected="Tukey's HSD", inline=TRUE)
                                                                                                                                                                                )## div # DE_class_twoCharHeatmap_postHoc_div
                                                                                                                                                                  ),## column 6
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::numericInput(inputId='DE_class_twoCharHeatmap_pval', label='p-value:',
                                                                                                                                                                                                    value=0.05, min=0.001, max=1, step=0.001) #numericInput #DE_class_twoCharHeatmap_pval
                                                                                                                                                                  ), ## column 6
                                                                                                                                                                  shiny::column(width=12),
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                shiny::radioButtons(inputId='DE_class_twoCharHeatmap_adj_stat_method', label='Multiple testing correction:',
                                                                                                                                                                                                    choices=c('Benjamini & Hochberg'='BH'), selected='BH', inline=TRUE) #radioButtons #DE_class_twoCharHeatmap_adj_stat_method
                                                                                                                                                                  ), ## column 6
                                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                                htmltools::div(id='DE_class_twoCharHeatmap_fc_div',
                                                                                                                                                                                               shiny::numericInput(inputId='DE_class_twoCharHeatmap_fc', label='Fold change (FC):',
                                                                                                                                                                                                                   value=1, min=1, max=8) #numericInput #DE_class_twoCharHeatmap_fc
                                                                                                                                                                                )## div # DE_class_twoCharHeatmap_fc_div
                                                                                                                                                                  ) ## column 6
                                                                                                                                                   ), #div #DE_class_twoCharHeatmap_reset_div
                                                                                                                                                   shiny::column(width=7, shiny::uiOutput('DE.class.twoChar.groupInfo.compare.message')), #column
                                                                                                                                                   shiny::column(width=5,
                                                                                                                                                                 shiny::actionButton(inputId='DE_class_twoCharHeatmap_cluster_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #DE_class_twoCharHeatmap_cluster_reset
                                                                                                                                                                 shiny::actionButton(inputId='DE_class_twoCharHeatmap_cluster_start', label='Submit', icon=shiny::icon('play')) #actionButton #DE_class_twoCharHeatmap_cluster_start
                                                                                                                                                   ) #column
                                                                                                                                    ) #div #DE_class_twoCharHeatmap_style_div
                                                                                                                      ) ## column 12
                                                                                                      ) #tabPanel #twoCharHeatmap
                                                                                  ), #navlistPanel #DE_specific_list
                                                                                  htmltools::div(id='DE_class_analysis_result_div',
                                                                                                 htmltools::div(
                                                                                                   shiny::column(width=12,style='border: solid 2px grey;',
                                                                                                                 htmltools::br(),
                                                                                                                 htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                 '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                       The plots displayed the differential expression analysis of a user-selected lipid characteristic.
                                                                                                                                       <br>
                                                                                                                                       The bar plot and line plot depict the different groups in each category of the lipid characteristic. All significant groups will be highlighted with an asterisk.
                                                                                                                                       <br>
                                                                                                                                       The box plot reveals each group\'s distribution of a selected lipid characteristic. Asterisks indicate the significance of P values: three for P values less than 0.001, two for less than 0.01, and one for less than 0.05.
                                                                                                                                       The absence of an asterisk or line denotes a non-significant difference between groups.
                                                                                                                                       </h4>
                                                                                                                                       <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                       <ul>
                                                                                                                                       <li>Switch over the tab to view the origin or sqrt-scaled bar plot and line plot.</li>
                                                                                                                                       <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                                                       <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                                       </ul></h4></div>'),
                                                                                                                 shiny::column(width=12,
                                                                                                                               htmltools::h3('Result table of lipid characteristic differiential expression analysis', style='text-align: center;'),
                                                                                                                               DT::dataTableOutput(outputId='DE.class.tab.all') %>% shinycssloaders::withSpinner(), #dataTableOutput #DE.class.tab.all
                                                                                                                               htmltools::br()
                                                                                                                 ),## column 12
                                                                                                                 shiny::column(width=12,htmltools::br()),
                                                                                                                 shiny::column(width=12,
                                                                                                                               shiny::column(width=4),
                                                                                                                               shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.class.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                               shiny::column(width=4, shiny::downloadButton("DE.class.download", "Download", style="visibility:hidden;"))),
                                                                                                                 shiny::column(width=12,htmltools::br()),
                                                                                                                 shiny::tabsetPanel(
                                                                                                                   shiny::tabPanel(title='Raw',
                                                                                                                                   shiny::column(width=12,
                                                                                                                                                 htmltools::h3('Bar plot of characteristics analysis', style='text-align: center;'),
                                                                                                                                                 plotly::plotlyOutput(outputId='DE.class.barplot', height='500px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.barplot
                                                                                                                                                 htmltools::br()
                                                                                                                                   ), ## column 12
                                                                                                                                   shiny::column(width=8,
                                                                                                                                                 htmltools::h3('Line plot of characteristics analysis', style='text-align: center;'),
                                                                                                                                                 plotly::plotlyOutput(outputId='DE.class.trendplot', height='400px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.trendplot
                                                                                                                                   ), ## column 8
                                                                                                                                   shiny::column(width=4,
                                                                                                                                                 htmltools::h3('Box plot of characteristics analysis',style='text-align: center;'),
                                                                                                                                                 plotly::plotlyOutput(outputId='DE.class.boxplot', height='400px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.boxplot
                                                                                                                                   ) ## column 4
                                                                                                                   ), #tabPanel Raw
                                                                                                                   shiny::tabPanel(title='Sqrt scale',
                                                                                                                                   shiny::column(width=12,
                                                                                                                                                 htmltools::h3('Sqrt-scaled bar plot of characteristics analysis', style='text-align:center;'),
                                                                                                                                                 plotly::plotlyOutput(outputId='DE.class.barplot.sqrt', height='500px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.barplot.sqrt
                                                                                                                                                 htmltools::br()
                                                                                                                                   ), ## column 12
                                                                                                                                   shiny::column(width=8,
                                                                                                                                                 htmltools::h3('Sqrt-scaled line plot of characteristics analysis',style='text-align: center;'),
                                                                                                                                                 plotly::plotlyOutput(outputId='DE.class.trendplot.sqrt', height='400px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.trendplot.sqrt
                                                                                                                                   ), ## column 8
                                                                                                                                   shiny::column(width=4,
                                                                                                                                                 htmltools::h3('Box plot of characteristics analysis', style='text-align: center;'),
                                                                                                                                                 plotly::plotlyOutput(outputId='DE.class.boxplot.sqrt', height='400px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.boxplot.2
                                                                                                                                   ) ## column 4
                                                                                                                   ) ## tabPanel # Sqrt scale
                                                                                                                 ) #tabsetPanel
                                                                                                   ),
                                                                                                   shiny::column(width=12, htmltools::br())),
                                                                                                 htmltools::div(id='DE_class_split_result_div',
                                                                                                                shiny::column(width=12, style='border: solid 2px grey;',
                                                                                                                              htmltools::h2('Subgroup analysis of lipid characteristics'),
                                                                                                                              htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                              '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                     In Subgroup analysis of lipid characteristics, lipid species will be further split by the characteristic that user-chosen in the second pull-down menu then undergo the first section analysis.
                                                                                                                                                     Two-way ANOVA is also applied with t-test as post hoc tests, and the cut-offs of differentially expressed lipids are inputted by users.
                                                                                                                                                     <br>
                                                                                                                                                     The drop-down menu lists all the categories within the selected subgroup characteristic. The corresponding result plots will displayed as a specific category is chosen.
                                                                                                                                                     </h4>
                                                                                                                                                     <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                     <ul>
                                                                                                                                                     <li>Switch over the tab to view the origin or sqrt-scaled bar plot and line plot.</li>
                                                                                                                                                     <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                                                                     <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                                                     </ul></h4></div>'),
                                                                                                                              htmltools::br(),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            shiny::column(width=4),
                                                                                                                                            shiny::column(width=4,
                                                                                                                                                          shiny::selectInput(inputId='DE_class_split_class', label='Select a category:',
                                                                                                                                                                             choices=c('TAG'='TAG'), selected='TAG')), #selectInput #DE_class_split_class
                                                                                                                                            shiny::column(width=4)
                                                                                                                              ),
                                                                                                                              htmltools::br(),
                                                                                                                              htmltools::h3('Result table of subgroup analysis',style='text-align: center;'),
                                                                                                                              DT::dataTableOutput(outputId='DE.class.sub.char.table') %>% shinycssloaders::withSpinner(), #dataTableOutput #DE.class.tab.all
                                                                                                                              htmltools::br(),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            shiny::column(width=4),
                                                                                                                                            shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.class.sub.char.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                            shiny::column(width=4, shiny::downloadButton("DE.class.sub.char.download", "Download", style="visibility:hidden;"))),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::tabsetPanel(
                                                                                                                                shiny::tabPanel(title='Raw',
                                                                                                                                                shiny::column(width=12,
                                                                                                                                                              htmltools::h3('Bar plot of subgroup analysis', style='text-align: center;'),
                                                                                                                                                              plotly::plotlyOutput(outputId='DE.class.sub.char.barplot', height='500px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.sub.char.barplot
                                                                                                                                                              htmltools::br()
                                                                                                                                                ), ## column 12
                                                                                                                                                shiny::column(width=12,
                                                                                                                                                              shiny::column(width=8,
                                                                                                                                                                            htmltools::h3('Line plot of subgroup analysis',style='text-align: center;'),
                                                                                                                                                                            plotly::plotlyOutput(outputId='DE.class.sub.char.barplot.lineplot', height='400px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.sub.char.barplot.lineplot
                                                                                                                                                              ), ## column 8
                                                                                                                                                              shiny::column(width=4,
                                                                                                                                                                            htmltools::h3('Box plot of subgroup analysis', style='text-align: center;'),
                                                                                                                                                                            plotly::plotlyOutput(outputId='DE.class.sub.char.barplot.boxplot', height='400px') %>% shinycssloaders::withSpinner() #plotlyOutput #DE.class.split.boxplot
                                                                                                                                                              ) #column
                                                                                                                                                ),
                                                                                                                                                shiny::column(width=12,htmltools::br())
                                                                                                                                ), #tabPanel
                                                                                                                                shiny::tabPanel(title='Sqrt scale',
                                                                                                                                                shiny::column(width=12,
                                                                                                                                                              htmltools::h3('Sqrt-scaled bar plot of subgroup analysis',style='text-align: center;'),
                                                                                                                                                              plotly::plotlyOutput(outputId='DE.class.sub.char.barplot.sqrt', height='500px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.split.barplot.sqrt
                                                                                                                                                              htmltools::br()
                                                                                                                                                ), ## column 12
                                                                                                                                                shiny::column(width=12,
                                                                                                                                                              shiny::column(width=8,
                                                                                                                                                                            htmltools::h3('Sqrt-scaled line plot of subgroup analysis',style='text-align: center;'),
                                                                                                                                                                            plotly::plotlyOutput(outputId='DE.class.sub.char.barplot.lineplot.sqrt', height='400px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.sub.char.barplot.lineplot.sqrt
                                                                                                                                                              ), #column
                                                                                                                                                              shiny::column(width=4,
                                                                                                                                                                            htmltools::h3('Box plot of subgroup analysis',style='text-align: center;'),
                                                                                                                                                                            plotly::plotlyOutput(outputId='DE.class.sub.char.barplot.boxplot.sqrt', height='400px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.split.boxplot.2
                                                                                                                                                              ) #column
                                                                                                                                                ), ## column 12
                                                                                                                                                shiny::column(width=12,htmltools::br())
                                                                                                                                ) ## tabPanel Sqrt scale
                                                                                                                              ) #tabsetPanel
                                                                                                                ) #column
                                                                                                 ) #div #DE_class_split_result_div
                                                                                  ), #div #DE_class_analysis_result_div
                                                                                  htmltools::div(id='DE_class_dim_redu_result_div',style='display: none;',
                                                                                                 htmltools::div(id='DE_class_dim_redu_pca_result_div',style='display: none;',
                                                                                                                shiny::column(width=12,style='border: solid 2px grey;',
                                                                                                                              htmltools::br(),
                                                                                                                              htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                              '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                     The PCA plot visually simplifies and discerns patterns within complex lipidomic data, effectively reducing multidimensional variables to principal components.
                                                                                                                                                     The distinct separation or overlap of the groups reflects the underlying differences or similarities.
                                                                                                                                                     <br>
                                                                                                                                                     The scree plot is a common method for determining the number of PCs to be retained.
                                                                                                                                                     The "elbow" of the graph indicates all components to the left of this point can explain most variability of the samples.
                                                                                                                                                     </h4>
                                                                                                                                                     <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                     <ul>
                                                                                                                                                     <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                                                                     <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                                                     </ul></h4></div>'),
                                                                                                                              htmltools::br(),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            shiny::column(width=4),
                                                                                                                                            shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.class.dim.redu.pca.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                            shiny::column(width=4, shiny::downloadButton("DE.class.dim.redu.pca.download", "Download", style="visibility:hidden;"))),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(12,
                                                                                                                                            shiny::column(width=6,
                                                                                                                                                          htmltools::h3('PCA plot', style='text-align: center;'),
                                                                                                                                                          plotly::plotlyOutput(outputId='DE.class.pca.biplot', height='320px') %>% shinycssloaders::withSpinner() #plotlyOutput #DE.class.pca.biplot
                                                                                                                                            ), ## column 6
                                                                                                                                            shiny::column(width=6,
                                                                                                                                                          htmltools::h3('PCA scree plot',style='text-align: center;'),
                                                                                                                                                          plotly::plotlyOutput(outputId='DE.class.pca.screeplot', height='320px') %>% shinycssloaders::withSpinner() #plotlyOutput #DE.class.pca.screeplot
                                                                                                                                            ) ## column 6
                                                                                                                              ),## column 12
                                                                                                                              shiny::column(12,
                                                                                                                                            shiny::column(width=6,
                                                                                                                                                          htmltools::br(),
                                                                                                                                                          htmltools::h3('Rotation table', style='text-align: center;'),
                                                                                                                                                          DT::dataTableOutput(outputId='DE.class.pca.rotated.data') %>% shinycssloaders::withSpinner(),
                                                                                                                                                          htmltools::br()
                                                                                                                                            ), ## column 6
                                                                                                                                            shiny::column(width=6,
                                                                                                                                                          htmltools::br(),
                                                                                                                                                          htmltools::h3('Table of PCA contribution table',style='text-align: center;'),
                                                                                                                                                          DT::dataTableOutput(outputId='DE.class.pca.contrib.table') %>% shinycssloaders::withSpinner(),
                                                                                                                                                          htmltools::br()
                                                                                                                                            ) ## column 6
                                                                                                                              ) ## column 12
                                                                                                                ), ## column 12
                                                                                                                shiny::column(width=12, htmltools::br()),
                                                                                                                shiny::column(width=12, style='border: solid 2px grey;',
                                                                                                                              htmltools::br(),
                                                                                                                              htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                              '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                     The correlation circle plot illustrates the relationship between top N individual features (lipid characteristics) and principal components (PCs).
                                                                                                                                                     It displays how all the variables are interrelated, with those positively correlated positioned in the same quadrant and negatively correlated ones located diametrically across the origin of the plot.
                                                                                                                                                     <br>
                                                                                                                                                     The feature contribution histogram offers an in-depth view of how individual features (lipid characteristics) contribute to a user-selected principal component, such as PC1, PC2, or a combination thereof (PC1+PC2).
                                                                                                                                                     It allows users to identify which features influence the chosen principal component more.
                                                                                                                                                     </h4>
                                                                                                                                                     <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                     <ul>
                                                                                                                                                     <li>Adjust the slider above each plot to choose the desired number of top features to display.</li>
                                                                                                                                                     <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                                                                     <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                                                     </ul></h4></div>'),
                                                                                                                              htmltools::br(),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            htmltools::div(style='padding: 19px;margin-bottom: 20px;background-color: #ecf0f1;border: 1px solid transparent;border-radius: 4px;height: 125px;',
                                                                                                                                                           shiny::column(width=6, shiny::sliderInput(inputId='DE_class_pca_variable_topN', label='top N feature:',min=1, max=30, value=10, step=1)),
                                                                                                                                                           shiny::column(width=6,
                                                                                                                                                                         shiny::selectInput(inputId='DE_class_pca_contrib_PC', label='Contribution of features to principal component:',
                                                                                                                                                                                            choices=c('Component 1'=1,
                                                                                                                                                                                                      'Component 2'=2,
                                                                                                                                                                                                      'Component 1 & Component 2'= '1_2'),
                                                                                                                                                                                            selected='PC1_PC2', multiple=FALSE)
                                                                                                                                                           ))
                                                                                                                              ), ## column 12
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            shiny::column(width=5),
                                                                                                                                            shiny::column(width=4, style='padding:0px;', shiny::actionButton("DE.class.dim.redu.pca.topN.download.start", "Download PDF", icon=shiny::icon("download"))),
                                                                                                                                            shiny::column(width=3, shiny::downloadButton("DE.class.dim.redu.pca.topN.download", "Download", style="visibility:hidden;"))),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::h3('PCA correlation circle plot', style='text-align: center;'),
                                                                                                                                            plotly::plotlyOutput(outputId='DE.class.pca.variable') %>% shinycssloaders::withSpinner() #plotlyOutput #DE.species.pca.variable
                                                                                                                              ),## columm 6
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::h3('Feature contribution histogram', style='text-align: center;'),
                                                                                                                                            plotly::plotlyOutput(outputId='DE.class.pca.contrib') %>% shinycssloaders::withSpinner() #plotlyOutput #DE.species.pca.contrib
                                                                                                                              ), ## column 6
                                                                                                                              shiny::column(width=12,htmltools::br())
                                                                                                                )## column 12
                                                                                                 ), ## div DE_class_dim_redu_pca_result_div
                                                                                                 htmltools::div(id='DE_class_dim_redu_plsda_result_div',style='display: none;',
                                                                                                                shiny::column(width=12, style='border: solid 2px grey;',
                                                                                                                              htmltools::br(),
                                                                                                                              htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                              '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                     The PLS-DA plot visually simplifies and discerns patterns within complex lipidomic data, effectively reducing multidimensional variables to principal components.
                                                                                                                                                     The distinct separation or overlap of the groups reflects the underlying differences or similarities.
                                                                                                                                                     <br>
                                                                                                                                                     In the PLS-DA varialble loading plot, the distance to the variables\' center indicates the variable\'s contribution.
                                                                                                                                                     The value of the x-axis reveals the contribution of the variable to PLS-DA-1, whereas the value of the y-axis discloses the contribution of the variable to PLS-DA-2.
                                                                                                                                                     </h4>
                                                                                                                                                     <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                     <ul>
                                                                                                                                                     <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                                                                     <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                                                     </ul></h4></div>'),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            shiny::column(width=4),
                                                                                                                                            shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.class.dim.redu.plsda.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                            shiny::column(width=4, shiny::downloadButton("DE.class.dim.redu.plsda.download", "Download", style="visibility:hidden;"))),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=6, plotly::plotlyOutput(outputId='DE.class.plsda.sample.plot') %>% shinycssloaders::withSpinner()), #plotlyOutput #DE.class.plsda.sample.plot
                                                                                                                              shiny::column(width=6, plotly::plotlyOutput(outputId='DE.class.plsda.variable.plot') %>% shinycssloaders::withSpinner()), #plotlyOutput #DE.class.plsda.variable.plot
                                                                                                                              shiny::column(width=12, htmltools::br()),
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::h3('Table of sample variate', style='text-align: center;'),
                                                                                                                                            DT::dataTableOutput(outputId='DE.class.plsda.variate.table') %>% shinycssloaders::withSpinner()
                                                                                                                              ), ## column 6
                                                                                                                              shiny::column(width=6,
                                                                                                                                            htmltools::h3('Table of sample loading',style='text-align: center;'),
                                                                                                                                            DT::dataTableOutput(outputId='DE.class.plsda.loading.table') %>% shinycssloaders::withSpinner()
                                                                                                                              ), ## column 6
                                                                                                                              shiny::column(width=12, htmltools::br())
                                                                                                                ) ## column 12
                                                                                                 ),## div # DE_class_dim_redu_plsda_result_div
                                                                                                 htmltools::div(id='DE_class_dim_redu_tsne_result_div', style='display: none;',
                                                                                                                shiny::column(width=12, style='border: solid 2px grey;',
                                                                                                                              htmltools::br(),
                                                                                                                              htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                              '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                     The t-SNE plot visually simplifies and discerns patterns within complex lipidomic data, effectively reducing multidimensional variables to principal components.
                                                                                                                                                     The distinct separation or overlap of the groups reflects the underlying differences or similarities.
                                                                                                                                                     </h4>
                                                                                                                                                     <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                     <ul>
                                                                                                                                                     <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                                                                     <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                                                     </ul></h4></div>'),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            shiny::column(width=4),
                                                                                                                                            shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.class.dim.redu.tsne.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                            shiny::column(width=4, shiny::downloadButton("DE.class.dim.redu.tsne.download", "Download", style="visibility:hidden;"))),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            shiny::column(width=6,
                                                                                                                                                          htmltools::h3('t-SNE plot', style='text-align: center;'),
                                                                                                                                                          plotly::plotlyOutput(outputId='DE.class.tsne.plot', height='400px') %>% shinycssloaders::withSpinner()
                                                                                                                                            ), ## column 6
                                                                                                                                            shiny::column(width=6,
                                                                                                                                                          htmltools::h3('Table of t-SNE data', style='text-align: center;'),
                                                                                                                                                          DT::dataTableOutput(outputId='DE.class.tsne.table') %>% shinycssloaders::withSpinner()
                                                                                                                                            ) ## column 6
                                                                                                                              )  ## column 12
                                                                                                                ) ## column 12
                                                                                                 ), ## div # DE_class_dim_redu_tsne_result_div
                                                                                                 htmltools::div(id='DE_class_dim_redu_umap_result_div', style='display: none;',
                                                                                                                shiny::column(width=12,style='border: solid 2px grey;',
                                                                                                                              htmltools::br(),
                                                                                                                              htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                              '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                     The UMAP plot visually simplifies and discerns patterns within complex lipidomic data, effectively reducing multidimensional variables to principal components.
                                                                                                                                                     The distinct separation or overlap of the groups reflects the underlying differences or similarities.
                                                                                                                                                     </h4>
                                                                                                                                                     <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                     <ul>
                                                                                                                                                     <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                                                                     <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                                                     </ul></h4></div>'),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            shiny::column(width=4),
                                                                                                                                            shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.class.dim.redu.umap.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                            shiny::column(width=4, shiny::downloadButton("DE.class.dim.redu.umap.download", "Download", style="visibility:hidden;"))),
                                                                                                                              shiny::column(width=12,htmltools::br()),
                                                                                                                              shiny::column(width=12,
                                                                                                                                            shiny::column(width=6,
                                                                                                                                                          htmltools::h3('UMAP plot',style='text-align: center;'),
                                                                                                                                                          plotly::plotlyOutput(outputId='DE.class.umap.plot', height='400px') %>% shinycssloaders::withSpinner()
                                                                                                                                            ),
                                                                                                                                            shiny::column(width=6,
                                                                                                                                                          htmltools::h3('Table of UMAP data',style='text-align: center;'),
                                                                                                                                                          DT::dataTableOutput(outputId='DE.class.umap.table') %>% shinycssloaders::withSpinner()
                                                                                                                                            )
                                                                                                                              ) ## column 12
                                                                                                                ) ## column 12
                                                                                                 ) ## div # DE_class_dim_redu_umap_result_div
                                                                                  ), #div #DE_class_dim_redu_result_div
                                                                                  htmltools::div(id='DE_class_heatmap_result_div', style='display: none;',
                                                                                                 htmltools::div(id='DE_class_heatmap_div', style='display: none;',
                                                                                                                shiny::column(width=12,
                                                                                                                              shiny::column(width=3,
                                                                                                                                            htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                                            '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                    Columns are all sample, and rows are the significant characteristic group (value) selected in the first `Characteristics` section from Step1.
                                                                                                                                                    Cell colors reveal the correlation values, with red indicating positive correlation and blue signifying negative correlation.
                                                                                                                                                    </h4>
                                                                                                                                                    <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                    <ul>
                                                                                                                                                    <li>If the lipid/sample number exceeds 50, their names will not be displayed on the heatmap.</li>
                                                                                                                                                    <li>Hover the mouse on the plot to view the corresponding detailed information.</li>
                                                                                                                                                    <li>For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                                                                    </ul></h4></div>')
                                                                                                                              ), ## column 3
                                                                                                                              shiny::column(width=9,
                                                                                                                                            shiny::column(width=12,htmltools::br()),
                                                                                                                                            shiny::column(width=12,
                                                                                                                                                          shiny::column(width=4),
                                                                                                                                                          shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.class.clustering.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                                          shiny::column(width=4, shiny::downloadButton("DE.class.clustering.download", "Download", style="visibility:hidden;"))),
                                                                                                                                            shiny::column(width=12,htmltools::br()),
                                                                                                                                            htmltools::h3('Lipid characteristic hierarchical clustering heatmap', style='text-align: center;'),
                                                                                                                                            shiny::uiOutput('DE.class.heatmap.text'),
                                                                                                                                            iheatmapr::iheatmaprOutput(outputId='DE.class.heatmap', height='800px') %>% shinycssloaders::withSpinner() #plotlyOutput #DE.class.cluster
                                                                                                                              ) ## column 9
                                                                                                                ) ## column 12
                                                                                                 )## div # DE_class_heatmap_div
                                                                                  ), #div #DE_class_heatmap_result_div
                                                                                  htmltools::div(id='DE_class_twoCharHeatmap_div',
                                                                                                 htmltools::div(id='DE_class_twoCharHeatmap_result_div', style='display:none;',
                                                                                                                tabsetPanel(id='DE_class_twoCharHeatmap_tab',
                                                                                                                            tabPanel(title="Total FA",
                                                                                                                                    htmltools::br(),
                                                                                                                                     shiny::column(width=12,style='border: solid 2px grey;',
                                                                                                                                                  htmltools::br(),
                                                                                                                                                   HTML(
                                                                                                                                                     '<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px;">',
                                                                                                                                                     '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                              The heatmaps illustrate the correlation between lipid species\' double bond counts and chain lengths based on a lipid characteristic selected in Step 1.
                                                                                                                              The correlation is visually represented by cell colors—red indicates a positive correlation, while blue indicates a negative. Significant correlations are highlighted with an asterisk sign on the plot.
                                                                                                                               </h4>
                                                                                                                               <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                  <ul>
                                                                                                                                  <li>
                                                                                                                                  Switch over the tab to view the heatmap of total FA or each FA.
                                                                                                                                  </li>
                                                                                                                                  </ul>
                                                                                                                              </h4>',
                                                                                                                                                     '</div>'
                                                                                                                                                   ),
                                                                                                                                                   htmltools::h3('Two characteristics correlation heatmap',style='text-align: center;'),
                                                                                                                                                   shiny::column(width=12,htmltools::br()),
                                                                                                                                                   shiny::column(width=12,
                                                                                                                                                                 shiny::column(width=4),
                                                                                                                                                                 shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.class.twoChar.total.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                                                 shiny::column(width=4, shiny::downloadButton("DE.class.twoChar.total.download", "Download", style="visibility:hidden;"))),
                                                                                                                                                   shiny::column(width=12,htmltools::br()),
                                                                                                                                                   shiny::column(width=12,
                                                                                                                                                                 shiny::plotOutput(outputId='DE.class.twoCharHeatmap.total.heatmap', height='500px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.twoCharHeatmap.total.heatmap
                                                                                                                                                                 htmltools::br(),
                                                                                                                                                                 htmltools::h3('Table of two characteristics analysis',style='text-align: center;'),
                                                                                                                                                                 DT::dataTableOutput(outputId='DE.class.twoCharHeatmap.total.table') %>% shinycssloaders::withSpinner(), #dataTableOutput #DE.class.twoCharHeatmap.total.table
                                                                                                                                                                 htmltools::br()
                                                                                                                                                   )
                                                                                                                                     ),
                                                                                                                                     shiny::column(width=12,br()),
                                                                                                                                     shiny::column(width=12,style='border: solid 2px grey;',
                                                                                                                                                  htmltools::br(),
                                                                                                                                                   HTML(
                                                                                                                                                     '<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                                     '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                              Choose a specific characteristic from the lipid characteristic selected in Step 1 via the drop-down menu to explore the correlation between the double bond counts and chain lengths.
                                                                                                                              The correlation is visually represented by cell colors—red indicates a positive correlation, while blue indicates a negative.
                                                                                                                              Significant correlations are highlighted with an asterisk sign on the plot.
                                                                                                                              </h4>',
                                                                                                                                                     '</div>'
                                                                                                                                                   ),
                                                                                                                                                  htmltools::br(),
                                                                                                                                                   shiny::column(width=4),
                                                                                                                                                   shiny::column(width=4,
                                                                                                                                                                 selectInput(inputId='DE_class_twoCharHeatmap_total_charFeature',
                                                                                                                                                                             label='Select the lipid characteristics:',
                                                                                                                                                                             choices='class',
                                                                                                                                                                             multiple=F
                                                                                                                                                                 ), #selectInput #DE_class_twoCharHeatmap_total_charFeature
                                                                                                                                                   ), #column
                                                                                                                                                   shiny::column(width=4),
                                                                                                                                                   shiny::column(width=12,htmltools::br()),
                                                                                                                                                   shiny::column(width=12,
                                                                                                                                                                 shiny::column(width=4),
                                                                                                                                                                 shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.class.twoChar.total.charFeature.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                                                 shiny::column(width=4, shiny::downloadButton("DE.class.twoChar.total.charFeature.download", "Download", style="visibility:hidden;"))),
                                                                                                                                                   shiny::column(width=12,htmltools::br()),
                                                                                                                                                   shiny::column(width=6,
                                                                                                                                                                 htmltools::h3('Two characteristics correlation heatmap of selected category',style='text-align: center;'),
                                                                                                                                                                 shiny::plotOutput(outputId='DE.class.twoCharHeatmap.total.charFeature.heatmap', height='500px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.twoCharHeatmap.total.charFeature.heatmap
                                                                                                                                                   ),
                                                                                                                                                   shiny::column(width=6,
                                                                                                                                                                 htmltools::h3('Table of two characteristics analysis of selected category',style='text-align: center;'),
                                                                                                                                                                 DT::dataTableOutput(outputId='DE.class.twoCharHeatmap.total.charFeature.table') %>% shinycssloaders::withSpinner() #dataTableOutput #DE.class.twoCharHeatmap.total.charFeature.table
                                                                                                                                                   ),
                                                                                                                                                   shiny::column(width=12,br())
                                                                                                                                     ),
                                                                                                                                     shiny::column(width=12,br()),
                                                                                                                                     shiny::column(width=12,style='padding: 0px;',
                                                                                                                                                   div(id='DE_class_twoCharHeatmap_total_box_div',style='display:none;border: solid 2px grey;height: 930px;',
                                                                                                                                                      htmltools::br(),
                                                                                                                                                       HTML(
                                                                                                                                                         '<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px;margin: 15px;">',
                                                                                                                                                         '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                              In this section, the abundance box plot displays data for lipid species based on the lipid characteristic selected previously.
                                                                                                                              You can investigate the relationship between double bond counts and chain lengths by choosing a particular lipid species from the drop-down menu.
                                                                                                                              An asterisk sign indicates significant differences between groups.
                                                                                                                              The absence of an asterisk or line denotes a non-significant difference between groups.
                                                                                                                               </h4>',
                                                                                                                                                         '</div>'
                                                                                                                                                       ),
                                                                                                                                                      htmltools::br(),
                                                                                                                                                       shiny::column(width=4),
                                                                                                                                                       shiny::column(width=4,
                                                                                                                                                                     selectInput(inputId='DE_class_twoCharHeatmap_total_box_char',
                                                                                                                                                                                 label='Select the lipid characteristics:',
                                                                                                                                                                                 choices='class',
                                                                                                                                                                                 multiple=F
                                                                                                                                                                     ), #selectInput #DE_class_twoCharHeatmap_each_charFeature
                                                                                                                                                       ), #column
                                                                                                                                                       shiny::column(width=4),
                                                                                                                                                       shiny::column(width=12,htmltools::br()),
                                                                                                                                                       shiny::column(width=12,
                                                                                                                                                                     shiny::column(width=4),
                                                                                                                                                                     shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.class.twoChar.total.boxplot.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                                                     shiny::column(width=4, shiny::downloadButton("DE.class.twoChar.total.boxplot.download", "Download", style="visibility:hidden;"))),
                                                                                                                                                       shiny::column(width=12,htmltools::br()),
                                                                                                                                                       shiny::column(width=6,
                                                                                                                                                                     htmltools::h3('Abundance box plot of selected lipid species',style='text-align: center;'),
                                                                                                                                                                     shiny::plotOutput(outputId='DE.class.twoCharHeatmap.total.sig.boxplot', height='500px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.twoCharHeatmap.total.sig.boxplot
                                                                                                                                                       ),
                                                                                                                                                       shiny::column(width=6,
                                                                                                                                                                     htmltools::h3('Table of selected lipid species',style='text-align: center;'),
                                                                                                                                                                     DT::dataTableOutput(outputId='DE.class.twoCharHeatmap.total.sig.table') %>% shinycssloaders::withSpinner() #dataTableOutput #DE.class.twoCharHeatmap.total.sig.table
                                                                                                                                                       ),
                                                                                                                                                       shiny::column(width=12,br())
                                                                                                                                                   )
                                                                                                                                     )
                                                                                                                            ),
                                                                                                                            tabPanel(title="Each FA",
                                                                                                                                    htmltools::br(),
                                                                                                                                     shiny::column(width=12,style='border: solid 2px grey;',
                                                                                                                                                  htmltools::br(),
                                                                                                                                                   HTML(
                                                                                                                                                     '<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                                     '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                              The heatmaps illustrate the correlation between lipid species\' double bond counts and chain lengths based on a lipid characteristic selected in Step 1.
                                                                                                                              The correlation is visually represented by cell colors—red indicates a positive correlation, while blue indicates a negative. Significant correlations are highlighted with an asterisk sign on the plot.
                                                                                                                               </h4>
                                                                                                                               <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                  <ul>
                                                                                                                                  <li>
                                                                                                                                  Switch over the tab to view the heatmap of total FA or each FA.
                                                                                                                                  </li>
                                                                                                                                  </ul>
                                                                                                                              </h4>',
                                                                                                                                                     '</div>'
                                                                                                                                                   ),
                                                                                                                                                   htmltools::h3('Two characteristics correlation heatmap',style='text-align: center;'),
                                                                                                                                                   shiny::column(width=12,htmltools::br()),
                                                                                                                                                   shiny::column(width=12,
                                                                                                                                                                 shiny::column(width=4),
                                                                                                                                                                 shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.class.twoChar.each.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                                                 shiny::column(width=4, shiny::downloadButton("DE.class.twoChar.each.download", "Download", style="visibility:hidden;"))),
                                                                                                                                                   shiny::column(width=12,htmltools::br()),
                                                                                                                                                   shiny::column(width=12,
                                                                                                                                                                 shiny::plotOutput(outputId='DE.class.twoCharHeatmap.each.heatmap', height='500px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.twoCharHeatmap.eah.heatmap
                                                                                                                                                                htmltools::br(),
                                                                                                                                                                 htmltools::h3('Table of two characteristics analysis',style='text-align: center;'),
                                                                                                                                                                 DT::dataTableOutput(outputId='DE.class.twoCharHeatmap.each.table') %>% shinycssloaders::withSpinner(), #dataTableOutput #DE.class.twoCharHeatmap.eah.table
                                                                                                                                                                htmltools::br()
                                                                                                                                                   )
                                                                                                                                     ),
                                                                                                                                     shiny::column(width=12,br()),
                                                                                                                                     shiny::column(width=12,style='border: solid 2px grey;',
                                                                                                                                                  htmltools::br(),
                                                                                                                                                   HTML(
                                                                                                                                                     '<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                                     '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                              Choose a specific characteristic from the lipid characteristic selected in Step 1 via the drop-down menu to explore the correlation between the double bond counts and chain lengths.
                                                                                                                              The correlation is visually represented by cell colors—red indicates a positive correlation, while blue indicates a negative.
                                                                                                                              Significant correlations are highlighted with an asterisk sign on the plot.
                                                                                                                              </h4>',
                                                                                                                                                     '</div>'
                                                                                                                                                   ),
                                                                                                                                                  htmltools::br(),
                                                                                                                                                   shiny::column(width=4),
                                                                                                                                                   shiny::column(width=4,
                                                                                                                                                                 selectInput(inputId='DE_class_twoCharHeatmap_each_charFeature',
                                                                                                                                                                             label='Select the lipid characteristics:',
                                                                                                                                                                             choices='class',
                                                                                                                                                                             multiple=F
                                                                                                                                                                 ), #selectInput #DE_class_twoCharHeatmap_each_charFeature
                                                                                                                                                   ), #column
                                                                                                                                                   shiny::column(width=4),
                                                                                                                                                   shiny::column(width=12,htmltools::br()),
                                                                                                                                                   shiny::column(width=12,
                                                                                                                                                                 shiny::column(width=4),
                                                                                                                                                                 shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.class.twoChar.each.charFeature.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                                                 shiny::column(width=4, shiny::downloadButton("DE.class.twoChar.each.charFeature.download", "Download", style="visibility:hidden;"))),
                                                                                                                                                   shiny::column(width=12,htmltools::br()),
                                                                                                                                                   shiny::column(width=6,
                                                                                                                                                                 htmltools::h3('Two characteristics correlation heatmap of selected category',style='text-align: center;'),
                                                                                                                                                                 shiny::plotOutput(outputId='DE.class.twoCharHeatmap.each.charFeature.heatmap', height='500px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.twoCharHeatmap.eah.charFeature.heatmap
                                                                                                                                                   ),
                                                                                                                                                   shiny::column(width=6,
                                                                                                                                                                 htmltools::h3('Table of two characteristics analysis of selected category',style='text-align: center;'),
                                                                                                                                                                 DT::dataTableOutput(outputId='DE.class.twoCharHeatmap.each.charFeature.table') %>% shinycssloaders::withSpinner() #dataTableOutput #DE.class.twoCharHeatmap.eah.charFeature.table
                                                                                                                                                   ),
                                                                                                                                                   shiny::column(width=12,br())
                                                                                                                                     ),
                                                                                                                                     shiny::column(width=12,br()),
                                                                                                                                     shiny::column(width=12,style='padding: 0px;',
                                                                                                                                                   div(id='DE_class_twoCharHeatmap_each_box_div',style='display:none;border: solid 2px grey;height: 930px;',
                                                                                                                                                      htmltools::br(),
                                                                                                                                                       HTML(
                                                                                                                                                         '<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px;margin: 15px;">',
                                                                                                                                                         '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                              In this section, the abundance box plot displays data for lipid species based on the lipid characteristic selected previously.
                                                                                                                              You can investigate the relationship between double bond counts and chain lengths by choosing a particular lipid species from the drop-down menu.
                                                                                                                              An asterisk sign indicates significant differences between groups.
                                                                                                                              The absence of an asterisk or line denotes a non-significant difference between groups.
                                                                                                                               </h4>',
                                                                                                                                                         '</div>'
                                                                                                                                                       ),
                                                                                                                                                      htmltools::br(),
                                                                                                                                                       shiny::column(width=4),
                                                                                                                                                       shiny::column(width=4,
                                                                                                                                                                     selectInput(inputId='DE_class_twoCharHeatmap_each_box_char',
                                                                                                                                                                                 label='Select the lipid characteristics:',
                                                                                                                                                                                 choices='class',
                                                                                                                                                                                 multiple=F
                                                                                                                                                                     ), #selectInput #DE_class_twoCharHeatmap_each_charFeature
                                                                                                                                                       ), #column
                                                                                                                                                       shiny::column(width=4),
                                                                                                                                                       shiny::column(width=12,htmltools::br()),
                                                                                                                                                       shiny::column(width=12,
                                                                                                                                                                     shiny::column(width=4),
                                                                                                                                                                     shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("DE.class.twoChar.each.boxplot.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                                                     shiny::column(width=4, shiny::downloadButton("DE.class.twoChar.each.boxplot.download", "Download", style="visibility:hidden;"))),
                                                                                                                                                       shiny::column(width=12,htmltools::br()),
                                                                                                                                                       shiny::column(width=6,
                                                                                                                                                                     htmltools::h3('Abundance box plot of selected lipid species',style='text-align: center;'),
                                                                                                                                                                     shiny::plotOutput(outputId='DE.class.twoCharHeatmap.each.sig.boxplot', height='500px') %>% shinycssloaders::withSpinner(), #plotlyOutput #DE.class.twoCharHeatmap.eah.sig.boxplot
                                                                                                                                                                     shiny::column(width=4),
                                                                                                                                                                     shiny::column(width=6,style='padding: 0px;', shiny::downloadButton("DE.class.twoCharHeatmap.each.sig.boxplot.download", "Download PDF and table")),
                                                                                                                                                                     shiny::column(width=2)
                                                                                                                                                       ),
                                                                                                                                                       shiny::column(width=6,
                                                                                                                                                                     htmltools::h3('Table of selected lipid species',style='text-align: center;'),
                                                                                                                                                                     DT::dataTableOutput(outputId='DE.class.twoCharHeatmap.each.sig.table') %>% shinycssloaders::withSpinner() #dataTableOutput #DE.class.twoCharHeatmap.eah.sig.table
                                                                                                                                                       ),
                                                                                                                                                       shiny::column(width=12,br())
                                                                                                                                                   )
                                                                                                                                     )
                                                                                                                            )
                                                                                                                ) #conditionalPanel
                                                                                                 )#div #DE_class_twoCharHeatmap_result_div
                                                                                  )#div #DE_class_twoCharHeatmap_div
                                                                  ) #tabPanel #Lipid specific analysis
                                               ) #tabsetPanel #DE_analysis_tab
                                ) #div #DE_tabPanel_div
                  )
                ) #column
) #fluidRow


