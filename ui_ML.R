##LipidSig v2.2 change R function to LipidSigR script
shiny::tabPanel(title=htmltools::HTML("<h4 style='font-size:18px;padding-top:9.5px;padding-bottom:11.5px;text-align:center;'>Machine<br>Learning</h4>"),

                value='ML',
                #### Profiling Header Panel ####
                htmltools::h1('Machine learning analysis'),
                htmltools::br(),
                shiny::fluidRow(
                  shiny::column(width=12,
                                ###############################
                                ####  ML Page Description  ####
                                ###############################
                                htmltools::div(style="background-color: PowderBlue;border-left: 8px solid Teal;padding: 15px",
                                               htmltools::HTML('<h6>In this section, lipid species and lipid characteristics data can be combined by users to predict the binary outcome using various machine learning methods and select the best feature combination to explore further relationships.
                                                                For cross-validation, Monte-Carlo cross-validation (CV) is executed to evaluate the model performance and to reach statistical significance. Additionally, we provide eight feature ranking methods (p-value, p-value*FC, ROC, Random Forest,
                                                                Linear SVM, Lasso, Ridge, ElasticNet) and six classification methods (Random Forest, Linear SVM, Lasso, Ridge, ElasticNet, XGBoost) for users to train and select the best model.
                                                                Feature ranking methods can be divided into two categories: a univariate and multivariate analysis.
                                                               <br>
                                                               A series of consequent analyses assist users to evaluate the methods and visualise the results of machine learning, including ROC/PR curve, Model predictivity, Sample probability, Feature importance, and Network.
                                                               </h6>
                                                               <br>
                                                               <em style="font-size:17px; text-align:left; line-height: 25px;">
                                                               <i class="fas fa-caret-right" role="presentation" aria-label="caret-right icon"></i>
                                                               Demo dataset source: <a href="https://pubmed.ncbi.nlm.nih.gov/31068703/" target="_blank">The landscape of cancer cell line metabolism (Nat Med. 2018)</a>
                                                               </em>')), #div 
                                
                                htmltools::br(),
                                ##########################
                                ####  ML Data Source  ####
                                ##########################
                                htmltools::h2('Data Source'),
                                shiny::sidebarLayout(fluid=TRUE,
                                                     shiny::sidebarPanel(width=4,
                                                                         shiny::radioButtons(inputId='ML_data_source', label=htmltools::h4('Data source'),
                                                                                             choices=c('Example dataset (Nat Med. 2018)'='ML_demo_data',
                                                                                                       'Upload your data!'='ML_user_data'),
                                                                                             selected='ML_demo_data') %>% #radioButtons #ML_data_source
                                                                           shinyhelper::helper(type="inline", title="Data source", size ="l",
                                                                                               content=c('<ol style="font-size: 0px;">',
                                                                                                         '<li style="font-size: 16px;">Lipid dataset can be uploaded by users or using example datasets. This information, namely Lipid abundance data and  Condition table. Two tables must assign to a vector all data needs to be uploaded in
                                                                                           <mark style="background-color: white;color: red;">CSV</mark>, <mark style="background-color: white;color: red;">TSV</mark>, or <mark style="background-color: white;color: red;">XLSX</mark> format.
                                                                                           The maximum file size is 30MB. <br>
                                                                                           <ul style="font-size: 16px; color:red;">
                                                                                           <li>NOTE: When uploading in XLSX format, ensure the data frame is on the first sheet. </li>
                                                                                           </ul>
                                                                                           </li>
                                                                                           <br>
                                                                                           <li style="font-size: 16px;">Once two files are chosen and shown ‘Upload complete’ then press ‘Upload’.</li>
                                                                                           </ol>')),
                                                                         shiny::conditionalPanel(condition='input.ML_data_source == "ML_demo_data"',
                                                                                                 shiny::actionButton(inputId='ML_demo_upload', label='Submit', icon=shiny::icon('upload')), #actionButton #ML_demo_upload
                                                                                                 shiny::downloadButton(outputId='ML.demo.download', label='Download example')), #conditionalPanel
                                                                         shiny::conditionalPanel(condition='input.ML_data_source == "ML_user_data"',
                                                                                                 htmltools::div(id='ML_user_reset_div',
                                                                                                                shiny::fileInput(inputId='ML_user_abundance', label='Lipid abundance data:', accept=c(".csv", ".tsv", ".xlsx"), multiple=FALSE) %>% #fileInput #ML_user_exp
                                                                                                                  shinyhelper::helper(type="inline", title="Lipid abundance data", size="l",
                                                                                                                                      content=c('<ol style="font-size: 0px;">',
                                                                                                                                                '<li style="font-size: 16px;">The first column must contain a list of unique lipids name(features). </li>',
                                                                                                                                                '<li style="font-size: 16px;">Other columns encompass the expressed values of groups under different conditions that you want to compare.</li>',
                                                                                                                                                '<li style="font-size: 16px;">An example of ‘Lipid abundance data’</li>',
                                                                                                                                                '</ol>',
                                                                                                                                                '<img src="Description/ML_Lipid expression data.webp" style="border:2px #ccc solid;padding:20px;" loading="lazy" width="100%"/>')),
                                                                                                                shiny::fileInput(inputId='ML_user_cond', label='Condition table:', accept=c(".csv", ".tsv", ".xlsx"), multiple=FALSE) %>% #fileInput #ML_user_cond
                                                                                                                  shinyhelper::helper(type="inline", title="Condition table", size="l",
                                                                                                                                      content=c('<ol style="font-size: 0px;">',
                                                                                                                                                '<li style="font-size: 16px;">‘Condition table’ can only contain 2 columns: in ‘sample_name’, ‘group’ order</li>',
                                                                                                                                                '<li style="font-size: 16px;">The first column must contain a list of samples name (features), MUST SAME AS THE SAMPLE NAME (COLUM NAMES) OF ‘Lipid abundance data’!</li>',
                                                                                                                                                '<li style="font-size: 16px;">‘group’ refers to the group of the sample (can only be 0 and 1). NOTE: THE GROUP MUST BE NUMERIC, AND SKEWED DATA ARE NOT RECOMMEND</li>',
                                                                                                                                                '<li style="font-size: 16px;">An example of ‘Condition table’</li>',
                                                                                                                                                '</ol>',
                                                                                                                                                '<img src="Description/ML_Condition table.webp" style="border:2px #ccc solid;padding:20px;" loading="lazy" width="100%"/>')),
                                                                                                                shiny::helpText("Upload your data table in .csv/.tsv/.xlsx") #helpText
                                                                                                 ), ##div #ML_user_reset_div
                                                                                                 shiny::actionButton(inputId='ML_user_reset', label='Reset uploaded data', icon=shiny::icon('redo')), #actionButton #ML_user_reset
                                                                                                 shiny::actionButton(inputId='ML_user_upload', label='Upload', icon=shiny::icon('upload')) #actionButton #ML_user_upload
                                                                         ), #conditionalPanel
                                                                         htmltools::br(),
                                                                         htmltools::HTML("<a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank' style='color: darkblue;'>How to prepare your dataset?</a>"),
                                                                         htmltools::br(),
                                                                         htmltools::HTML("<a href='https://lipidsig.bioinfomics.org/Tutorial/?ML' target='_blank' style='color: darkblue;'>How to use this function?</a>")
                                                     ), #sidebarPanel
                                                     shiny::mainPanel(width=8,
                                                                      htmltools::div(
                                                                        id='ML_data_check_progress',
                                                                        style="display:none;text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                                        shiny::htmlOutput('ML.data.check.progress'))
                                                     ) #mainPanel
                                ), #sidebarLayout
                                htmltools::div(id='ML_data_check_successful', style='display:none;',
                                               htmltools::div(id='ML_data_Uploaded',
                                                              htmltools::h4(htmltools::strong('Uploaded data')),
                                                              tabsetPanel(id='ML_user_raw_table_tab',
                                                                          shiny::tabPanel(title="Lipid abundance data", DT::dataTableOutput(outputId='ML.raw.exp') %>% shinycssloaders::withSpinner()), #dataTableOutput #ML.user.exp
                                                                          shiny::tabPanel(title="Condition table", DT::dataTableOutput(outputId='ML.raw.cond') %>% shinycssloaders::withSpinner()) #dataTableOutput #ML.user.exp.raw
                                                              )##  tabsetPanel ML_user_raw_table_tab 
                                                              ),
                                               htmltools::br(),
                                               htmltools::div(id='ML_data_warning_div',style='display: none;',
                                                              htmltools::div(
                                                                style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                                shiny::htmlOutput("ML.Check.SE")
                                                              ), #div
                                                              shiny::helpText(tags$p(shiny::icon("check"),": Successfully uploaded.", style="font-size: 16px;", htmltools::HTML('&nbsp;'), shiny::icon("times"), ": Error happaned. Please check your dataset.", htmltools::HTML('&nbsp;'), shiny::icon("exclamation"), ": Warning message.", style="font-size: 16px;")),
                                                              htmltools::br()
                                               ),
                                               htmltools::div(id='ML_data_processing_div',
                                                              style='display: none;background: #ecf0f1;border: 1px solid transparent;border-radius: 4px;height: 350px;',
                                                              shiny::column(width=12,
                                                                            htmltools::h3('Data processing')%>%
                                                                              shinyhelper::helper(type="inline", title="Data processing", size ="l",
                                                                                                  content=c('<h4 style="font-size: 18px; text-align:left; line-height: 25px; color:darkblue;">
                                                                                                             <ul>
                                                                                                             <li>For detailed descriptions of missing values, sample normalization, and data transformation methods, please refer to the <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ13" target="_blank" style="color: red;">FAQ</a>.</li>
                                                                                                             </ul>
                                                                                                             </h4>
                                                                                                            <ol style="font-size: 0px;">
                                                                                                            <li style="font-size: 16px;">If you clicking on ‘Remove features with many missing values’, the threshold of the percentage of blank in the dataset that will be deleted can be defined by users.</li>
                                                                                                            <img src="Description/Data processing_1.webp" loading="lazy" />
                                                                                                            <li style="font-size: 16px;">The ‘Missing values imputation’ is for users to choose minimum, mean, and median to replace the missing values in the dataset.
                                                                                                            If users select minimum, minimum will be multiplied by the value of user inputted. After uploading data, three datasets will show on the right-hand side.
                                                                                                            When finishing checking data, click ‘Start’ for further analysis.</li>
                                                                                                            <img src="Description/Data processing_2.webp" loading="lazy" />
                                                                                                            <li style="font-size: 16px;">A variety of data transformation and normalization methods are available for selection. Use the drop-down menu to choose your preferred method.</li>
                                                                                                            <img src="Description/Data processing_3.webp" loading="lazy" />
                                                                                                            </ol>'))
                                                              ), ## column 12
                                                              column(width=12,
                                                                     shiny::column(width=4,
                                                                                   shiny::checkboxInput(inputId="ML_rm_NA", label="Remove features with many missing values", value=TRUE), #checkboxInput #ML_rm_NA
                                                                                   shiny::conditionalPanel(condition='input.ML_rm_NA',
                                                                                                           shiny::numericInput(inputId='ML_filtration_param', label='More than % missing values',
                                                                                                                               value=70, min=5, max=100, step=5)
                                                                                   ) #conditionalPanel
                                                                     ),## column 4
                                                                     shiny::column(width=4,
                                                                                   shiny::selectInput(inputId='ML_fill_NA', label='Fill missing value with:',
                                                                                                      choices=c('Mean'='mean', 'Median'='median', 'Minimum'='min',
                                                                                                                'Quantile regression imputation of left-censored data'='QRILC',
                                                                                                                'Singular value decomposition'='SVD',
                                                                                                                'K Nearest Neighbors'='KNN',
                                                                                                                'International Risk Management Institute'='IRMI',
                                                                                                                'Probabilistic principal component analysis'='PPCA',
                                                                                                                'Bayesian. Principal Component Analysis'='BPCA'),
                                                                                                      selected='min', multiple=FALSE), ## selectInput ML_fill_NA
                                                                                   shiny::conditionalPanel(condition='input.ML_fill_NA == "min"',
                                                                                                           shiny::numericInput(inputId='ML_fill_min', label='Multiply by minimum',
                                                                                                                               value=0.5, min=0.1, max=0.5, step=0.1)
                                                                                   ), ## conditionalPanel (If the user chooses fill missing value with Minimum)
                                                                                   shiny::conditionalPanel(condition='input.ML_fill_NA == "QRILC"',
                                                                                                           shiny::numericInput(inputId='ML_fill_QRILC', label='Tune sigma',
                                                                                                                               value=1, min=0.1, max=1, step=0.1)
                                                                                   ), ## conditionalPanel (If the user chooses fill missing value with QRILC)
                                                                                   shiny::conditionalPanel(condition="['SVD', 'PPCA', 'BPCA'].includes(input.ML_fill_NA)",
                                                                                                           shiny::numericInput(inputId='ML_fill_param', label='nPCs',
                                                                                                                               value=3, min=1, max=10, step=1)
                                                                                   ), ## conditionalPanel (If the user chooses fill missing value with SVD/PPCA/BPCA)
                                                                                   shiny::conditionalPanel(condition='input.ML_fill_NA == "KNN"',
                                                                                                           shiny::numericInput(inputId='ML_fill_KNN', label='The number of neighbors',
                                                                                                                               value=3, min=1, max=10, step=1)
                                                                                   ) ## conditionalPanel (If the user chooses fill missing value with KNN)
                                                                     ),## column 4
                                                                     shiny::column(width=4,
                                                                                   htmltools::h4('Data Normalization'),
                                                                                   shiny::selectInput(inputId='ML_normalization', label='Normalization with:', 
                                                                                                      choices=c('None'='none', 
                                                                                                                'Percentage'='Percentage',
                                                                                                                'Probabilistic Quotient Normalization'='PQN',
                                                                                                                'Quantile normalization'='Quantile',
                                                                                                                'Normalization by sum'='Sum',
                                                                                                                'Normalization by median'='Median'),
                                                                                                      selected='Percentage', multiple=FALSE),
                                                                                   htmltools::h4('Data Transformation'),
                                                                                   shiny::selectInput(inputId='ML_transformation',
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
                                                                            shiny::column(width=3, shiny::actionButton(inputId='ML_processing_reset', label='Reset processing method', icon=shiny::icon('redo'))), #actionButton #CORR_user_reset
                                                                            shiny::column(width=2, shiny::actionButton(inputId='ML_processing_start', label='Processing', icon=shiny::icon('play'))) #actionButton #CORR_user_upload
                                                              ) ## column 12
                                               ), ## div # ML_data_processing_div
                                               htmltools::br(),
                                               htmltools::div(id='ML_data_summary_div',style="display: none;text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                              shiny::htmlOutput("ML.data.summary")
                                               ),
                                               htmltools::br(),
                                               htmltools::div(id='ML_data_process_table_div',style='display: none;',
                                                              shiny::column(width=12,
                                                                            htmltools::h3(htmltools::strong('Processed data')),
                                                                            htmltools::div(style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                                                           htmltools::HTML('<h4 style="font-size: 18px; text-align:left; line-height: 30px; color:black;">
                                                                                                            <ul>
                                                                                                            <li><strong>Processed abundance data</strong>: User-uploaded abundance data after data processing.</li>
                                                                                                            <li><strong>Condition table</strong>: User-uploaded condition table.</li>
                                                                                                            <li><strong>Lipid characteristics</strong>: Lipid characteristics converted according to the uploaded lipids in the abundance data. Detailed information about the converted characteristics can be found in the <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ11" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                            <li><strong>Lipid id</strong>: Links to the LION ID, LIPID MAPS ID, and other resource IDs for the uploaded lipids.</li>
                                                                                                            <li><strong>Data quality</strong>: Box and density plots of the abundance data before and after data processing.</li>
                                                                                                           </ul></h4>')
                                                                            ),## div 
                                                                            htmltools::br(),
                                                                            tabsetPanel(id='ML_user_process_table_tab',
                                                                                        shiny::tabPanel(title="Processed abundance data", DT::dataTableOutput(outputId='ML.processed.abundance') %>% shinycssloaders::withSpinner()), #dataTableOutput #ML.user.exp.raw
                                                                                        shiny::tabPanel(title="Condition table", DT::dataTableOutput(outputId='ML.processed.cond') %>% shinycssloaders::withSpinner()), #dataTableOutput #ML.user.exp
                                                                                        shiny::tabPanel(title="Lipid characteristics", DT::dataTableOutput(outputId='ML.processed.lipid.char') %>% shinycssloaders::withSpinner()), #dataTableOutput #ML.user.exp
                                                                                        shiny::tabPanel(title="Lipid id", DT::dataTableOutput(outputId='ML.lipid.id') %>% shinycssloaders::withSpinner()), #dataTableOutput #ML.user.exp
                                                                                        shiny::tabPanel(title="Data quality", style='height:900px;',
                                                                                                        shiny::column(width=12,htmltools::br()),
                                                                                                        shiny::column(width=12,
                                                                                                                      shiny::column(width=5),
                                                                                                                      shiny::column(width=2, style='padding: 0px;',
                                                                                                                                    shiny::actionButton("ML.processed.download.start", "Download PDF", icon=shiny::icon("download"))),
                                                                                                                      shiny::column(width=5,downloadButton("ML.processed.download", "Download", style="visibility: hidden;"))
                                                                                                        ), ## column 12
                                                                                                        shiny::column(width=12,htmltools::br()),
                                                                                                        shiny::column(width=6, plotly::plotlyOutput(outputId='ML.before.processed.boxplot') %>% shinycssloaders::withSpinner()),
                                                                                                        shiny::column(width=6, plotly::plotlyOutput(outputId='ML.after.processed.boxplot') %>% shinycssloaders::withSpinner()),
                                                                                                        shiny::column(width=12,htmltools::br()),
                                                                                                        shiny::column(width=6, plotly::plotlyOutput(outputId='ML.before.processed.density') %>% shinycssloaders::withSpinner()),
                                                                                                        shiny::column(width=6, plotly::plotlyOutput(outputId='ML.after.processed.density') %>% shinycssloaders::withSpinner())
                                                                                        )## tabPanel # Data quality
                                                                            ) #tabsetPanel
                                                              ) ## column 12
                                               ), ## div # ML_data_process_table_div
                                               shiny::column(width=12,
                                                             htmltools::div(id='ML_data_control_panel_div',
                                                                            style="display: none;text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:330px",
                                                                            htmltools::div(id='ML_reset2_div',
                                                                                           shiny::column(width=6,
                                                                                                         shiny::selectInput(inputId='ML_feature_rank_method', label='Feature selection method:',
                                                                                                                            choices=c('p-value'='p_value',
                                                                                                                                      'pvalue_FC'='pvalue_FC',
                                                                                                                                      'ROC'='ROC',
                                                                                                                                      'Random Forest'='Random_forest',
                                                                                                                                      'Linear SVM'='SVM',
                                                                                                                                      'Lasso'='Lasso',
                                                                                                                                      'Ridge'='Ridge',
                                                                                                                                      'ElasticNet'='ElasticNet'),
                                                                                                                            selected='Random_forest', multiple=FALSE),
                                                                                                         shiny::selectInput(inputId='ML_classification_method', label='Classifier:',
                                                                                                                            choices=c('Random Forest'='Random_forest',
                                                                                                                                      'Linear SVM'='SVM',
                                                                                                                                      'Lasso'='Lasso',
                                                                                                                                      'Ridge'='Ridge',
                                                                                                                                      'ElasticNet'='ElasticNet',
                                                                                                                                      'XGBoost'='xgboost'),
                                                                                                                            selected='Random_forest', multiple=FALSE),
                                                                                                         shiny::conditionalPanel(condition='input.ML_feature_rank_method == "ElasticNet" |  input.ML_classification_method == "ElasticNet"',
                                                                                                                                 shiny::numericInput(inputId='ML_alpha', label='alpha:',
                                                                                                                                                     value=0.5, min=0, max=1)
                                                                                                         )#conditionalPanel
                                                                                           ), ## column 6
                                                                                           shiny::column(width=6,
                                                                                                         shiny::selectInput(inputId='ML_cross_vali_time', label='Cross validation times:',
                                                                                                                            choices=c(5, 10, 15, 20, 25, 30),
                                                                                                                            selected=5, multiple=FALSE),
                                                                                                         shiny::selectInput(inputId='ML_split_for_test', label='Test data propotion:',
                                                                                                                            choices=c('1/5'=0.2,
                                                                                                                                      '1/4'=0.25,
                                                                                                                                      '1/3'=0.33,
                                                                                                                                      '1/2'=0.5),
                                                                                                                            selected=0.25, multiple=FALSE),
                                                                                                         shiny::uiOutput("ML.add.var")
                                                                                           ) ## column 6
                                                                            ), ## div # ML_reset2_div
                                                                            shiny::column(width=9), #column
                                                                            shiny::column(width=3,
                                                                                          shiny::actionButton(inputId='ML_reset2', label='Reset', icon=shiny::icon('redo')),
                                                                                          shiny::actionButton(inputId='ML_start', label='Start!', icon=shiny::icon('play')) #actionButton #ML_user_start
                                                                            ) #column
                                                             ) #div #ML_data_control_panel_div'
                                               ) ## column 12
                                ),## div # data_check_successful
                                shiny::column(width=12,htmltools::hr()),
                                ###########################
                                ####  ML analysis tab  ####
                                ###########################
                                shiny::column(width=12,
                                              htmltools::div(id='ML_result_div',style='display: none;',
                                                             htmltools::h2('Result'),
                                                             shiny::tabsetPanel(id='ML_analysis_tab',
                                                                                ########################
                                                                                ####  ROC/PR curve  ####
                                                                                ########################
                                                                                shiny::tabPanel(title='ROC/PR curve',
                                                                                                htmltools::h3('ROC/PR curve'),
                                                                                                htmltools::h6("The ROC and Precision-Recall (PR) curve are very common methods to evaluate the diagnostic ability of a binary classifier.
                                                                                                       Mean AUC and 95% confidence interval for the ROC and PR curve are calculated from all CV runs in each feature number.
                                                                                                       Theoretically, the higher the AUC, the better the model performs. PR curve is more sensitive to data with highly skewed datasets and offers a more informative view of an algorithm's performance.
                                                                                                       An AUC equal to 1 both represents perfect performance in two methods.
                                                                                                       We provide an overall ROC/PR Curve shown curve of CVs with different feature numbers and a ROC/PR Curve shown curve of average CVs by user-selected feature number.",
                                                                                                              style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px"),
                                                                                                htmltools::br(),
                                                                                                shiny::column(width=12,
                                                                                                              shiny::column(width=4),
                                                                                                              shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("ML.ROC.PR.all.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                              shiny::column(width=4, shiny::downloadButton("ML.ROC.PR.all.download", "Download", style="visibility:hidden;"))
                                                                                                              ), ## column 12
                                                                                                shiny::column(width=12,
                                                                                                              shiny::column(width=6,
                                                                                                                            htmltools::h3('ROC curve plot',style='text-align: center;'),
                                                                                                                            plotly::plotlyOutput(outputId='ML.ROC.all', height='360px') %>% shinycssloaders::withSpinner(),
                                                                                                                            htmltools::br()
                                                                                                              ), ## column 6
                                                                                                              shiny::column(width=6,
                                                                                                                            htmltools::h3('PR curve plot',style='text-align: center;'),
                                                                                                                            plotly::plotlyOutput(outputId='ML.PR.all', height='360px') %>% shinycssloaders::withSpinner(),
                                                                                                                            htmltools::br()
                                                                                                              ) ## column 6
                                                                                                ), ## column 12
                                                                                                shiny::column(width=12,
                                                                                                              shiny::column(width=4),
                                                                                                              shiny::column(width=8,
                                                                                                                            shiny::selectInput(inputId='ML_ROC_PR_feature_number', label='Feature number:',
                                                                                                                                               choices=c(2, 3, 5, 10, 20, 50),
                                                                                                                                               selected=10, multiple=FALSE))
                                                                                                ), ## column 12
                                                                                                shiny::column(width=12,
                                                                                                              shiny::column(width=4),
                                                                                                              shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("ML.ROC.PR.feature.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                              shiny::column(width=4, shiny::downloadButton("ML.ROC.PR.feature.download", "Download", style="visibility:hidden;"))
                                                                                                ), ## column 12
                                                                                                shiny::column(width=12,
                                                                                                              shiny::column(width=5,
                                                                                                                            htmltools::br(),
                                                                                                                            htmltools::h3('Average ROC curve plot of N features',style='text-align: center;'),
                                                                                                                            plotly::plotlyOutput(outputId='ML.ROC', height='360px') %>% shinycssloaders::withSpinner(),
                                                                                                                            htmltools::br()), ## column 5
                                                                                                              shiny::column(width=1),
                                                                                                              shiny::column(width=5,
                                                                                                                            htmltools::br(),
                                                                                                                            htmltools::h3('Average PR curve plot of N features',style='text-align: center;'),
                                                                                                                            plotly::plotlyOutput(outputId='ML.PR', height='360px') %>% shinycssloaders::withSpinner(),
                                                                                                                            htmltools::br()), ## column 5
                                                                                                              shiny::column(width=1)
                                                                                                ) ## column 12
                                                                                ), ## tabPanel ROC/PR curve
                                                                                ##############################
                                                                                ####  Model predictivity  ####
                                                                                ##############################
                                                                                shiny::tabPanel(title='Model performance',
                                                                                                htmltools::h3('Model performance'),
                                                                                                htmltools::h6(p("In this part, many useful indicators are provided for users to evaluate model performance. For each feature number, we calculate and plot the average value and 95% confidence interval of accuracy,
                                                                                                         sensitivity (recall), specificity, positive predictive value (precision), negative predictive value, F1 score, prevalence, detection rate, detection prevalence, balanced accuracy in all CV runs with confusion Matrix function in carat package.
                                                                                                         All these indicators can be described in terms of true positive (TP), false positive (FP), false negative (FN) and true negative (TN) and are summarized as follows.",
                                                                                                                style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                htmltools::br(),
                                                                                                shiny::column(width=12,
                                                                                                              shiny::column(width=4),
                                                                                                              shiny::column(width=8,
                                                                                                                            shiny::selectInput(inputId='ML_evaluation_method', label='Evaluation method:',
                                                                                                                                               choices=c('Accuracy'='Accuracy',
                                                                                                                                                         'Sensitivity'='Sensitivity',
                                                                                                                                                         'Specificity'='Specificity',
                                                                                                                                                         'Positive Predictive Value'='Pos Pred Value',
                                                                                                                                                         'Negative Predictive Value'='Neg Pred Value',
                                                                                                                                                         'Precision'='Precision',
                                                                                                                                                         'Recall'='Recall',
                                                                                                                                                         'F1'='F1',
                                                                                                                                                         'Prevalence'='Prevalence',
                                                                                                                                                         'Detection Rate'='Detection Rate',
                                                                                                                                                         'Detection Prevalence'='Detection Prevalence',
                                                                                                                                                         'Balanced Accuracy'='Balanced Accuracy'),
                                                                                                                                               selected='Accuracy',multiple=FALSE))
                                                                                                ), ## column 12
                                                                                                shiny::column(width=12,
                                                                                                              shiny::column(width=4),
                                                                                                              shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("ML.evalution.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                              shiny::column(width=4, shiny::downloadButton("ML.evalution.download", "Download", style="visibility:hidden;"))
                                                                                                ), ## column 12
                                                                                                shiny::column(width=12,
                                                                                                              shiny::column(width=6,
                                                                                                                            htmltools::h3('Model performance plot',style='text-align: center;'),
                                                                                                                            plotly::plotlyOutput(outputId='ML.evalution.plot', height='100%') %>% shinycssloaders::withSpinner()),
                                                                                                              shiny::column(width=6,
                                                                                                                            htmltools::h3('Table of model evaluation information',style='text-align: center;'),
                                                                                                                            DT::dataTableOutput(outputId='ML.evalution.table') %>% shinycssloaders::withSpinner())
                                                                                                ) ## column 12
                                                                                ), ## tabPanel # Model performance
                                                                                ##############################
                                                                                ####  Sample probability  ####
                                                                                ##############################
                                                                                shiny::tabPanel(title='Predicted probability',
                                                                                                htmltools::h3('Predicted probability'),
                                                                                                htmltools::h6(p("This page shows the average predicted probabilities of each sample in testing data from all CV runs and allows users to explore those incorrect or uncertain labels.
                                                                                                         We show the distribution of predicted probabilities in two reference labels on the left panel while a confusion matrix composed of sample number and proportion is laid out on the right.
                                                                                                         Results for different feature number can be selected manually by users.",
                                                                                                                style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                htmltools::br(),
                                                                                                shiny::column(width=12,
                                                                                                              shiny::column(width=4),
                                                                                                              shiny::column(width=8,
                                                                                                                            shiny::selectInput(inputId='ML_sam_prob_feature_number', label='Feature number:',
                                                                                                                                               choices=c(2, 3, 5, 10, 20, 50),
                                                                                                                                               selected=10, multiple=FALSE))
                                                                                                ),
                                                                                                shiny::column(width=12,
                                                                                                              shiny::column(width=4),
                                                                                                              shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("ML.probability.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                              shiny::column(width=4, shiny::downloadButton("ML.probability.download", "Download", style="visibility:hidden;"))
                                                                                                ), ## column 12
                                                                                                shiny::column(width=12,
                                                                                                              shiny::column(width=6,
                                                                                                                            htmltools::br(),
                                                                                                                            htmltools::h3('Predicted probability distribution',style='text-align: center;'),
                                                                                                                            plotly::plotlyOutput(outputId='ML.probability.plot', height='100%') %>% shinycssloaders::withSpinner(),
                                                                                                                            htmltools::br()),
                                                                                                              shiny::column(width=6,
                                                                                                                            htmltools::br(),
                                                                                                                            htmltools::h3('Confusion matrix of sample number and proportion',style='text-align: center;'),
                                                                                                                            shiny::plotOutput(outputId='ML.cm.plot') %>% shinycssloaders::withSpinner(),
                                                                                                                            htmltools::br())
                                                                                                ), ## column 12
                                                                                                
                                                                                                shiny::column(width=12,
                                                                                                              htmltools::br(),
                                                                                                              htmltools::h3('Table of predicted probability and labels',style='text-align: center;'),
                                                                                                              DT::dataTableOutput(outputId='ML.probability.table') %>% shinycssloaders::withSpinner(),
                                                                                                              htmltools::br())
                                                                                ), ## tabPanel Predicted probability
                                                                                ##############################
                                                                                ####  Feature importance  ####
                                                                                ##############################
                                                                                shiny::tabPanel(title='Feature importance',
                                                                                                htmltools::h3('Feature importance'),
                                                                                                htmltools::br(),
                                                                                                shiny::navlistPanel(widths=c(2, 10),
                                                                                                                    id='ML_fea_impo_list',
                                                                                                                    ###########################
                                                                                                                    ####  Algorithm-based  ####
                                                                                                                    ###########################
                                                                                                                    shiny::tabPanel(title='Algorithm-based',
                                                                                                                                    htmltools::h3('Algorithm-based'),
                                                                                                                                    htmltools::div(style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px",
                                                                                                                                                   htmltools::h6(p("After building a high-accuracy model, users are encouraged to explore the contribution of each feature on this page.
                                                                                                                                                            Two methods here namely ‘Algorithm-based’ and ‘SHAP analysis’ can rank and visualize the feature importance.",style="text-align: left")),
                                                                                                                                                   htmltools::h6(p("In ‘Algorithm-based’ part, when users choose a certain feature number, the selected frequency and the average feature importance of top 10 features from all CV runs will be displayed.
                                                                                                                                                                            For a Linear SVM, Lasso, Ridge or ElasticNet model, the importance of each feature depends on the absolute value of their coefficients in the algorithm,
                                                                                                                                                                            while Random Forest and XGBoost use built-in feature importance results. ",
                                                                                                                                                                   style="text-align: left")),
                                                                                                                                                   
                                                                                                                                    ),
                                                                                                                                    htmltools::br()
                                                                                                                    ), #tabPanel
                                                                                                                    #########################
                                                                                                                    ####  SHAP analysis  ####
                                                                                                                    #########################
                                                                                                                    shiny::tabPanel(title='SHAP analysis',
                                                                                                                                    htmltools::h3('SHAP analysis'),
                                                                                                                                    htmltools::h6(p("After building a high-accuracy model, users are encouraged to explore the contribution of each feature on this page.
                                                                                          Two methods here namely ‘Algorithm-based’ and ‘SHAP analysis’ can rank and visualize the feature importance.
                                                                                          SHapley Additive exPlanations (SHAP) approach on the basis of Shapley values in game theory has recently been introduced to explain individual predictions of any machine learning model.",
                                                                                                                                                    style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                                                    htmltools::br(),
                                                                                                                                    shiny::column(width=12,
                                                                                                                                                  htmltools::div(id='ML_SHAP_control_panel_div',
                                                                                                                                                                 htmltools::div(id='ML_SHAP_reset_div',
                                                                                                                                                                                shiny::column(width=4,
                                                                                                                                                                                              shiny::selectInput(inputId='ML_SHAP_feature_number',
                                                                                                                                                                                                                 label='Feature number:',
                                                                                                                                                                                                                 choices=c(2, 3, 5, 10, 20, 50),
                                                                                                                                                                                                                 selected=10,
                                                                                                                                                                                                                 multiple=F)
                                                                                                                                                                                ),
                                                                                                                                                                                shiny::column(width=4,
                                                                                                                                                                                              shiny::numericInput(inputId='ML_SHAP_n_sim',
                                                                                                                                                                                                                  label='Simulation times:',
                                                                                                                                                                                                                  value=10,
                                                                                                                                                                                                                  min=1,
                                                                                                                                                                                                                  max=100,
                                                                                                                                                                                                                  step=1) %>%
                                                                                                                                                                                                shinyhelper::helper(type="inline",
                                                                                                                                                                                                                    title="Simulation times",
                                                                                                                                                                                                                    content=c("Except XGBoost, Monte Carlo repetitions are conducted to estimate each Shapley value and the simulation times are defined by the parameter ‘Simulation times’."))
                                                                                                                                                                                )
                                                                                                                                                                 ), #div #ML_SHAP_reset_div
                                                                                                                                                                 shiny::column(width=4,
                                                                                                                                                                               shiny::actionButton(inputId='ML_SHAP_reset', label='Reset', icon=shiny::icon('redo')),
                                                                                                                                                                               shiny::actionButton(inputId='ML_SHAP_start', label='Start!', icon=shiny::icon('play'))
                                                                                                                                                                 ),
                                                                                                                                                                 style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:100px"
                                                                                                                                                  ), #div #ML_SHAP_control_panel_div
                                                                                                                                                  shiny::column(width=12, htmltools::br())
                                                                                                                                    ) #column
                                                                                                                    ) #tabPanel
                                                                                                ), #navlistPanel
                                                                                                htmltools::div(id='ML_algorithm_feature_result_div',
                                                                                                               shiny::column(width=4),
                                                                                                               shiny::column(width=8,
                                                                                                                             shiny::selectInput(inputId='ML_algorithm_feature_number',
                                                                                                                                                label='Feature number:',
                                                                                                                                                choices=c(2, 3, 5, 10, 20, 50),
                                                                                                                                                selected=10, multiple=FALSE)
                                                                                                               ),
                                                                                                               shiny::column(width=12,
                                                                                                                             shiny::column(width=4),
                                                                                                                             shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("ML.fea.freq.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                             shiny::column(width=4, shiny::downloadButton("ML.fea.freq.download", "Download", style="visibility:hidden;"))
                                                                                                               ), ## column 12
                                                                                                               shiny::column(width=6,
                                                                                                                             htmltools::br(),
                                                                                                                             htmltools::h3('Selected frequency plot',style='text-align: center;'),
                                                                                                                             plotly::plotlyOutput(outputId='ML.fea.freq.plot', height='100%') %>% shinycssloaders::withSpinner(),
                                                                                                                             htmltools::br(),
                                                                                                                             htmltools::h3('Table of the selected frequency',style='text-align: center;'),
                                                                                                                             DT::dataTableOutput(outputId='ML.fea.freq.table') %>% shinycssloaders::withSpinner(),
                                                                                                                             htmltools::br()
                                                                                                               ),
                                                                                                               shiny::column(width=6,
                                                                                                                             htmltools::br(),
                                                                                                                             htmltools::h3('Feature importance plot',style='text-align: center;'),
                                                                                                                             plotly::plotlyOutput(outputId='ML.fea.impo.plot', height='100%') %>% shinycssloaders::withSpinner(),
                                                                                                                             htmltools::br(),
                                                                                                                             htmltools::h3('Table of feature importance',style='text-align: center;'),
                                                                                                                             DT::dataTableOutput(outputId='ML.fea.impo.table') %>% shinycssloaders::withSpinner(),
                                                                                                                             htmltools::br()
                                                                                                               )
                                                                                                ), #div ML_algorithm_feature_result_div
                                                                                                htmltools::div(id='ML_SHAP_feature_result_div',
                                                                                                               shiny::conditionalPanel(condition='input.ML_SHAP_start',
                                                                                                                                       shiny::column(width=12,
                                                                                                                                                     shiny::column(width=4),
                                                                                                                                                     shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("ML.shap.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                                     shiny::column(width=4, shiny::downloadButton("ML.shap.download", "Download", style="visibility:hidden;"))
                                                                                                                                       ), ## column 12
                                                                                                                                       shiny::column(width=5,
                                                                                                                                                     htmltools::h3('SHAP feature importance plot',style='text-align: center;'),
                                                                                                                                                     plotly::plotlyOutput(outputId='ML.mean.shapley.plot', height='100%') %>% shinycssloaders::withSpinner()
                                                                                                                                       ),
                                                                                                                                       shiny::column(width=7,
                                                                                                                                                     htmltools::h3('SHAP summary plot',style='text-align: center;'),
                                                                                                                                                     plotly::plotlyOutput(outputId='ML.all.shapley.plot', height='100%') %>% shinycssloaders::withSpinner()
                                                                                                                                       ),
                                                                                                                                       shiny::column(width=12,
                                                                                                                                                     htmltools::br(),
                                                                                                                                                     htmltools::h3('Table of SHAP analysis',style='text-align: center;'),
                                                                                                                                                     DT::dataTableOutput(outputId='ML.shap.long.table') %>% shinycssloaders::withSpinner(),
                                                                                                                                                     htmltools::br(),
                                                                                                                                                     htmltools::div(id='ML_SHAP_forceplot_control_panel_div',
                                                                                                                                                                    htmltools::div(id='ML_SHAP_forceplot_reset_div',
                                                                                                                                                                                   shiny::column(width=4,
                                                                                                                                                                                                 shiny::selectInput(inputId='ML_SHAP_forceplot_top_n_feature',
                                                                                                                                                                                                                    label='Show top N feature:',
                                                                                                                                                                                                                    choices=c(1:10),
                                                                                                                                                                                                                    selected=5,
                                                                                                                                                                                                                    multiple=F,
                                                                                                                                                                                                                    width='100%')
                                                                                                                                                                                   ),
                                                                                                                                                                                   shiny::column(width=4,
                                                                                                                                                                                                 shiny::selectInput(inputId='ML_SHAP_forceplot_group_number',
                                                                                                                                                                                                                    label='Number of group:',
                                                                                                                                                                                                                    choices=c(1:10),
                                                                                                                                                                                                                    selected=2,
                                                                                                                                                                                                                    multiple=F,
                                                                                                                                                                                                                    width='100%')
                                                                                                                                                                                   )
                                                                                                                                                                    ), #div #ML_SHAP_forceplot_reset_div
                                                                                                                                                                    shiny::column(width=4,
                                                                                                                                                                                  shiny::actionButton(inputId='ML_SHAP_forceplot_reset', label='Reset', icon=shiny::icon('redo')),
                                                                                                                                                                                  shiny::actionButton(inputId='ML_SHAP_forceplot_start', label='Start!', icon=shiny::icon('play'))
                                                                                                                                                                    ),
                                                                                                                                                                    shiny::column(width=12,
                                                                                                                                                                                  shiny::helpText('‘Show top N feature’ can be chosen for SHAP force plot.
                                                                                                                                     The samples are clustered into multiple groups (‘Number of group’) based on Shapley values using ward.D method. ')),
                                                                                                                                                                    style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:150px"
                                                                                                                                                     ), #div #ML_SHAP_forceplot_control_panel_div
                                                                                                                                                     shiny::conditionalPanel(condition='input.ML_SHAP_forceplot_start',
                                                                                                                                                                             htmltools::br(),
                                                                                                                                                                             shiny::column(width=12,
                                                                                                                                                                                           shiny::column(width=4),
                                                                                                                                                                                           shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("ML.SHAP.force.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                                                                           shiny::column(width=4, shiny::downloadButton("ML.SHAP.force.download", "Download", style="visibility:hidden;"))
                                                                                                                                                                             ), ## column 12
                                                                                                                                                                             htmltools::br(),
                                                                                                                                                                             htmltools::h3('SHAP force plot of top N features',style='text-align: center;'),
                                                                                                                                                                             plotly::plotlyOutput(outputId='ML.SHAP.forceplot', height='100%') %>% shinycssloaders::withSpinner(),
                                                                                                                                                                             htmltools::br(),
                                                                                                                                                                             htmltools::h3('Table of force plot information',style='text-align: center;'),
                                                                                                                                                                             DT::dataTableOutput(outputId='ML.SHAP.forceplot.table') %>% shinycssloaders::withSpinner(),
                                                                                                                                                                             htmltools::br()
                                                                                                                                                     ), #conditionalPanel
                                                                                                                                                     htmltools::br(),
                                                                                                                                                     htmltools::div(id='ML_SHAP_dependence_control_panel_div',
                                                                                                                                                                    htmltools::div(id='ML_SHAP_dependence_reset_div',
                                                                                                                                                                                   shiny::column(width=3,
                                                                                                                                                                                                 shiny::selectInput(inputId='ML_SHAP_dependence_x',
                                                                                                                                                                                                                    label='X axis:',
                                                                                                                                                                                                                    choices=c(2, 3, 5, 10, 20, 50, 100),
                                                                                                                                                                                                                    selected=10, multiple=FALSE, selectize=TRUE, width='100%')
                                                                                                                                                                                   ),
                                                                                                                                                                                   shiny::column(width=3,
                                                                                                                                                                                                 shiny::selectInput(inputId='ML_SHAP_dependence_y',
                                                                                                                                                                                                                    label='Y axis:',
                                                                                                                                                                                                                    choices=c(2, 3, 5, 10, 20, 50, 100),
                                                                                                                                                                                                                    selected=10, multiple=FALSE, selectize=TRUE, width='100%')
                                                                                                                                                                                   ),
                                                                                                                                                                                   shiny::column(width=3,
                                                                                                                                                                                                 shiny::selectInput(inputId='ML_SHAP_dependence_color',
                                                                                                                                                                                                                    label='Color:',
                                                                                                                                                                                                                    choices=c(2, 3, 5, 10, 20, 50, 100),
                                                                                                                                                                                                                    selected=10, multiple=FALSE, selectize=TRUE, width='100%')
                                                                                                                                                                                   )
                                                                                                                                                                    ), #div #ML_SHAP_dependence_reset_div
                                                                                                                                                                    shiny::column(width=3,
                                                                                                                                                                                  shiny::actionButton(inputId='ML_SHAP_dependence_reset', label='Reset', icon=shiny::icon('redo')),
                                                                                                                                                                                  shiny::actionButton(inputId='ML_SHAP_dependence_start', label='Start!', icon=shiny::icon('play'))),
                                                                                                                                                                    shiny::column(width=12,
                                                                                                                                                                                  shiny::helpText('The x-axis represents the value of a certain feature while the y-axis is the corresponding Shapley value.
                                                                                                                                     The colour parameter can be assigned to check if a second feature has an interaction effect with the feature we are plotting.')),
                                                                                                                                                                    style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:150px"
                                                                                                                                                     ), #div #ML_SHAP_dependence_control_panel_div
                                                                                                                                                     shiny::conditionalPanel(condition='input.ML_SHAP_dependence_start',
                                                                                                                                                                             shiny::column(width=12,htmltools::br()),
                                                                                                                                                                             shiny::column(width=12,
                                                                                                                                                                                           shiny::column(width=4),
                                                                                                                                                                                           shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("ML.SHAP.dependence.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                                                                           shiny::column(width=4, shiny::downloadButton("ML.SHAP.dependence.download", "Download", style="visibility:hidden;"))
                                                                                                                                                                             ), ## column 12
                                                                                                                                                                             shiny::column(width=12, htmltools::br()),
                                                                                                                                                                             shiny::column(width=12, htmltools::h3('SHAP dependence plot',style='text-align: center;')),
                                                                                                                                                                             shiny::column(width=12, plotly::plotlyOutput(outputId='ML.SHAP.dependence.plot', height='100%') %>% shinycssloaders::withSpinner()),
                                                                                                                                                                             shiny::column(width=12,htmltools::br())
                                                                                                                                                     ) #conditionalPanel
                                                                                                                                       ) #column
                                                                                                               ) #conditionalPanel
                                                                                                ) #div ML_SHAP_feature_result_div
                                                                                ), #tabPanel
                                                                                ###################
                                                                                ####  Network  ####
                                                                                ###################
                                                                                shiny::tabPanel(title='Network',
                                                                                                htmltools::h3('Network'),
                                                                                                htmltools::h6(p("Correlation network helps users interrogate the interaction of features in a machine learning model. In this section,
                                                                        users can choose an appropriate feature number according to previous cross-validation results and the features in the best model
                                                                        (based on ROC-AUC+PR-AUC) will be picked up to compute the correlation coefficients between each other. ",
                                                                                                                style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                htmltools::br(),
                                                                                                shiny::column(width=12,
                                                                                                              htmltools::div(id='ML_network_control_panel_div',
                                                                                                                             htmltools::div(id='ML_network_reset_div',
                                                                                                                                            shiny::column(width=6,
                                                                                                                                                          shiny::selectInput(inputId='ML_network_feature_number',
                                                                                                                                                                             label='Feature number:',
                                                                                                                                                                             choices=c(2, 3, 5, 10, 20, 50),
                                                                                                                                                                             selected=10,
                                                                                                                                                                             multiple=FALSE),
                                                                                                                                                          shiny::selectInput(inputId='ML_network_fea_impo_method',
                                                                                                                                                                             label='Feature importance method:',
                                                                                                                                                                             choices=c('Algorithm-based'='Algorithm-based',
                                                                                                                                                                                       'SHAP analysis'='SHAP'),
                                                                                                                                                                             selected='Algorithm-based',
                                                                                                                                                                             multiple=FALSE),
                                                                                                                                                          shiny::numericInput(inputId='ML_network_n_sim',
                                                                                                                                                                              label='Simulation times:',
                                                                                                                                                                              value=10, min=1, max=100, step=1)
                                                                                                                                            ), #column
                                                                                                                                            shiny::column(width=6,
                                                                                                                                                          shiny::selectInput(inputId='ML_network_corr_method',
                                                                                                                                                                             label='Correlation method:',
                                                                                                                                                                             choices=c('Pearson'='pearson',
                                                                                                                                                                                       'Spearman'='spearman',
                                                                                                                                                                                       'Kendall'='kendall'),
                                                                                                                                                                             selected='pearson', multiple=FALSE),
                                                                                                                                                          shiny::numericInput(inputId='ML_network_corr_coef',
                                                                                                                                                                              label='Coefficient cutoff:',
                                                                                                                                                                              value=0, min=0, max=1, step=0.1)
                                                                                                                                            ) #column
                                                                                                                             ), #div #ML_network_reset_div
                                                                                                                             shiny::column(width=6),
                                                                                                                             shiny::column(width=6,
                                                                                                                                           shiny::actionButton(inputId='ML_network_reset', label='Reset', icon=shiny::icon('redo')),
                                                                                                                                           shiny::actionButton(inputId='ML_network_start', label='Start!', icon=shiny::icon('play'))
                                                                                                                             ),
                                                                                                                             style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:260px"
                                                                                                              ), #div #ML_network_control_panel_div
                                                                                                              htmltools::br(),
                                                                                                              shiny::conditionalPanel(condition='input.ML_network_start',
                                                                                                                                      htmltools::div(id='ML_network_result_div',
                                                                                                                                                     htmltools::h3('Network of feature importance',style='text-align: center;'),
                                                                                                                                                     shiny::column(width=12,
                                                                                                                                                                   shiny::column(width=4),
                                                                                                                                                                   shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("ML.network.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                                                   shiny::column(width=4, shiny::downloadButton("ML.network.download", "Download", style="visibility:hidden;"))), ## column 12
                                                                                                                                                     shiny::column(width=12, visNetwork::visNetworkOutput(outputId='ML.network', height='600px') %>% shinycssloaders::withSpinner()),
                                                                                                                                                     shiny::column(width=5),
                                                                                                                                                     shiny::column(width=7,
                                                                                                                                                                   shiny::actionButton(inputId='ML_network_refresh',
                                                                                                                                                                                       label='Refresh',
                                                                                                                                                                                       icon=shiny::icon('sync-alt')
                                                                                                                                                                   ) #actionButton #ML_network_refresh
                                                                                                                                                     ) #column
                                                                                                                                      ) #div #ML_network_result_div
                                                                                                              ), #conditionalPanel
                                                                                                              htmltools::br(),
                                                                                                              htmltools::br()
                                                                                                ) #column
                                                                                ) #tabPanel
                                                             ) #tabsetPanel
                                              ) #div 
                                )
                  ) #column
                ) #fluidRow
) #tabPanel
