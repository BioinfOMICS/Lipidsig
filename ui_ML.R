
tabPanel(title = h4('Machine learning'),
         value = 'ML',
         #### Profiling Header Panel ####
         h1('Machine learning analysis'), 
         br(),
         fluidRow(column(width=12,
                         ###############################
                         ####  ML Page Description  ####
                         ###############################
                         div(id = 'ML_description_style_div',
                             h6('In this section, lipid species and lipid characteristics data can be combined by users to predict the binary outcome using various machine learning methods and select the best feature combination to explore further relationships.
                                For cross-validation, Monte-Carlo cross-validation (CV) is executed to evaluate the model performance and to reach statistical significance. Additionally, we provide eight feature ranking methods (p-value, p-value*FC, ROC, Random Forest,
                                Linear SVM, Lasso, Ridge, ElasticNet) and six classification methods (Random Forest, Linear SVM, Lasso, Ridge, ElasticNet, XGBoost) for users to train and select the best model.
                                Feature ranking methods can be divided into two categories: a univariate and multivariate analysis. ',style="text-align: left"),
                             h6('A series of consequent analyses assist users to evaluate the methods and visualise the results of machine learning, including ROC/PR curve, Model predictivity, Sample probability, Feature importance, and Network.',style="text-align: left"),
                             style="background-color: PowderBlue;border-left: 8px solid Teal;padding: 15px"
                             ), #div #ML_description_style_div
                         br(),
                         ##########################
                         ####  ML Data Source  ####
                         ##########################
                         h2('Data Source'),
                         sidebarLayout(fluid = T,
                                       sidebarPanel(width = 4, 
                                                    radioButtons(inputId = 'ML_data_source', 
                                                                 label = h4('Data source'), 
                                                                 choices = c('Example dataset (Nat Med. 2018)' = 'ML_demo_data', 
                                                                             'Upload your data!' = 'ML_user_data'), 
                                                                 selected = 'ML_demo_data'
                                                                 )%>% #radioButtons #ML_data_source
                                                      helper(type = "inline",
                                                             title = "Data source",
                                                             size ="l",
                                                             content = c('<ol style="font-size: 0px;">',
                                                                         '<li style="font-size: 16px;">Lipid dataset can be uploaded by users or using example datasets. This information, namely Lipid expression data, Condition table and Lipid characteristics (optional). Three tables must assign to a vector  all data needs to be uploaded in  
                                                                         <mark style="background-color: white;color: red;">CSV</mark> or <mark style="background-color: white;color: red;">TSV</mark> format. The maximum file size is 30MB.</li>',
                                                                         '<li style="font-size: 16px;">Once two files are chosen and shown ‘Upload complete’ then press ‘Upload’.</li>',
                                                                         '</ol>')),
                                                    conditionalPanel(condition = 'input.ML_data_source == "ML_demo_data"', 
                                                                     actionButton(inputId = 'ML_demo_upload', label = 'Submit', icon = icon('upload')), #actionButton #ML_demo_upload
                                                                     downloadButton(outputId = 'ML.demo.download', label = 'Download example')
                                                                     ), #conditionalPanel
                                                    conditionalPanel(condition = 'input.ML_data_source == "ML_user_data"',
                                                                     div(id = 'ML_user_reset_div', 
                                                                         fileInput(inputId = 'ML_user_exp', label = 'Lipid expression data:', accept = c(".csv",".tsv"), multiple = F) %>% #fileInput #ML_user_exp
                                                                           helper(type = "inline",
                                                                                  title = "Lipid expression data",
                                                                                  size = "l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">The first column must contain a list of unique lipids name(features). NOTE: THE FEATURE LIST OF THE FIRST COLUMN MUST SAME AS ‘Lipid characteristics’!</li>',
                                                                                              '<li style="font-size: 16px;">Other columns encompass the expressed values of groups under different conditions that you want to compare.</li>',
                                                                                              '<li style="font-size: 16px;">An example of ‘Lipid expression data’</li>',
                                                                                              '<img src="Description/ML_Lipid expression data.png" width="100%"/>',
                                                                                              '</ol>')), 
                                                                         fileInput(inputId = 'ML_user_cond', label = 'Condition table:', accept = c(".csv",".tsv"),multiple = F)%>% #fileInput #ML_user_cond
                                                                           helper(type = "inline",
                                                                                  title = "Condition table",
                                                                                  size = "l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">‘Condition table’ can only contain 2 columns: in ‘sample_name’, ‘group’ order</li>',
                                                                                              '<li style="font-size: 16px;">The first column must contain a list of samples name (features), MUST SAME AS THE SAMPLE NAME (COLUM NAMES) OF ‘Lipid expression data’!</li>',
                                                                                              '<li style="font-size: 16px;">‘group’ refers to the group of the sample (can only be 0 and 1). NOTE: THE GROUP MUST BE NUMERIC, AND SKEWED DATA ARE NOT RECOMMEND</li>',
                                                                                              '<li style="font-size: 16px;">An example of ‘Condition table’</li>',
                                                                                              '<img src="Description/ML_Condition table.png" width="100%"/>',
                                                                                              '</ol>')),
                                                                         fileInput(inputId = 'ML_user_char', label = 'Lipid characteristics (optional):', accept = c(".csv",".tsv"), multiple = F)%>% #fileInput #ML_user_char
                                                                           helper(type = "inline",
                                                                                  title = "Lipid characteristics",
                                                                                  size = "l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">The first column must contain a list of unique lipids name (features). NOTE: : THE FEATURE LIST OF THE FIRST COLUMN MUST SAME AS ‘Lipid expression data’!</li>',
                                                                                              '<li style="font-size: 16px;">Other columns can contain a wide variety of lipid characteristics, including class, total length, the total number of double bonds, or any other characteristics. The value can be the number of characters.</li>',
                                                                                              '<li style="font-size: 16px;">Due to the numbers of fatty acids attached to lipid are various, hence, if users assign the column name starting with “FA_”, system will automatically extract values separated by commas; Value between commas needs to be positive integer or zero. Please see the example listed below.</li>',
                                                                                              '<img src="Description/ML_Lipid characteristics_1.png"/>',
                                                                                              '<li style="font-size: 16px;">If the name of characteristics matches ‘class’ (whole word only), the content must be characters, while ‘totallength’, ‘totaldb’, ‘totaloh’ (whole word only) must be numeric</li>',
                                                                                              '<li style="font-size: 16px;">The values of other columns can be the number of characters. The lipid characteristic information (i.e. Lipid class, structural category, and total double bonds etc.) that can further be applied as additional variables to train a model. For example, if you choose class, and structural_category, these two variables will be calculated together with species, then, predicting a model. If no additional variable is selected, the algorism will use species only to build the model.</li>',
                                                                                              '<img src="Description/ML_Lipid characteristics_2.png" width="100%"/>',
                                                                                              '<li style="font-size: 16px;">An example of ‘Lipid characteristics’</li>',
                                                                                              '<img src="Description/ML_Lipid characteristics_3.png" width="100%"/>',
                                                                                              '</ol>')),
                                                                         helpText("Upload your data table in .csv/.tsv"), #helpText
                                                                         br(),
                                                                         h4('Data processing')%>%
                                                                           helper(type = "inline",
                                                                                  title = "Data processing",
                                                                                  size = "l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">If you clicking on ‘Remove features with many missing values’, the threshold of the percentage of blank in the dataset that will be deleted can be defined by users.</li>',
                                                                                              '<img src="Description/Data processing_1.png" />',
                                                                                              '<li style="font-size: 16px;">The ‘Missing values imputation’ is for users to choose minimum, mean, and median to replace the missing values in the dataset. If users select minimum, minimum will be multiplied by the value of user inputted. After uploading data, three datasets will show on the right-hand side. When finishing checking data, click ‘Start’ for further analysis.</li>',
                                                                                              '<img src="Description/Data processing_2.png" />',
                                                                                              '<li style="font-size: 16px;">‘Percentage transformation’, ‘Log10 transformation’ can transform data into log10 or percentage. The purpose of ‘Log10 transformation’ is to make highly skewed distributions less skewed, while ‘Percentage transformation’ is to standardize variation between groups.</li>',
                                                                                              '<img src="Description/Data processing_3.png" />',
                                                                                              '<li style="font-size: 16px;">‘Data scaling’ standardised input values (x-mean/sd) </li>',
                                                                                              '</ol>')),
                                                                         checkboxInput(inputId = "ML_rm_NA", 
                                                                                       label = "Remove features with many missing values", 
                                                                                       value = T), #checkboxInput #ML_rm_NA
                                                                         conditionalPanel(condition = 'input.ML_rm_NA', 
                                                                                          numericInput(inputId = 'ML_rm_NA_pct', 
                                                                                                       label = 'More than % missing values', 
                                                                                                       value = 50, 
                                                                                                       min = 5, 
                                                                                                       max = 100, 
                                                                                                       step = 5)
                                                                                          ), #conditionalPanel
                                                                         checkboxInput(inputId = "ML_rp_NA", 
                                                                                       label = "Missing values imputation", 
                                                                                       value = T),
                                                                         conditionalPanel(condition = 'input.ML_rp_NA',
                                                                                          selectInput(inputId = 'ML_fill_NA',
                                                                                                      label = 'Fill missing value with:',
                                                                                                      choices = c('Mean' = 'mean', 
                                                                                                                  'Median' = 'median', 
                                                                                                                  'Minimum' = 'min'), 
                                                                                                      selected = 'min', 
                                                                                                      multiple = F),
                                                                                          conditionalPanel(condition = 'input.ML_fill_NA == "min"', 
                                                                                                           numericInput(inputId = 'ML_fill_min', 
                                                                                                                        label = 'Multiply by minimum', 
                                                                                                                        value = 0.5, 
                                                                                                                        min = 0.1, 
                                                                                                                        max = 0.5, 
                                                                                                                        step = 0.1)
                                                                                                           ) #conditionalPanel
                                                                                          ), #conditionalPanel
                                                                         checkboxInput(inputId = "ML_pct_trans", 
                                                                                       label = "Percentage transformation", 
                                                                                       value = T),
                                                                         checkboxInput(inputId = "ML_log_trans", 
                                                                                       label = "Log10 transformation", 
                                                                                       value = T),
                                                                         checkboxInput(inputId = "ML_scaling", 
                                                                                       label = "Data scaling", 
                                                                                       value = T)
                                                                         ), #div #ML_user_reset_div
                                                                     actionButton(inputId = 'ML_user_reset', label = 'Reset', icon = icon('redo')), #actionButton #ML_user_reset
                                                                     actionButton(inputId = 'ML_user_upload', label = 'Upload', icon = icon('upload')) #actionButton #ML_user_upload
                                                                     ), #conditionalPanel
                                                    tags$p(actionLink("ML_link_to_FAQ4", 
                                                                      "How to prepare your dataset?",
                                                                      style = "color: darkblue;"))
                                                    ), #sidebarPanel
                                       mainPanel(width = 8, 
                                                 conditionalPanel(condition = 'input.ML_data_source == "ML_demo_data" & input.ML_demo_upload', 
                                                                  div(id = 'ML_demo_mainPanel_div', 
                                                                      column(width = 12,
                                                                             div(id = 'ML_demo_data_description_style_div', 
                                                                                 h3(p("Demo dataset",style="text-align: left")),
                                                                                 h6('The landscape of cancer cell line metabolism (Nat Med. 2018)',style="text-align:left"),
                                                                                 h6('We modified the data from this paper and divided cancer cell lines into sensitive or resistant to SCD gene knockout evenly based on gene dependency scores (CERES).
                                                                                    In machine learning analysis, 89 lipid species were used to predict 228 cancer cell lines with a label of 0 (sensitive) or 1 (resistant) by a binary classifier.',
                                                                                    style="text-align:left"),
                                                                                 h6('For the data sources, users can either upload their datasets or use our Demo datasets. Two necessary tables, ‘lipid expression data’ and ‘Group assignment’
                                                                                    and one optional ‘Lipid characteristics table’ will be passed to the machine learning algorithm. All datasets must be uploaded in csv or tsv format.',
                                                                                    style="text-align:left"),
                                                                                 style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px"
                                                                             ), #div #ML_demo_data_description_style_div
                                                                             br(), 
                                                                             h4(strong('Lipid expression data')),
                                                                             tabsetPanel(id = 'ML_demo_lipid_exp_tab',
                                                                                         tabPanel(title = "Processed data", 
                                                                                                  dataTableOutput(outputId = 'ML.demo.exp') %>% withSpinner(), #dataTableOutput #ML.demo.exp
                                                                                                  ),
                                                                                         tabPanel(title = "Raw data", 
                                                                                                  dataTableOutput(outputId = 'ML.demo.exp.raw') %>% withSpinner(), #dataTableOutput #ML.demo.exp.raw
                                                                                                  )
                                                                                         ), #tabsetPanel
                                                                             br()
                                                                      ), #column
                                                                      column(width = 6,
                                                                             h4(strong('Condition table')),
                                                                             dataTableOutput(outputId = 'ML.demo.cond') %>% withSpinner(), #dataTableOutput #ML.demo.cond
                                                                             br()
                                                                      ), #column
                                                                      column(width = 6,
                                                                             h4(strong('Lipid characteristics')),
                                                                             dataTableOutput(outputId = 'ML.demo.lipid.char') %>% withSpinner(), #dataTableOutput #ML.demo.lipid.char
                                                                             br()
                                                                      ), #column
                                                                      column(width = 12,
                                                                             div(id = 'ML_demo_data_control_panel_div',
                                                                                 div(id = 'ML_demo_reset2_div',
                                                                                     column(width = 5,
                                                                                            selectInput(inputId = 'ML_demo_feature_rank_method', 
                                                                                                        label = 'Feature selection method:', 
                                                                                                        choices = c('p-value' = 'p_value', 
                                                                                                                    'pvalue_FC' = 'pvalue_FC', 
                                                                                                                    'ROC' = 'ROC', 
                                                                                                                    'Random Forest' = 'Random_forest', 
                                                                                                                    'Linear SVM' = 'SVM', 
                                                                                                                    'Lasso' = 'Lasso', 
                                                                                                                    'Ridge' = 'Ridge', 
                                                                                                                    'ElasticNet' = 'ElasticNet'), 
                                                                                                        selected = 'Random_forest', 
                                                                                                        multiple = F, 
                                                                                                        width = '100%') %>%
                                                                                              helper(type = "inline",
                                                                                                     title = "Feature selection method",
                                                                                                     content = c("Feature selection methods are aimed to rank the most significant variables to a model to predict the target variable.")) ,
                                                                                            selectInput(inputId = 'ML_demo_classification_method', 
                                                                                                        label = 'Classifier:', 
                                                                                                        choices = c('Random Forest' = 'Random_forest', 
                                                                                                                    'Linear SVM' = 'SVM', 
                                                                                                                    'Lasso' = 'Lasso', 
                                                                                                                    'Ridge' = 'Ridge', 
                                                                                                                    'ElasticNet' = 'ElasticNet', 
                                                                                                                    'XGBoost' = 'xgboost'), 
                                                                                                        selected = 'Random_forest', 
                                                                                                        multiple = F, 
                                                                                                        width = '100%') %>%
                                                                                              helper(type = "inline",
                                                                                                     title = "Classifier",
                                                                                                     content = c("Classifier is the method that assigns a label of one of the classes to every lipid uploaded by users.")) ,
                                                                                            conditionalPanel(condition = 'input.ML_demo_feature_rank_method == "ElasticNet" | 
                                                                                                             input.ML_demo_classification_method == "ElasticNet"', 
                                                                                                             numericInput(inputId = 'ML_demo_alpha', 
                                                                                                                          label = 'alpha:', 
                                                                                                                          value = 0.5, 
                                                                                                                          min = 0, 
                                                                                                                          max = 1, 
                                                                                                                          width = '100%')
                                                                                                             ) #conditionalPanel
                                                                                            ), #column
                                                                                     column(width = 1),
                                                                                     column(width = 5, 
                                                                                            selectInput(inputId = 'ML_demo_cross_vali_time',
                                                                                                        label = 'Cross validation times:', 
                                                                                                        choices = c(5, 10, 15, 20, 25, 30), 
                                                                                                        selected = 5, 
                                                                                                        multiple = F, 
                                                                                                        width = '100%'),
                                                                                            helpText("Note: the more 'Cross validation times', the longer it takes to calculate the results."),
                                                                                            selectInput(inputId = 'ML_demo_split_for_test',
                                                                                                        label = 'Test data propotion:', 
                                                                                                        choices = c('1/5', '1/4', '1/3', '1/2'), 
                                                                                                        selected = '1/3', 
                                                                                                        multiple = F, 
                                                                                                        width = '100%'), 
                                                                                            selectInput(inputId = 'ML_demo_add_var',
                                                                                                        label = 'Additional variable:',
                                                                                                        choices = c('None' = 'none'),
                                                                                                        selected = 'none',
                                                                                                        multiple = T, 
                                                                                                        width = '100%'),
                                                                                            ) #column
                                                                                 ), #div #ML_demo_reset2_div
                                                                                 style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:300px"
                                                                             ), #div #ML_demo_data_control_panel_div'
                                                                             br()
                                                                             ), #column
                                                                      column(width = 4), #column
                                                                      column(width = 8, 
                                                                             actionButton(inputId = 'ML_demo_reset2', label = 'Reset', icon = icon('redo')),
                                                                             actionButton(inputId = 'ML_demo_start', label = 'Start!', icon = icon('play')) #actionButton #ML_demo_start
                                                                             ) #column
                                                                      ) #div #ML_demo_mainPanel_div
                                                                  ), #conditionalPanel
                                                 conditionalPanel(condition = 'input.ML_data_source == "ML_user_data" & input.ML_user_upload', 
                                                                  div(id = 'ML_user_mainPanel_div', 
                                                                      column(width = 12,
                                                                             div(id = 'ML_user_data_description_style_div', 
                                                                                 htmlOutput('ML_Check_Exp_Data'),
                                                                                 htmlOutput('ML_Check_condition'),
                                                                                 htmlOutput('ML_Check_lipid_char'),
                                                                                 htmlOutput('ML_Data_summary'),
                                                                                 style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px"
                                                                                 ), #div #ML_user_data_description_style_div
                                                                             helpText(tags$p(icon("check"),": Successfully uploaded.", style="font-size: 16px;", HTML('&nbsp;'), icon("times"), ": Error happaned. Please check your dataset.", HTML('&nbsp;'), icon("exclamation"), ": Warning message.", style="font-size: 16px;")),
                                                                             br()
                                                                             ),
                                                                      div(id = 'ML_user_input_table_div',
                                                                          column(width = 12,
                                                                                 h4(strong('Lipid expression data')),
                                                                                 tabsetPanel(id = 'ML_user_lipid_exp_tab',
                                                                                             tabPanel(title = "Processed data", 
                                                                                                      dataTableOutput(outputId = 'ML.user.exp') %>% withSpinner(), #dataTableOutput #ML.user.exp
                                                                                                      ),
                                                                                             tabPanel(title = "Uploaded data", 
                                                                                                      dataTableOutput(outputId = 'ML.user.exp.raw') %>% withSpinner(), #dataTableOutput #ML.user.exp.raw
                                                                                                      )
                                                                                             ), #tabsetPanel
                                                                                 br()
                                                                                 ), #column
                                                                          column(width = 6,
                                                                                 h4(strong('Condition table')),
                                                                                 dataTableOutput(outputId = 'ML.user.cond') %>% withSpinner(), #dataTableOutput #ML.user.cond
                                                                                 br()
                                                                                 ), #column
                                                                          column(width = 6,
                                                                                 h4(strong('Lipid characteristics')),
                                                                                 dataTableOutput(outputId = 'ML.user.lipid.char') %>% withSpinner(), #dataTableOutput #ML.user.lipid.char
                                                                                 br()
                                                                                 ), #column
                                                                          column(width = 12,
                                                                                 div(id = 'ML_user_data_control_panel_div',
                                                                                     div(id = 'ML_user_reset2_div',
                                                                                         column(width = 6,
                                                                                                selectInput(inputId = 'ML_user_feature_rank_method', 
                                                                                                            label = 'Feature selection method:', 
                                                                                                            choices = c('p-value' = 'p_value', 
                                                                                                                        'pvalue_FC' = 'pvalue_FC', 
                                                                                                                        'ROC' = 'ROC', 
                                                                                                                        'Random Forest' = 'Random_forest', 
                                                                                                                        'Linear SVM' = 'SVM', 
                                                                                                                        'Lasso' = 'Lasso', 
                                                                                                                        'Ridge' = 'Ridge', 
                                                                                                                        'ElasticNet' = 'ElasticNet'), 
                                                                                                            selected = 'Random_forest', 
                                                                                                            multiple = F), 
                                                                                                selectInput(inputId = 'ML_user_classification_method', 
                                                                                                            label = 'Classifier:', 
                                                                                                            choices = c('Random Forest' = 'Random_forest', 
                                                                                                                        'Linear SVM' = 'SVM', 
                                                                                                                        'Lasso' = 'Lasso', 
                                                                                                                        'Ridge' = 'Ridge', 
                                                                                                                        'ElasticNet' = 'ElasticNet', 
                                                                                                                        'XGBoost' = 'xgboost'), 
                                                                                                            selected = 'Random_forest', 
                                                                                                            multiple = F), 
                                                                                                conditionalPanel(condition = 'input.ML_user_feature_rank_method == "ElasticNet" | 
                                                                                                             input.ML_user_classification_method == "ElasticNet"', 
                                                                                                                 numericInput(inputId = 'ML_user_alpha', 
                                                                                                                              label = 'alpha:', 
                                                                                                                              value = 0.5, 
                                                                                                                              min = 0, 
                                                                                                                              max = 1)
                                                                                                                 ) #conditionalPanel
                                                                                                ), #column
                                                                                         column(width = 6, 
                                                                                                selectInput(inputId = 'ML_user_cross_vali_time',
                                                                                                            label = 'Cross validation times:', 
                                                                                                            choices = c(5, 10, 15, 20, 25, 30), 
                                                                                                            selected = 10, 
                                                                                                            multiple = F), 
                                                                                                selectInput(inputId = 'ML_user_split_for_test',
                                                                                                            label = 'Test data propotion:', 
                                                                                                            choices = c('1/5', '1/4', '1/3', '1/2'), 
                                                                                                            selected = '1/3', 
                                                                                                            multiple = F), 
                                                                                                selectInput(inputId = 'ML_user_add_var',
                                                                                                            label = 'Additional variable:',
                                                                                                            choices = c('None' = 'none'),
                                                                                                            selected = 'none',
                                                                                                            multiple = T),
                                                                                                ) #column
                                                                                         ), #div #ML_user_reset2_div
                                                                                     style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:300px"
                                                                                     ), #div #ML_user_data_control_panel_div'
                                                                                 br()
                                                                                 ), #column
                                                                          ), #div #ML_user_input_table_div
                                                                      column(width = 4), #column
                                                                      column(width = 8, 
                                                                             actionButton(inputId = 'ML_user_reset2', label = 'Reset', icon = icon('redo')),
                                                                             actionButton(inputId = 'ML_user_start', label = 'Start!', icon = icon('play')) #actionButton #ML_user_start
                                                                             ) #column
                                                                  ) #div #ML_user_mainPanel_div
                                                 ) #conditionalPanel
                                       ) #mainPanel
                         ), #sidebarLayout
                         hr(),
                         ###########################
                         ####  ML analysis tab  ####
                         ###########################
                         div(id = 'ML_tabPanel_div',
                             div(id = 'ML_result_div', 
                                 h2('Result'),
                                 ), #div #ML_result_div
                             conditionalPanel(condition = 'input.ML_data_source == "ML_demo_data" & input.ML_demo_start | 
                                          input.ML_data_source == "ML_user_data" & input.ML_user_start',
                                              tabsetPanel(id = 'ML_analysis_tab',
                                                          ########################
                                                          ####  ROC/PR curve  ####
                                                          ########################
                                                          tabPanel(title = 'ROC/PR curve',
                                                                   h3('ROC/PR curve'), 
                                                                   h6("The ROC and Precision-Recall (PR) curve are very common methods to evaluate the diagnostic ability of a binary classifier.
                                                                      Mean AUC and 95% confidence interval for the ROC and PR curve are calculated from all CV runs in each feature number.
                                                                      Theoretically, the higher the AUC, the better the model performs. PR curve is more sensitive to data with highly skewed datasets and offers a more informative view of an algorithm's performance.
                                                                      An AUC equal to 1 both represents perfect performance in two methods.
                                                                      We provide an overall ROC/PR Curve shown curve of CVs with different feature numbers and a ROC/PR Curve shown curve of average CVs by user-selected feature number.", 
                                                                        style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px"),
                                                                   br(),
                                                                   column(width = 6,
                                                                          plotlyOutput(outputId = 'ML.ROC.all', height = '360px') %>% withSpinner(),
                                                                          br()
                                                                   ),
                                                                   column(width = 6,
                                                                          plotlyOutput(outputId = 'ML.PR.all', height = '360px') %>% withSpinner(),
                                                                          br()
                                                                   ),
                                                                   column(width = 4),
                                                                   column(width = 8, 
                                                                          selectInput(inputId = 'ML_ROC_PR_feature_number', 
                                                                                      label = 'Feature number:', 
                                                                                      choices = c(2, 3, 5, 10, 20, 50), 
                                                                                      selected = 10, 
                                                                                      multiple = F)
                                                                   ),
                                                                   column(width = 5,
                                                                          br(),
                                                                          plotlyOutput(outputId = 'ML.ROC', height = '360px') %>% withSpinner(),
                                                                          br()
                                                                   ),
                                                                   column(width = 1),
                                                                   column(width = 5,
                                                                          br(),
                                                                          plotlyOutput(outputId = 'ML.PR', height = '360px') %>% withSpinner(),
                                                                          br()
                                                                   ),
                                                                   column(width = 1)
                                                          ), #tabPanel
                                                          ##############################
                                                          ####  Model predictivity  ####
                                                          ##############################
                                                          tabPanel(title = 'Model performance',
                                                                   h3('Model performance'), 
                                                                   h6(p("In this part, many useful indicators are provided for users to evaluate model performance. For each feature number, we calculate and plot the average value and 95% confidence interval of accuracy,
                                                                    sensitivity (recall), specificity, positive predictive value (precision), negative predictive value, F1 score, prevalence, detection rate, detection prevalence, balanced accuracy in all CV runs with confusion Matrix function in carat package.
                                                                    All these indicators can be described in terms of true positive (TP), false positive (FP), false negative (FN) and true negative (TN) and are summarized as follows.", 
                                                                        style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                   br(),
                                                                   column(width = 4), 
                                                                   column(width = 8,
                                                                          selectInput(inputId = 'ML_evaluation_method', 
                                                                                      label = 'Evaluation method:',
                                                                                      choices = c('Accuracy' = 'Accuracy', 
                                                                                                  'Sensitivity' = 'Sensitivity', 
                                                                                                  'Specificity' = 'Specificity',
                                                                                                  'Positive Predictive Value' = 'Pos Pred Value', 
                                                                                                  'Negative Predictive Value' = 'Neg Pred Value', 
                                                                                                  'Precision' = 'Precision', 
                                                                                                  'Recall' = 'Recall', 
                                                                                                  'F1' = 'F1', 
                                                                                                  'Prevalence' = 'Prevalence', 
                                                                                                  'Detection Rate' = 'Detection Rate', 
                                                                                                  'Detection Prevalence' = 'Detection Prevalence', 
                                                                                                  'Balanced Accuracy' = 'Balanced Accuracy'),
                                                                                      selected = 'Accuracy', 
                                                                                      multiple = F),
                                                                          br()
                                                                   ),
                                                                   column(width = 1),
                                                                   column(width = 10, 
                                                                          plotlyOutput(outputId = 'ML.evalution.plot', height = '100%') %>% withSpinner(),
                                                                          br(), 
                                                                          dataTableOutput(outputId = 'ML.evalution.table') %>% withSpinner(),
                                                                          br()
                                                                   ),
                                                                   column(width = 1)
                                                          ), #tabPanel
                                                          ##############################
                                                          ####  Sample probability  ####
                                                          ##############################
                                                          tabPanel(title = 'Predicted probability',
                                                                   h3('Predicted probability'), 
                                                                   h6(p("This page shows the average predicted probabilities of each sample in testing data from all CV runs and allows users to explore those incorrect or uncertain labels.
                                                                    We show the distribution of predicted probabilities in two reference labels on the left panel while a confusion matrix composed of sample number and proportion is laid out on the right.
                                                                    Results for different feature number can be selected manually by users.", 
                                                                        style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                   br(),
                                                                   column(width = 4), 
                                                                   column(width = 8, 
                                                                          selectInput(inputId = 'ML_sam_prob_feature_number', 
                                                                                      label = 'Feature number:', 
                                                                                      choices = c(2, 3, 5, 10, 20, 50), 
                                                                                      selected = 10, 
                                                                                      multiple = F)
                                                                   ),
                                                                   column(width = 6, 
                                                                          br(),
                                                                          plotlyOutput(outputId = 'ML.probability.plot', height = '100%') %>% withSpinner(),
                                                                          br()
                                                                   ), 
                                                                   column(width = 6, 
                                                                          br(),
                                                                          plotlyOutput(outputId = 'ML.cm.plot', height = '100%') %>% withSpinner(),
                                                                          br()
                                                                   ), 
                                                                   column(width = 12, 
                                                                          br(),
                                                                          dataTableOutput(outputId = 'ML.probability.table') %>% withSpinner(),
                                                                          br()
                                                                   )
                                                          ), #tabPanel
                                                          ##############################
                                                          ####  Feature importance  ####
                                                          ##############################
                                                          tabPanel(title = 'Feature importance',
                                                                   h3('Feature importance'), 
                                                                   br(),
                                                                   navlistPanel(widths = c(2, 10), 
                                                                                id = 'ML_fea_impo_list',
                                                                                ###########################
                                                                                ####  Algorithm-based  ####
                                                                                ###########################
                                                                                tabPanel(title = 'Algorithm-based',
                                                                                         h3('Algorithm-based'),
                                                                                         div(h6(p("After building a high-accuracy model, users are encouraged to explore the contribution of each feature on this page.
                                                                                              Two methods here namely ‘Algorithm-based’ and ‘SHAP analysis’ can rank and visualize the feature importance.",style="text-align: left")),
                                                                                             h6(p("In ‘Algorithm-based’ part, when users choose a certain feature number, the selected frequency and the average feature importance of top 10 features from all CV runs will be displayed.
                                                                                              For a Linear SVM, Lasso, Ridge or ElasticNet model, the importance of each feature depends on the absolute value of their coefficients in the algorithm,
                                                                                              while Random Forest and XGBoost use built-in feature importance results. ",style="text-align: left")),
                                                                                             style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px"
                                                                                         ),
                                                                                         br(),
                                                                                         column(width = 4),
                                                                                         column(width = 8,
                                                                                                selectInput(inputId = 'ML_algorithm_feature_number', 
                                                                                                            label = 'Feature number:', 
                                                                                                            choices = c(2, 3, 5, 10, 20, 50), 
                                                                                                            selected = 10, 
                                                                                                            multiple = F)
                                                                                         ),
                                                                                         column(width = 6,
                                                                                                br(),
                                                                                                plotlyOutput(outputId = 'ML.fea.freq.plot', height = '100%') %>% withSpinner(),
                                                                                                br(),
                                                                                                dataTableOutput(outputId = 'ML.fea.freq.table') %>% withSpinner(),
                                                                                                br()
                                                                                         ),
                                                                                         column(width = 6,
                                                                                                br(),
                                                                                                plotlyOutput(outputId = 'ML.fea.impo.plot', height = '100%') %>% withSpinner(),
                                                                                                br(),
                                                                                                dataTableOutput(outputId = 'ML.fea.impo.table') %>% withSpinner(),
                                                                                                br()
                                                                                         )
                                                                                ), #tabPanel
                                                                                #########################
                                                                                ####  SHAP analysis  ####
                                                                                #########################
                                                                                tabPanel(title = 'SHAP analysis',
                                                                                         h3('SHAP analysis'),
                                                                                         h6(p("After building a high-accuracy model, users are encouraged to explore the contribution of each feature on this page.
                                                                                          Two methods here namely ‘Algorithm-based’ and ‘SHAP analysis’ can rank and visualize the feature importance.
                                                                                          SHapley Additive exPlanations (SHAP) approach on the basis of Shapley values in game theory has recently been introduced to explain individual predictions of any machine learning model.", 
                                                                                              style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                         br(),
                                                                                         column(width = 12,
                                                                                                div(id = 'ML_SHAP_control_panel_div',
                                                                                                    div(id = 'ML_SHAP_reset_div',
                                                                                                        column(width = 4,
                                                                                                               selectInput(inputId = 'ML_SHAP_feature_number', 
                                                                                                                           label = 'Feature number:', 
                                                                                                                           choices = c(2, 3, 5, 10, 20, 50), 
                                                                                                                           selected = 10, 
                                                                                                                           multiple = F)
                                                                                                               ), 
                                                                                                        column(width = 4,
                                                                                                               numericInput(inputId = 'ML_SHAP_n_sim', 
                                                                                                                            label = 'Simulation times:',
                                                                                                                            value = 10,
                                                                                                                            min = 1, 
                                                                                                                            max = 100,
                                                                                                                            step = 1) %>%
                                                                                                                 helper(type = "inline",
                                                                                                                        title = "Simulation times",
                                                                                                                        content = c("Except XGBoost, Monte Carlo repetitions are conducted to estimate each Shapley value and the simulation times are defined by the parameter ‘Simulation times’.")) 
                                                                                                        )
                                                                                                    ), #div #ML_SHAP_reset_div
                                                                                                    column(width = 4,
                                                                                                           actionButton(inputId = 'ML_SHAP_reset', label = 'Reset', icon = icon('redo')),
                                                                                                           actionButton(inputId = 'ML_SHAP_start', label = 'Start!', icon = icon('play'))
                                                                                                    ),
                                                                                                    style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:100px"
                                                                                                ), #div #ML_SHAP_control_panel_div
                                                                                                column(width = 12, br()),
                                                                                                conditionalPanel(condition = 'input.ML_SHAP_start', 
                                                                                                                 column(width = 5,
                                                                                                                        plotlyOutput(outputId = 'ML.mean.shapley.plot', height = '100%') %>% withSpinner()
                                                                                                                        ),
                                                                                                                 column(width = 7,
                                                                                                                        plotlyOutput(outputId = 'ML.all.shapley.plot', height = '100%') %>% withSpinner()
                                                                                                                        ), 
                                                                                                                 column(width = 12, 
                                                                                                                        br(), 
                                                                                                                        dataTableOutput(outputId = 'ML.shap.long.table') %>% withSpinner(),
                                                                                                                        br(),
                                                                                                                        div(id = 'ML_SHAP_forceplot_control_panel_div',
                                                                                                                            div(id = 'ML_SHAP_forceplot_reset_div',
                                                                                                                                column(width = 4, 
                                                                                                                                       selectInput(inputId = 'ML_SHAP_forceplot_top_n_feature', 
                                                                                                                                                   label = 'Show top N feature:', 
                                                                                                                                                   choices = c(1:10), 
                                                                                                                                                   selected = 5, 
                                                                                                                                                   multiple = F, 
                                                                                                                                                   width = '100%')
                                                                                                                                       ),
                                                                                                                                column(width = 4, 
                                                                                                                                       selectInput(inputId = 'ML_SHAP_forceplot_group_number', 
                                                                                                                                                   label = 'Number of group:', 
                                                                                                                                                   choices = c(1:10), 
                                                                                                                                                   selected = 2, 
                                                                                                                                                   multiple = F, 
                                                                                                                                                   width = '100%')
                                                                                                                                       )
                                                                                                                                ), #div #ML_SHAP_forceplot_reset_div
                                                                                                                            column(width = 4, 
                                                                                                                                   actionButton(inputId = 'ML_SHAP_forceplot_reset', label = 'Reset', icon = icon('redo')),
                                                                                                                                   actionButton(inputId = 'ML_SHAP_forceplot_start', label = 'Start!', icon = icon('play'))
                                                                                                                            ),
                                                                                                                            column(width = 12,
                                                                                                                            helpText('‘Show top N feature’ can be chosen for SHAP force plot.
                                                                                                                                     The samples are clustered into multiple groups (‘Number of group’) based on Shapley values using ward.D method. ')),
                                                                                                                            style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:150px"
                                                                                                                        ), #div #ML_SHAP_forceplot_control_panel_div
                                                                                                                        conditionalPanel(condition = 'input.ML_SHAP_forceplot_start', 
                                                                                                                                         br(),
                                                                                                                                         plotlyOutput(outputId = 'ML.SHAP.forceplot', height = '100%') %>% withSpinner(),
                                                                                                                                         br(),
                                                                                                                                         dataTableOutput(outputId = 'ML.SHAP.forceplot.table') %>% withSpinner(),
                                                                                                                                         br()
                                                                                                                                         ), #conditionalPanel
                                                                                                                        br(),
                                                                                                                        div(id = 'ML_SHAP_dependence_control_panel_div',
                                                                                                                            div(id = 'ML_SHAP_dependence_reset_div',
                                                                                                                                column(width = 3, 
                                                                                                                                       selectInput(inputId = 'ML_SHAP_dependence_x',
                                                                                                                                                   label = 'X axis:',
                                                                                                                                                   choices = c(2, 3, 5, 10, 20, 50, 100),
                                                                                                                                                   selected = 10,
                                                                                                                                                   multiple = F, 
                                                                                                                                                   width = '100%')
                                                                                                                                       ), 
                                                                                                                                column(width = 3, 
                                                                                                                                       selectInput(inputId = 'ML_SHAP_dependence_y',
                                                                                                                                                   label = 'Y axis:',
                                                                                                                                                   choices = c(2, 3, 5, 10, 20, 50, 100),
                                                                                                                                                   selected = 10,
                                                                                                                                                   multiple = F, 
                                                                                                                                                   width = '100%')
                                                                                                                                       ),
                                                                                                                                column(width = 3, 
                                                                                                                                       selectInput(inputId = 'ML_SHAP_dependence_color',
                                                                                                                                                   label = 'Color:',
                                                                                                                                                   choices = c(2, 3, 5, 10, 20, 50, 100),
                                                                                                                                                   selected = 10,
                                                                                                                                                   multiple = F, 
                                                                                                                                                   width = '100%')
                                                                                                                                       )
                                                                                                                            ), #div #ML_SHAP_dependence_reset_div
                                                                                                                            column(width = 3, 
                                                                                                                                   actionButton(inputId = 'ML_SHAP_dependence_reset', label = 'Reset', icon = icon('redo')),
                                                                                                                                   actionButton(inputId = 'ML_SHAP_dependence_start', label = 'Start!', icon = icon('play'))),
                                                                                                                            column(width = 12,
                                                                                                                            helpText('The x-axis represents the value of a certain feature while the y-axis is the corresponding Shapley value.
                                                                                                                                     The colour parameter can be assigned to check if a second feature has an interaction effect with the feature we are plotting.')),
                                                                                                                            style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:150px"
                                                                                                                            ), #div #ML_SHAP_dependence_control_panel_div
                                                                                                                        conditionalPanel(condition = 'input.ML_SHAP_dependence_start', 
                                                                                                                                         br(),
                                                                                                                                         plotlyOutput(outputId = 'ML.SHAP.dependence.plot', height = '100%') %>% withSpinner(),
                                                                                                                                         br()
                                                                                                                                         ) #conditionalPanel
                                                                                                                        ) #column
                                                                                                                 ) #conditionalPanel
                                                                                                ) #column
                                                                                         ) #tabPanel
                                                                                ) #navlistPanel
                                                                   ), #tabPanel
                                                          ###################
                                                          ####  Network  ####
                                                          ###################
                                                          tabPanel(title = 'Network',
                                                                   h3('Network'), 
                                                                   h6(p("Correlation network helps users interrogate the interaction of features in a machine learning model. In this section,
                                                                        users can choose an appropriate feature number according to previous cross-validation results and the features in the best model
                                                                        (based on ROC-AUC+PR-AUC) will be picked up to compute the correlation coefficients between each other. ", 
                                                                        style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                   br(),
                                                                   column(width = 12,
                                                                          div(id = 'ML_network_control_panel_div',
                                                                              div(id = 'ML_network_reset_div',
                                                                                  column(width = 6, 
                                                                                         selectInput(inputId = 'ML_network_feature_number', 
                                                                                                     label = 'Feature number:', 
                                                                                                     choices = c(2, 3, 5, 10, 20, 50), 
                                                                                                     selected = 10, 
                                                                                                     multiple = F),
                                                                                         selectInput(inputId = 'ML_network_fea_impo_method',
                                                                                                     label = 'Feature importance method:',
                                                                                                     choices = c('Algorithm-based' = 'Algorithm-based', 
                                                                                                                 'SHAP analysis' = 'SHAP analysis'),
                                                                                                     selected = 'Algorithm-based',
                                                                                                     multiple = F),
                                                                                         conditionalPanel(condition = 'input.ML_network_fea_impo_method == "SHAP analysis"', 
                                                                                                          numericInput(inputId = 'ML_network_n_sim', 
                                                                                                                       label = 'Simulation times:',
                                                                                                                       value = 10,
                                                                                                                       min = 1, 
                                                                                                                       max = 100,
                                                                                                                       step = 1)
                                                                                                          ) #conditonalPanel
                                                                                         ), #column
                                                                                  column(width = 6, 
                                                                                         selectInput(inputId = 'ML_network_corr_method',
                                                                                                     label = 'Correlation method:',
                                                                                                     choices = c('Pearson' = 'pearson', 
                                                                                                                 'Spearman' = 'spearman', 
                                                                                                                 'Kendall'='kendall'),
                                                                                                     selected = 'pearson',
                                                                                                     multiple = F), 
                                                                                         numericInput(inputId = 'ML_network_corr_coef', 
                                                                                                      label = 'Coefficient cutoff:',
                                                                                                      value = 0,
                                                                                                      min = 0, 
                                                                                                      max = 1, 
                                                                                                      step = 0.1)
                                                                                         ) #column
                                                                                  ), #div #ML_network_reset_div
                                                                              column(width = 6),
                                                                              column(width = 6, 
                                                                                     actionButton(inputId = 'ML_network_reset', label = 'Reset', icon = icon('redo')),
                                                                                     actionButton(inputId = 'ML_network_start', label = 'Start!', icon = icon('play'))
                                                                                     ),
                                                                              style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:260px"
                                                                              ), #div #ML_network_control_panel_div
                                                                          br(),
                                                                          conditionalPanel(condition = 'input.ML_network_start',
                                                                                           div(id = 'ML_network_result_div', 
                                                                                               visNetworkOutput(outputId = 'ML.network', height = '600px') %>% withSpinner(), 
                                                                                               column(width = 5), 
                                                                                               column(width = 7, 
                                                                                                      actionButton(inputId = 'ML_network_refresh', 
                                                                                                                   label = 'Refresh', 
                                                                                                                   icon = icon('sync-alt')
                                                                                                                   ) #actionButton #ML_network_refresh
                                                                                                      ) #column
                                                                                               ) #div #ML_network_result_div
                                                                                           ), #conditionalPanel 
                                                                          br(),
                                                                          br()
                                                                          ) #column
                                                                   ) #tabPanel
                                                          ) #tabsetPanel
                                              ) #conditionalPanel
                             ) #div #ML_tabPanel_div
                         ) #column
                  ) #fluidRow
         ) #tabPanel






