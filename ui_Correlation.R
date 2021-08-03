
tabPanel(title = h4('Correlation'),
         value = 'Correlation',
         #### Correlation Header Panel ####
         h1('Correlation analysis'), 
         br(),
         fluidRow(column(width=12, 
                         ########################################
                         ####  Correlation Page Description  ####
                         ########################################
                         div(id = 'CORR_description_style_div', 
                             h6('In this section, we provide a comprehensive correlation analysis to assist researchers to interrogate the clinical features that connect to lipids species and other mechanistically relevant lipid characteristics.
                                Correlation analysis between lipids and clinical features is broadly used in many fields of study, such as Bowler RP et al. discovering that sphingomyelins are strongly associated with emphysema and glycosphingolipids are associated with COPD exacerbations.
                                Hence, continuous clinical data can be uploaded here, and diverse correlation analyses are offered. For instance, the Correlation Coefficient and Linear Regression are supported for continuous clinical data. Moreover,
                                lipids can be classified either by lipid species or by lipid categories when conducting these correlation analyses.',style="text-align: left"),
                             style="background-color: PowderBlue;border-left: 8px solid Teal;padding: 15px"
                             ), #div #CORR_description_style_div
                         br(), 
                         ###################################
                         ####  Correlation Data Source  ####
                         ###################################
                         h2('Data Source'),
                         sidebarLayout(fluid = T,
                                       sidebarPanel(width = 4, 
                                                    radioButtons(inputId = 'CORR_data_source', 
                                                                 label = h4('Data source'), 
                                                                 choices = c('Example dataset (Am J Respir Crit Care Med. 2015)' = 'CORR_demo_data', 
                                                                             'Upload your data!' = 'CORR_user_data'), 
                                                                 selected = 'CORR_demo_data'
                                                                 )%>% #radioButtons #CORR_data_source
                                                      helper(type = "inline",
                                                             title = "Data source",
                                                             size ="l",
                                                             content = c('<ol style="font-size: 0px;">',
                                                                         '<li style="font-size: 16px;">Lipid dataset can be uploaded by users or using example datasets. This information, namely Lipid expression data, Condition table and Lipid characteristics (optional). Three tables must assign to a vector  all data needs to be uploaded in 
                                                                         <mark style="background-color: white;color: red;">CSV</mark> or <mark style="background-color: white;color: red;">TSV</mark> format. The maximum file size is 30MB.</li>',
                                                                         '<li style="font-size: 16px;">Once two files are chosen and shown ‘Upload complete’ then press ‘Upload’.</li>',
                                                                         '</ol>')),
                                                    conditionalPanel(condition = 'input.CORR_data_source == "CORR_demo_data"', 
                                                                     actionButton(inputId = 'CORR_demo_cont_upload', label = 'Submit', icon = icon('upload')), #actionButton #CORR_demo_cont_upload
                                                                     downloadButton(outputId = 'CORR.demo.download', label = 'Download example')
                                                                     ), #conditionalPanel
                                                    conditionalPanel(condition = 'input.CORR_data_source == "CORR_user_data"',
                                                                     div(id = 'CORR_user_reset_div', 
                                                                         fileInput(inputId = 'CORR_user_exp', label = 'Lipid expression data:', accept = c(".csv",".tsv"), multiple = F) %>% #fileInput #CORR_user_exp
                                                                           helper(type = "inline",
                                                                                  title = "Lipid expression data",
                                                                                  size ="l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">The first column must contain a list of unique lipids name(features). NOTE: THE FEATURE LIST OF THE FIRST COLUMN MUST SAME AS ‘Lipid characteristics’!</li>',
                                                                                              '<li style="font-size: 16px;">Other columns encompass the expressed values of groups under different conditions that you want to compare.</li>',
                                                                                              '<li style="font-size: 16px;">An example of ‘Lipid expression data’</li>',
                                                                                              '<img src="Description/CORR_Lipid expression data.png" width="100%"/>',
                                                                                              '</ol>')), 
                                                                         fileInput(inputId = 'CORR_user_cond', label = 'Condition table:', accept = c(".csv",".tsv"), multiple = F)%>% #fileInput #CORR_user_cond
                                                                           helper(type = "inline",
                                                                                  title = "Condition table",
                                                                                  size ="l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">The condition table encompasses sample names and clinical conditions (disease status, gene dependence score etc.), which assigned each sample to a specific condition for further association analysis.</li>',
                                                                                              '<li style="font-size: 16px;">The first column must contain a list of samples name, MUST SAME AS THE SAMPLE NAME (COLUM NAMES) OF ‘Lipid expression data’ !</li>',
                                                                                              '<li style="font-size: 16px;">Other columns are clinical conditions, such as Emphysema, Exacerbations. NOTE: ALL VALUES MUST BE NUMERIC</li>',
                                                                                              '<li style="font-size: 16px;">An example of ‘Condition table’</li>',
                                                                                              '<img src="Description/CORR_Condition table.png" width="100%"/>',
                                                                                              '</ol>')), 
                                                                         fileInput(inputId = 'CORR_user_adj', label = 'Adjusted table (optional):', accept = c(".csv",".tsv"), multiple = F)%>% #fileInput #CORR_user_adj
                                                                           helper(type = "inline",
                                                                                  title = "Adjusted table",
                                                                                  size ="l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">‘Adjusted table’ represents the user-defined variables that will be corrected in linear regression or logistic regression analysis, which can be the cancer types or the clinical information, like gender, age, or BMI.</li>',
                                                                                              '<li style="font-size: 16px;">The first column must contain a list of samples name, MUST SAME AS THE SAMPLE NAME (COLUM NAMES) OF ‘Lipid expression data’ !</li>',
                                                                                              '<li style="font-size: 16px;">An example of ‘Adjusted table’:</li>',
                                                                                              '<img src="Description/CORR_Adjusted table.png" width="100%"/>',
                                                                                              '</ol>')), 
                                                                         fileInput(inputId = 'CORR_user_char', label = 'Lipid characteristics (optional):', accept = c(".csv",".tsv"), multiple = F)%>% #fileInput #CORR_user_char
                                                                           helper(type = "inline",
                                                                                  title = "Lipid characteristics",
                                                                                  size ="l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">The first column must contain a list of unique lipids name (features). NOTE: : THE FEATURE LIST OF THE FIRST COLUMN MUST SAME AS ‘Lipid expression data’!</li>',
                                                                                              '<li style="font-size: 16px;">Other columns can contain a wide variety of lipid characteristics, including class, total length, the total number of double bonds, or any other characteristics. The value can be the number of characters.</li>',
                                                                                              '<li style="font-size: 16px;">Due to the numbers of fatty acids attached to lipid are various, hence, if users assign the column name starting with “FA_”, system will automatically extract values separated by commas; Value between commas needs to be positive integer or zero.</li>',
                                                                                              '<li style="font-size: 16px;">If the name of characteristics matches ‘class’ (whole word only), the content must be characters, while ‘totallength’, ‘totaldb’, ‘totaloh’ (whole word only) must be numeric</li>',
                                                                                              '<li style="font-size: 16px;">An example of ‘Lipid characteristics’</li>',
                                                                                              '<img src="Description/CORR_Lipid characteristics.png" width="100%"/>',
                                                                                              '</ol>')), 
                                                                         helpText("Upload your data table in .csv/.tsv"), #helpText
                                                                         br(),
                                                                         h4('Data processing') %>%
                                                                           helper(type = "inline",
                                                                                  title = "Data processing",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">If you clicking on ‘Remove features with many missing values’, the threshold of the percentage of blank in the dataset that will be deleted can be defined by users.</li>',
                                                                                              '<img src="Description/Data processing_1.png" />',
                                                                                              '<li style="font-size: 16px;">The ‘Missing values imputation’ is for users to choose minimum, mean, and median to replace the missing values in the dataset. If users select minimum, minimum will be multiplied by the value of user inputted. After uploading data, three datasets will show on the right-hand side. When finishing checking data, click ‘Start’ for further analysis.</li>',
                                                                                              '<img src="Description/Data processing_2.png" />',
                                                                                              '<li style="font-size: 16px;">‘Percentage transformation’, ‘Log10 transformation’ can transform data into log10 or percentage. The purpose of ‘Log10 transformation’ is to make highly skewed distributions less skewed, while ‘Percentage transformation’ is to standardize variation between groups.</li>',
                                                                                              '<img src="Description/Data processing_3.png" />',
                                                                                              '</ol>')),
                                                                         checkboxInput(inputId = "CORR_rm_NA", 
                                                                                       label = "Remove features with many missing values", 
                                                                                       value = T), #checkboxInput #CORR_rm_NA
                                                                         conditionalPanel(condition = 'input.CORR_rm_NA', 
                                                                                          numericInput(inputId = 'CORR_rm_NA_pct', 
                                                                                                       label = 'More than % missing values', 
                                                                                                       value = 50, 
                                                                                                       min = 5, 
                                                                                                       max = 100, 
                                                                                                       step = 5)
                                                                                          ), #conditionalPanel
                                                                         checkboxInput(inputId = "CORR_rp_NA", 
                                                                                       label = "Missing values imputation", 
                                                                                       value = T),
                                                                         conditionalPanel(condition = 'input.CORR_rp_NA',
                                                                                          selectInput(inputId = 'CORR_fill_NA',
                                                                                                      label = 'Fill missing value with:',
                                                                                                      choices = c('Mean' = 'mean', 
                                                                                                                  'Median' = 'median', 
                                                                                                                  'Minimum' = 'min'), 
                                                                                                      selected = 'min', 
                                                                                                      multiple = F),
                                                                                          conditionalPanel(condition = 'input.CORR_fill_NA == "min"', 
                                                                                                           numericInput(inputId = 'CORR_fill_min', 
                                                                                                                        label = 'Multiply by minimum', 
                                                                                                                        value = 0.5, 
                                                                                                                        min = 0.1, 
                                                                                                                        max = 0.5, 
                                                                                                                        step = 0.1)
                                                                                                           ) #conditionalPanel
                                                                                          ), #conditionalPanel
                                                                         checkboxInput(inputId = "CORR_pct_trans", 
                                                                                       label = "Percentage transformation", 
                                                                                       value = T),
                                                                         checkboxInput(inputId = "CORR_log_trans", 
                                                                                       label = "Log10 transformation", 
                                                                                       value = T)
                                                                         ), #div #CORR_user_reset_div
                                                                     actionButton(inputId = 'CORR_user_reset', label = 'Reset', icon = icon('redo')), #actionButton #CORR_user_reset
                                                                     actionButton(inputId = 'CORR_user_upload', label = 'Upload', icon = icon('upload')) #actionButton #CORR_user_upload
                                                                     ), #conditionalPanel
                                                    tags$p(actionLink("CORR_link_to_FAQ4", 
                                                                      "How to prepare your dataset?",
                                                                      style = "color: darkblue;"))
                                                    ), #sidebarPanel
                                       mainPanel(width = 8, 
                                                 conditionalPanel(condition = 'input.CORR_data_source == "CORR_demo_data" & input.CORR_demo_cont_upload', 
                                                                  div(id = 'CORR_demo_cont_mainPanel_div', 
                                                                      column(width = 12,
                                                                             div(id = 'CORR_demo_cont_description_style_div', 
                                                                                 h3(p("Demo dataset",style="text-align: left")),
                                                                                 h6('Bowler, Russell P., et al. "Plasma sphingolipids associated with chronic obstructive pulmonary disease phenotypes." 
                                                                                    American journal of respiratory and critical care medicine 191.3 (2015): 275-284.
                                                                                    This paper detected 69 distinct plasma sphingolipid species in 129 current and former smokers by targeted mass spectrometry.
                                                                                    This cohort was used to interrogate the associations of plasma sphingolipids with subphenotypes of COPD including airflow obstruction, emphysema, and frequent exacerbations.',style="text-align:left"),
                                                                                 h6('For the data sources, users can either upload their datasets or use our Demo datasets.
                                                                                    The datatype of the dataset must be continuous data. The dataset needs to contain two tables,
                                                                                    ‘lipid expression data’ and ‘condition table’. Another optional tables for adjustment and lipid characteristics analysis are also can be uploaded by users. ',
                                                                                    style="text-align:left"),
                                                                                 style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px"
                                                                                 ), #div #CORR_demo_data_description_style_div
                                                                             br(), 
                                                                             h4(strong('Lipid expression data')),
                                                                             tabsetPanel(id = 'CORR_demo_lipid_exp_tab',
                                                                                         tabPanel(title = "Processed data", 
                                                                                                  dataTableOutput(outputId = 'CORR.demo.cont.exp') %>% withSpinner(), #dataTableOutput #CORR.demo.cont.exp
                                                                                                  ),
                                                                                         tabPanel(title = "Raw data", 
                                                                                                  dataTableOutput(outputId = 'CORR.demo.cont.exp.raw') %>% withSpinner(), #dataTableOutput #CORR.demo.cont.exp.raw
                                                                                                  )
                                                                                         ), #tabsetPanel
                                                                             br(), 
                                                                             h4(strong('Condition table (clinical factor)')),
                                                                             dataTableOutput(outputId = 'CORR.demo.cont.cond') %>% withSpinner(), #dataTableOutput #CORR.demo.cont.cond
                                                                             br()
                                                                             ), #column
                                                                      column(width = 6, 
                                                                             h4(strong('Adjusted table')),
                                                                             dataTableOutput(outputId = 'CORR.demo.cont.adj') %>% withSpinner() #dataTableOutput #CORR.demo.cont.adj
                                                                             ), #column
                                                                      column(width = 6, 
                                                                             h4(strong('Lipid characteristics')),
                                                                             dataTableOutput(outputId = 'CORR.demo.cont.char') %>% withSpinner() #dataTableOutput #CORR.demo.cont.char
                                                                             ), #column
                                                                      column(width = 12, br()), #column
                                                                      column(width = 5), #column
                                                                      column(width = 2, 
                                                                             actionButton(inputId = 'CORR_demo_cont_start', label = 'Start!', icon = icon('play')) #actionButton #CORR_demo_cont_start
                                                                             ), #column
                                                                      column(width = 5) #column
                                                                      ) #div #CORR_demo_cont_mainPanel_div
                                                                  ), #conditionalPanel
                                                 conditionalPanel(condition = 'input.CORR_data_source == "CORR_user_data" & input.CORR_user_upload', 
                                                                  div(id = 'CORR_user_mainPanel_div', 
                                                                      column(width = 12,
                                                                             div(id = 'CORR_user_data_description_style_div', 
                                                                                 htmlOutput("CORR_Check_Exp_Data"),
                                                                                 htmlOutput("CORR_Check_condition"),
                                                                                 htmlOutput("CORR_Check_adjusted_table"),
                                                                                 htmlOutput("CORR_Check_lipid_char"),
                                                                                 htmlOutput("CORR_Data_summary"),
                                                                                 style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px"
                                                                                 ), #div #CORR_user_data_description_style_div
                                                                             helpText(tags$p(icon("check"),": Successfully uploaded.", style="font-size: 16px;", HTML('&nbsp;'), icon("times"), ": Error happaned. Please check your dataset.", HTML('&nbsp;'), icon("exclamation"), ": Warning message.", style="font-size: 16px;")),
                                                                             br()
                                                                             ), #column
                                                                      div(id = 'CORR_user_input_table_div',
                                                                          column(width = 12,
                                                                                 h4(strong('Lipid expression data')),
                                                                                 tabsetPanel(id = 'CORR_user_lipid_exp_tab',
                                                                                             tabPanel(title = "Processed data", 
                                                                                                      dataTableOutput(outputId = 'CORR.user.exp') %>% withSpinner(), #dataTableOutput #CORR.user.exp
                                                                                                      ),
                                                                                             tabPanel(title = "Raw data", 
                                                                                                      dataTableOutput(outputId = 'CORR.user.exp.raw') %>% withSpinner(), #dataTableOutput #CORR.user.exp.raw
                                                                                                      )
                                                                                             ), #tabsetPanel
                                                                                 br(), 
                                                                                 h4(strong('Condition table (clinical factor)')),
                                                                                 dataTableOutput(outputId = 'CORR.user.cond') %>% withSpinner(), #dataTableOutput #CORR.user.cond
                                                                                 br()
                                                                                 ), #column
                                                                          column(width = 6, 
                                                                                 h4(strong('Adjusted table')),
                                                                                 dataTableOutput(outputId = 'CORR.user.adj') %>% withSpinner() #dataTableOutput #CORR.user.adj
                                                                                 ), #column
                                                                          column(width = 6, 
                                                                                 h4(strong('Lipid characteristics')),
                                                                                 dataTableOutput(outputId = 'CORR.user.char') %>% withSpinner() #dataTableOutput #CORR.user.char
                                                                                 ), #column
                                                                          ), #div #CORR_user_input_table_div
                                                                      column(width = 12, br()), #column
                                                                      column(width = 5), #column
                                                                      column(width = 2, 
                                                                             actionButton(inputId = 'CORR_user_start', label = 'Start!', icon = icon('play')) #actionButton #CORR_user_start
                                                                             ), #column
                                                                      column(width = 5) #column
                                                                      ) #div #CORR_user_mainPanel_div
                                                                  ) #conditionalPanel
                                                 ) #mainPanel
                                       ), #sidebarLayout
                         hr(),
                         ####################################
                         ####  Correlation analysis tab  ####
                         ####################################
                         div(id = 'CORR_tabPanel_div',
                             div(id = 'CORR_result_div', 
                                 h2('Result'),
                                 ), #div #CORR_result_div
                             conditionalPanel(condition = '(input.CORR_data_source == "CORR_demo_data" & input.CORR_demo_cont_start) | 
                                          (input.CORR_data_source == "CORR_user_data" & input.CORR_user_start)',
                                              tabsetPanel(id = 'CORR_analysis_tab', 
                                                          tabPanel(title = 'Lipid species analysis', 
                                                                   br(),
                                                                   navlistPanel(widths = c(3, 9), 
                                                                                id = 'CORR_species_list',
                                                                                #######################
                                                                                ####  Correlation  ####
                                                                                #######################
                                                                                tabPanel(title = 'Correlation', 
                                                                                         h3('Correlation'), 
                                                                                         column(width = 12,
                                                                                                div(id = 'CORR_species_description_style_div',
                                                                                                    h6('The Correlation Coefficient gives a summary view that tells researchers whether a relationship exists between clinical features and lipid species,
                                                                                               how strong that relationship is and whether the relationship is positive or negative. Here we provide three types of correlations, Pearson, Spearman, and Kendall,
                                                                                               and adjusted by Benjamini & Hochberg methods. The cut-offs for correlation coefficient and the p-value can be decided by users.'),
                                                                                                    h6('A heatmap will show after users inputting cut-offs and choosing a value for clustering/methods for clustering.
                                                                                               Users can use either correlation coefficient between clinical features (e.g. genes) and lipid species or choose their statistic instead.'),
                                                                                                    style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px"),
                                                                                                br(),
                                                                                                div(id = 'CORR_species_corr_style_div', 
                                                                                                    div(id = 'CORR_species_corr_reset_div', 
                                                                                                        column(width = 4, 
                                                                                                               selectInput(inputId = 'CORR_species_corr_method', 
                                                                                                                           label = 'Correlation method:', 
                                                                                                                           choices = c('Pearson' = 'pearson', 
                                                                                                                                       #'Kendall'='kendall',
                                                                                                                                       'Spearman' = 'spearman'), 
                                                                                                                           selected = 'pearson', 
                                                                                                                           multiple = F
                                                                                                               ), #selectInput #CORR_species_corr_method
                                                                                                               radioButtons(inputId = 'CORR_species_corr_adj_stat_method', 
                                                                                                                            label = 'Multiple testing correction:', 
                                                                                                                            choices = c('Benjamini & Hochberg' = 'BH'), 
                                                                                                                            selected = 'BH', 
                                                                                                                            inline = F
                                                                                                               ) #radioButtons #CORR_species_corr_adj_stat_method
                                                                                                        ), #column
                                                                                                        column(width = 4, 
                                                                                                               selectInput(inputId = 'CORR_species_corr_sig_p', 
                                                                                                                           label = 'Identify significant lipids:', 
                                                                                                                           choices = c('p-value' = 'p', 
                                                                                                                                       'adjusted p-value' = 'p.adj'), 
                                                                                                                           selected = 'p.adj', 
                                                                                                                           multiple = F
                                                                                                               ), #selectInput #CORR_species_corr_sig_p
                                                                                                               numericInput(inputId = 'CORR_species_corr_pval', 
                                                                                                                            label = 'p-value:', 
                                                                                                                            value = 1, 
                                                                                                                            min = 0, 
                                                                                                                            max = 1
                                                                                                               ), #numericInput #CORR_species_corr_pval
                                                                                                               numericInput(inputId = 'CORR_species_corr_coef', 
                                                                                                                            label = 'Correlation coefficient cutoff:', 
                                                                                                                            value = 0, 
                                                                                                                            min = 0, 
                                                                                                                            max = 1, 
                                                                                                                            step = 0.1
                                                                                                               ) %>% #numericInput #CORR_species_corr_coef
                                                                                                                 helper(type = "inline",
                                                                                                                        title = "Cutoff of correlation coefficient Help",
                                                                                                                        content = c("A coefficient of <0.1 indicates a negligible and >0.9 a very strong relationship, values in-between are disputable.")) ,
                                                                                                        ), #column
                                                                                                        column(width = 4, 
                                                                                                               radioButtons(inputId = 'CORR_species_corr_color', 
                                                                                                                            label = 'Value for clustering:', 
                                                                                                                            choices = c('correlation coefficient' = 'cor_coef', 
                                                                                                                                        'statistics' = 'statistic'), 
                                                                                                                            selected = 'statistic', 
                                                                                                                            inline = F
                                                                                                               ), #radioButtons #CORR_species_corr_color
                                                                                                               selectInput(inputId = 'CORR_species_corr_dist', 
                                                                                                                           label = 'Distance measure:', 
                                                                                                                           choices = c('Pearson' = 'pearson', 
                                                                                                                                       'Spearman' = 'spearman', 
                                                                                                                                       'Kendall'='kendall',
                                                                                                                                       "Euclidean" = "euclidean",
                                                                                                                                       "Maximum" = "maximum",
                                                                                                                                       "Manhattan" = "manhattan",
                                                                                                                                       "Canberra" = "canberra",
                                                                                                                                       "Binary" = "binary",
                                                                                                                                       "Minkowski" = "minkowski"),
                                                                                                                           selected = 'spearman', 
                                                                                                                           multiple = F
                                                                                                               ), #selectInput #CORR_species_corr_dist
                                                                                                               selectInput(inputId = 'CORR_species_corr_hclust', 
                                                                                                                           label = 'Clustering method:', 
                                                                                                                           choices = c('Complete' = 'complete', 
                                                                                                                                       'Single' = 'single', 
                                                                                                                                       'Median'='median', 
                                                                                                                                       'Average' = 'average', 
                                                                                                                                       "Ward.D" = "ward.D",
                                                                                                                                       "Ward.D2" = "ward.D2",
                                                                                                                                       "WPGMA" = "mcquitty",
                                                                                                                                       "WOGMC" = "median",
                                                                                                                                       "UPGMC" = "centroid"), 
                                                                                                                           selected = 'average', 
                                                                                                                           multiple = F
                                                                                                               ) #selectInput #CORR_species_corr_hclust
                                                                                                        ) #column
                                                                                                    ), #div #CORR_species_corr_reset_div
                                                                                                    column(width = 12),
                                                                                                    column(width = 4),
                                                                                                    column(width = 8, 
                                                                                                           actionButton(inputId = 'CORR_species_corr_reset', 
                                                                                                                        label = 'Reset', icon = icon('redo')
                                                                                                           ), #actionButton #CORR_species_corr_reset
                                                                                                           actionButton(inputId = 'CORR_species_corr_start', 
                                                                                                                        label = 'Submit', icon = icon('play')
                                                                                                           ) #actionButton #CORR_species_corr_start
                                                                                                    ), #column
                                                                                                    style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:320px"
                                                                                                ), #div #CORR_species_corr_style_div
                                                                                                br(),
                                                                                                conditionalPanel(condition = 'input.CORR_species_corr_start',
                                                                                                                 div(id = 'CORR_species_corr_result_div', 
                                                                                                                     column(width = 12,
                                                                                                                            iheatmaprOutput(outputId = 'CORR.species.corr.heatmap', height = '100%') %>% withSpinner(), #plotlyOutput #CORR.species.corr.heatmap
                                                                                                                            br(),
                                                                                                                            column(width = 4),
                                                                                                                            column(width = 8,
                                                                                                                                   downloadButton(outputId = 'CORR.species.corr.heatmap.matrix', label = 'Download matrix'),
                                                                                                                                   br(),
                                                                                                                                   br(),
                                                                                                                                   br(),
                                                                                                                                   br()
                                                                                                                            ) #column
                                                                                                                     ) #column
                                                                                                                 ) #div #CORR_species_corr_result_div
                                                                                                ) #conditionalPanel
                                                                                         ) #column
                                                                                ), #tabPanel #Correlation
                                                                                #############################
                                                                                ####  Linear regression  ####
                                                                                #############################
                                                                                tabPanel(title = 'Linear regression', 
                                                                                         h3('Linear regression'), 
                                                                                         column(width = 12,
                                                                                                h6('Linear regression is a statistical technique that uses several explanatory variables to predict the outcome of a continuous response variable,
                                                                                               allowing researchers to estimate the associations between lipid levels and clinical features.
                                                                                               For multiple linear regression analysis, additional variables in ‘adjusted table’ will be added into the algorithm and used to adjust the confounding effect.
                                                                                               Once calculation completes, each lipid species will be assigned a beta coefficient and t statistic (p-value), which can be chosen for clustering.', 
                                                                                                   style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px"),
                                                                                                br(),
                                                                                                div(id = 'CORR_species_linear_style_div', 
                                                                                                    div(id = 'CORR_species_linear_reset_div', 
                                                                                                        column(width = 4, 
                                                                                                               #textOutput(outputId = 'CORR.species.linear.adj.factor'), #textOutput #CORR.species.linear.adj.factor
                                                                                                               radioButtons(inputId = 'CORR_species_linear_adj_stat_method', 
                                                                                                                            label = 'Multiple testing correction:', 
                                                                                                                            choices = c('Benjamini & Hochberg' = 'BH'), 
                                                                                                                            selected = 'BH', 
                                                                                                                            inline = T
                                                                                                               ) #radioButtons #CORR_species_linear_adj_stat_method
                                                                                                        ), #column
                                                                                                        column(width = 4, 
                                                                                                               selectInput(inputId = 'CORR_species_linear_sig_p', 
                                                                                                                           label = 'Identify significant lipids:', 
                                                                                                                           choices = c('p-value' = 'p', 
                                                                                                                                       'adjusted p-value' = 'p.adj'), 
                                                                                                                           selected = 'p', 
                                                                                                                           multiple = F
                                                                                                               ), #selectInput #CORR_species_linear_sig_p
                                                                                                               numericInput(inputId = 'CORR_species_linear_pval', 
                                                                                                                            label = 'p-value:', 
                                                                                                                            value = 1, 
                                                                                                                            min = 0, 
                                                                                                                            max = 1
                                                                                                               ) #numericInput #CORR_species_linear_pval
                                                                                                        ), #column
                                                                                                        column(width = 4, 
                                                                                                               radioButtons(inputId = 'CORR_species_linear_color', 
                                                                                                                            label = 'Value for clustering:', 
                                                                                                                            choices = c('beta coefficient' = 'beta_coef', 
                                                                                                                                        't statistics' = 't_statistic'), 
                                                                                                                            selected = 't_statistic', 
                                                                                                                            inline = T
                                                                                                               ), #radioButtons #CORR_species_linear_color
                                                                                                               selectInput(inputId = 'CORR_species_linear_dist', 
                                                                                                                           label = 'Distance measure:', 
                                                                                                                           choices = c('Pearson' = 'pearson', 
                                                                                                                                       'Spearman' = 'spearman', 
                                                                                                                                       'Kendall'='kendall',
                                                                                                                                       "Euclidean" = "euclidean",
                                                                                                                                       "Maximum" = "maximum",
                                                                                                                                       "Manhattan" = "manhattan",
                                                                                                                                       "Canberra" = "canberra",
                                                                                                                                       "Binary" = "binary",
                                                                                                                                       "Minkowski" = "minkowski"),
                                                                                                                           selected = 'pearson', 
                                                                                                                           multiple = F
                                                                                                               ), #selectInput #CORR_species_linear_dist
                                                                                                               selectInput(inputId = 'CORR_species_linear_hclust', 
                                                                                                                           label = 'Clustering method:', 
                                                                                                                           choices = c('Complete' = 'complete', 
                                                                                                                                       'Single' = 'single', 
                                                                                                                                       'Median'='median', 
                                                                                                                                       'Average' = 'average', 
                                                                                                                                       "Ward.D" = "ward.D",
                                                                                                                                       "Ward.D2" = "ward.D2",
                                                                                                                                       "WPGMA" = "mcquitty",
                                                                                                                                       "WOGMC" = "median",
                                                                                                                                       "UPGMC" = "centroid"), 
                                                                                                                           selected = 'centroid', 
                                                                                                                           multiple = F
                                                                                                               ) #selectInput #CORR_species_linear_hclust
                                                                                                        ) #column
                                                                                                    ), #div #CORR_species_linear_reset_div
                                                                                                    column(width = 12),
                                                                                                    column(width = 4),
                                                                                                    column(width = 8, 
                                                                                                           actionButton(inputId = 'CORR_species_linear_reset', 
                                                                                                                        label = 'Reset', icon = icon('redo')
                                                                                                           ), #actionButton #CORR_species_linear_reset
                                                                                                           actionButton(inputId = 'CORR_species_linear_start', 
                                                                                                                        label = 'Submit', icon = icon('play')
                                                                                                           ) #actionButton #CORR_species_linear_start
                                                                                                    ), #column
                                                                                                    style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:290px"
                                                                                                ), #div #CORR_species_linear_style_div
                                                                                                br(),
                                                                                                conditionalPanel(condition = 'input.CORR_species_linear_start',
                                                                                                                 div(id = 'CORR_species_linear_result_div', 
                                                                                                                     column(width = 12,
                                                                                                                            iheatmaprOutput(outputId = 'CORR.species.linear.heatmap', height = '100%') %>% withSpinner(), #plotlyOutput #CORR.species.linear.heatmap
                                                                                                                            br(),
                                                                                                                            column(width = 4),
                                                                                                                            column(width = 8,
                                                                                                                                   downloadButton(outputId = 'CORR.species.linear.heatmap.matrix', label = 'Download matrix'),
                                                                                                                                   br(),
                                                                                                                                   br(),
                                                                                                                                   br(),
                                                                                                                                   br()
                                                                                                                            ) #column
                                                                                                                     ) #column
                                                                                                                 ) #div #CORR_species_linear_result_div
                                                                                                ) #conditionalPanel
                                                                                         ) #column
                                                                                ) #tabPanel #Linear regression
                                                                   ) #navlistPanel
                                                          ), #tabPanel #Lipid species analysis
                                                          tabPanel(title = 'Lipid characteristics analysis', 
                                                                   br(),
                                                                   navlistPanel(widths = c(3, 9), 
                                                                                id = 'CORR_class_list',
                                                                                #######################
                                                                                ####  Correlation  ####
                                                                                #######################
                                                                                tabPanel(title = 'Correlation', 
                                                                                         h3('Correlation'), 
                                                                                         column(width = 12,
                                                                                                div(id = 'CORR_class_description_style_div',
                                                                                                    h6('The Correlation Coefficient gives a summary view that tells researchers whether a relationship exists between clinical features and user-defined lipid characteristics,
                                                                                                   how strong that relationship is and whether the relationship is positive or negative. Here we provide three types of correlations, Pearson, Spearman, and Kendall,
                                                                                                   and adjusted by Benjamini & Hochberg methods. The cut-offs for correlation coefficient and the p-value can be decided by users. '),
                                                                                                    h6('A heatmap will show after users inputting cut-offs and choosing a value for clustering/methods for clustering.
                                                                                                   Users can use either correlation coefficient between clinical features (e.g. genes) and lipid characteristics or choose their statistic instead.'),
                                                                                                    style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px"),
                                                                                                br(),
                                                                                                div(id = 'CORR_class_corr_style_div', 
                                                                                                    div(id = 'CORR_class_corr_reset_div', 
                                                                                                        column(width = 4, 
                                                                                                               selectInput(inputId = 'CORR_class_corr_lipid_char', 
                                                                                                                           label = 'Select the lipid characteristic:', 
                                                                                                                           choices = 'class', 
                                                                                                                           multiple = F
                                                                                                               ), #selectInput #CORR_class_corr_lipid_char
                                                                                                               selectInput(inputId = 'CORR_class_corr_method', 
                                                                                                                           label = 'Correlation method:', 
                                                                                                                           choices = c('Pearson' = 'pearson', 
                                                                                                                                       #'Kendall'='kendall',
                                                                                                                                       'Spearman' = 'spearman'), 
                                                                                                                           selected = 'pearson', 
                                                                                                                           multiple = F
                                                                                                               ), #selectInput #CORR_class_corr_method
                                                                                                               radioButtons(inputId = 'CORR_class_corr_adj_stat_method', 
                                                                                                                            label = 'Multiple testing correction:', 
                                                                                                                            choices = c('Benjamini & Hochberg' = 'BH'), 
                                                                                                                            selected = 'BH', 
                                                                                                                            inline = T
                                                                                                               ) #radioButtons #CORR_class_corr_adj_stat_method
                                                                                                        ), #column
                                                                                                        column(width = 4, 
                                                                                                               selectInput(inputId = 'CORR_class_corr_sig_p', 
                                                                                                                           label = 'Identify significant lipids:', 
                                                                                                                           choices = c('p-value' = 'p', 
                                                                                                                                       'adjusted p-value' = 'p.adj'), 
                                                                                                                           selected = 'p.adj', 
                                                                                                                           multiple = F
                                                                                                               ), #selectInput #CORR_class_corr_sig_p
                                                                                                               numericInput(inputId = 'CORR_class_corr_pval', 
                                                                                                                            label = 'p-value:', 
                                                                                                                            value = 1, 
                                                                                                                            min = 0, 
                                                                                                                            max = 1
                                                                                                               ), #numericInput #CORR_class_corr_pval
                                                                                                               numericInput(inputId = 'CORR_class_corr_coef', 
                                                                                                                            label = 'Correlation coefficient cutoff:', 
                                                                                                                            value = 0, 
                                                                                                                            min = 0, 
                                                                                                                            max = 1, 
                                                                                                                            step = 0.1
                                                                                                               )%>% #numericInput #CORR_class_corr_coef
                                                                                                                 helper(type = "inline",
                                                                                                                        title = "Cutoff of correlation coefficientg Help",
                                                                                                                        content = c("A coefficient of <0.1 indicates a negligible and >0.9 a very strong relationship, values in-between are disputable."))
                                                                                                        ), #column
                                                                                                        column(width = 4, 
                                                                                                               radioButtons(inputId = 'CORR_class_corr_color', 
                                                                                                                            label = 'Value for clustering:', 
                                                                                                                            choices = c('correlation coefficient' = 'cor_coef', 
                                                                                                                                        'statistics' = 'statistic'), 
                                                                                                                            selected = 'statistic', 
                                                                                                                            inline = F
                                                                                                               ), #radioButtons #CORR_class_corr_color
                                                                                                               selectInput(inputId = 'CORR_class_corr_dist', 
                                                                                                                           label = 'Distance measure:', 
                                                                                                                           choices = c('Pearson' = 'pearson', 
                                                                                                                                       'Spearman' = 'spearman', 
                                                                                                                                       'Kendall'='kendall',
                                                                                                                                       "Euclidean" = "euclidean",
                                                                                                                                       "Maximum" = "maximum",
                                                                                                                                       "Manhattan" = "manhattan",
                                                                                                                                       "Canberra" = "canberra",
                                                                                                                                       "Binary" = "binary",
                                                                                                                                       "Minkowski" = "minkowski"),
                                                                                                                           selected = 'spearman', 
                                                                                                                           multiple = F
                                                                                                               ), #selectInput #CORR_class_corr_dist
                                                                                                               selectInput(inputId = 'CORR_class_corr_hclust', 
                                                                                                                           label = 'Clustering method:', 
                                                                                                                           choices = c('Complete' = 'complete', 
                                                                                                                                       'Single' = 'single', 
                                                                                                                                       'Median'='median', 
                                                                                                                                       'Average' = 'average', 
                                                                                                                                       "Ward.D" = "ward.D",
                                                                                                                                       "Ward.D2" = "ward.D2",
                                                                                                                                       "WPGMA" = "mcquitty",
                                                                                                                                       "WOGMC" = "median",
                                                                                                                                       "UPGMC" = "centroid"), 
                                                                                                                           selected = 'average', 
                                                                                                                           multiple = F
                                                                                                               ) #selectInput #CORR_class_corr_hclust
                                                                                                        ) #column
                                                                                                    ), #div #CORR_class_corr_reset_div
                                                                                                    column(width = 12),
                                                                                                    column(width = 4),
                                                                                                    column(width = 8, 
                                                                                                           actionButton(inputId = 'CORR_class_corr_reset', 
                                                                                                                        label = 'Reset', icon = icon('redo')
                                                                                                           ), #actionButton #CORR_class_corr_reset
                                                                                                           actionButton(inputId = 'CORR_class_corr_start', 
                                                                                                                        label = 'Submit', icon = icon('play')
                                                                                                           ) #actionButton #CORR_class_corr_start
                                                                                                    ), #column
                                                                                                    style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:320px"
                                                                                                ), #div #CORR_class_corr_style_div
                                                                                                br(),
                                                                                                conditionalPanel(condition = 'input.CORR_class_corr_start',
                                                                                                                 div(id = 'CORR_class_corr_result_div', 
                                                                                                                     column(width = 12,
                                                                                                                            iheatmaprOutput(outputId = 'CORR.class.corr.heatmap', height = '100%') %>% withSpinner(), #plotlyOutput #CORR.class.corr.heatmap
                                                                                                                            br(),
                                                                                                                            column(width = 4),
                                                                                                                            column(width = 8,
                                                                                                                                   downloadButton(outputId = 'CORR.class.corr.heatmap.matrix', label = 'Download matrix'), 
                                                                                                                                   br(),
                                                                                                                                   br(),
                                                                                                                                   br(),
                                                                                                                                   br()
                                                                                                                            ) #column
                                                                                                                     ) #column
                                                                                                                 ) #div #CORR_class_corr_result_div
                                                                                                ) #conditionalPanel
                                                                                         ) #column
                                                                                ), #tabPanel #Correlation
                                                                                #############################
                                                                                ####  Linear regression  ####
                                                                                #############################
                                                                                tabPanel(title = 'Linear regression', 
                                                                                         h3('Linear regression'), 
                                                                                         column(width = 12,
                                                                                                h6(p("Multiple linear regression is a statistical technique that uses several explanatory variables to predict the outcome of a response variable,
                                                                                                 allowing researchers to estimate the associations between lipid levels and clinical features (i.e., genetic polymorphisms).
                                                                                                 In this page, the lipids will be classified by the user-selected lipid characteristics (e.g. class), then implementing multiple linear regression analysis.
                                                                                                 Each variable (the pair of lipid characteristics and clinical features) will be assigned a beta coefficient and t statistic (p-value), which can be chosen for clustering.", 
                                                                                                     style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                br(),
                                                                                                div(id = 'CORR_class_linear_style_div', 
                                                                                                    div(id = 'CORR_class_linear_reset_div', 
                                                                                                        column(width = 4, 
                                                                                                               #textOutput(outputId = 'CORR.class.linear.adj.factor'), #textOutput #CORR.class.linear.adj.factor
                                                                                                               selectInput(inputId = 'CORR_class_linear_lipid_char', 
                                                                                                                           label = 'Select the lipid characteristic:', 
                                                                                                                           choices = 'class', 
                                                                                                                           multiple = F
                                                                                                               ), #selectInput #CORR_class_linear_lipid_char
                                                                                                               radioButtons(inputId = 'CORR_class_linear_adj_stat_method', 
                                                                                                                            label = 'Multiple testing correction:', 
                                                                                                                            choices = c('Benjamini & Hochberg' = 'BH'), 
                                                                                                                            selected = 'BH', 
                                                                                                                            inline = T
                                                                                                               ) #radioButtons #CORR_class_linear_adj_stat_method
                                                                                                        ), #column
                                                                                                        column(width = 4, 
                                                                                                               selectInput(inputId = 'CORR_class_linear_sig_p', 
                                                                                                                           label = 'Identify significant lipids:', 
                                                                                                                           choices = c('p-value' = 'p', 
                                                                                                                                       'adjusted p-value' = 'p.adj'), 
                                                                                                                           selected = 'p', 
                                                                                                                           multiple = F
                                                                                                               ), #selectInput #CORR_class_linear_sig_p
                                                                                                               numericInput(inputId = 'CORR_class_linear_pval', 
                                                                                                                            label = 'p-value:', 
                                                                                                                            value = 1, 
                                                                                                                            min = 0, 
                                                                                                                            max = 1
                                                                                                               ) #numericInput #CORR_class_linear_pval
                                                                                                        ), #column
                                                                                                        column(width = 4, 
                                                                                                               radioButtons(inputId = 'CORR_class_linear_color', 
                                                                                                                            label = 'Value for clustering:', 
                                                                                                                            choices = c('beta coefficient' = 'beta_coef', 
                                                                                                                                        't statistics' = 't_statistic'), 
                                                                                                                            selected = 't_statistic', 
                                                                                                                            inline = T
                                                                                                               ), #radioButtons #CORR_class_linear_color
                                                                                                               selectInput(inputId = 'CORR_class_linear_dist', 
                                                                                                                           label = 'Distance measure:', 
                                                                                                                           choices = c('Pearson' = 'pearson', 
                                                                                                                                       'Spearman' = 'spearman', 
                                                                                                                                       'Kendall'='kendall',
                                                                                                                                       "Euclidean" = "euclidean",
                                                                                                                                       "Maximum" = "maximum",
                                                                                                                                       "Manhattan" = "manhattan",
                                                                                                                                       "Canberra" = "canberra",
                                                                                                                                       "Binary" = "binary",
                                                                                                                                       "Minkowski" = "minkowski"),
                                                                                                                           selected = 'pearson', 
                                                                                                                           multiple = F
                                                                                                               ), #selectInput #CORR_class_linear_dist
                                                                                                               selectInput(inputId = 'CORR_class_linear_hclust', 
                                                                                                                           label = 'Clustering method:', 
                                                                                                                           choices = c('Complete' = 'complete', 
                                                                                                                                       'Single' = 'single', 
                                                                                                                                       'Median'='median', 
                                                                                                                                       'Average' = 'average', 
                                                                                                                                       "Ward.D" = "ward.D",
                                                                                                                                       "Ward.D2" = "ward.D2",
                                                                                                                                       "WPGMA" = "mcquitty",
                                                                                                                                       "WOGMC" = "median",
                                                                                                                                       "UPGMC" = "centroid"), 
                                                                                                                           selected = 'centroid', 
                                                                                                                           multiple = F
                                                                                                               ) #selectInput #CORR_class_linear_hclust
                                                                                                        ) #column
                                                                                                    ), #div #CORR_class_linear_reset_div
                                                                                                    column(width = 12),
                                                                                                    column(width = 4),
                                                                                                    column(width = 8, 
                                                                                                           actionButton(inputId = 'CORR_class_linear_reset', 
                                                                                                                        label = 'Reset', icon = icon('redo')
                                                                                                           ), #actionButton #CORR_class_linear_reset
                                                                                                           actionButton(inputId = 'CORR_class_linear_start', 
                                                                                                                        label = 'Submit', icon = icon('play')
                                                                                                           ) #actionButton #CORR_class_linear_start
                                                                                                    ), #column
                                                                                                    style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:290px"
                                                                                                ), #div #CORR_class_linear_style_div
                                                                                                br(),
                                                                                                conditionalPanel(condition = 'input.CORR_class_linear_start',
                                                                                                                 div(id = 'CORR_class_linear_result_div', 
                                                                                                                     column(width = 12,
                                                                                                                            iheatmaprOutput(outputId = 'CORR.class.linear.heatmap', height = '100%') %>% withSpinner(), #plotlyOutput #CORR.class.linear.heatmap
                                                                                                                            br(),
                                                                                                                            column(width = 4),
                                                                                                                            column(width = 8,
                                                                                                                                   downloadButton(outputId = 'CORR.class.linear.heatmap.matrix', label = 'Download matrix'), 
                                                                                                                                   br(),
                                                                                                                                   br(),
                                                                                                                                   br(),
                                                                                                                                   br()
                                                                                                                            ) #column
                                                                                                                     ) #column
                                                                                                                 ) #div #CORR_class_linear_result_div
                                                                                                ) #conditionalPanel
                                                                                         ) #column
                                                                                ) #tabPanel #Linear regression
                                                                   ) #navlistPanel
                                                          ) #tabPanel #Lipid category analysis
                                              ) #tabsetPanel
                                              ) #conditionalPanel
                             ) #div #CORR_tabPanel_div
                         
                         ) #column
                  ) #fluidRow
) #tabPanel




