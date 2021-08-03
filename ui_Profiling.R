
tabPanel(title = h4('Profiling'),
         value = 'Profiling',
         #### Profiling Header Panel ####
         h1('Profiling analysis'), 
         br(),
         fluidRow(column(width=12,
                         ######################################
                         ####  Profiling Page Description  ####
                         ######################################
                         div(id = 'PRO_description_style_div', 
                             h6('Lipidomics technology provides a fast and high-throughput screening to identify thousands of lipid species in cells,
                                tissues or other biological samples and has been broadly used in several areas of studies.
                                In this page, we present an overview that gathers comprehensive analyses that allow researchers to explore the quality and the clustering of samples,
                                correlation between lipids and samples, and the expression and composition of lipids.'),
                             style="background-color: PowderBlue;border-left: 8px solid Teal;padding: 15px"
                             ), #div #PRO_description_style_div
                         br(),
                         #################################
                         ####  Profiling Data Source  ####
                         #################################
                         h2('Data Source'),
                         sidebarLayout(fluid = T,
                                       sidebarPanel(width = 4, 
                                                    radioButtons(inputId = 'PRO_data_source', 
                                                                 label = h4('Data source'), 
                                                                 choices = c('Example dataset (PLoS Genet. 2018)' = 'PRO_demo_data', 
                                                                             'Upload your data!' = 'PRO_user_data'), 
                                                                 selected = 'PRO_demo_data'
                                                                 )%>% #radioButtons #PRO_data_source
                                                      helper(type = "inline",
                                                             title = "Data source",
                                                             size ="l",
                                                             content = c('<ol style="font-size: 0px;">',
                                                                         '<li style="font-size: 16px;">Lipid dataset can be uploaded by users or using example datasets.
                                                                         This information, namely Lipid expression data, and Lipid characteristics (optional),
                                                                         all data needs to be uploaded in 
                                                                         <mark style="background-color: white;color: red;">CSV</mark> or <mark style="background-color: white;color: red;">TSV</mark> format. The maximum file size is 30MB.</li>',
                                                                         '<li style="font-size: 16px;">Once two files are chosen and shown ‘Upload complete’ then press ‘Upload’.</li>',
                                                                         '</ol>')),
                                                    conditionalPanel(condition = 'input.PRO_data_source == "PRO_demo_data"', 
                                                                     actionButton(inputId = 'PRO_demo_upload', label = 'Submit', icon = icon('upload')), #actionButton #PRO_demo_upload
                                                                     downloadButton(outputId = 'PRO.demo.download', label = 'Download example')
                                                                     ), #conditionalPanel
                                                    conditionalPanel(condition = 'input.PRO_data_source == "PRO_user_data"',
                                                                     div(id = 'PRO_user_reset_div', 
                                                                         fileInput(inputId = 'PRO_user_exp', label = 'Lipid expression data:', accept = c(".csv",".tsv"), multiple = F)%>% #fileInput #PRO_user_exp
                                                                           helper(type = "inline",
                                                                                  title = "Lipid expression data",
                                                                                  size ="l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">The first column must contain a list of unique lipids names (features). NOTE: THE FEATURE LIST OF THE FIRST COLUMN MUST SAME AS ‘Lipid characteristics’!</li>',
                                                                                              '<li style="font-size: 16px;">Other columns encompass the expressed values of groups under different conditions that you want to compare.</li>',
                                                                                              '<li style="font-size: 16px;">An example of ‘Lipid expression data’</li>',
                                                                                              '<img src="Description/Profiling_Lipid expression data.png" width="100%"/>',
                                                                                              '</ol>')),
                                                                         fileInput(inputId = 'PRO_user_char', label = 'Lipid characteristics (optional):', accept = c(".csv",".tsv"), multiple = F)%>% #fileInput #PRO_user_char
                                                                           helper(type = "inline",
                                                                                  title = "Lipid characteristics",
                                                                                  size ="l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">The first column must contain a list of unique lipids name (features). NOTE: : THE FEATURE LIST OF THE FIRST COLUMN MUST SAME AS ‘Lipid expression data’!</li>',
                                                                                              '<li style="font-size: 16px;">Other columns can contain a wide variety of lipid characteristics, including class, total length, the total number of double bonds, or any other characteristics. The value can be the number of characters. </li>',
                                                                                              '<li style="font-size: 16px;">Due to the numbers of fatty acids attached to lipid are various, hence, if users assign the column name starting with “FA_”, system will automatically extract values separated by commas; Value between commas needs to be positive integer or zero.</li>',
                                                                                              '<li style="font-size: 16px;">If the name of characteristics matches ‘class’ (whole word only), the content must be characters, while ‘totallength’, ‘totaldb’, ‘totaloh’ (whole word only) must be numeric.</li>',
                                                                                              '<li style="font-size: 16px;">An example of ‘Lipid characteristics’</li>',
                                                                                              '<img src="Description/Profiling_Lipid characteristics.png" width="100%"/>',
                                                                                              '</ol>')),
                                                                         helpText("Upload your data table in .csv/.tsv"), #helpText
                                                                         br(),
                                                                         h4('Data processing')%>%
                                                                           helper(type = "inline",
                                                                                  title = "Data processing",
                                                                                  size ="l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">If you clicking on ‘Remove features with many missing values’, the threshold of the percentage of blank in the dataset that will be deleted can be defined by users.</li>',
                                                                                              '<img src="Description/Data processing_1.png" />',
                                                                                              '<li style="font-size: 16px;">The ‘Missing values imputation’ is for users to choose minimum, mean, and median to replace the missing values in the dataset.
                                                                                              If users select minimum, minimum will be multiplied by the value of user inputted. After uploading data, three datasets will show on the right-hand side.
                                                                                              When finishing checking data, click ‘Start’ for further analysis.</li>',
                                                                                              '<img src="Description/Data processing_2.png" />',
                                                                                              '<li style="font-size: 16px;">‘Percentage transformation’, ‘Log10 transformation’ can transform data into log10 or percentage.
                                                                                              The purpose of ‘Log10 transformation’ is to make highly skewed distributions less skewed, while ‘Percentage transformation’ is to standardize variation between groups.</li>',
                                                                                              '<img src="Description/Data processing_3.png" />',
                                                                                              '</ol>')),
                                                                         checkboxInput(inputId = "PRO_rm_NA", 
                                                                                       label = "Remove features with many missing values", 
                                                                                       value = T), #checkboxInput #PRO_rm_NA
                                                                         conditionalPanel(condition = 'input.PRO_rm_NA', 
                                                                                          numericInput(inputId = 'PRO_rm_NA_pct', 
                                                                                                       label = 'More than % missing values', 
                                                                                                       value = 50, 
                                                                                                       min = 5, 
                                                                                                       max = 100, 
                                                                                                       step = 5)
                                                                                          ), #conditionalPanel
                                                                         checkboxInput(inputId = "PRO_rp_NA", 
                                                                                       label = "Missing values imputation", 
                                                                                       value = T),
                                                                         conditionalPanel(condition = 'input.PRO_rp_NA',
                                                                                          selectInput(inputId = 'PRO_fill_NA',
                                                                                                      label = 'Fill missing value with:',
                                                                                                      choices = c('Mean' = 'mean', 
                                                                                                                  'Median' = 'median', 
                                                                                                                  'Minimum' = 'min'), 
                                                                                                      selected = 'min', 
                                                                                                      multiple = F),
                                                                                          conditionalPanel(condition = 'input.PRO_fill_NA == "min"', 
                                                                                                           numericInput(inputId = 'PRO_fill_min', 
                                                                                                                        label = 'Multiply by minimum', 
                                                                                                                        value = 0.5, 
                                                                                                                        min = 0.1, 
                                                                                                                        max = 0.5, 
                                                                                                                        step = 0.1)
                                                                                                           ) #conditionalPanel
                                                                                          ), #conditionalPanel
                                                                         checkboxInput(inputId = "PRO_pct_trans", 
                                                                                       label = "Percentage transformation", 
                                                                                       value = T),
                                                                         checkboxInput(inputId = "PRO_log_trans", 
                                                                                       label = "Log10 transformation", 
                                                                                       value = T)
                                                                         ), #div #PRO_user_reset_div
                                                                     actionButton(inputId = 'PRO_user_reset', 
                                                                                  label = 'Reset', icon = icon('redo')
                                                                                  ), #actionButton #PRO_user_reset
                                                                     actionButton(inputId = 'PRO_user_upload', 
                                                                                  label = 'Upload', icon = icon('upload')
                                                                                  ) #actionButton #PRO_user_upload
                                                                     ), #conditionalPanel
                                                    tags$p(actionLink("PRO_link_to_FAQ4", 
                                                                      "How to prepare your dataset?",
                                                                      style = "color: darkblue;"))
                                                    ), #sidebarPanel
                                       mainPanel(width = 8, 
                                                 conditionalPanel(condition = 'input.PRO_data_source == "PRO_demo_data" & input.PRO_demo_upload', 
                                                                  div(id = 'PRO_demo_mainPanel_div', 
                                                                      column(width = 12,
                                                                             div(id = 'PRO_demo_data_description_style_div', 
                                                                                 h3(p("Demo dataset",style="text-align: left")),
                                                                                 h6("Adipose tissue ATGL modifies the cardiac lipidome in pressure-overload-induced left Ventricular failure (PLoS Genet. 2018)"),
                                                                                 h6("Lipid expression data can be uploaded by users or example datasets are also provided. The" ,strong("Lipid expression data"),", and", strong("Lipid characteristics"), "(optional), needs to be uploaded in CSV format."),
                                                                                 style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px"
                                                                                 ), #div #PRO_demo_data_description_style_div
                                                                             br()
                                                                             ),
                                                                      column(width = 12,
                                                                             h4(strong('Lipid expression data')),
                                                                             tabsetPanel(id = 'PRO_demo_lipid_exp_tab',
                                                                                         tabPanel(title = "Processed data", 
                                                                                                  dataTableOutput(outputId = 'PRO.demo.exp') %>% withSpinner(), #dataTableOutput #PRO.demo.exp
                                                                                                  ),
                                                                                         tabPanel(title = "Raw data", 
                                                                                                  dataTableOutput(outputId = 'PRO.demo.exp.raw') %>% withSpinner(), #dataTableOutput #PRO.demo.exp.raw
                                                                                                  )
                                                                                         ), #tabsetPanel
                                                                             br(), 
                                                                             h4(strong('Lipid characteristics')),
                                                                             dataTableOutput(outputId = 'PRO.demo.lipid.char') %>% withSpinner(), #dataTableOutput #PRO.demo.lipid.char
                                                                             br()
                                                                             ), #column
                                                                      column(width = 5), #column
                                                                      column(width = 2, 
                                                                             actionButton(inputId = 'PRO_demo_start', 
                                                                                          label = 'Start!', icon = icon('play')
                                                                                          ) #actionButton #PRO_demo_start
                                                                             ), #column
                                                                      column(width = 5) #column
                                                                      ) #div #PRO_demo_mainPanel_div
                                                                  ), #conditionalPanel
                                                 conditionalPanel(condition = 'input.PRO_data_source == "PRO_user_data" & input.PRO_user_upload', 
                                                                  div(id = 'PRO_user_mainPanel_div', 
                                                                      column(width = 12,
                                                                             div(id = 'PRO_user_data_description_style_div', 
                                                                                 htmlOutput('PRO_Check_Exp_Data'),
                                                                                 htmlOutput('PRO_Check_lipid_char'),
                                                                                 htmlOutput('PRO_Data_summary'),
                                                                                 style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px"
                                                                                 ), #div #PRO_user_data_description_style_div
                                                                             helpText(tags$p(icon("check"),": Successfully uploaded.", HTML('&nbsp;'), icon("times"), ": Error happaned. Please check your dataset.", HTML('&nbsp;'), icon("exclamation"), ": Warning message.", style="font-size: 16px;")),
                                                                             br()
                                                                             ),
                                                                      column(width = 12,
                                                                             div(id = 'PRO_user_input_table_div',
                                                                                 h4(strong('Lipid expression data')),
                                                                                 tabsetPanel(id = 'PRO_user_lipid_exp_tab',
                                                                                             tabPanel(title = "Processed data", 
                                                                                                      dataTableOutput(outputId = 'PRO.user.exp') %>% withSpinner(), #dataTableOutput #PRO.user.exp
                                                                                                      ),
                                                                                             tabPanel(title = "Uploaded data", 
                                                                                                      dataTableOutput(outputId = 'PRO.user.exp.raw') %>% withSpinner(), #dataTableOutput #PRO.user.exp.raw
                                                                                                      )
                                                                                             ), #tabsetPanel
                                                                                 
                                                                                 br(),
                                                                                 h4(strong('Lipid characteristics')),
                                                                                 dataTableOutput(outputId = 'PRO.user.lipid.char') %>% withSpinner(), #dataTableOutput #PRO.user.lipid.char
                                                                                 br()
                                                                                 ) #div #PRO_user_input_table_div
                                                                             ), #column
                                                                      column(width = 5), #column
                                                                      column(width = 2, 
                                                                             actionButton(inputId = 'PRO_user_start', 
                                                                                          label = 'Start!', icon = icon('play')
                                                                                          ) #actionButton #PRO_user_start
                                                                             ), #column
                                                                      column(width = 5) #column
                                                                      ) #div #PRO_user_mainPanel_div
                                                                  ) #conditionalPanel
                                                 ) #mainPanel
                                       ), #sidebarLayout
                         hr(),
                         ##################################
                         ####  Profiling analysis tab  ####
                         ##################################
                         div(id = 'PRO_tabPanel_div',
                             div(id = 'PRO_result_div', 
                                 h2('Result'),
                                 ), #div #PRO_result_div
                             conditionalPanel(condition = '(input.PRO_data_source == "PRO_demo_data" & input.PRO_demo_start) | 
                                                    (input.PRO_data_source == "PRO_user_data" & input.PRO_user_start)',
                                              tabsetPanel(id = 'PRO_analysis_tab',
                                                          ####################################
                                                          ####  Cross-sample variability  ####
                                                          ####################################
                                                          tabPanel(title = 'Cross-sample variability',
                                                                   h3('Cross-sample variability'), 
                                                                   h6('In this page, three types of distribution plot provide a simple view of sample variability. The first histogram depicts the numbers of lipids expressed in each sample.
                                                                      The second histogram illustrates the total amount of lipid in each sample. The last density plot visualizes the underlying probability distribution of the lipid expression in each sample (line).
                                                                      Through these plots, users can easily compare the amount/expression difference of lipid between samples (i.e., patients vs. control).', 
                                                                        style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px"),
                                                                   br(),
                                                                   column(width = 6,
                                                                          plotlyOutput(outputId = 'PRO.num.of.expressed.lipid', height = '100%') %>% withSpinner()
                                                                   ),
                                                                   column(width = 6,
                                                                          plotlyOutput(outputId = 'PRO.lipid.amount', height = '100%') %>% withSpinner()
                                                                   ), 
                                                                   column(width = 6,
                                                                          plotlyOutput(outputId = 'PRO.expr.distribution', height = '100%') %>% withSpinner()
                                                                   )
                                                          ), #tabPanel #Cross-sample variability
                                                          ####################################
                                                          ####  Dimensionality reduction  ####
                                                          ####################################
                                                          tabPanel(title = 'Dimensionality reduction', 
                                                                   h3('Dimensionality reduction'), 
                                                                   h6("Dimensionality reduction is common when dealing with large numbers of observations and/or large numbers of variables in lipids analysis.
                                                                     It transforms data from a high-dimensional space into a low-dimensional space so that to retain vital properties of the original data and close to its intrinsic dimension.
                                                                     Three dimensionality reduction methods are provided in this page, PCA, t-SNE, UMAP.", 
                                                                        style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px"),
                                                                   br(),
                                                                   div(id = 'PRO_dim_redu_reset_div', 
                                                                       column(width = 5, 
                                                                              div(id = 'PRO_dim_redu_style1_div',
                                                                                  selectInput(inputId = 'PRO_dim_redu_method', 
                                                                                              label = 'Dimensionality reduction method:', 
                                                                                              choices = c('PCA' = 'pca', 
                                                                                                          #'PLS-DA' = 'plsda', 
                                                                                                          't-SNE' = 'tsne', 
                                                                                                          'UMAP' = 'umap'), 
                                                                                              selected = 'pca', 
                                                                                              multiple = FALSE, 
                                                                                              width = '80%'
                                                                                              ), #selectInput #PRO_dim_redu_method
                                                                                  conditionalPanel(condition = 'input.PRO_dim_redu_method == "pca"', 
                                                                                                   materialSwitch(inputId = 'PRO_pca_scale',
                                                                                                                  label = 'Scaling:', 
                                                                                                                  value = TRUE,
                                                                                                                  status = "primary",
                                                                                                                  right = FALSE) %>% 
                                                                                                     helper(type = "inline",
                                                                                                            title = "Scaling",
                                                                                                            content = c("Scaling (standardization) is advisable for data transformation when the variables in the original dataset have been measured on a significantly different scale.")), 
                                                                                                   materialSwitch(inputId = 'PRO_pca_center',
                                                                                                                  label = 'Centering:', 
                                                                                                                  value = TRUE,
                                                                                                                  status = "primary",
                                                                                                                  right = FALSE) %>%
                                                                                                     helper(type = "inline",
                                                                                                            title = "Centering",
                                                                                                            content = c("Mean-centring, subtracting the mean of each variable from the values, making the mean of each variable equal to zero.")),
                                                                                                   helpText("NOTE: Scaling and Centering are on by default. Centering is strongly recommended for pre-processing steps.")
                                                                                                   ), #conditionalPanel
                                                                                  conditionalPanel(condition = 'input.PRO_dim_redu_method == "tsne"', 
                                                                                                   materialSwitch(inputId = 'PRO_tsne_pca',
                                                                                                                  label = 'PCA:', 
                                                                                                                  value = TRUE,
                                                                                                                  status = "primary",
                                                                                                                  right = FALSE), #materialSwitch #PRO_tsne_pca
                                                                                                   numericInput(inputId = 'PRO_tsne_perplexity',
                                                                                                                label = 'Perplexity:', 
                                                                                                                value = 5, 
                                                                                                                min = 3, 
                                                                                                                max = 7, 
                                                                                                                step = 1,
                                                                                                                width = '80%') %>% 
                                                                                                     helper(type = "inline",
                                                                                                            title = "Perplexity",
                                                                                                            content = c("The perplexity may be considered as a knob that sets the number of effective nearest neighbours.
                                                                                                                    The typical perplexity range between 5 and 50.")),
                                                                                                   numericInput(inputId = 'PRO_tsne_max_iter',
                                                                                                                label = 'Number of iterations:', 
                                                                                                                value = 500, 
                                                                                                                min = 100, 
                                                                                                                max = 5000,
                                                                                                                step = 100, 
                                                                                                                width = '80%') %>% 
                                                                                                     helper(type = "inline",
                                                                                                            title = "Number of iterations",
                                                                                                            content = c("The number of iterations is the maximum number of iterations to perform."))
                                                                                                   ), #conditionalPanel
                                                                                  conditionalPanel(condition = 'input.PRO_dim_redu_method == "umap"', 
                                                                                                   materialSwitch(inputId = 'PRO_umap_scale',
                                                                                                                  label = 'Scaling:', 
                                                                                                                  value = TRUE,
                                                                                                                  status = "primary",
                                                                                                                  right = FALSE
                                                                                                                  ) %>% 
                                                                                                     helper(type = "inline",
                                                                                                            title = "Scaling",
                                                                                                            content = c("Scaling (standardization) is advisable for data transformation when the variables in the original dataset have been measured on a significantly different scale.")), 
                                                                                                   numericInput(inputId = 'PRO_umap_n_neighbors', 
                                                                                                                label = 'Number of neighbors:', 
                                                                                                                value = 15, 
                                                                                                                min = 2, 
                                                                                                                max = 23, 
                                                                                                                step = 1, 
                                                                                                                width = '80%') %>% 
                                                                                                     helper(type = "inline",
                                                                                                            title = "Number of neighbors",
                                                                                                            content = c("The number of neighbors (the number of neighbouring sample points), which is used for manifold approximation.
                                                                                                                    Larger values lead to more global views of the manifold, whilst smaller values result in more local data being preserved.
                                                                                                                    In general values should be in the range 2 to 100.")),
                                                                                                   selectInput(inputId = 'PRO_umap_metric', 
                                                                                                               label = 'Distance metric:', 
                                                                                                               choices = c('Euclidean' = 'euclidean',
                                                                                                                           'Cosine' = 'cosine',
                                                                                                                           'Manhattan' = 'manhattan',
                                                                                                                           #'Categorical' = 'categorical',
                                                                                                                           'Hamming' = 'hamming'), 
                                                                                                               selected = 'euclidean', 
                                                                                                               multiple = F, 
                                                                                                               width = '80%') %>% 
                                                                                                     helper(type = "inline",
                                                                                                            title = "Distance metric",
                                                                                                            content = c("The distance metric is use to find nearest neighbors."))
                                                                                                   ), #conditionalPanel
                                                                                  style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:340px"
                                                                                  ), #div #PRO_dim_redu_style1_div
                                                                              ), #column
                                                                       column(width = 5,
                                                                              div(id = 'PRO_dim_redu_style2_div',
                                                                                  selectInput(inputId = 'PRO_cluster_method', 
                                                                                              label = 'Clustering method:', 
                                                                                              choices = c('K-means' = 'kmeans', 
                                                                                                          'Partitioning around medoids (PAM)' = 'kmedoids', 
                                                                                                          'Hierarchical clustering' = 'hclustering', 
                                                                                                          'DBSCAN' = 'dbscan'), 
                                                                                              selected = 'kmeans', 
                                                                                              multiple = FALSE, 
                                                                                              width = '80%'
                                                                                              ), #selectInput #PRO_cluster_method
                                                                                  conditionalPanel(condition = 'input.PRO_cluster_method == "kmeans"',
                                                                                                   sliderInput(inputId = 'PRO_kmeans_group',
                                                                                                               label = 'Number of groups:', 
                                                                                                               value = 2, 
                                                                                                               min = 2, 
                                                                                                               max = 10,
                                                                                                               step = 1, 
                                                                                                               width = '80%'
                                                                                                               )
                                                                                                   ), #conditionalPanel
                                                                                  conditionalPanel(condition = 'input.PRO_cluster_method == "kmedoids"', 
                                                                                                   numericInput(inputId = 'PRO_pam_group',
                                                                                                                label = 'Number of groups:', 
                                                                                                                value = 2, 
                                                                                                                min = 1, 
                                                                                                                max = 10,
                                                                                                                step = 1,
                                                                                                                width = '80%'
                                                                                                                ),
                                                                                                   selectInput(inputId = 'PRO_pam_metric', 
                                                                                                               label = 'Distance metrics:', 
                                                                                                               choices = c('Euclidean' = 'euclidean', 
                                                                                                                           'Manhattan' = 'manhattan'), 
                                                                                                               selected = 'euclidean', 
                                                                                                               multiple = F, 
                                                                                                               width = '80%'
                                                                                                               ) #selectInput #PRO_pca_metric
                                                                                                   ), #conditionalPanel
                                                                                  conditionalPanel(condition = 'input.PRO_cluster_method == "hclustering"', 
                                                                                                   numericInput(inputId = 'PRO_hclust_group',
                                                                                                                label = 'Number of groups:', 
                                                                                                                value = 2, 
                                                                                                                min = 1, 
                                                                                                                max = 10,
                                                                                                                step = 1,
                                                                                                                width = '80%'
                                                                                                                ),
                                                                                                   selectInput(inputId = 'PRO_hclust_dist', 
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
                                                                                                               multiple = F, 
                                                                                                               width = '80%'
                                                                                                               ), #selectInput #PRO_pca_dist
                                                                                                   selectInput(inputId = 'PRO_hclust_hclust', 
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
                                                                                                               selected = 'complete', 
                                                                                                               multiple = F, 
                                                                                                               width = '80%'
                                                                                                               ) #selectInput #PRO_pca_hclust
                                                                                                   ), #conditionalPanel
                                                                                  conditionalPanel(condition = 'input.PRO_cluster_method == "dbscan"', 
                                                                                                   numericInput(inputId = 'PRO_dbscan_eps', 
                                                                                                                label = 'Epsilon:', 
                                                                                                                value = 0.5, 
                                                                                                                width = '80%'
                                                                                                                ), #numericInput #PRO_pca_eps
                                                                                                   numericInput(inputId = 'PRO_dbscan_minPts', 
                                                                                                                label = 'minPts:', 
                                                                                                                value = 1, 
                                                                                                                min = 1, 
                                                                                                                max = 22, 
                                                                                                                width = '80%'
                                                                                                                ) #numericInput #PRO_pca_minPts
                                                                                                   ), #conditionalPanel
                                                                                  style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:340px"
                                                                                  ), #div #PRO_dim_redu_style2_div
                                                                              ) #column
                                                                       ), #div #PRO_dim_redu_reset_div
                                                                   column(width = 2, 
                                                                          br(),
                                                                          br(),
                                                                          br(),
                                                                          br(),
                                                                          br(),
                                                                          br(),
                                                                          actionButton(inputId = 'PRO_dim_redu_start', label = 'Submit', icon = icon('play'), width = '60%'), #PRO_dim_redu_start
                                                                          br(),
                                                                          br(),
                                                                          actionButton(inputId = 'PRO_dim_redu_reset', label = 'Reset', icon = icon('redo'), width = '60%'), #actionButton #PRO_dim_redu_reset
                                                                          ), #column
                                                                   column(width = 12, br()),
                                                                   div(id = 'PRO_dim_redu_result_div', 
                                                                       conditionalPanel(condition = 'input.PRO_dim_redu_method == "pca"', 
                                                                                        column(width = 6, 
                                                                                               plotlyOutput(outputId = 'PRO.pca.biplot') %>% withSpinner(), #plotlyOutput #PRO.pca.biplot
                                                                                               ), #column
                                                                                        column(width = 6, 
                                                                                               plotlyOutput(outputId = 'PRO.pca.screeplot') %>% withSpinner(), #plotlyOutput #PRO.pca.screeplot
                                                                                               ), #column
                                                                                        column(width = 12, 
                                                                                               dataTableOutput(outputId = 'PRO.pca.rotated.data') %>% withSpinner(),
                                                                                               dataTableOutput(outputId = 'PRO.pca.contrib.table') %>% withSpinner()
                                                                                               ), 
                                                                                        br(),
                                                                                        br(),
                                                                                        sidebarLayout(fluid = T, 
                                                                                                      position = 'right', 
                                                                                                      sidebarPanel(width = 4, 
                                                                                                                   sliderInput(inputId = 'PRO_pca_variable_topN', 
                                                                                                                               label = 'top N feature:', 
                                                                                                                               min = 1, 
                                                                                                                               max = 30, 
                                                                                                                               value = 10,
                                                                                                                               step = 1)
                                                                                                                   ), #sidebarPanel
                                                                                                      mainPanel(width = 8, 
                                                                                                                plotlyOutput(outputId = 'PRO.pca.variable') %>% withSpinner() #plotlyOutput #PRO.pca.variable
                                                                                                                ) #mainPanel
                                                                                                      ), #sidebarLayout
                                                                                        br(),
                                                                                        br(),
                                                                                        sidebarLayout(fluid = T, 
                                                                                                      position = 'left', 
                                                                                                      sidebarPanel(width = 4, 
                                                                                                                   sliderInput(inputId = 'PRO_pca_contrib_topN', 
                                                                                                                               label = 'top N feature:', 
                                                                                                                               min = 1, 
                                                                                                                               max = 30, 
                                                                                                                               value = 10,
                                                                                                                               step = 1, 
                                                                                                                               width = '100%'),
                                                                                                                   selectInput(inputId = 'PRO_pca_contrib_PC', 
                                                                                                                               label = 'Contribution of features to principal component:', 
                                                                                                                               choices = c('Component 1' = 1, 
                                                                                                                                           'Component 2' = 2, 
                                                                                                                                           'Component 1 & Component 2'= '1_2'), 
                                                                                                                               selected = 'PC1_PC2', 
                                                                                                                               multiple = F)
                                                                                                                   ), #sidebarPanel
                                                                                                      mainPanel(width = 8, 
                                                                                                                plotlyOutput(outputId = 'PRO.pca.contrib') %>% withSpinner() #plotlyOutput #PRO.pca.contrib
                                                                                                                ) #mainPanel
                                                                                                      ), #sidebarLayout
                                                                                        br(),
                                                                                        br()
                                                                                        ), #conditionalPanel
                                                                       conditionalPanel(condition = 'input.PRO_dim_redu_method == "tsne"', 
                                                                                        column(width = 1),
                                                                                        column(width = 10,
                                                                                               plotlyOutput(outputId = 'PRO.tsne.plot', height = '600px') %>% withSpinner(),
                                                                                               br(),
                                                                                               dataTableOutput(outputId = 'PRO.tsne.table') %>% withSpinner()
                                                                                               )
                                                                                        ), #conditionalPanel
                                                                       conditionalPanel(condition = 'input.PRO_dim_redu_method == "umap"', 
                                                                                        column(width = 1),
                                                                                        column(width = 10,
                                                                                               plotlyOutput(outputId = 'PRO.umap.plot', height = '600px') %>% withSpinner(),
                                                                                               br(),
                                                                                               dataTableOutput(outputId = 'PRO.umap.table') %>% withSpinner()
                                                                                               )
                                                                                        ) #conditionalPanel
                                                                       ) #div #PRO_dim_redu_result_div
                                                                   ), #tabPanel #Dimensionality reduction
                                                          ###############################
                                                          ####  Correlation heatmap  ####
                                                          ###############################
                                                          tabPanel(title = 'Correlation heatmap',
                                                                   h3('Correlation heatmap'), 
                                                                   h6(p("Correlation heatmap illustrates the correlation between lipid species or samples, and also depicts the patterns in each group.
                                                                  The correlation can be calculated by Pearson or Spearman. The correlation coefficient then be clustered depend on user-defined method and distance.
                                                                  Two heatmaps will then be shown by lipid species or by samples.",
                                                                        style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                   br(),
                                                                   div(id = 'PRO_corr_heatmap_style_div', 
                                                                       div(id = 'PRO_corr_heatmap_reset_div', 
                                                                           column(width=3, 
                                                                                  selectInput(inputId = 'PRO_corr_heatmap_method', 
                                                                                              label = 'Correlation method:', 
                                                                                              choices = c('Pearson' = 'pearson',
                                                                                                          #'Kendall'='kendall',
                                                                                                          'Spearman' = 'spearman'), 
                                                                                              selected = 'spearman', 
                                                                                              multiple = F
                                                                                              ) #selectInput #PRO_corr_heatmap_method
                                                                                  ), #column
                                                                           column(width = 3,
                                                                                  selectInput(inputId = 'PRO_corr_heatmap_dist', 
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
                                                                                              selected = 'maximum', 
                                                                                              multiple = F
                                                                                              ) #selectInput #PRO_corr_heatmap_dist
                                                                                  ), #column
                                                                           column(width=3, 
                                                                                  selectInput(inputId = 'PRO_corr_heatmap_hclust', 
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
                                                                                              ) #selectInput #PRO_corr_heatmap_hclust
                                                                                  ), #column
                                                                           ), #div #PRO_corr_heatmap_reset_div
                                                                       column(width = 3, 
                                                                              br(),
                                                                              actionButton(inputId = 'PRO_corr_heatmap_reset', label = 'Reset', icon = icon('redo'), width = '40%'), #actionButton #PRO_corr_heatmap_reset
                                                                              actionButton(inputId = 'PRO_corr_heatmap_start', label = 'Submit', icon = icon('play'), width = '40%') #actionButton #PRO_corr_heatmap_start
                                                                              ), #column
                                                                       style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:95px"
                                                                   ), #div #PRO_corr_heatmap_style_div
                                                                   br(),
                                                                   conditionalPanel(condition = 'input.PRO_corr_heatmap_start', 
                                                                                    column(width = 6,
                                                                                           h4('By lipid species'),
                                                                                           iheatmaprOutput(outputId = 'PRO.corr.heatmap.lipid', height = '520px') %>% withSpinner(), 
                                                                                           br(),
                                                                                           column(width = 4),
                                                                                           column(width = 4,
                                                                                                  downloadButton(outputId = 'PRO.corr.heatmap.lipid.matrix', label = 'Download matrix')
                                                                                                  )
                                                                                           ),
                                                                                    column(width = 6,
                                                                                           h4('By samples'),
                                                                                           iheatmaprOutput(outputId = 'PRO.corr.heatmap.sample', height = '520px') %>% withSpinner(), 
                                                                                           br(),
                                                                                           column(width = 4),
                                                                                           column(width = 4,
                                                                                                  downloadButton(outputId = 'PRO.corr.heatmap.sample.matrix', label = 'Download matrix')
                                                                                                  )
                                                                                           )
                                                                                    ) #conditionalPanel
                                                                   ), #tabPanel #Correlation heatmap
                                                          ##########################################
                                                          ####  Lipid characteristics analysis  ####
                                                          ##########################################
                                                          tabPanel(title = 'Lipid characteristics profiling',
                                                                   h3('Lipid characteristics profiling'), 
                                                                   h6(p("In this page, users can discover lipid expression over specific lipid characteristics by scrolling dropdown menu.
                                                                    Lipids will be firstly classified by the selected characteristics from ‘Lipid characteristics’ table uploaded by users. Next,
                                                                    the lipid expression will be shown in bar plot, which depicts the expression level of each sample within each group (e.g., PE, PC) of selected characteristics (e.g., class).
                                                                    Additionally, a stacked horizontal bar chart reveals the percentage of characteristics in each sample.
                                                                    For instance, if users select class as lipid characteristics from the dropdown menu, the stacked bar chart will tell users the percentage of TAG, ST, SM etc. of each sample,
                                                                    the variability of percentage between samples can also be obtained from this plot.", 
                                                                        style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                   br(),
                                                                   column(width = 4),
                                                                   column(width = 8, 
                                                                          selectInput(inputId = 'PRO_lipid_char', 
                                                                                      label = 'Select the lipid characteristic:', 
                                                                                      choices = 'class', 
                                                                                      multiple = F
                                                                                      ), #selectInput #PRO_lipid_char
                                                                          ), #column
                                                                   br(),
                                                                   br(),
                                                                   column(width = 1),
                                                                   column(width = 10, 
                                                                          plotlyOutput(outputId = 'PRO.lipid.char.barplot', height = '500px') %>% withSpinner(),
                                                                          br(),
                                                                          br(),
                                                                          plotlyOutput(outputId = 'PRO.lipid.char.composition', height = '600px') %>% withSpinner(),
                                                                          br(),
                                                                          br()
                                                                          ) #column
                                                                   ) #tabPanel #Lipid characteristics analysis
                                                          ) #tabsetPanel
                                              ) #conditionalPanel
                             ) #div #PRO_tabPanel_div
                         ) #column
                  ), #fluidRow
         ) #tabPanel



