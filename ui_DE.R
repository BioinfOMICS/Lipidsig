
tabPanel(title = h4('Differential expression'),
         value = 'Differential expression',
         #### DE Header Panel ####
         h1('Differential expression analysis'), 
         br(),
         fluidRow(column(width=12,
                         ###############################
                         ####  DE Page Description  ####
                         ###############################
                         div(id = 'DE_description_style_div', 
                             h6("In Differential Expression Page, significant lipid species or lipid characteristics can be explored through two main customised analysis,
                                by ", strong("‘Lipid species’"), "or by ", strong("‘Lipid characteristics’"),", with user-uploaded data. Subsequently, further analysis and visualisation methods,
                                including dimensionality reduction, hierarchical clustering, characteristics analysis, and enrichment,
                                can be implemented based on the results of differential expressed analysis by utilising user-defined methods and characteristics."),
                             style="background-color: PowderBlue;border-left: 8px solid Teal;padding: 15px"
                             ), #div #DE_description_style_div
                         br(),
                         ##########################
                         ####  DE Data Source  ####
                         ##########################
                         h2('Data Source'),
                         sidebarLayout(fluid = T,
                                       sidebarPanel(width = 4,
                                                    radioButtons(inputId = 'DE_data_source', 
                                                                 label = h4('Data source'), 
                                                                 choices = c('Example dataset (PLoS Genet. 2018)' = 'DE_demo_data', 
                                                                             'Upload your data!' = 'DE_user_data'), 
                                                                 selected = 'DE_demo_data') %>% #radioButtons #DE_data_source
                                                      helper(type = "inline",
                                                             title = "Data source",
                                                             size ="l",
                                                             content = c('<ol style="font-size: 0px;">',
                                                                         '<li style="font-size: 16px;">Lipid dataset can be uploaded by users or using example datasets. This information, namely Lipid expression data, Group information and Lipid characteristics (optional), all data needs to be uploaded in 
                                                                         <mark style="background-color: white;color: red;">CSV</mark> or <mark style="background-color: white;color: red;">TSV</mark> format. The maximum file size is 30MB.</li>',
                                                                         '<li style="font-size: 16px;">Once two files are chosen and shown ‘Upload complete’ then press ‘Upload’.</li>',
                                                                         '</ol>')),
                                                    conditionalPanel(condition = 'input.DE_data_source == "DE_demo_data"', 
                                                                     actionButton(inputId = 'DE_demo_upload', label = 'Submit', icon = icon('upload')), #actionButton #DE_demo_upload
                                                                     downloadButton(outputId = 'DE.demo.download', label = 'Download example')
                                                                     ), #conditionalPanel
                                                    conditionalPanel(condition = 'input.DE_data_source == "DE_user_data"',
                                                                     div(id = 'DE_user_reset_div', 
                                                                         fileInput(inputId = 'DE_user_exp', label = 'Lipid expression data:', accept = c(".csv",".tsv"),multiple = F)%>% #fileInput #DE_user_exp
                                                                           helper(type = "inline",
                                                                                  title = "Lipid expression data",
                                                                                  size ="l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">The first column must contain a list of unique lipids name(features). NOTE: THE FEATURE LIST OF THE FIRST COLUMN MUST SAME AS ‘Lipid characteristics’!</li>',
                                                                                              '<li style="font-size: 16px;">Other columns encompass the expressed values of groups under different conditions that you want to compare.</li>',
                                                                                              '<li style="font-size: 16px;">An example of ‘Lipid expression data’</li>',
                                                                                              '<img src="Description/DE_Lipid expression data.png" width="100%"/>',
                                                                                              '</ol>')),
                                                                         fileInput(inputId = 'DE_user_group', label = 'Group information:', accept = c(".csv",".tsv"),multiple = F)%>% #fileInput #DE_user_group
                                                                           helper(type = "inline",
                                                                                  title = "Group information",
                                                                                  size ="l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">Columns must be 4 or 3 columns, arranging in order of sample_name, label_name, group, pair(optional). Note that the first 3 columns cannot contain NA or blanks.</li>',
                                                                                              '<li style="font-size: 16px;">‘sample_name’ must same as the name of samples of ‘Lipid expression data’!</li>',
                                                                                              '<li style="font-size: 16px;">‘lable_name’ give each group labels, such as ctrl1, treatment1.</li>',
                                                                                              '<li style="font-size: 16px;">‘group’ separates samples into 2 groups. The name of reference group will be input by users.</li>',
                                                                                              '<li style="font-size: 16px;">The ‘pair’ column (optional) can be added if your sample wants to be calculated in pair, you must assign each pair to a specific number, starting from 1 to N, cannot have NA, blank, or skip numbers. i.e., if OVCAR8_ctrl1 & OVCAR8_sgAGPS1 are a pair, input 1 in column ‘pair’ for both rows</li>',
                                                                                              '<li style="font-size: 16px;">At least 1 sample must be included for each sample</li>',
                                                                                              '<li style="font-size: 16px;">An example of ‘Group Information’</li>',
                                                                                              '<img src="Description/DE_Group Information.png" width="75%"/>')),
                                                                         fileInput(inputId = 'DE_user_char', label = 'Lipid characteristics (optional):', accept = c(".csv",".tsv"),multiple = F)%>% #fileInput #DE_user_char
                                                                           helper(type = "inline",
                                                                                  title = "Lipid characteristics",
                                                                                  size ="l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">The first column must contain a list of unique lipids name (features). NOTE: : THE FEATURE LIST OF THE FIRST COLUMN MUST SAME AS ‘Lipid expression data’!</li>',
                                                                                              '<li style="font-size: 16px;">Other columns can contain a wide variety of lipid characteristics, including class, total length, the total number of double bonds, or any other characteristics. The value can be the number of characters. </li>',
                                                                                              '<li style="font-size: 16px;">Due to the numbers of fatty acids attached to lipid are various, hence, if users assign the column name starting with “FA_”, system will automatically extract values separated by commas; Value between commas needs to be positive integer or zero.</li>',
                                                                                              '<li style="font-size: 16px;">If the name of characteristics matches ‘class’ (whole word only), the content must be characters, while ‘totallength’, ‘totaldb’, ‘totaloh’ (whole word only) must be numeric</li>',
                                                                                              '<li style="font-size: 16px;">An example of ‘Lipid characteristics’</li>',
                                                                                              '<img src="Description/DE_Lipid characteristics.png" width="100%"/>',
                                                                                              '</ol>')),
                                                                         helpText("Upload your data table in .csv/.tsv"), #helpText
                                                                         textInput(inputId = 'DE_user_ref_group', 
                                                                                   label = 'Reference group (Control):')%>%
                                                                           helper(type = "inline",
                                                                                  title = "Reference group (Control):",
                                                                                  size ="l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<img src="Description/DE_Reference_group.png" width="70%"/>',
                                                                                              '</ol>')),
                                                                         br(),
                                                                         h4('Data processing')%>%
                                                                           helper(type = "inline",
                                                                                  title = "Data processing",
                                                                                  size ="l",
                                                                                  content = c('<ol style="font-size: 0px;">',
                                                                                              '<li style="font-size: 16px;">If you clicking on ‘Remove features with many missing values’, the threshold of the percentage of blank in the dataset that will be deleted can be defined by users.</li>',
                                                                                              '<img src="Description/Data processing_1.png" />',
                                                                                              '<li style="font-size: 16px;">The ‘Missing values imputation’ is for users to choose minimum, mean, and median to replace the missing values in the dataset. If users select minimum,
                                                                                              minimum will be multiplied by the value of user inputted. After uploading data, three datasets will show on the right-hand side. When finishing checking data, click ‘Start’ for further analysis.</li>',
                                                                                              '<img src="Description/Data processing_2.png" />',
                                                                                              '<li style="font-size: 16px;">‘Percentage transformation’, ‘Log10 transformation’ can transform data into log10 or percentage. The purpose of ‘Log10 transformation’ is to make highly skewed distributions less skewed,
                                                                                              while ‘Percentage transformation’ is to standardize variation between groups.</li>',
                                                                                              '<img src="Description/Data processing_3.png" />',
                                                                                              '</ol>')),
                                                                         checkboxInput(inputId = "DE_rm_NA", 
                                                                                       label = "Remove features with many missing values", 
                                                                                       value = T), 
                                                                         conditionalPanel(condition = 'input.DE_rm_NA', 
                                                                                          numericInput(inputId = 'DE_rm_NA_pct', 
                                                                                                       label = 'More than % missing values', 
                                                                                                       value = 50, 
                                                                                                       min = 5, 
                                                                                                       max = 100, 
                                                                                                       step = 5)
                                                                                          ), #conditionalPanel
                                                                         checkboxInput(inputId = "DE_rp_NA", 
                                                                                       label = "Missing values imputation", 
                                                                                       value = T),
                                                                         conditionalPanel(condition = 'input.DE_rp_NA',
                                                                                          selectInput(inputId = 'DE_fill_NA',
                                                                                                      label = 'Fill missing value with:',
                                                                                                      choices = c('Mean' = 'mean', 
                                                                                                                  'Median' = 'median', 
                                                                                                                  'Minimum' = 'min'), 
                                                                                                      selected = 'min', 
                                                                                                      multiple = F),
                                                                                          conditionalPanel(condition = 'input.DE_fill_NA == "min"', 
                                                                                                           numericInput(inputId = 'DE_fill_min', 
                                                                                                                        label = 'Multiply by minimum', 
                                                                                                                        value = 0.5, 
                                                                                                                        min = 0.1, 
                                                                                                                        max = 0.5, 
                                                                                                                        step = 0.1)
                                                                                                           ) #conditionalPanel
                                                                                          ), #conditionalPanel
                                                                         checkboxInput(inputId = "DE_pct_trans", 
                                                                                       label = "Percentage transformation", 
                                                                                       value = T),
                                                                         checkboxInput(inputId = "DE_log_trans", 
                                                                                       label = "Log10 transformation", 
                                                                                       value = T)
                                                                         ), #div #DE_user_reset_div
                                                                     actionButton(inputId = 'DE_user_reset', label = 'Reset', icon = icon('redo')), #actionButton #DE_user_reset
                                                                     actionButton(inputId = 'DE_user_upload', label = 'Upload', icon = icon('upload')) #actionButton #DE_user_upload
                                                                     ), #conditionalPanel
                                                    tags$p(actionLink("DE_link_to_FAQ4", 
                                                                      "How to prepare your dataset?",
                                                                      style = "color: darkblue;"))
                                                    ), #sidebarPanel
                                       mainPanel(width = 8,
                                                 conditionalPanel(condition = 'input.DE_data_source == "DE_demo_data" & input.DE_demo_upload', 
                                                                  div(id = 'DE_demo_mainPanel_div', 
                                                                      column(width = 12,
                                                                             div(id = 'DE_demo_data_description_style_div', 
                                                                                 h3(p("Demo dataset",style="text-align: left")),
                                                                                 h6(p("Adipose tissue ATGL modifies the cardiac lipidome in pressure-overload-induced left Ventricular failure (PLoS Genet. 2018)",style="text-align:left")),
                                                                                 h6('Lipid dataset can be uploaded by users or using example datasets. This information, namely ',
                                                                                      strong('Lipid expression data'),', ',strong('Group information'),', ',strong('and Lipid characteristics'),
                                                                                      ', needs to be uploaded in CSV format.',style="text-align:left"),
                                                                                 h6('Human plasma lipidome from 10 healthy controls and 13 patients with systolic heart failure (HFrEF) were analyzed by MS-based shotgun lipidomics.
                                                                                    The data revealed dysregulation of individual lipid classes and lipid species in the presence of HFrEF.',style="text-align:left"),
                                                                                 style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px"
                                                                                 ), #div #DE_demo_data_description_style_div
                                                                             br()
                                                                             ),
                                                                      column(width = 12,
                                                                             h4(strong('Lipid expression data')),
                                                                             tabsetPanel(id = 'DE_demo_lipid_exp_tab',
                                                                               tabPanel(title = "Processed data", 
                                                                                        dataTableOutput(outputId = 'DE.demo.exp') %>% withSpinner(), #dataTableOutput #DE.demo.exp
                                                                                        ),
                                                                               tabPanel(title = "Raw data", 
                                                                                        dataTableOutput(outputId = 'DE.demo.exp.raw') %>% withSpinner(), #dataTableOutput #DE.demo.exp.raw
                                                                                        )
                                                                               ), #tabsetPanel
                                                                             br()
                                                                             ), #column
                                                                      column(width = 6, 
                                                                             h4(strong('Group information')),
                                                                             dataTableOutput(outputId = 'DE.demo.group.info') %>% withSpinner() #dataTableOutput #DE.demo.group.info
                                                                             ), #column
                                                                      column(width = 6, 
                                                                             h4(strong('Lipid characteristics')),
                                                                             dataTableOutput(outputId = 'DE.demo.lipid.char') %>% withSpinner() #dataTableOutput #DE.demo.lipid.char
                                                                             ), #column
                                                                      column(width = 12, br()), #column
                                                                      column(width = 5), #column
                                                                      column(width = 2, 
                                                                             actionButton(inputId = 'DE_demo_start', label = 'Start!', icon = icon('play')) #actionButton #DE_demo_start
                                                                             ), #column
                                                                      column(width = 5) #column
                                                                      ) #div #DE_demo_mainPanel_div
                                                                  ), #conditionalPanel
                                                 conditionalPanel(condition = 'input.DE_data_source == "DE_user_data" & input.DE_user_upload', 
                                                                  div(id = 'DE_user_mainPanel_div', 
                                                                      column(width = 12,
                                                                             div(id = 'DE_user_data_description_style_div', 
                                                                                 htmlOutput("DE_Check_Exp_Data"),
                                                                                 htmlOutput("DE_Check_Group_Data"),
                                                                                 htmlOutput("DE_Check_lipid_char"),
                                                                                 htmlOutput("DE_Data_summary"),
                                                                                 style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px"
                                                                                 ), #div #DE_user_data_description_style_div
                                                                             helpText(tags$p(icon("check"),": Successfully uploaded.", style="font-size: 16px;", HTML('&nbsp;'), icon("times"), ": Error happaned. Please check your dataset.", HTML('&nbsp;'), icon("exclamation"), ": Warning message.", style="font-size: 16px;")),
                                                                             br()
                                                                             ), 
                                                                      div(id = 'DE_user_input_table_div',
                                                                          column(width = 12,
                                                                                 h4(strong('Lipid expression data')),
                                                                                 tabsetPanel(id = 'DE_user_lipid_exp_tab',
                                                                                             tabPanel(title = "Processed data", 
                                                                                                      dataTableOutput(outputId = 'DE.user.exp') %>% withSpinner(), #dataTableOutput #DE.user.exp
                                                                                                      ),
                                                                                             tabPanel(title = "Uploaded data", 
                                                                                                      dataTableOutput(outputId = 'DE.user.exp.raw') %>% withSpinner(), #dataTableOutput #DE.user.exp.raw
                                                                                                      )
                                                                                             ), #tabsetPanel
                                                                                 br()
                                                                                 ), #column
                                                                          column(width = 6, 
                                                                                 h4(strong('Group information')),
                                                                                 dataTableOutput(outputId = 'DE.user.group.info') %>% withSpinner() #dataTableOutput #DE.user.group.info
                                                                                 ), #column
                                                                          column(width = 6, 
                                                                                 h4(strong('Lipid characteristics')),
                                                                                 dataTableOutput(outputId = 'DE.user.lipid.char') %>% withSpinner() #dataTableOutput #DE.user.lipid.char
                                                                                 ) #column
                                                                      ), #div #DE_user_input_table_div
                                                                      
                                                                      column(width = 12, br()), #column
                                                                      column(width = 5), #column
                                                                      column(width = 2, 
                                                                             actionButton(inputId = 'DE_user_start', label = 'Start!', icon = icon('play')) #actionButton #DE_user_start
                                                                             ), #column
                                                                      column(width = 5) #column
                                                                      ) #div #DE_user_mainPanel_div
                                                                  ) #conditionalPanel
                                                 ) #mainPanel
                                       ), #sidebarLayout
                         hr(),
                         ###########################
                         ####  DE analysis tab  ####
                         ###########################
                         div(id = 'DE_tabPanel_div',
                             div(id = 'DE_result_div', 
                                 h2('Result'),
                                 ), #div #DE_result_div
                             conditionalPanel(condition = '(input.DE_data_source == "DE_demo_data" & input.DE_demo_start) | 
                                                    (input.DE_data_source == "DE_user_data" & input.DE_user_start)', 
                                              tabsetPanel(id = 'DE_analysis_tab', 
                                                          tabPanel(title = 'Lipid species analysis', 
                                                                   br(),
                                                                   navlistPanel(widths = c(3, 9), 
                                                                                id = 'DE_species_list',
                                                                                'Step 1:', 
                                                                                ###########################################
                                                                                ####  Differential expressed analysis  ####
                                                                                ###########################################
                                                                                tabPanel(title = 'Differential expression', 
                                                                                         h3('Differential expression'), 
                                                                                         column(width = 12, 
                                                                                                h6(p("In lipid species analysis section, differentially expressed analysis is performed to find significant lipid species.
                                                                                          In short, samples will be divided into two groups (independent) based on the Group Information of input.
                                                                                          Two statistical methods, t-test and Wilcoxon test (Wilcoxon rank-sum test), are provided and p-value will be adjusted by Benjamini-Hochberg procedure.
                                                                                          The condition and cut-offs for significant lipid species are also users selected. ", 
                                                                                                     style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                br(),
                                                                                                div(id = 'DE_analysis_style_div', 
                                                                                                    div(id = 'DE_analysis_reset_div',
                                                                                                        column(width = 6, 
                                                                                                               #textOutput(outputId = 'DE.group.count'), #textOutput #DE.group.count
                                                                                                               #strong(textOutput(outputId = 'DE.pair.detect')), #textOutput #DE.pair.detect
                                                                                                               br(),
                                                                                                               selectInput(inputId = 'DE_stat_method', 
                                                                                                                           label = 'Method:', 
                                                                                                                           choices = c('t-test' = 't.test', 
                                                                                                                                       'Wilcoxon test' = 'wilcox.test'), 
                                                                                                                           selected = 't.test', 
                                                                                                                           multiple = F, 
                                                                                                                           width = '80%'
                                                                                                                           ), #selectInput #DE_stat_method
                                                                                                               radioButtons(inputId = 'DE_adj_stat_method', 
                                                                                                                            label = 'Multiple testing correction:', 
                                                                                                                            choices = c('Benjamini & Hochberg' = 'BH'), 
                                                                                                                            selected = 'BH', 
                                                                                                                            inline = T
                                                                                                                            ), #radioButtons #DE_adj_stat_method
                                                                                                               selectInput(inputId = 'DE_sig_p', 
                                                                                                                           label = 'Identify significant lipids:', 
                                                                                                                           choices = c('p-value' = 'p', 
                                                                                                                                       'adjusted p-value (padj)' = 'p.adj'), 
                                                                                                                           selected = 'p.adj', 
                                                                                                                           multiple = F, 
                                                                                                                           width = '80%'
                                                                                                                           ), #selectInput #DE_sig_p
                                                                                                        ), #column
                                                                                                        column(width = 6, 
                                                                                                               numericInput(inputId = 'DE_pval', 
                                                                                                                            label = 'p-value:', 
                                                                                                                            value = 0.05, 
                                                                                                                            min = 0.001, 
                                                                                                                            max = 0.05, 
                                                                                                                            width = '80%'
                                                                                                                            ), #numericInput #DE_pval
                                                                                                               numericInput(inputId = 'DE_fc', 
                                                                                                                            label = 'Fold change (FC):', 
                                                                                                                            value = 2, 
                                                                                                                            min = 1, 
                                                                                                                            max = 8, 
                                                                                                                            width = '80%'
                                                                                                                            ), #numericInput #DE_fc
                                                                                                               helpText("Please noted that the Fold change you filled in here means only the target lipid species with 2 fold up-regulation or down-regrulation than control will be selected and shown on plots and table.")
                                                                                                        ), #column
                                                                                                    ), #div #DE_analysis_reset_div
                                                                                                    column(width = 12),
                                                                                                    column(width = 7),
                                                                                                    column(width = 5, 
                                                                                                           actionButton(inputId = 'DE_analysis_reset', label = 'Reset', icon = icon('redo')), #actionButton #DE_analysis_reset
                                                                                                           actionButton(inputId = 'DE_analysis_start', label = 'Submit', icon = icon('play')) #actionButton #DE_analysis_start
                                                                                                           ), #column
                                                                                                    style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:360px"
                                                                                                ), #div #DE_analysis_style_div
                                                                                                br()
                                                                                         ), #column
                                                                                         conditionalPanel(condition = 'input.DE_analysis_start', 
                                                                                                          div(id = 'DE_analysis_result_div', 
                                                                                                              column(width = 12,
                                                                                                                     dataTableOutput(outputId = 'DE.species.tab.all') %>% withSpinner(), #dataTableOutput #DE.species.tab.all
                                                                                                                     br()
                                                                                                                     ), #column
                                                                                                              column(width = 1), #column
                                                                                                              div(id = 'DE_species_dotchart_sig',
                                                                                                                  column(width = 10, 
                                                                                                                         plotlyOutput(outputId = 'DE.species.dotchart.sig', height = '100%') %>% withSpinner() #plotlyOutput #DE.species.dotchart.sig
                                                                                                                         )#column
                                                                                                                  ),#div
                                                                                                              column(width = 1), #column
                                                                                                              column(width = 12, 
                                                                                                                     br(), 
                                                                                                                     uiOutput("MAPlotUI"), #uiOutput #MAPlotUI
                                                                                                                     br(), 
                                                                                                                     uiOutput("VolPlotUI"), #uiOutput #VolPlotUI
                                                                                                                     br(),
                                                                                                                     br()
                                                                                                                     ) #column
                                                                                                              ) #div #DE_analysis_result_div
                                                                                                          ) #conditionalPanel
                                                                                         ),#tabPanel #Differential expressed analysis
                                                                                'Step 2:',
                                                                                ####################################
                                                                                ####  Dimensionality reduction  ####
                                                                                ####################################
                                                                                tabPanel(title = 'Dimensionality reduction', 
                                                                                         h3('Dimensionality reduction'),
                                                                                         column(width=12,
                                                                                                h6(p("Dimensionality reduction is common when dealing with large numbers of observations and/or large numbers of variables in lipids analysis.
                                                                                          It transforms data from a high-dimensional space into a low-dimensional space so that to retain vital properties of the original data and close to its intrinsic dimension.", 
                                                                                                     style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                br(),
                                                                                                div(id = 'DE_species_dim_redu_reset_div', 
                                                                                                    column(width = 5,
                                                                                                           div(id = 'DE_species_dim_redu_style1_div',
                                                                                                               selectInput(inputId = 'DE_species_dim_redu_method', 
                                                                                                                           label = 'Dimensionality reduction method:', 
                                                                                                                           choices = c('PCA' = 'pca', 
                                                                                                                                       'PLS-DA' = 'plsda', 
                                                                                                                                       't-SNE' = 'tsne', 
                                                                                                                                       'UMAP' = 'umap'), 
                                                                                                                           selected = 'pca', 
                                                                                                                           multiple = FALSE, 
                                                                                                                           width = '100%'
                                                                                                                           ), #selectInput #DE_species_dim_redu_method
                                                                                                               conditionalPanel(condition = 'input.DE_species_dim_redu_method == "pca"', 
                                                                                                                                materialSwitch(inputId = 'DE_species_pca_scale',
                                                                                                                                               label = 'Scaling:', 
                                                                                                                                               value = TRUE,
                                                                                                                                               status = "primary",
                                                                                                                                               right = FALSE) %>% 
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Scaling",
                                                                                                                                         content = c("Scaling (standardization) is advisable for data transformation when the variables in the original dataset have been measured on a significantly different scale.")), 
                                                                                                                                materialSwitch(inputId = 'DE_species_pca_center',
                                                                                                                                               label = 'Centering:', 
                                                                                                                                               value = TRUE,
                                                                                                                                               status = "primary",
                                                                                                                                               right = FALSE) %>%
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Centering",
                                                                                                                                         content = c("Mean-centering, subtracting the mean of each variable from the values, making the mean of each variable equal to zero.")),
                                                                                                                                helpText("NOTE: Scaling and Centering are on by default. Centering is strongly recommended for pre-processing steps.")
                                                                                                                                ), #conditionalPanel
                                                                                                               conditionalPanel(condition = 'input.DE_species_dim_redu_method == "plsda"', 
                                                                                                                                materialSwitch(inputId = 'DE_species_plsda_scale',
                                                                                                                                               label = 'Scaling:', 
                                                                                                                                               value = TRUE,
                                                                                                                                               status = "primary",
                                                                                                                                               right = FALSE) %>% 
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Scaling",
                                                                                                                                         content = c("Scaling (standardization) is advisable for data transformation when the variables in the original dataset have been measured on a significantly different scale."))
                                                                                                                                ), #conditionalPanel
                                                                                                               conditionalPanel(condition = 'input.DE_species_dim_redu_method == "tsne"', 
                                                                                                                                materialSwitch(inputId = 'DE_species_tsne_pca',
                                                                                                                                               label = 'PCA:', 
                                                                                                                                               value = TRUE,
                                                                                                                                               status = "primary",
                                                                                                                                               right = FALSE
                                                                                                                                               ), #materialSwitch #DE_species_tsne_pca
                                                                                                                                numericInput(inputId = 'DE_species_tsne_perplexity',
                                                                                                                                             label = 'Perplexity:', 
                                                                                                                                             value = 5, 
                                                                                                                                             min = 3, 
                                                                                                                                             max = 7, 
                                                                                                                                             step = 1, 
                                                                                                                                             width = '100%') %>% 
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Perplexity",
                                                                                                                                         content = c("The perplexity may be considered as a knob that sets the number of effective nearest neighbours.
                                                                                                                    The typical perplexity range between 5 and 50.")),
                                                                                                                                numericInput(inputId = 'DE_species_tsne_max_iter',
                                                                                                                                             label = 'Number of iterations:', 
                                                                                                                                             value = 500, 
                                                                                                                                             min = 100, 
                                                                                                                                             max = 5000,
                                                                                                                                             step = 100, 
                                                                                                                                             width = '100%') %>% 
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Number of iterations",
                                                                                                                                         content = c("The number of iterations is the maximum number of iterations to perform."))
                                                                                                                                ), #conditionalPanel
                                                                                                               conditionalPanel(condition = 'input.DE_species_dim_redu_method == "umap"', 
                                                                                                                                materialSwitch(inputId = 'DE_species_umap_scale',
                                                                                                                                               label = 'Scaling:', 
                                                                                                                                               value = TRUE,
                                                                                                                                               status = "primary",
                                                                                                                                               right = FALSE
                                                                                                                                               ) %>% 
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Scaling",
                                                                                                                                         content = c("Scaling (standardization) is advisable for data transformation when the variables in the original dataset have been measured on a significantly different scale.")), 
                                                                                                                                numericInput(inputId = 'DE_species_umap_n_neighbors', 
                                                                                                                                             label = 'Number of neighbors:', 
                                                                                                                                             value = 15, 
                                                                                                                                             min = 2, 
                                                                                                                                             max = 23, 
                                                                                                                                             step = 1, 
                                                                                                                                             width = '100%') %>% 
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Number of neighbors",
                                                                                                                                         content = c("The number of neighbors (the number of neighbouring sample points), which is used for manifold approximation.
                                                                                                                    Larger values lead to more global views of the manifold, whilst smaller values result in more local data being preserved.
                                                                                                                    In general values should be in the range 2 to 100.")),
                                                                                                                                selectInput(inputId = 'DE_species_umap_metric', 
                                                                                                                                            label = 'Distance metric:', 
                                                                                                                                            choices = c('Euclidean' = 'euclidean',
                                                                                                                                                        'Cosine' = 'cosine',
                                                                                                                                                        'Manhattan' = 'manhattan',
                                                                                                                                                        #'Categorical' = 'categorical',
                                                                                                                                                        'Hamming' = 'hamming'), 
                                                                                                                                            selected = 'euclidean', 
                                                                                                                                            multiple = F, 
                                                                                                                                            width = '100%') %>% 
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Distance metric",
                                                                                                                                         content = c("The distance metric is use to find nearest neighbors"))
                                                                                                                                ), #conditionalPanel
                                                                                                               style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:340px"
                                                                                                               ) #div #DE_species_dim_redu_style1_div
                                                                                                           ), #column
                                                                                                    column(width = 5,
                                                                                                           div(id = 'DE_species_dim_redu_style2_div',
                                                                                                               selectInput(inputId = 'DE_species_cluster_method', 
                                                                                                                           label = 'Clustering method:', 
                                                                                                                           choices = c('Group information' = 'group_info', 
                                                                                                                                       'K-means' = 'kmeans', 
                                                                                                                                       'Partitioning around medoids (PAM)' = 'kmedoids', 
                                                                                                                                       'Hierarchical clustering' = 'hclustering', 
                                                                                                                                       'DBSCAN' = 'dbscan'), 
                                                                                                                           selected = 'group_info', 
                                                                                                                           multiple = FALSE, 
                                                                                                                           width = '100%'
                                                                                                                           ), #selectInput #DE_species_cluster_method
                                                                                                               conditionalPanel(condition = 'input.DE_species_cluster_method == "group_info"',
                                                                                                                                strong(textOutput(outputId = 'DE.species.group.count')), 
                                                                                                                                br()
                                                                                                                                ), #conditionalPanel
                                                                                                               conditionalPanel(condition = 'input.DE_species_cluster_method == "kmeans"',
                                                                                                                                sliderInput(inputId = 'DE_species_kmeans_group',
                                                                                                                                            label = 'Number of groups:', 
                                                                                                                                            value = 2, 
                                                                                                                                            min = 2, 
                                                                                                                                            max = 10,
                                                                                                                                            step = 1, 
                                                                                                                                            width = '100%'
                                                                                                                                            )
                                                                                                                                ), #conditionalPanel
                                                                                                               conditionalPanel(condition = 'input.DE_species_cluster_method == "kmedoids"', 
                                                                                                                                numericInput(inputId = 'DE_species_pam_group',
                                                                                                                                             label = 'Number of groups:', 
                                                                                                                                             value = 2, 
                                                                                                                                             min = 1, 
                                                                                                                                             max = 10,
                                                                                                                                             step = 1, 
                                                                                                                                             width = '100%'
                                                                                                                                             ),
                                                                                                                                selectInput(inputId = 'DE_species_pam_metric', 
                                                                                                                                            label = 'Distance metrics:', 
                                                                                                                                            choices = c('Euclidean' = 'euclidean', 
                                                                                                                                                        'Manhattan' = 'manhattan'), 
                                                                                                                                            selected = 'euclidean', 
                                                                                                                                            multiple = F, 
                                                                                                                                            width = '100%'
                                                                                                                                            ) #selectInput #DE_species_pca_metric
                                                                                                                                ), #conditionalPanel
                                                                                                               conditionalPanel(condition = 'input.DE_species_cluster_method == "hclustering"', 
                                                                                                                                numericInput(inputId = 'DE_species_hclust_group',
                                                                                                                                             label = 'Number of groups:', 
                                                                                                                                             value = 2, 
                                                                                                                                             min = 1, 
                                                                                                                                             max = 10,
                                                                                                                                             step = 1, 
                                                                                                                                             width = '100%'
                                                                                                                                             ),
                                                                                                                                selectInput(inputId = 'DE_species_hclust_dist', 
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
                                                                                                                                            width = '100%'
                                                                                                                                            ), #selectInput #DE_species_pca_dist
                                                                                                                                selectInput(inputId = 'DE_species_hclust_hclust', 
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
                                                                                                                                            width = '100%'
                                                                                                                                            ) #selectInput #DE_species_pca_hclust
                                                                                                                                ), #conditionalPanel
                                                                                                               conditionalPanel(condition = 'input.DE_species_cluster_method == "dbscan"', 
                                                                                                                                numericInput(inputId = 'DE_species_dbscan_eps', 
                                                                                                                                             label = 'Epsilon:', 
                                                                                                                                             value = 0.5, 
                                                                                                                                             width = '100%'
                                                                                                                                             ), #numericInput #DE_species_pca_eps
                                                                                                                                numericInput(inputId = 'DE_species_dbscan_minPts', 
                                                                                                                                             label = 'minPts:', 
                                                                                                                                             value = 1, 
                                                                                                                                             min = 1, 
                                                                                                                                             max = 22, 
                                                                                                                                             width = '100%'
                                                                                                                                             ) #numericInput #DE_species_pca_minPts
                                                                                                                                ), #conditionalPanel
                                                                                                               style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:340px"
                                                                                                               ) #div #DE_species_dim_redu_style2_div
                                                                                                           ) #column
                                                                                                    ), #div #DE_species_dim_redu_reset_div
                                                                                                column(width = 2, 
                                                                                                       br(),
                                                                                                       br(),
                                                                                                       br(),
                                                                                                       br(),
                                                                                                       br(),
                                                                                                       br(),
                                                                                                       actionButton(inputId = 'DE_species_dim_redu_start', label = 'Submit', icon = icon('play'), width = '100%'), #DE_species_dim_redu_start
                                                                                                       br(),
                                                                                                       br(),
                                                                                                       actionButton(inputId = 'DE_species_dim_redu_reset', label = 'Reset', icon = icon('redo'), width = '100%') #actionButton #DE_species_dim_redu_reset
                                                                                                ), #column
                                                                                                column(width = 12, br()),
                                                                                                div(id = 'DE_species_dim_redu_result_div', 
                                                                                                    conditionalPanel(condition = 'input.DE_species_dim_redu_method == "pca" & input.DE_species_dim_redu_start', 
                                                                                                                     column(width = 6, 
                                                                                                                            plotlyOutput(outputId = 'DE.species.pca.biplot', height = '320px') %>% withSpinner(), #plotlyOutput #DE.species.pca.biplot
                                                                                                                     ), #column
                                                                                                                     column(width = 6, 
                                                                                                                            plotlyOutput(outputId = 'DE.species.pca.screeplot', height = '320px') %>% withSpinner(), #plotlyOutput #DE.species.pca.screeplot
                                                                                                                     ), #column
                                                                                                                     column(width = 12, 
                                                                                                                            br(),
                                                                                                                            br(),
                                                                                                                            dataTableOutput(outputId = 'DE.species.pca.rotated.data') %>% withSpinner(),
                                                                                                                            br(),
                                                                                                                            br(),
                                                                                                                            dataTableOutput(outputId = 'DE.species.pca.contrib.table') %>% withSpinner(),
                                                                                                                            br(),
                                                                                                                            br(),
                                                                                                                            br()
                                                                                                                     ), 
                                                                                                                     sidebarLayout(fluid = T, 
                                                                                                                                   position = 'right', 
                                                                                                                                   sidebarPanel(width = 4, 
                                                                                                                                                sliderInput(inputId = 'DE_species_pca_variable_topN', 
                                                                                                                                                            label = 'top N feature:', 
                                                                                                                                                            min = 1, 
                                                                                                                                                            max = 30, 
                                                                                                                                                            value = 10,
                                                                                                                                                            step = 1)
                                                                                                                                   ), #sidebarPanel
                                                                                                                                   mainPanel(width = 8, 
                                                                                                                                             plotlyOutput(outputId = 'DE.species.pca.variable') %>% withSpinner() #plotlyOutput #DE.species.pca.variable
                                                                                                                                   ) #mainPanel
                                                                                                                     ), #sidebarLayout
                                                                                                                     br(),
                                                                                                                     br(),
                                                                                                                     sidebarLayout(fluid = T, 
                                                                                                                                   position = 'left', 
                                                                                                                                   sidebarPanel(width = 4, 
                                                                                                                                                sliderInput(inputId = 'DE_species_pca_contrib_topN', 
                                                                                                                                                            label = 'top N feature:', 
                                                                                                                                                            min = 1, 
                                                                                                                                                            max = 30, 
                                                                                                                                                            value = 10,
                                                                                                                                                            step = 1, 
                                                                                                                                                            width = '100%'),
                                                                                                                                                selectInput(inputId = 'DE_species_pca_contrib_PC', 
                                                                                                                                                            label = 'Contribution of features to principal component:', 
                                                                                                                                                            choices = c('Component 1' = 1, 
                                                                                                                                                                        'Component 2' = 2, 
                                                                                                                                                                        'Component 1 & Component 2'= '1_2'), 
                                                                                                                                                            selected = 'PC1_PC2', 
                                                                                                                                                            multiple = F)
                                                                                                                                   ), #sidebarPanel
                                                                                                                                   mainPanel(width = 8, 
                                                                                                                                             plotlyOutput(outputId = 'DE.species.pca.contrib') %>% withSpinner() #plotlyOutput #DE.species.pca.contrib
                                                                                                                                   ) #mainPanel
                                                                                                                     ), #sidebarLayout
                                                                                                                     br(),
                                                                                                                     br()
                                                                                                    ), #conditionalPanel
                                                                                                    conditionalPanel(condition = 'input.DE_species_dim_redu_method == "plsda" & input.DE_species_dim_redu_start', 
                                                                                                                     column(width = 6, 
                                                                                                                            plotlyOutput(outputId = 'DE.species.plsda.sample.plot') %>% withSpinner(), #plotlyOutput #DE.species.plsda.sample.plot
                                                                                                                     ), 
                                                                                                                     column(width = 6, 
                                                                                                                            plotlyOutput(outputId = 'DE.species.plsda.variable.plot') %>% withSpinner(), #plotlyOutput #DE.species.plsda.variable.plot
                                                                                                                     ), 
                                                                                                                     column(width = 12,
                                                                                                                            dataTableOutput(outputId = 'DE.species.plsda.variate.table') %>% withSpinner(),
                                                                                                                            dataTableOutput(outputId = 'DE.species.plsda.loading.table') %>% withSpinner()
                                                                                                                     )
                                                                                                    ), #conditionalPanel
                                                                                                    conditionalPanel(condition = 'input.DE_species_dim_redu_method == "tsne" & input.DE_species_dim_redu_start', 
                                                                                                                     column(width = 1),
                                                                                                                     column(width = 10,
                                                                                                                            plotlyOutput(outputId = 'DE.species.tsne.plot', height = '600px') %>% withSpinner(),
                                                                                                                            br(),
                                                                                                                            dataTableOutput(outputId = 'DE.species.tsne.table') %>% withSpinner()
                                                                                                                     )
                                                                                                    ), #conditionalPanel
                                                                                                    conditionalPanel(condition = 'input.DE_species_dim_redu_method == "umap" & input.DE_species_dim_redu_start', 
                                                                                                                     column(width = 1),
                                                                                                                     column(width = 10,
                                                                                                                            plotlyOutput(outputId = 'DE.species.umap.plot', height = '600px') %>% withSpinner(),
                                                                                                                            br(),
                                                                                                                            dataTableOutput(outputId = 'DE.species.umap.table') %>% withSpinner()
                                                                                                                     )
                                                                                                    ) #conditionalPanel
                                                                                                ) #div #DE_species_dim_redu_result_div
                                                                                         ) #column 
                                                                                ), #tabPanel #Dimensionality reduction
                                                                                ######################
                                                                                ####  Clustering  ####
                                                                                ######################
                                                                                tabPanel(title = 'Hierarchical clustering', 
                                                                                         h3('Hierarchical clustering'),
                                                                                         column(width=12,
                                                                                                h6(p("Lipid species that derived from two groups will be clustered and visualised on heatmap using hierarchical clustering.
                                                                                          Through heatmap, users may discover the difference between the two groups by observing the distribution of lipid species.
                                                                                          This analysis provides an overview of lipid species differences between the control group and the experimental group. ", 
                                                                                                     style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                br(),
                                                                                                div(id = 'DE_species_cluster_style_div', 
                                                                                                    div(id = 'DE_species_cluster_reset_div', 
                                                                                                        column(width=6, 
                                                                                                               selectInput(inputId = 'DE_species_cluster_by', 
                                                                                                                           label = 'Select feature for clustering:', 
                                                                                                                           choices = c('By all lipid species' = 'all_lipid', 
                                                                                                                                       'By significant lipid species' = 'sig_lipid'), 
                                                                                                                           selected = 'sig_lipid', 
                                                                                                                           multiple = F
                                                                                                               ) #selectInput #DE_species_cluster_by
                                                                                                        ), #column
                                                                                                        column(width = 6, 
                                                                                                               selectInput(inputId = 'DE_species_sidecolor', 
                                                                                                                           label = 'Side color by lipid characteristics:', 
                                                                                                                           choices = c('Without side color' = 'none',
                                                                                                                                       'Lipid class' = 'class'), 
                                                                                                                           selected = 'class', 
                                                                                                                           multiple = F
                                                                                                               ) #selectInput #DE_species_sidecolor
                                                                                                        ), #column
                                                                                                        column(width = 12), #column
                                                                                                        column(width = 6,
                                                                                                               selectInput(inputId = 'DE_species_dist', 
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
                                                                                                               ) #selectInput #DE_species_dist
                                                                                                        ), #column
                                                                                                        column(width=6, 
                                                                                                               selectInput(inputId = 'DE_species_hclust', 
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
                                                                                                                           multiple = F
                                                                                                               ) #selectInput #DE_species_hclust
                                                                                                        ), #column
                                                                                                    ), #div #DE_species_cluster_reset_div
                                                                                                    column(width = 7), #column
                                                                                                    column(width = 5, 
                                                                                                           actionButton(inputId = 'DE_species_cluster_reset', 
                                                                                                                        label = 'Reset', icon = icon('redo')
                                                                                                           ), #actionButton #DE_species_cluster_reset
                                                                                                           actionButton(inputId = 'DE_species_cluster_start', 
                                                                                                                        label = 'Submit', icon = icon('play')
                                                                                                           ) #actionButton #DE_species_cluster_start
                                                                                                    ), #column
                                                                                                    style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:230px"
                                                                                                ), #div #DE_species_cluster_style_div
                                                                                                br(),
                                                                                                div(id = 'DE_species_heatmap_div',
                                                                                                    conditionalPanel(condition = 'input.DE_species_cluster_start', 
                                                                                                                     iheatmaprOutput(outputId = 'DE.species.heatmap', height = '800px') %>% withSpinner(), #plotlyOutput #DE.species.cluster
                                                                                                                     column(width = 4), 
                                                                                                                     column(width = 8,
                                                                                                                            downloadButton(outputId = 'DE.species.heatmap.matrix', label = 'Download matrix')
                                                                                                                    )
                                                                                                   )
                                                                                                ) #conditionalPanel
                                                                                         ) #column
                                                                                ), #tabPanel #Clustering
                                                                                #################################
                                                                                ####  Lipid characteristics  ####
                                                                                #################################
                                                                                tabPanel(title = 'Characteristics association', 
                                                                                         h3(p('Characteristics association')),
                                                                                         column(width=12,
                                                                                                h6(p("In this part, we categorize significant lipid species based on different lipid characteristics and visualise the difference between control and experimental groups by applying log2 Fold Change.", 
                                                                                                     style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                br(),
                                                                                                column(width = 12),
                                                                                                column(width = 3),
                                                                                                column(width = 9, 
                                                                                                       selectInput(inputId = 'DE_species_lipid_char', 
                                                                                                                   label = 'Select the lipid characteristics:', 
                                                                                                                   choices = 'class', 
                                                                                                                   multiple = F
                                                                                                       ), #selectInput #DE_species_lipid_char
                                                                                                ), #column
                                                                                                br(), 
                                                                                                br(),
                                                                                                column(width = 12),
                                                                                                column(width = 1), 
                                                                                                column(width = 10, 
                                                                                                       plotlyOutput(outputId = 'DE.species.lipid.char.bar') %>% withSpinner(), #plotlyOutput #DE.species.lipid.char.bar
                                                                                                       br(), 
                                                                                                       plotlyOutput(outputId = 'DE.species.lipid.char.dot') %>% withSpinner(), #plotlyOutput #DE.species.lipid.char.dot
                                                                                                       br(), 
                                                                                                       hwordcloudOutput(outputId = 'DE.species.lipid.char.word') %>% withSpinner(), #hwordcloudOutput #DE.species.lipid.char.word
                                                                                                       br(), 
                                                                                                       br(), 
                                                                                                       br()
                                                                                                ), #column
                                                                                                column(width = 1)
                                                                                                
                                                                                         ) #column
                                                                                ), #tabPanel #Lipid characteristics
                                                                                ######################
                                                                                ####  Enrichment  ####
                                                                                ######################
                                                                                tabPanel(title = 'Enrichment', 
                                                                                         h3(p('Enrichment')),
                                                                                         column(width=12,
                                                                                                h6(p("Enrichment plot assists users to determine whether significant lipid species are enriched in the categories of the selected characteristics.
                                                                                          The enrichment plots and a summary table are further classified into up/down/all groups by log2 fold change of significant lipid species.
                                                                                          Each group (value) of the selected characteristics will have a value of significant count and p-value within a summary table.", 
                                                                                                     style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                br(),
                                                                                                div(id = 'DE_species_enrichment_style_div',
                                                                                                    div(id = 'DE_species_enrichment_reset_div',
                                                                                                        column(width = 3, 
                                                                                                               radioButtons(inputId = 'DE_species_enrichment_method', 
                                                                                                                            label = 'Method:', 
                                                                                                                            choices = c("Fisher's exact test" = 'fisher'), 
                                                                                                                            selected = 'fisher', 
                                                                                                                            inline = TRUE
                                                                                                                            ), #radioButtons #DE_species_enrichment_method
                                                                                                               ), 
                                                                                                        column(width = 4,
                                                                                                               selectInput(inputId = 'DE_species_enrichment_lipid_char', 
                                                                                                                           label = 'Select the lipid characteristics:',
                                                                                                                           choices = c('class'), 
                                                                                                                           selected = 'class'
                                                                                                                           ) %>% #selectInput #DE_species_enrichment_lipid_char
                                                                                                                 helper(type = "inline",
                                                                                                                        title = "Lipid characteristics",
                                                                                                                        content = c("NOTE: Note, a lipid species may have more than one fatty acid attached.
                                                                                                                  Thus, if the selected lipid characteristics are FA-related term, instead of counting species,
                                                                                                                  we decompose lipid species into FA then do the enrichment. 
                                                                                                                  Also, all data used in this analysis is derived from the results of the differentially expressed analysis.",
                                                                                                                                    " ",
                                                                                                                                    "Example: When choosing class, under ‘UP & DOWN’, the group of PC O- has 9 significant species out of 31 with a p-value of 0.017"))
                                                                                                               ), 
                                                                                                        column(width = 4, 
                                                                                                               numericInput(inputId = 'DE_species_enrichment_pval', 
                                                                                                                            label = 'p-value:', 
                                                                                                                            value = 0.05, 
                                                                                                                            min = 0.001, 
                                                                                                                            max = 0.05
                                                                                                                            ) #numericInput #DE_species_enrichment_pval
                                                                                                               )
                                                                                                    ), #div #DE_species_enrichment_reset_div
                                                                                                    column(width = 12),
                                                                                                    column(width = 4), #column
                                                                                                    column(width = 8, 
                                                                                                           actionButton(inputId = 'DE_species_enrichment_reset', label = 'Reset', icon = icon('redo')), #actionButton #DE_species_enrichment_reset
                                                                                                           actionButton(inputId = 'DE_species_enrichment_start', label = 'Submit', icon = icon('play')) #actionButton #DE_species_enrichment_start
                                                                                                    ), #column
                                                                                                    style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:160px"
                                                                                                ), #div #DE_species_enrichment_style_div
                                                                                                br(), 
                                                                                                div(id = 'DE_species_enrichment_result_div', 
                                                                                                    conditionalPanel(condition = 'input.DE_species_enrichment_start', 
                                                                                                                     column(width = 1),
                                                                                                                     column(width = 10, 
                                                                                                                            plotlyOutput(outputId = 'DE.species.enrichment.barplot', height = '600px') %>% withSpinner(), #plotlyOutput #DE.species.enrichment.barplot
                                                                                                                            br(), 
                                                                                                                            dataTableOutput(outputId = 'DE.species.enrichment.table') %>% withSpinner(), 
                                                                                                                            br(), 
                                                                                                                            br()
                                                                                                                     ), #column
                                                                                                                     column(width = 1)
                                                                                                    )#conditionalPanel
                                                                                                ), #div #DE_species_enrichment_result_div
                                                                                                div(id = 'DE_species_enrichment_pathview_start_div', 
                                                                                                    column(width = 12,
                                                                                                           br(),
                                                                                                           br()
                                                                                                    ), 
                                                                                                    column(width = 1),
                                                                                                    column(width = 5, 
                                                                                                           h4(p('KEGG pathway analysis'))
                                                                                                    ), 
                                                                                                    column(width = 6, 
                                                                                                           actionButton(inputId = 'DE_species_enrichment_pathview', 
                                                                                                                        label = 'Start analysis!', icon = icon('play')
                                                                                                           ) #actionButton #DE_species_enrichment_pathview
                                                                                                    )
                                                                                                ), #div #DE_species_enrichment_pathview_start_div
                                                                                                div(id = 'DE_species_enrichment_pathview_result_div', 
                                                                                                    column(width = 12, 
                                                                                                           br()
                                                                                                    ),
                                                                                                    conditionalPanel(condition = 'input.DE_species_enrichment_lipid_char == "class" & input.DE_species_enrichment_pathview', 
                                                                                                                     column(width = 1),
                                                                                                                     column(width = 10, 
                                                                                                                            #dataTableOutput(outputId = 'DE.species.enrichment.kegg.pathway', width = '100%') %>% withSpinner(),
                                                                                                                            reactableOutput(outputId = 'DE.species.enrichment.kegg.pathway') %>% withSpinner(),
                                                                                                                            br()
                                                                                                                     ), #column
                                                                                                                     column(width = 1), 
                                                                                                                     column(width = 12,
                                                                                                                            imageOutput(outputId = 'DE.species.enrichment.kegg.pathview', width = '100%')
                                                                                                                     ) #column
                                                                                                    ) #conditionalPanel
                                                                                                ) #div #DE_species_enrichment_pathview_result_div
                                                                                         ) #column
                                                                                ) #tabPanel #Enrichment
                                                                   ) #navlistPanel #DE_species_list
                                                          ), #tabPanel #Lipid species analysis
                                                          tabPanel(title = 'Lipid characteristics analysis', 
                                                                   br(),
                                                                   navlistPanel(widths = c(3, 9),
                                                                                id = 'DE_specific_list',
                                                                                'Step 1:', 
                                                                                ###########################################
                                                                                ####  Differential expressed analysis  ####
                                                                                ###########################################
                                                                                tabPanel(title = 'Differential expression', 
                                                                                         h3(p('Differential expression')),
                                                                                         column(width = 12,
                                                                                                h6('In ',strong('Lipid Characteristics Analysis'),' section, lipid species are categorised and summarised into new lipid expression table according to two selected lipid characteristic,
                                                                                                   then conducted differential expressed analysis. Samples will be divided into two groups based on the ',strong('Group Information'),' of input data. Two-way ANOVA is applied with t-test as post hoc tests. This ',
                                                                                                   strong('Differentially Expressed Analysis'),' section separates into 2 sections, analysing based on first ‘Characteristics’ and adding ',strong('‘Subgroup of characteristics’'),
                                                                                                   ' to the analysis. The first section is analysed based on the first selected ',strong('‘characteristics’'),
                                                                                                   '. The second section is the subgroup analysis of the first section.',
                                                                                                   style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px"),
                                                                                                br(),
                                                                                                div(id = 'DE_class_analysis_style_div', 
                                                                                                    div(id = 'DE_class_analysis_reset_div',
                                                                                                        column(width = 6, 
                                                                                                               selectInput(inputId = 'DE_class_analysis_char',
                                                                                                                           label = 'Characteristics:',
                                                                                                                           choices = c('totallength'), 
                                                                                                                           selected = 'totallength'
                                                                                                               ) #selectInput #DE_class_analysis_char
                                                                                                        ), #column
                                                                                                        column(width = 3, 
                                                                                                               radioButtons(inputId = 'DE_class_stat_method', 
                                                                                                                            label = 'Method:', 
                                                                                                                            choices = c('Two-way ANOVA' = 'two.way.anova'), 
                                                                                                                            selected = 'two.way.anova', 
                                                                                                                            inline = T
                                                                                                               ) #radioButtons #DE_class_stat_method
                                                                                                        ), #column
                                                                                                        column(width = 3, 
                                                                                                               radioButtons(inputId = 'DE_class_post_hoc_method', 
                                                                                                                            label = 'Post hoc tests:', 
                                                                                                                            choices = c('t-test' = 't.test'), 
                                                                                                                            selected = 't.test', 
                                                                                                                            inline = T
                                                                                                               ) #radioButtons #DE_class_adj_stat_method
                                                                                                        ), #column
                                                                                                        column(width = 12),
                                                                                                        column(width = 6, 
                                                                                                               selectInput(inputId = 'DE_class_split_char', 
                                                                                                                           label = 'Subgroup of characteristics:', 
                                                                                                                           choices = c('none' = 'none'), 
                                                                                                                           selected = 'none'
                                                                                                                           ), #selectInput #DE_class_split_char
                                                                                                               #textOutput(outputId = 'DE.class.group.count'), #textOutput #DE.group.count
                                                                                                               #br(),
                                                                                                               #strong(textOutput(outputId = 'DE.class.pair.detect')), #textOutput #DE.pair.detect
                                                                                                               helpText("NOTE: two selected characteristics should be both continuous data or one categorical data with one continuous data.")
                                                                                                        ), #column
                                                                                                        column(width = 6, 
                                                                                                               numericInput(inputId = 'DE_class_pval', 
                                                                                                                            label = 'p-value:', 
                                                                                                                            value = 0.05, 
                                                                                                                            min = 0.001, 
                                                                                                                            max = 0.05
                                                                                                               ), #numericInput #DE_class_pval
                                                                                                               numericInput(inputId = 'DE_class_fc', 
                                                                                                                            label = 'Fold change (FC):', 
                                                                                                                            value = 2, 
                                                                                                                            min = 1, 
                                                                                                                            max = 8
                                                                                                               ) #numericInput #DE_class_fc
                                                                                                        ), #column
                                                                                                    ), #div #DE_analysis_reset_div
                                                                                                    column(width = 12),
                                                                                                    column(width = 7),
                                                                                                    column(width = 5, 
                                                                                                           actionButton(inputId = 'DE_class_analysis_reset', 
                                                                                                                        label = 'Reset', icon = icon('redo')
                                                                                                           ), #actionButton #DE_class_analysis_reset
                                                                                                           actionButton(inputId = 'DE_class_analysis_start', 
                                                                                                                        label = 'Submit', icon = icon('play')
                                                                                                           ) #actionButton #DE_class_analysis_start
                                                                                                    ), #column
                                                                                                    style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:330px"
                                                                                                ), #div #DE_analysis_style_div
                                                                                                br()
                                                                                         ), #column
                                                                                         conditionalPanel(condition = 'DE_class_analysis_start', 
                                                                                                          div(id = 'DE_class_analysis_result_div', 
                                                                                                              column(width = 12,
                                                                                                                     dataTableOutput(outputId = 'DE.class.tab.all') %>% withSpinner(), #dataTableOutput #DE.class.tab.all
                                                                                                                     br()
                                                                                                                     ),
                                                                                                              tabsetPanel(#id = '',
                                                                                                                          tabPanel(title = 'Raw', 
                                                                                                                                   column(width = 12,
                                                                                                                                          plotlyOutput(outputId = 'DE.class.barplot', height = '500px') %>% withSpinner(), #plotlyOutput #DE.class.barplot
                                                                                                                                          br()
                                                                                                                                          ), #column
                                                                                                                                   column(width = 8, 
                                                                                                                                          plotlyOutput(outputId = 'DE.class.trendplot', height = '400px') %>% withSpinner(), #plotlyOutput #DE.class.trendplot
                                                                                                                                          ), #column
                                                                                                                                   column(width = 4,
                                                                                                                                          plotlyOutput(outputId = 'DE.class.boxplot', height = '400px') %>% withSpinner(), #plotlyOutput #DE.class.boxplot
                                                                                                                                          ), #column
                                                                                                                                   ), #tabPanel
                                                                                                                          tabPanel(title = 'Sqrt scale', 
                                                                                                                                   column(width = 12,
                                                                                                                                          plotlyOutput(outputId = 'DE.class.barplot.sqrt', height = '500px') %>% withSpinner(), #plotlyOutput #DE.class.barplot.sqrt
                                                                                                                                          br()
                                                                                                                                          ), #column
                                                                                                                                   column(width = 8, 
                                                                                                                                          plotlyOutput(outputId = 'DE.class.trendplot.sqrt', height = '400px') %>% withSpinner(), #plotlyOutput #DE.class.trendplot.sqrt
                                                                                                                                          ), #column
                                                                                                                                   column(width = 4,
                                                                                                                                          plotlyOutput(outputId = 'DE.class.boxplot.2', height = '400px') %>% withSpinner(), #plotlyOutput #DE.class.boxplot.2
                                                                                                                                          ), #column
                                                                                                                                   ) #tabPanel
                                                                                                                          ), #tabsetPanel
                                                                                                              column(width = 12, 
                                                                                                                     br()
                                                                                                              ) #column
                                                                                                          ), #div #DE_class_analysis_result_div
                                                                                                          div(id = 'DE_class_split_result_div', 
                                                                                                              column(width = 12, 
                                                                                                                     h4('Subgroup of characteristics:'),
                                                                                                                     h6('This is the ‘Subgroup of characteristics’ section.
                                                                                                                        In short, lipid species will be further split by the characteristic that user-chosen in the second pull-down menu then undergo the first section analysis.
                                                                                                                        Two-way ANOVA is also applied with t-test as post hoc tests, and the cut-offs of differentially expressed lipids are inputted by users.', 
                                                                                                                          style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px"),
                                                                                                                     br(), 
                                                                                                                     dataTableOutput(outputId = 'DE.class.split.tab.all') %>% withSpinner(), #dataTableOutput #DE.class.tab.all
                                                                                                                     br(), 
                                                                                                                     selectInput(inputId = 'DE_class_split_class',
                                                                                                                                 label = 'Select a category:',
                                                                                                                                 choices = c('TAG' = 'TAG'), 
                                                                                                                                 selected = 'TAG'
                                                                                                                     ), #selectInput #DE_class_split_class
                                                                                                                     br()
                                                                                                                     ), #column
                                                                                                              tabsetPanel(#id = '',
                                                                                                                          tabPanel(title = 'Raw', 
                                                                                                                                   column(width = 12,
                                                                                                                                          plotlyOutput(outputId = 'DE.class.split.barplot', height = '500px') %>% withSpinner(), #plotlyOutput #DE.class.split.barplot
                                                                                                                                          br()
                                                                                                                                          ), #column
                                                                                                                                   column(width = 8, 
                                                                                                                                          plotlyOutput(outputId = 'DE.class.split.trendplot', height = '400px') %>% withSpinner(), #plotlyOutput #DE.class.split.trendplot
                                                                                                                                          ), #column
                                                                                                                                   column(width = 4,
                                                                                                                                          plotlyOutput(outputId = 'DE.class.split.boxplot', height = '400px') %>% withSpinner(), #plotlyOutput #DE.class.split.boxplot
                                                                                                                                          ), #column
                                                                                                                                   column(width = 12,
                                                                                                                                          br(),
                                                                                                                                          br(),
                                                                                                                                          br(),
                                                                                                                                          br(),
                                                                                                                                          br(),
                                                                                                                                          br())
                                                                                                                                   ), #tabPanel
                                                                                                                          tabPanel(title = 'Sqrt scale', 
                                                                                                                                   column(width = 12,
                                                                                                                                          plotlyOutput(outputId = 'DE.class.split.barplot.sqrt', height = '500px') %>% withSpinner(), #plotlyOutput #DE.class.split.barplot.sqrt
                                                                                                                                          br()
                                                                                                                                          ), #column
                                                                                                                                   column(width = 8, 
                                                                                                                                          plotlyOutput(outputId = 'DE.class.split.trendplot.sqrt', height = '400px') %>% withSpinner(), #plotlyOutput #DE.class.split.trendplot.sqrt
                                                                                                                                          ), #column
                                                                                                                                   column(width = 4,
                                                                                                                                          plotlyOutput(outputId = 'DE.class.split.boxplot.2', height = '400px') %>% withSpinner(), #plotlyOutput #DE.class.split.boxplot.2
                                                                                                                                          ), #column
                                                                                                                                   column(width = 12,
                                                                                                                                          br(),
                                                                                                                                          br(),
                                                                                                                                          br(),
                                                                                                                                          br(),
                                                                                                                                          br(),
                                                                                                                                          br())
                                                                                                                                   ) #tabPanel
                                                                                                                          ) #tabsetPanel
                                                                                                          ) #div #DE_class_split_result_div
                                                                                         ), #conditionalPanel
                                                                                ), #tabPanel #Differential expressed analysis
                                                                                'Step 2:',
                                                                                ####################################
                                                                                ####  Dimensionality reduction  ####
                                                                                ####################################
                                                                                tabPanel(title = 'Dimensionality reduction', 
                                                                                         h3('Dimensionality reduction'),
                                                                                         column(width=12,
                                                                                                h6(p("Dimensionality reduction in this section assists users to tackle with large numbers of variables in lipids analysis.
                                                                                          The high-dimensional space is transformed into a low-dimensional space. Hence, the crucial properties of the lipid data are revealed and still close to its intrinsic characteristics.
                                                                                          Here, we provide four types of dimensionality reduction approaches, PCA, PLS-DA, t-SNE, UMAP, and four clustering methods, K-means, partitioning around medoids (PAM), Hierarchical clustering, and DBSCAN.", 
                                                                                                     style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                br(),
                                                                                                div(id = 'DE_class_dim_redu_reset_div', 
                                                                                                    column(width = 5,
                                                                                                           div(id = 'DE_class_dim_redu_style1_div',
                                                                                                               selectInput(inputId = 'DE_class_dim_redu_method', 
                                                                                                                           label = 'Dimensionality reduction method:', 
                                                                                                                           choices = c('PCA' = 'pca', 
                                                                                                                                       'PLS-DA' = 'plsda', 
                                                                                                                                       't-SNE' = 'tsne', 
                                                                                                                                       'UMAP' = 'umap'), 
                                                                                                                           selected = 'pca', 
                                                                                                                           multiple = FALSE, 
                                                                                                                           width = '100%'
                                                                                                               ), #selectInput #DE_class_dim_redu_method
                                                                                                               conditionalPanel(condition = 'input.DE_class_dim_redu_method == "pca"', 
                                                                                                                                materialSwitch(inputId = 'DE_class_pca_scale',
                                                                                                                                               label = 'Scaling:', 
                                                                                                                                               value = TRUE,
                                                                                                                                               status = "primary",
                                                                                                                                               right = FALSE) %>% 
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Scaling",
                                                                                                                                         content = c("Scaling (standardization) is advisable for data transformation when the variables in the original dataset have been measured on a significantly different scale.")), 
                                                                                                                                materialSwitch(inputId = 'DE_class_pca_center',
                                                                                                                                               label = 'Centering:', 
                                                                                                                                               value = TRUE,
                                                                                                                                               status = "primary",
                                                                                                                                               right = FALSE) %>%
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Centering",
                                                                                                                                         content = c("Mean-centering, subtracting the mean of each variable from the values, making the mean of each variable equal to zero.")),
                                                                                                                                helpText("NOTE: Scaling and Centering are on by default. Centering is strongly recommended for pre-processing steps.")
                                                                                                               ), #conditionalPanel
                                                                                                               conditionalPanel(condition = 'input.DE_class_dim_redu_method == "plsda"', 
                                                                                                                                materialSwitch(inputId = 'DE_class_plsda_scale',
                                                                                                                                               label = 'Scaling:', 
                                                                                                                                               value = TRUE,
                                                                                                                                               status = "primary",
                                                                                                                                               right = FALSE) %>% 
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Scaling",
                                                                                                                                         content = c("Scaling (standardization) is advisable for data transformation when the variables in the original dataset have been measured on a significantly different scale."))
                                                                                                               ), #conditionalPanel
                                                                                                               conditionalPanel(condition = 'input.DE_class_dim_redu_method == "tsne"', 
                                                                                                                                materialSwitch(inputId = 'DE_class_tsne_pca',
                                                                                                                                               label = 'PCA:', 
                                                                                                                                               value = TRUE,
                                                                                                                                               status = "primary",
                                                                                                                                               right = FALSE
                                                                                                                                ), #materialSwitch #DE_class_tsne_pca
                                                                                                                                numericInput(inputId = 'DE_class_tsne_perplexity',
                                                                                                                                             label = 'Perplexity:', 
                                                                                                                                             value = 5, 
                                                                                                                                             min = 3, 
                                                                                                                                             max = 7, 
                                                                                                                                             step = 1, 
                                                                                                                                             width = '100%') %>% 
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Perplexity",
                                                                                                                                         content = c("The perplexity may be considered as a knob that sets the number of effective nearest neighbours.
                                                                                                                    The typical perplexity range between 5 and 50.")),
                                                                                                                                numericInput(inputId = 'DE_class_tsne_max_iter',
                                                                                                                                             label = 'Number of iterations:', 
                                                                                                                                             value = 500, 
                                                                                                                                             min = 100, 
                                                                                                                                             max = 5000,
                                                                                                                                             step = 100, 
                                                                                                                                             width = '100%') %>% 
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Number of iterations",
                                                                                                                                         content = c("The number of iterations is the maximum number of iterations to perform."))
                                                                                                               ), #conditionalPanel
                                                                                                               conditionalPanel(condition = 'input.DE_class_dim_redu_method == "umap"', 
                                                                                                                                materialSwitch(inputId = 'DE_class_umap_scale',
                                                                                                                                               label = 'Scaling:', 
                                                                                                                                               value = TRUE,
                                                                                                                                               status = "primary",
                                                                                                                                               right = FALSE
                                                                                                                                ) %>% 
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Scaling",
                                                                                                                                         content = c("Scaling (standardization) is advisable for data transformation when the variables in the original dataset have been measured on a significantly different scale.")), 
                                                                                                                                numericInput(inputId = 'DE_class_umap_n_neighbors', 
                                                                                                                                             label = 'Number of neighbors:', 
                                                                                                                                             value = 15, 
                                                                                                                                             min = 2, 
                                                                                                                                             max = 23, 
                                                                                                                                             step = 1, 
                                                                                                                                             width = '100%') %>% 
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Number of neighbors",
                                                                                                                                         content = c("The number of neighbors (the number of neighbouring sample points), which is used for manifold approximation.
                                                                                                                    Larger values lead to more global views of the manifold, whilst smaller values result in more local data being preserved.
                                                                                                                    In general values should be in the range 2 to 100.")),
                                                                                                                                selectInput(inputId = 'DE_class_umap_metric', 
                                                                                                                                            label = 'Distance metric:', 
                                                                                                                                            choices = c('Euclidean' = 'euclidean',
                                                                                                                                                        'Cosine' = 'cosine',
                                                                                                                                                        'Manhattan' = 'manhattan',
                                                                                                                                                        #'Categorical' = 'categorical',
                                                                                                                                                        'Hamming' = 'hamming'), 
                                                                                                                                            selected = 'euclidean', 
                                                                                                                                            multiple = F, 
                                                                                                                                            width = '100%') %>% 
                                                                                                                                  helper(type = "inline",
                                                                                                                                         title = "Distance metric",
                                                                                                                                         content = c("The distance metric is use to find nearest neighbors"))
                                                                                                               ), #conditionalPanel
                                                                                                               style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:340px"
                                                                                                           ) #div #DE_class_dim_redu_style1_div
                                                                                                    ), #column
                                                                                                    column(width = 5,
                                                                                                           div(id = 'DE_class_dim_redu_style2_div',
                                                                                                               selectInput(inputId = 'DE_class_cluster_method', 
                                                                                                                           label = 'Clustering method:', 
                                                                                                                           choices = c('Group information' = 'group_info', 
                                                                                                                                       'K-means' = 'kmeans', 
                                                                                                                                       'Partitioning around medoids (PAM)' = 'kmedoids', 
                                                                                                                                       'Hierarchical clustering' = 'hclustering', 
                                                                                                                                       'DBSCAN' = 'dbscan'), 
                                                                                                                           selected = 'group_info', 
                                                                                                                           multiple = FALSE, 
                                                                                                                           width = '100%'
                                                                                                               ), #selectInput #DE_class_cluster_method
                                                                                                               conditionalPanel(condition = 'input.DE_class_cluster_method == "group_info"',
                                                                                                                                strong(textOutput(outputId = 'DE.class.group.count')), 
                                                                                                                                br()
                                                                                                               ), #conditionalPanel
                                                                                                               conditionalPanel(condition = 'input.DE_class_cluster_method == "kmeans"',
                                                                                                                                sliderInput(inputId = 'DE_class_kmeans_group',
                                                                                                                                            label = 'Number of groups:', 
                                                                                                                                            value = 2, 
                                                                                                                                            min = 2, 
                                                                                                                                            max = 10,
                                                                                                                                            step = 1, 
                                                                                                                                            width = '100%'
                                                                                                                                            )
                                                                                                               ), #conditionalPanel
                                                                                                               conditionalPanel(condition = 'input.DE_class_cluster_method == "kmedoids"', 
                                                                                                                                numericInput(inputId = 'DE_class_pam_group',
                                                                                                                                             label = 'Number of groups:', 
                                                                                                                                             value = 2, 
                                                                                                                                             min = 1, 
                                                                                                                                             max = 10,
                                                                                                                                             step = 1, 
                                                                                                                                             width = '100%'
                                                                                                                                ),
                                                                                                                                selectInput(inputId = 'DE_class_pam_metric', 
                                                                                                                                            label = 'Distance metrics:', 
                                                                                                                                            choices = c('Euclidean' = 'euclidean', 
                                                                                                                                                        'Manhattan' = 'manhattan'), 
                                                                                                                                            selected = 'euclidean', 
                                                                                                                                            multiple = F, 
                                                                                                                                            width = '100%'
                                                                                                                                ) #selectInput #DE_class_pca_metric
                                                                                                               ), #conditionalPanel
                                                                                                               conditionalPanel(condition = 'input.DE_class_cluster_method == "hclustering"', 
                                                                                                                                numericInput(inputId = 'DE_class_hclust_group',
                                                                                                                                             label = 'Number of groups:', 
                                                                                                                                             value = 2, 
                                                                                                                                             min = 1, 
                                                                                                                                             max = 10,
                                                                                                                                             step = 1, 
                                                                                                                                             width = '100%'
                                                                                                                                ),
                                                                                                                                selectInput(inputId = 'DE_class_hclust_dist', 
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
                                                                                                                                            width = '100%'
                                                                                                                                ), #selectInput #DE_class_pca_dist
                                                                                                                                selectInput(inputId = 'DE_class_hclust_hclust', 
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
                                                                                                                                            width = '100%'
                                                                                                                                ) #selectInput #DE_class_pca_hclust
                                                                                                               ), #conditionalPanel
                                                                                                               conditionalPanel(condition = 'input.DE_class_cluster_method == "dbscan"', 
                                                                                                                                numericInput(inputId = 'DE_class_dbscan_eps', 
                                                                                                                                             label = 'Epsilon:', 
                                                                                                                                             value = 0.5, 
                                                                                                                                             width = '100%'
                                                                                                                                ), #numericInput #DE_class_pca_eps
                                                                                                                                numericInput(inputId = 'DE_class_dbscan_minPts', 
                                                                                                                                             label = 'minPts:', 
                                                                                                                                             value = 1, 
                                                                                                                                             min = 1, 
                                                                                                                                             max = 22, 
                                                                                                                                             width = '100%'
                                                                                                                                ) #numericInput #DE_class_pca_minPts
                                                                                                               ), #conditionalPanel
                                                                                                               style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:340px"
                                                                                                           ) #div #DE_class_dim_redu_style2_div
                                                                                                    ) #column
                                                                                                ), #div #DE_class_dim_redu_reset_div
                                                                                                column(width = 2, 
                                                                                                       br(),
                                                                                                       br(),
                                                                                                       br(),
                                                                                                       br(),
                                                                                                       br(),
                                                                                                       br(),
                                                                                                       actionButton(inputId = 'DE_class_dim_redu_start', label = 'Submit', icon = icon('play'), width = '100%'), #DE_class_dim_redu_start
                                                                                                       br(),
                                                                                                       br(),
                                                                                                       actionButton(inputId = 'DE_class_dim_redu_reset', label = 'Reset', icon = icon('redo'), width = '100%') #actionButton #DE_class_dim_redu_reset
                                                                                                ), #column
                                                                                                column(width = 12, br()),
                                                                                                div(id = 'DE_class_dim_redu_result_div', 
                                                                                                    conditionalPanel(condition = 'input.DE_class_dim_redu_method == "pca" & input.DE_class_dim_redu_start', 
                                                                                                                     column(width = 6, 
                                                                                                                            plotlyOutput(outputId = 'DE.class.pca.biplot', height = '320px') %>% withSpinner(), #plotlyOutput #DE.class.pca.biplot
                                                                                                                            ), #column
                                                                                                                     column(width = 6, 
                                                                                                                            plotlyOutput(outputId = 'DE.class.pca.screeplot', height = '320px') %>% withSpinner(), #plotlyOutput #DE.class.pca.screeplot
                                                                                                                            ), #column
                                                                                                                     column(width = 12, 
                                                                                                                            br(), 
                                                                                                                            br(),
                                                                                                                            dataTableOutput(outputId = 'DE.class.pca.rotated.data') %>% withSpinner(),
                                                                                                                            br(), 
                                                                                                                            br(),
                                                                                                                            dataTableOutput(outputId = 'DE.class.pca.contrib.table') %>% withSpinner(), 
                                                                                                                            br(), 
                                                                                                                            br(), 
                                                                                                                            br()
                                                                                                                            ), 
                                                                                                                     br(),
                                                                                                                     br(),
                                                                                                                     sidebarLayout(fluid = T, 
                                                                                                                                   position = 'right', 
                                                                                                                                   sidebarPanel(width = 4, 
                                                                                                                                                sliderInput(inputId = 'DE_class_pca_variable_topN', 
                                                                                                                                                            label = 'top N feature:', 
                                                                                                                                                            min = 1, 
                                                                                                                                                            max = 30, 
                                                                                                                                                            value = 10,
                                                                                                                                                            step = 1)
                                                                                                                                   ), #sidebarPanel
                                                                                                                                   mainPanel(width = 8, 
                                                                                                                                             plotlyOutput(outputId = 'DE.class.pca.variable') %>% withSpinner() #plotlyOutput #DE.class.pca.variable
                                                                                                                                   ) #mainPanel
                                                                                                                     ), #sidebarLayout
                                                                                                                     br(),
                                                                                                                     br(),
                                                                                                                     sidebarLayout(fluid = T, 
                                                                                                                                   position = 'left', 
                                                                                                                                   sidebarPanel(width = 4, 
                                                                                                                                                sliderInput(inputId = 'DE_class_pca_contrib_topN', 
                                                                                                                                                            label = 'top N feature:', 
                                                                                                                                                            min = 1, 
                                                                                                                                                            max = 30, 
                                                                                                                                                            value = 10,
                                                                                                                                                            step = 1, 
                                                                                                                                                            width = '100%'),
                                                                                                                                                selectInput(inputId = 'DE_class_pca_contrib_PC', 
                                                                                                                                                            label = 'Contribution of features to principal component:', 
                                                                                                                                                            choices = c('Component 1' = 1, 
                                                                                                                                                                        'Component 2' = 2, 
                                                                                                                                                                        'Component 1 & Component 2'= '1_2'), 
                                                                                                                                                            selected = 'PC1_PC2', 
                                                                                                                                                            multiple = F)
                                                                                                                                   ), #sidebarPanel
                                                                                                                                   mainPanel(width = 8, 
                                                                                                                                             plotlyOutput(outputId = 'DE.class.pca.contrib') %>% withSpinner() #plotlyOutput #DE.class.pca.contrib
                                                                                                                                             ) #mainPanel
                                                                                                                                   ), #sidebarLayout
                                                                                                                     br(),
                                                                                                                     br()
                                                                                                                     ), #conditionalPanel
                                                                                                    conditionalPanel(condition = 'input.DE_class_dim_redu_method == "plsda" & input.DE_class_dim_redu_start', 
                                                                                                                     column(width = 6, 
                                                                                                                            plotlyOutput(outputId = 'DE.class.plsda.sample.plot') %>% withSpinner(), #plotlyOutput #DE.class.plsda.sample.plot
                                                                                                                            ), 
                                                                                                                     column(width = 6, 
                                                                                                                            plotlyOutput(outputId = 'DE.class.plsda.variable.plot') %>% withSpinner(), #plotlyOutput #DE.class.plsda.variable.plot
                                                                                                                            ), 
                                                                                                                     column(width = 12,
                                                                                                                            br(),
                                                                                                                            br(),
                                                                                                                            dataTableOutput(outputId = 'DE.class.plsda.variate.table') %>% withSpinner(),
                                                                                                                            br(),
                                                                                                                            br(),
                                                                                                                            dataTableOutput(outputId = 'DE.class.plsda.loading.table') %>% withSpinner(), 
                                                                                                                            br(),
                                                                                                                            br(),
                                                                                                                            br()
                                                                                                                            )
                                                                                                                     ), #conditionalPanel
                                                                                                    conditionalPanel(condition = 'input.DE_class_dim_redu_method == "tsne" & input.DE_class_dim_redu_start', 
                                                                                                                     column(width = 1),
                                                                                                                     column(width = 10,
                                                                                                                            plotlyOutput(outputId = 'DE.class.tsne.plot', height = '600px') %>% withSpinner(),
                                                                                                                            br(),
                                                                                                                            dataTableOutput(outputId = 'DE.class.tsne.table') %>% withSpinner()
                                                                                                                     )
                                                                                                    ), #conditionalPanel
                                                                                                    conditionalPanel(condition = 'input.DE_class_dim_redu_method == "umap" & input.DE_class_dim_redu_start', 
                                                                                                                     column(width = 1),
                                                                                                                     column(width = 10,
                                                                                                                            plotlyOutput(outputId = 'DE.class.umap.plot', height = '600px') %>% withSpinner(),
                                                                                                                            br(),
                                                                                                                            dataTableOutput(outputId = 'DE.class.umap.table') %>% withSpinner()
                                                                                                                     )
                                                                                                    ) #conditionalPanel
                                                                                                ) #div #DE_class_dim_redu_result_div
                                                                                         ) #column 
                                                                                ), #tabPanel #Dimensionality reduction
                                                                                ######################
                                                                                ####  Clustering  ####
                                                                                ######################
                                                                                tabPanel(title = 'Hierarchical clustering', 
                                                                                         h3('Hierarchical clustering'),
                                                                                         column(width=12,
                                                                                                h6(p("New lipid expression table summed up from species will be clustered and shown on the heatmap using hierarchical clustering.
                                                                                          Through heatmap, users may discover the difference between the two groups by observing the distribution of lipid characteristic expression.
                                                                                          This analysis provides an overview of lipid characteristic expression differences between the control group and the experimental group.
                                                                                          Four distance measures can be chosen, Person, Spearman, or Kendall, and eight clustering methods can be selected by pulling down the menu. ", 
                                                                                                     style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                                br(),
                                                                                                div(id = 'DE_class_cluster_style_div', 
                                                                                                    div(id = 'DE_class_cluster_reset_div', 
                                                                                                        column(width=6, 
                                                                                                               selectInput(inputId = 'DE_class_cluster_by', 
                                                                                                                           label = 'Select feature for clustering:', 
                                                                                                                           choices = c('By all lipid categories' = 'all_lipid', 
                                                                                                                                       'By significant lipid categories' = 'sig_lipid'), 
                                                                                                                           selected = 'sig_lipid', 
                                                                                                                           multiple = F
                                                                                                               ) #selectInput #DE_class_cluster_by
                                                                                                        ), #column
                                                                                                        column(width = 6#, 
                                                                                                               # selectInput(inputId = 'DE_class_sidecolor', 
                                                                                                               #             label = 'Side color by lipid characteristics:', 
                                                                                                               #             choices = c('Without side color' = 'none',
                                                                                                               #                         'Lipid class' = 'class'), 
                                                                                                               #             selected = 'class', 
                                                                                                               #             multiple = F
                                                                                                               #             ) #selectInput #DE_class_sidecolor
                                                                                                        ), #column
                                                                                                        column(width = 12), #column
                                                                                                        column(width = 6,
                                                                                                               selectInput(inputId = 'DE_class_dist', 
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
                                                                                                               ) #selectInput #DE_class_dist
                                                                                                        ), #column
                                                                                                        column(width=6, 
                                                                                                               selectInput(inputId = 'DE_class_hclust', 
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
                                                                                                                           multiple = F
                                                                                                               ) #selectInput #DE_class_hclust
                                                                                                        ), #column
                                                                                                    ), #div #DE_class_cluster_reset_div
                                                                                                    column(width = 7), #column
                                                                                                    column(width = 5, 
                                                                                                           actionButton(inputId = 'DE_class_cluster_reset', 
                                                                                                                        label = 'Reset', icon = icon('redo')
                                                                                                           ), #actionButton #DE_class_cluster_reset
                                                                                                           actionButton(inputId = 'DE_class_cluster_start', 
                                                                                                                        label = 'Submit', icon = icon('play')
                                                                                                           ) #actionButton #DE_class_cluster_start
                                                                                                    ), #column
                                                                                                    style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:230px"
                                                                                                ), #div #DE_class_cluster_style_div
                                                                                                br(),
                                                                                                div(id = 'DE_class_heatmap_div', 
                                                                                                    conditionalPanel(condition = 'input.DE_class_cluster_start', 
                                                                                                                     #condition = 'output.DE.class.heatmap', 
                                                                                                                     iheatmaprOutput(outputId = 'DE.class.heatmap', height = '800px') %>% withSpinner(), #plotlyOutput #DE.class.cluster
                                                                                                                     column(width = 4), 
                                                                                                                     column(width = 8, 
                                                                                                                            downloadButton(outputId = 'DE.class.heatmap.matrix', label = 'Download matrix')
                                                                                                                    )
                                                                                                    )
                                                                                                ) #conditionalPanel
                                                                                         ) #column
                                                                                ) #tabPanel #Clustering
                                                                   ) #navlistPanel #DE_specific_list
                                                          ) #tabPanel #Lipid specific analysis
                                              ) #tabsetPanel #DE_analysis_tab
                             ) #conditionalPanel
                             
                         ) #div #DE_tabPanel_div
                         ) #column
                  ) #fluidRow
         
         
         
         
)