shiny::tabPanel(title=htmltools::HTML("<h4 style='font-size:18px;padding-top:9.5px;padding-bottom:11.5px;text-align:center;'>Data<br>Check</h4>"),
                value='Data_Check',
         #### Data Check Header Panel ####
         htmltools::h1('Data Check'),
         shiny::fluidRow(
           shiny::column(width=12,
                         htmltools::div(id='Check_description_div', 
                                        style="background-color: PowderBlue;border-left: 8px solid Teal;padding: 15px",
                                        h6('This page is specifically designed to pre-check user-uploaded data against the requirements of various analyses.
                                        It will return checking results according to the data needs of specific analysis types.
                                        This page supports checks for \'Profiling,\' \'Differential Expression,\' \'Machine Learning,\' and \'Correlation\' analyses.
                                        Please note that \'Enrichment\' and \'Network\' analyses require output data from differential expression analysis.'),
                                        htmltools::em(shiny::icon("caret-right"), 'For detailed information on the input data requirements of each analysis type, please refer to ', 
                                                      htmltools::HTML('<a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ5" target="_blank" style="color: darkblue;">FAQ</a>'), '.', style='font-style:italic; font-weight: bolder; font-size:17px; color: #CD5C5C; padding-left: 25px;'),
                                        htmltools::br(),
                                        htmltools::em(shiny::icon("caret-right"), 'For step-by-step guidance on performing this page, please refer to ', 
                                                      htmltools::HTML('<a href="https://lipidsig.bioinfomics.org/Tutorial/" target="_blank" style="color: darkblue;">Tutorial</a>'), '.', style='font-style:italic; font-weight: bolder; font-size:17px; color: #CD5C5C; padding-left: 25px;')
                             
                    ), #div 
                    htmltools::br(),
                    #################################
                    ###   Data Check Data Source  ###
                    #################################
                    htmltools::h2('Data Source'),
                    shiny::sidebarLayout(fluid=TRUE, 
                                         shiny::sidebarPanel(width=4,
                                                             shiny::radioButtons(inputId='Check_type',
                                                                                 label=h4('Analysis type'),
                                                                                 choices=c('Profiling'='Profiling',
                                                                                           'Differential Expression'='DE',
                                                                                           'Machine Learning'='ML',
                                                                                           'Correlation'='Correlation'),
                                                                                 selected='Profiling') %>%
                                                               shinyhelper::helper(type="inline", 
                                                                                   title="Data source", 
                                                                                   size ="l", 
                                                                                   content=c('<ol style="font-size: 0px;">',
                                                                                   '<li style="font-size: 16px;">Lipid dataset can be uploaded by users or using example datasets. All the required data needs to be uploaded in
                                                                                   <mark style="background-color: white;color: red;">CSV</mark>, <mark style="background-color: white;color: red;">TSV</mark>, or <mark style="background-color: white;color: red;">XLSX</mark> format.
                                                                                   The maximum file size is 30MB. <br>
                                                                                   <ul style="font-size: 16px; color:red;">
                                                                                   <li>NOTE: When uploading in XLSX format, ensure the data frame is on the first sheet. </li>
                                                                                   </ul>
                                                                                   </li>',
                                                                                   '<li style="font-size: 16px;">Once the files are chosen and shown ‘Upload complete’ then press ‘Upload’.</li>',
                                                                                   '</ol>')),
                                                             shiny::radioButtons(inputId='Check_data_source',
                                                                                 label=h4('Data source'),
                                                                                 choices=c('Example dataset'='Check_demo_data',
                                                                                           'Upload your data!'='Check_user_data'),
                                                                                 selected='Check_demo_data'),
                                                             shiny::conditionalPanel(condition='input.Check_data_source == "Check_demo_data"',
                                                                                     shiny::conditionalPanel(condition='input.Check_type == "DE"',
                                                                                                             shiny::selectInput(inputId='Check_demoe_ngroup',
                                                                                                                                label='Number of groups',
                                                                                                                                choices=c('Two'='two', 
                                                                                                                                          'Multiple'='multiple'),
                                                                                                                                selected='two', multiple=FALSE) ## selectInput #Check_demoe_ngroup
                                                                     ),
                                                                     shiny::actionButton(inputId='Check_demo_upload', label='Submit', icon=shiny::icon('upload')), ## actionButton #Check_demo_upload
                                                                     shiny::downloadButton(outputId='Check.demo.download', label='Download example')
                                                                     ),## conditionalPanel (If the user chooses to use the demo dataset)
                                                             shiny::conditionalPanel(condition='input.Check_data_source == "Check_user_data"',
                                                                                     htmltools::div(id='Check_reset_div',
                                                                                                    shiny::fileInput(inputId='Check_exp', label='Lipid abundance data:', accept=c(".csv", ".tsv", '.xlsx'), multiple=FALSE) %>% ## fileInput #Check_exp
                                                                                                      shinyhelper::helper(type="inline",
                                                                                                                          title="Lipid abundance data",
                                                                                                                          size="l",
                                                                                                                          content=c('<ol style="font-size: 0px;">',
                                                                                                                                    '<li style="font-size: 16px;">The first column must contain a list of unique lipids name(features). </li>',
                                                                                                                                    '<li style="font-size: 16px;">Other columns encompass the expressed values of groups under different conditions that you want to compare.</li>',
                                                                                                                                    '<li style="font-size: 16px;">An example of ‘Lipid abundance data’</li>',
                                                                                                                                    '<img src="Description/DE_Lipid expression data.webp" style="border:2px #ccc solid;padding:20px;" loading="lazy" width="100%"/>',
                                                                                                                                    '</ol>')),## helper (for fileInput Check_exp)
                                                                                                    shiny::conditionalPanel(condition='input.Check_type == "DE"',
                                                                                                                            shiny::fileInput(inputId='Check_group', label='Group information:', accept=c(".csv",".tsv",'.xlsx'),multiple=FALSE)%>% ## fileInput #Check_group
                                                                                                                              shinyhelper::helper(type="inline",
                                                                                                                                                  title="Group information",
                                                                                                                                                  size="l",
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
                                                                                                                                                            '<img src="Description/DE_Group Information.webp" style="border:2px #ccc solid;padding:20px;" loading="lazy" width="75%"/>')), ## helper (for fileInput Check_group)
                                                                                                                            shiny::selectInput(inputId='Check_ngroup',
                                                                                                                                               label='Number of groups',
                                                                                                                                               choices=c('Two'='two', 
                                                                                                                                                         'Multiple'='multiple'),
                                                                                                                                               selected='two', multiple=FALSE) ## selectInput #Check_ngroup
                                                                                                                            ), ## conditionalPanel (If the user chooses to check DE dataset)
                                                                                                    shiny::conditionalPanel(condition='input.Check_type == "ML" | input.Check_type == "Correlation"',
                                                                                                                            fileInput(inputId='Check_cond', label='Condition table:', accept=c(".csv", ".tsv", '.xlsx'), multiple=FALSE) %>% ## fileInput #Check_cond
                                                                                                                              shinyhelper::helper(type="inline",
                                                                                                                                                  title="Condition table",
                                                                                                                                                  size ="l",
                                                                                                                                                  content=c(
                                                                                                                                                    '<h4 style="text-align:left; font-size: 16px;">&bull; for Maching Learning Condition table</h4>',
                                                                                                                                                    '<ol style="font-size: 0px;">',
                                                                                                                                                    '<li style="font-size: 16px;">‘Condition table’ can only contain 2 columns: in ‘sample_name’, ‘group’ order</li>',
                                                                                                                                                    '<li style="font-size: 16px;">The first column must contain a list of samples name (features), MUST SAME AS THE SAMPLE NAME (COLUM NAMES) OF ‘Lipid abundance data’!</li>',
                                                                                                                                                    '<li style="font-size: 16px;">‘group’ refers to the group of the sample (can only be 0 and 1). NOTE: THE GROUP MUST BE NUMERIC, AND SKEWED DATA ARE NOT RECOMMEND</li>',
                                                                                                                                                    '<li style="font-size: 16px;">An example of ‘Condition table’</li>',
                                                                                                                                                    '<img src="Description/ML_Condition table.webp" style="border:2px #ccc solid;padding:20px;" loading="lazy" width="100%"/>',
                                                                                                                                                    '</ol>',
                                                                                                                                                    '<h4 style="text-align:left; font-size: 16px;">&bull; for Correlation Condition table</h4>',
                                                                                                                                                    '<ol style="font-size: 0px;">',
                                                                                                                                                    '<li style="font-size: 16px;">The condition table encompasses sample names and clinical conditions (disease status, gene dependence score etc.), which assigned each sample to a specific condition for further association analysis.</li>',
                                                                                                                                                    '<li style="font-size: 16px;">The first column must contain a list of samples name, MUST SAME AS THE SAMPLE NAME (COLUM NAMES) OF ‘Lipid abundance data’ !</li>',
                                                                                                                                                    '<li style="font-size: 16px;">Other columns are clinical conditions, such as Emphysema, Exacerbations. NOTE: ALL VALUES MUST BE NUMERIC</li>',
                                                                                                                                                    '<li style="font-size: 16px;">An example of ‘Condition table’</li>',
                                                                                                                                                    '<img src="Description/CORR_Condition table.webp" style="border:2px #ccc solid;padding:20px;" loading="lazy" width="100%"/>',
                                                                                                                                                    '</ol>')) ## helper (for fileInput Check_cond)
                                                                                                                            ), ## conditionalPanel (If the user chooses to check ML or Correlation dataset)
                                                                                                    shiny::conditionalPanel(condition='input.Check_type == "Correlation"',
                                                                                                                            shiny::fileInput(inputId='Check_adj', label='Adjusted table (optional):', accept=c(".csv",".tsv",'.xlsx'), multiple=FALSE)%>% ##fileInput #Check_adj
                                                                                                                              shinyhelper::helper(type="inline",
                                                                                                                                                  title="Adjusted table",
                                                                                                                                                  size ="l",
                                                                                                                                                  content=c('<ol style="font-size: 0px;">',
                                                                                                                                                            '<li style="font-size: 16px;">‘Adjusted table’ represents the user-defined variables that will be corrected in linear regression or logistic regression analysis, which can be the cancer types or the clinical information, like gender, age, or BMI.</li>',
                                                                                                                                                            '<li style="font-size: 16px;">The first column must contain a list of samples name, MUST SAME AS THE SAMPLE NAME (COLUM NAMES) OF ‘Lipid abundance data’ !</li>',
                                                                                                                                                            '<li style="font-size: 16px;">An example of ‘Adjusted table’:</li>',
                                                                                                                                                            '<img src="Description/CORR_Adjusted table.webp" style="border:2px #ccc solid;padding:20px;" loading="lazy" width="100%"/>',
                                                                                                                                                            '</ol>')) ## helper (for fileInput Check_adj)
                                                                                                                            ),## conditionalPanel (If the user chooses to check Correlation dataset)
                                                                                                    shiny::helpText("Upload your data table in .csv/.tsv/.xlsx") #helpText
                                                                                                    ),## div # Check_reset_div
                                                                                     shiny::actionButton(inputId='Check_reset', label='Reset', icon=shiny::icon('redo')), ## actionButton #Check_reset
                                                                                     shiny::actionButton(inputId='Check_upload', label='Upload', icon=shiny::icon('upload')),## actionButton #Check_upload
                                                                                     shiny::actionButton(inputId='Check_url_start', label='Submit', icon=shiny::icon('play'), style="visibility: hidden;")## actionButton #Check_url_start
                                                                                     ),## conditionalPanel (If the user chooses to use his own dataset)
                                                             htmltools::br(), 
                                                             htmltools::HTML("<a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank' style='color: darkblue;'>How to prepare your dataset?</a>"),
                                                             htmltools::br(), 
                                                             htmltools::HTML("<a href='https://lipidsig.bioinfomics.org/Tutorial/?DataCheck' target='_blank' style='color: darkblue;'>How to use this function?</a>")
                                       ), ## sidebarPanel
                                       shiny::mainPanel(width=8,
                                                        htmltools::div(id='Check_upload_raw_div',style='display:none;',
                                                                       htmltools::div(
                                                                         style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                                         shiny::htmlOutput("Data.check.progress") %>% shinycssloaders::withSpinner()
                                                                        ), ## div
                                                                       htmltools::br(), 
                                                                       htmltools::div(id='data_warning',
                                                                                      htmltools::div(
                                                                                        style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                                                        shiny::htmlOutput("Check.web.div") %>% shinycssloaders::withSpinner()
                                                                                        ), ## div 
                                                                                      shiny::helpText(htmltools::tags$p(shiny::icon("check"),": Successfully uploaded.", style="font-size: 16px;",
                                                                                                                        htmltools::HTML('&nbsp;'), shiny::icon("times"), ": Error happaned. Please check your dataset.", 
                                                                                                                        htmltools::HTML('&nbsp;'), shiny::icon("exclamation"), ": Warning message.", style="font-size: 16px;")),
                                                                                      htmltools::br()
                                                                                      )## div # data_warning
                                                                       )## div # Check_upload_raw_div
                                       ) ## mainPanel
                         ), ## sidebarLayout
                    htmltools::div(id='data_check_successful',style='display:none;',
                                   htmltools::div(id='data_process_table_div',style='display:none;height:440px;',
                                                  shiny::column(width=12,
                                                                shiny::column(12, style='padding-right: 1050px;',
                                                                              htmltools::h3(htmltools::strong('Data')) %>%
                                                                                shinyhelper::helper(type="inline", title="Data",
                                                                                                    size ="l",
                                                                                                    content=c('<h4 style="font-size: 18px; text-align:left; line-height: 30px; color:black;">
                                                                                                    <ul>
                                                                                                    <li><strong>Raw data</strong>: User-uploaded abundance data.</li>
                                                                                                    <li><strong>Processed data</strong>: User-uploaded abundance data after automatic format correction, including the removal of features/samples with constant NAs or zeros, unrecognized lipids, and lipids with naming errors.</li>
                                                                                                    <li><strong>Unrecognized lipid</strong>: User-uploaded lipids that are not recognized. For lipid species naming format, please refer to the <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ12" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                    <li><strong>Error lipid</strong>: User-uploaded lipids with naming errors.</li>
                                                                                                    <li><strong>Lipid characteristics</strong>: Lipid characteristics converted according to the uploaded lipids in the abundance data. Detailed information about the converted characteristics can be found in the <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ11" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                                    <li><strong>Lipid id</strong>: Links to the LION ID, LIPID MAPS ID, and other resource IDs for the uploaded lipids.</li>
                                                                                                    <li><strong>Group information (if provided)</strong>: The user-uploaded group information table. If your data contain samples with all NAs or zeros, it will be removed.</li>
                                                                                                    <li><strong>Condition table (if provided)</strong>: User-uploaded condition table. If your data contain samples with all NAs or zeros, it will be removed.</li>
                                                                                                    <li><strong>Adjusted table (if provided)</strong>: User-uploaded adjusted table.</li>
                                                                                                    </ul></h4>'))## helper (for h3 Data)
                                                                              ),## column 12
                                                                shiny::column(12,
                                                                              htmltools::div(id='data_process_description_div', 
                                                                                             style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                                                             htmltools::HTML(
                                                                                             '<h4 style="font-size: 18px; text-align:left; line-height: 30px; color:black;">
                                                                                             The uploaded data after the data-checking process are listed below. Switch between tabs to view different data.  <br>
                                                                                             Click the question mark in the upper-right corner for a brief description of each data.
                                                                                             </h4>') ## HTML
                                                                                             )## div #data_process_description_div
                                                                              ),## column 12
                                                                shiny::column(12, htmltools::br()),
                                                                shiny::column(12, 
                                                                              shiny::tabsetPanel(id='Check_process_table_tab',
                                                                                                 shiny::tabPanel(title="Raw data",
                                                                                                                 htmltools::br(),
                                                                                                                 DT::dataTableOutput(outputId='Check.exp.raw') %>% shinycssloaders::withSpinner(), #dataTableOutput #Check.exp.raw
                                                                                                                 ), ## tabPanel # Raw data
                                                                                                 shiny::tabPanel(title="Processed data",
                                                                                                                 htmltools::br(),
                                                                                                                 DT::dataTableOutput(outputId='Check.exp.tran') %>% shinycssloaders::withSpinner(), #dataTableOutput #Check.exp.tran
                                                                                                                 ), ## tabPanel # Processed data
                                                                                                 shiny::tabPanel(title="Unrecognized lipid",
                                                                                                                 htmltools::br(),
                                                                                                                 DT::dataTableOutput(outputId='Check.nonParseable.lipid') %>% shinycssloaders::withSpinner(), #dataTableOutput #Check.nonParseable.lipid
                                                                                                                 ), ## tabPanel # Unrecognized lipid
                                                                                                 shiny::tabPanel(title="Error lipid",
                                                                                                                 htmltools::br(),
                                                                                                                 DT::dataTableOutput(outputId='Check.naming.error.lipid') %>% shinycssloaders::withSpinner(), #dataTableOutput #Check.naming.error.lipid
                                                                                                                 ), ## tabPanel # Error lipid
                                                                                                 shiny::tabPanel(title="Lipid characteristics",
                                                                                                                 htmltools::br(),
                                                                                                                 DT::dataTableOutput(outputId='Check.lipid.char.tran') %>% shinycssloaders::withSpinner(), #dataTableOutput #Check.lipid.char.tran
                                                                                                                 ), ## tabPanel # Lipid characteristics
                                                                                                 shiny::tabPanel(title="Lipid id",
                                                                                                                 htmltools::br(),
                                                                                                                 DT::dataTableOutput(outputId='Check.lipid.id') %>% shinycssloaders::withSpinner(), #dataTableOutput #Check.lipid.id
                                                                                                                 ), ## tabPanel # Lipid id
                                                                                                 shiny::tabPanel(title="Group information",
                                                                                                                 htmltools::br(),
                                                                                                                 DT::dataTableOutput(outputId='Check.group.info.tran') %>% shinycssloaders::withSpinner(), #dataTableOutput #Check.group.info.tran
                                                                                                                 ), ## tabPanel # Group information
                                                                                                 shiny::tabPanel(title="Condition table",
                                                                                                                 htmltools::br(),
                                                                                                                 DT::dataTableOutput(outputId='Check.cond.tran') %>% shinycssloaders::withSpinner(), #dataTableOutput #Check.cond.tran
                                                                                                                 ), ## tabPanel # Condition table
                                                                                                 shiny::tabPanel(title="Adjusted table",
                                                                                                                 htmltools::br(),
                                                                                                                 DT::dataTableOutput(outputId='Check.adj.tran') %>% shinycssloaders::withSpinner(), #dataTableOutput #Check.adj.tran
                                                                                                                 ) ## tabPanel # Adjusted table
                                                                                                 ) ## tabsetPanel
                                                                              ) ## column 12
                                                                ) ## column 12
                                                  ) ## div # data_process_table_div
                                   ) ##div # data_check_successful
                    ) ## column
         ) ## fluidRow
) ## tabPanel



