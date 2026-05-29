shiny::tabPanel(title=htmltools::HTML("<h4 style='font-size:18px;padding-top:9.5px;padding-bottom:11.5px;text-align:center;'>ID<br>Conversion</h4></a></li>
                                       <li><a href='https://lipidsig.bioinfomics.org/Tutorial/' onclick='show()'><h4 style='font-size:18px;padding-top:20.5px;padding-bottom:20.5px;'>Tutorial</h4></a></li>
                                       <li><a href='https://lipidsig.bioinfomics.org/FAQ/' onclick='show()'><h4 style='font-size:18px;padding-top:20.5px;padding-bottom:20.5px;'>FAQ</h4></a></li>
                                       <li><a href='https://lipidsig.bioinfomics.org/lipidsigr/index.html' target='_blank'><h4 style='font-size:18px;padding-top:20.5px;padding-bottom:20.5px;'>LipidSigR</h4></a></li>"),
                value='ID_conversion',
         #### Profiling Header Panel ####
         htmltools::h1('ID conversion'),
         shiny::fluidRow(
           shiny::column(width=12,
                         ##########################################
                         ####  ID conversion Page Description  ####
                         ##########################################
                         htmltools::div(id='IDconversion_description_div',
                                        style="background-color: PowderBlue;border-left: 8px solid Teal;padding: 15px",
                                        htmltools::h6('The ID conversion function is designed to map to 9 resource IDs, and automatically assign 29 lipid characteristics. Once the input features are converted, the right-side window will show the uploaded, recognized, and unrecognized features.'),
                                        htmltools::em(shiny::icon("caret-right"), 'For detailed information on the 29 lipid characteristics, please refer to ', htmltools::HTML('<a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ11" target="_blank" style="color: darkblue;">FAQ</a>'), '.', style='font-style:italic; font-weight: bolder; font-size:17px; color: #CD5C5C; padding-left: 25px;'),
                                        htmltools::br(),
                                        htmltools::em(shiny::icon("caret-right"), 'For lipid species naming format, please refer to ', htmltools::HTML('<a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ12" target="_blank" style="color: darkblue;">FAQ</a>'), '.', style='font-style:italic; font-weight: bolder; font-size:17px; color: #CD5C5C; padding-left: 25px;')
                                        ), #div #NET_description_div
                         ####################################
                         ###   ID conversion Data Source  ###
                         ####################################
                         htmltools::h2('ID conversion'),
                         shiny::sidebarLayout(fluid=TRUE,
                                              shiny::sidebarPanel(width=4,
                                                                  shiny::radioButtons(inputId='IDconversion_source',
                                                                                      label=h4('Data source'),
                                                                                      choices=c('Example dataset'='IDconversion_demo_data',
                                                                                                'Upload your data!'='IDconversion_user_data'),
                                                                                      selected='IDconversion_demo_data'),
                                                                  shiny::conditionalPanel(condition='input.IDconversion_source == "IDconversion_demo_data"',
                                                                                          shiny::actionButton(inputId='IDconversion_demo_upload', label='Submit', icon=shiny::icon('upload')), #actionButton #IDconversion_demo_upload
                                                                                          shiny::downloadButton(outputId='IDconversion.demo.download', label='Download example')
                                                                                          ), #conditionalPanel
                                                                  shiny::conditionalPanel(condition='input.IDconversion_source == "IDconversion_user_data"',
                                                                                          htmltools::div(id='IDconversion_reset_div',
                                                                                                         shiny::radioButtons(inputId='IDconversion_species_type',
                                                                                                                             label='Lipid species input format',
                                                                                                                             choices=c("typing input (one lipid species per line, enter-separated.)"='typing',
                                                                                                                                        "csv/tsv file"='txt'),
                                                                                                                             selected='typing'),
                                                                                                         shiny::conditionalPanel(condition='input.IDconversion_species_type == "txt"',
                                                                                                                                 shiny::fileInput(inputId='IDconversion_species_file', label='Lipid species:', accept=c(".csv", ".tsv", '.xlsx'), multiple=FALSE) %>% #fileInput #IDconversion_species_file
                                                                                                                                   shinyhelper::helper(type="inline",
                                                                                                                                                       title="Lipid species", size ="l",
                                                                                                                                                       content=c('<ol style="font-size: 0px;">',
                                                                                                                                                                 '<li style="font-size: 16px;">Lipid species names can be uploaded by users or using example datasets.
                                                                                                                                                                  The data needs to be uploaded in <mark style="background-color: white;color: red;">CSV</mark> or <mark style="background-color: white;color: red;">TSV</mark> format.
                                                                                                                                                                  The data must contain only one column named "feature," listing all lipid names.
                                                                                                                                                                  The maximum file size is 30MB.</li>',
                                                                                                                                                                 '<li style="font-size: 16px;">Once the file is chosen and shown ‘Upload complete’ then press ‘Upload’.</li>',
                                                                                                                                                                 '</ol>'))
                                                                                                                                 ), ## conditionalPanel (if user choose to upload txt/csc/xlsx file)
                                                                                                         shiny::conditionalPanel(condition='input.IDconversion_species_type == "typing"',
                                                                                                                                 shiny::textAreaInput("IDconversion_species_typing", "", "", height="150px")
                                                                                                                                 ), ## conditionalPanel (if user choose to typing)
                                                                                                         shiny::helpText("Upload your data table in .csv/.tsv/.xlsx"), #helpText
                                                                                                         shiny::actionButton(inputId='IDconversion_reset', label='Reset', icon=shiny::icon('redo')), #actionButton #ID_conversion_user_reset
                                                                                                         shiny::actionButton(inputId='IDconversion_upload', label='Upload', icon=shiny::icon('upload')) #actionButton #ID_conversion_user_upload
                                                                                                         ) ## div # IDconversion_reset_div
                                                                                          ),## conditionalPanel (if user choose to upload file)
                                                                  htmltools::br(),
                                                                  htmltools::HTML("<a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ12' target='_blank' style='color: darkblue;'>How to prepare your data (lipid naming)?</a>"),
                                                                  htmltools::br(),
                                                                  htmltools::HTML("<a href='https://lipidsig.bioinfomics.org/Tutorial/?IDconversion' target='_blank' style='color: darkblue;'>How to use this function?</a>")
                                                                  ), ## sidebarPanel
                                              shiny::mainPanel(width=8,
                                                               htmltools::div(id='ID_conversion_upload_div',style='display:none;',
                                                                              style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                                              shiny::htmlOutput("IDconversion.progress") %>% shinycssloaders::withSpinner()
                                                                              )
                                                               ) ## mainPanel
                                              ), ## sidebarLayout
                         htmltools::div(id='ID_conversion_table_div', style='display:none;',
                                        htmltools::div(style="height: 200px;text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                       htmltools::HTML('<h4 style="font-size: 18px; text-align:left; line-height: 30px; color:black;">
                                                                        <ul>
                                                                          <li><strong>Raw data</strong>: User-uploaded lipids.</li>
                                                                          <li><strong>Recognized lipid</strong>: Recognized user-uploaded lipids with their converted lipid characteristics. Detailed information about the converted characteristics can be found in the <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ11" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                          <li><strong>Lipid id</strong>: Links to the LION ID, LIPID MAPS ID, and other resource IDs for the uploaded lipids.</li>
                                                                          <li><strong>Unrecognized lipid</strong>: User-uploaded lipids that are not recognized. For lipid species naming format, please refer to the <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ12" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                        </ul></h4>')
                                                       ),## div 
                                        htmltools::br(),
                                        htmltools::div(
                                          shiny::tabsetPanel(id='IDconversion_raw_table_tab',
                                                             shiny::tabPanel(title="Raw data",
                                                                             htmltools::br(),
                                                                             DT::dataTableOutput(outputId='IDconversion.exp.raw') %>% shinycssloaders::withSpinner() ## dataTableOutput # IDconversion.exp.raw
                                                                             ), ## tabPanel Raw data
                                                             shiny::tabPanel(title="Recognized lipid",
                                                                             htmltools::br(),
                                                                             DT::dataTableOutput(outputId='IDconversion.recognized.lipid') %>% shinycssloaders::withSpinner() ## dataTableOutput # IDconversion.recognized.lipid
                                                                             ),
                                                             shiny::tabPanel(title="Lipid id",
                                                                             htmltools::br(),
                                                                             DT::dataTableOutput(outputId='IDconversion.lipid.id') %>% shinycssloaders::withSpinner() ## dataTableOutput # IDconversion.lipid.id
                                                                             ),
                                                             shiny::tabPanel(title="Unrecognized lipid",
                                                                             htmltools::br(),
                                                                             DT::dataTableOutput(outputId='IDconversion.unrecognized.lipid') %>% shinycssloaders::withSpinner() ## dataTableOutput # IDconversion.unrecognized.lipid
                                                                             )
                                                             ), ## tabsetPanel
                                          htmltools::br()
                                          ) ## div 
                                        ) ## div # ID_conversion_table_div
                         ) ## column 12
           ) ## fluidRow
) ## tabPanel
