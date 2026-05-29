shiny::tabPanel(title=htmltools::HTML("<h4 style='font-size:18px;padding-top:18.5px;padding-bottom:20.5px;'>Profiling</h4>"),

         value='Profiling',
         #### Profiling Header Panel ####
         htmltools::h1('Profiling analysis'),
         htmltools::br(),
         shiny::fluidRow(
           shiny::column(width=12,
                         ######################################
                         ####  Profiling Page Description  ####
                         ######################################
                         htmltools::div(id='PRO_description_div',
                                        style="background-color: PowderBlue;border-left: 8px solid Teal;padding: 15px",
                                        htmltools::h6('Lipidomics technology provides a fast and high-throughput screening to identify thousands of lipid species in cells,
                                        tissues or other biological samples and has been broadly used in several areas of studies.
                                        In this page, we present an overview that gathers comprehensive analyses that allow researchers to explore the quality and the clustering of samples,
                                                      correlation between lipids and samples, and the lipid abundance and composition.'),
                                        htmltools::br(),
                                        htmltools::HTML('<em style="font-size:17px; text-align:left; line-height: 25px;">
                                        <i class="fas fa-caret-right" role="presentation" aria-label="caret-right icon"></i>
                                        Demo dataset source: <a href="https://pubmed.ncbi.nlm.nih.gov/29320510/" target="_blank">Adipose tissue ATGL modifies the cardiac lipidome in pressure-overload-induced left Ventricular failure (PLoS Genet. 2018)</a>
                                                        </em>')
                         ), #div #PRO_description_div
                         htmltools::br(),
                         #################################
                         ####  Profiling Data Source  ####
                         #################################
                         htmltools::h2('Data Source'),
                         shiny::sidebarLayout(fluid=TRUE,
                                              shiny::sidebarPanel(width=4,
                                                                  shiny::radioButtons(inputId='PRO_data_source',
                                                                                      label=htmltools::h4('Data source'),
                                                                                      choices=c('Example dataset (PLoS Genet. 2018)'='PRO_demo_data',
                                                                                                'Upload your data!'='PRO_user_data'),
                                                                                      selected='PRO_demo_data') %>% #radioButtons #PRO_data_source
                                                                    shinyhelper::helper(type="inline", title="Data source", size ="l",
                                                                                        content=c('<ol style="font-size: 0px;">',
                                                                                        '<li style="font-size: 16px;">
                                                                                        Lipid dataset can be uploaded by users or using example datasets.
                                                                                        The required data,  Lipid abundance data, should be uploaded in
                                                                                        <mark style="background-color: white;color: red;">CSV</mark>, <mark style="background-color: white;color: red;">TSV</mark>, or <mark style="background-color: white;color: red;">XLSX</mark> format.
                                                                                        The maximum file size is 30MB. <br>
                                                                                        <ul style="font-size: 16px; color:red;">
                                                                                        <li>NOTE: When uploading in XLSX format, ensure the data frame is on the first sheet. </li>
                                                                                        </ul>
                                                                                        </li>',
                                                                                        '<br>
                                                                                        <br />',
                                                                                        '<li style="font-size: 16px;">
                                                                                        Once the file is chosen and shown ‘Upload complete’ then press ‘Upload’.
                                                                                        </li>',
                                                                                        '</ol>')),## helper (for radioButtons PRO_data_source)
                                                                  shiny::conditionalPanel(condition='input.PRO_data_source == "PRO_demo_data"',
                                                                                          shiny::actionButton(inputId='PRO_demo_upload', label='Submit', icon=shiny::icon('upload')), #actionButton #PRO_demo_upload
                                                                                          shiny::downloadButton(outputId='PRO.demo.download', label='Download example')
                                                                                          ), #conditionalPanel  (If the user chooses to use the demo dataset)
                                                                  shiny::conditionalPanel(condition='input.PRO_data_source == "PRO_user_data"',
                                                                                          htmltools::div(id='PRO_user_reset_div',
                                                                                                         shiny::fileInput(inputId='PRO_user_exp', label='Lipid abundance data:', accept=c(".csv", ".tsv", '.xlsx'), multiple=FALSE)%>% #fileInput #PRO_user_exp
                                                                                                           shinyhelper::helper(type="inline", title="Lipid abundance data", size ="l",
                                                                                                                               content=c('<ol style="font-size: 0px;">',
                                                                                                                                         '<li style="font-size: 16px;">The first column must contain a list of unique lipids names (features). </li>',
                                                                                                                                         '<li style="font-size: 16px;">Other columns encompass the expressed values of groups under different conditions that you want to compare.</li>',
                                                                                                                                         '<li style="font-size: 16px;">An example of ‘Lipid abundance data’</li>',
                                                                                                                                         '</ol>',
                                                                                                                                         '<img src="Description/Profiling_Lipid expression data.webp" style="border:2px #ccc solid; padding:20px;" loading="lazy" width="100%"/>')), ## helper (for fileInput PRO_user_exp)
                                                                                                         shiny::helpText("Upload your data table in .csv/.tsv/.xlsx") #helpText
                                                                                                         ), ## div #PRO_user_reset_div
                                                                                          shiny::actionButton(inputId='PRO_user_reset', label='Reset uploaded data', icon=shiny::icon('redo')), #actionButton #PRO_user_reset
                                                                                          shiny::actionButton(inputId='PRO_user_upload', label='Upload', icon=shiny::icon('upload')) #actionButton #PRO_user_upload
                                                                                          ), ## conditionalPanel (If the user chooses to use his own dataset)
                                                                  htmltools::br(),
                                                                  htmltools::HTML("<a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ5' target='_blank' style='color: darkblue;'>How to prepare your dataset?</a>"),
                                                                  htmltools::br(),
                                                                  htmltools::HTML("<a href='https://lipidsig.bioinfomics.org/Tutorial/?Profiling' target='_blank' style='color: darkblue;'>How to use this function?</a>")
                                                                  ), ## sidebarPanel
                                              shiny::mainPanel(width=8,
                                                               shiny::conditionalPanel(condition='input.PRO_user_upload | input.PRO_demo_upload',
                                                                                       htmltools::div(
                                                                                         style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                                                         shiny::htmlOutput("PRO.data.check.progress"))
                                                                                       )## conditionalPanel (if click upload)
                                                               ) ## mainPanel
                                              ), ## sidebarLayout
                         htmltools::div(id='PRO_data_check_successful',style='display:none;',
                                        htmltools::div(id='PRO_data_Uploaded',
                                                       htmltools::h4(htmltools::strong('Uploaded data')),
                                                       DT::dataTableOutput(outputId='PRO.raw.abundance') %>% shinycssloaders::withSpinner() #dataTableOutput #PRO.raw.abundance
                                                       ),
                                        htmltools::br(),
                                        htmltools::div(id='PRO_data_warning', style='display: none;',
                                                     htmltools::div(
                                                       style="text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                       shiny::htmlOutput("PRO.Check.SE")
                                                       ), #div
                                                     shiny::helpText(htmltools::tags$p(icon("check"),": Successfully uploaded.", style="font-size: 16px;", htmltools::HTML('&nbsp;'), shiny::icon("times"), ": Error happaned. Please check your dataset.", htmltools::HTML('&nbsp;'), shiny::icon("exclamation"), ": Warning message.", style="font-size: 16px;")),
                                                     htmltools::br()
                                                     ), ## div # data_warning
                                        htmltools::div(id='PRO_data_processing_div',style='display:none;',
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
                                                                     shiny::checkboxInput(inputId="PRO_rm_NA", label="Remove features with many missing values", value=TRUE), #checkboxInput #PRO_rm_NA
                                                                     shiny::conditionalPanel(condition='input.PRO_rm_NA',
                                                                                             shiny::numericInput(inputId='PRO_filtration_param', label='More than % missing values', 
                                                                                                                 value=70, min=5, max=100, step=5) ## numericInput PRO_rm_NA_pct
                                                                                             )## conditionalPanel (If the user chooses to remove features)
                                                                     ),## column 4
                                                       shiny::column(width=4,
                                                                     shiny::selectInput(inputId='PRO_fill_NA', label='Fill missing value with:',
                                                                                        choices=c('Mean'='mean', 'Median'='median', 'Minimum'='min',
                                                                                                  'Quantile regression imputation of left-censored data'='QRILC',
                                                                                                  'Singular value decomposition'='SVD',
                                                                                                  'K Nearest Neighbors'='KNN',
                                                                                                  'International Risk Management Institute'='IRMI',
                                                                                                  'Probabilistic principal component analysis'='PPCA',
                                                                                                  'Bayesian. Principal Component Analysis'='BPCA'),
                                                                                        selected='min', multiple=FALSE), ## selectInput PRO_fill_NA
                                                                     shiny::conditionalPanel(condition='input.PRO_fill_NA == "min"',
                                                                                             shiny::numericInput(inputId='PRO_fill_min', label='Multiply by minimum',
                                                                                                                 value=0.5, min=0.1, max=0.5, step=0.1)
                                                                                             ), ## conditionalPanel (If the user chooses fill missing value with Minimum)
                                                                     shiny::conditionalPanel(condition='input.PRO_fill_NA == "QRILC"',
                                                                                             shiny::numericInput(inputId='PRO_fill_QRILC', label='Tune sigma',
                                                                                                                 value=1, min=0.1, max=1, step=0.1)
                                                                                             ), ## conditionalPanel (If the user chooses fill missing value with QRILC)
                                                                     shiny::conditionalPanel(condition="['SVD', 'PPCA', 'BPCA'].includes(input.PRO_fill_NA)",
                                                                                             shiny::numericInput(inputId='PRO_fill_param', label='nPCs',
                                                                                                                 value=3, min=1, max=10, step=1)
                                                                                             ), ## conditionalPanel (If the user chooses fill missing value with SVD/PPCA/BPCA)
                                                                     shiny::conditionalPanel(condition='input.PRO_fill_NA == "KNN"',
                                                                                             shiny::numericInput(inputId='PRO_fill_KNN', label='The number of neighbors',
                                                                                                                 value=3, min=1, max=10, step=1)
                                                                                             ) ## conditionalPanel (If the user chooses fill missing value with KNN)
                                                                     ),## column 4
                                                       shiny::column(width=4,
                                                                     htmltools::h4('Data Normalization'),
                                                                     shiny::selectInput(inputId='PRO_normalization', label='Normalization with:', 
                                                                                        choices=c('None'='none',
                                                                                                  'Percentage'='Percentage',
                                                                                                  'Probabilistic Quotient Normalization'='PQN',
                                                                                                  'Quantile normalization'='Quantile',
                                                                                                  'Normalization by sum'='Sum',
                                                                                                  'Normalization by median'='Median'),
                                                                                        selected='Percentage', multiple=FALSE),
                                                                     htmltools::h4('Data Transformation'),
                                                                     shiny::selectInput(inputId='PRO_transformation',
                                                                                        label='Transformation with:',
                                                                                        choices=c('None'='none',
                                                                                                  'log10'='log10',
                                                                                                  'Square root'='square',
                                                                                                  'Cube root'='cube'),
                                                                                        selected='log10', multiple=FALSE)
                                                                     ),## column 4
                                                       shiny::column(width=7),
                                                       shiny::column(width=3, shiny::actionButton(inputId='PRO_processing_reset', label='Reset processing method', icon=shiny::icon('redo'))), #actionButton #PRO_processing_reset
                                                       shiny::column(width=2, shiny::actionButton(inputId='PRO_processing_start', label='Processing', icon=shiny::icon('play'))) #actionButton #PRO_processing_start
                                                       ), ## div # data_processing_div
                                        htmltools::br(),
                                        htmltools::div(id='data_summary_div', 
                                                       style="display: none;text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                       shiny::htmlOutput("PRO.Data.summary")
                                                       ), # div # data_summary_div
                                        htmltools::br(),
                                        htmltools::div(id='data_process_table_div', style='display: none;height: 440px;',
                                                       shiny::column(width=12, htmltools::h3(htmltools::strong('Processed data')),
                                                                     htmltools::div(id='data_process_description_div', 
                                                                                    style="display: none;text-align:justify;background-color:AliceBlue;padding:15px;border-radius:10px",
                                                                                    htmltools::HTML('<h4 style="font-size:18px; text-align:left;line-height: 30px;color:black;">
                                                                                    <ul>
                                                                                    <li><strong>Processed abundance data</strong>: User-uploaded abundance data after data processing.</li>
                                                                                    <li><strong>Lipid characteristics</strong>: Lipid characteristics converted according to the uploaded lipids in the abundance data. Detailed information about the converted characteristics can be found in the <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ11" target="_blank" style="color: darkblue;">FAQ</a>.</li>
                                                                                    <li><strong>Lipid id</strong>: Links to the LION ID, LIPID MAPS ID, and other resource IDs for the uploaded lipids.</li>
                                                                                    <li><strong>Data quality</strong>: Box and density plots of the abundance data before and after data processing.</li>
                                                                                    </ul></h4>')
                                                                                    ),## div # data_process_description_div
                                                                     htmltools::br(),
                                                                     shiny::tabsetPanel(id='PRO_user_process_table_tab',
                                                                                        shiny::tabPanel(title="Processed abundance data",
                                                                                                        DT::dataTableOutput(outputId='PRO.processed.abundance') %>% shinycssloaders::withSpinner() ## dataTableOutput #PRO.exp.tran
                                                                                                        ), ## tabPanel # Processed abundance data
                                                                                        shiny::tabPanel(title="Lipid characteristics",
                                                                                                        DT::dataTableOutput(outputId='PRO.processed.lipid.char.tab') %>% shinycssloaders::withSpinner() ## dataTableOutput #PRO.lipid.char.process
                                                                                                        ), ## tabPanel # Lipid characteristics
                                                                                        shiny::tabPanel(title="Lipid id",
                                                                                                        DT::dataTableOutput(outputId='PRO.lipid.id') %>% shinycssloaders::withSpinner() #dataTableOutput #PRO.lipid.id
                                                                                                        ), ## tabPanel # Lipid id
                                                                                        shiny::tabPanel(title="Data quality", style='height:900px;',
                                                                                                        shiny::column(width=12, htmltools::br()),
                                                                                                        shiny::column(12,
                                                                                                               shiny::column(width=5),
                                                                                                               shiny::column(width=2, style='padding:0px;text-align:center;', shiny::actionButton("PRO.processed.download.start", "Download PDF", icon=shiny::icon("download"))),
                                                                                                               shiny::column(width=5, shiny::downloadButton("PRO.processed.download", "Download", style="visibility:hidden;"))
                                                                                                               ), ## column 12
                                                                                                        shiny::column(width=12, htmltools::br()),
                                                                                                        shiny::column(width=6, plotly::plotlyOutput(outputId='PRO.before.processed.boxplot') %>% shinycssloaders::withSpinner()),
                                                                                                        shiny::column(width=6, plotly::plotlyOutput(outputId='PRO.after.processed.boxplot') %>% shinycssloaders::withSpinner()),
                                                                                                        shiny::column(width=12, htmltools::br()),
                                                                                                        shiny::column(width=6, plotly::plotlyOutput(outputId='PRO.before.processed.density') %>% shinycssloaders::withSpinner()),
                                                                                                        shiny::column(width=6,plotly::plotlyOutput(outputId='PRO.after.processed.density') %>% shinycssloaders::withSpinner())
                                                                                                        ) ## tabPanel # Data quality
                                                                                        ) ## tabsetPanel # PRO_user_process_table_tab
                                                                     ) ## column 12
                                                       ) ## div # data_process_table_div
                                        ), ## div # data_check_successful
                         htmltools::div(id='PRO_start_div', style='display:none;height:50px;',
                             shiny::column(width=5), #column
                             shiny::column(width=2, shiny::actionButton(inputId='PRO_start', label='Start!', icon=shiny::icon('play'))), #actionButton #DE_user_start
                             column(width=5) #column
                         ), ## div # PRO_start_div
                         shiny::column(width=12,htmltools::hr()),
                         ##################################
                         ####  Profiling analysis tab  ####
                         ##################################
                         shiny::column(width=12, 
                                       htmltools::div(id='PRO_tabPanel_div', style='display:none;',
                                                      htmltools::div(htmltools::h2('Result')),
                                                      shiny::tabsetPanel(id='PRO_analysis_tab',
                                                                         ####################################
                                                                         ####  Cross-sample variability  ####
                                                                         ####################################
                                                                         shiny::tabPanel(title='Cross-sample variability',
                                                                                         htmltools::h3('Cross-sample variability'),
                                                                                         htmltools::h6('In this page, three types of distribution plot provide a simple view of sample variability. The first histogram depicts the numbers of lipids expressed in each sample.
                                                                                         The second histogram illustrates the total amount of lipid in each sample. The last density plot visualizes the underlying probability distribution of the lipid abundance in each sample (line). 
                                                                                         Through these plots, users can easily compare the amount/abundance difference of lipid between samples (i.e., patients vs. control).', 
                                                                                                       style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px"),
                                                                                         htmltools::br(),
                                                                                         shiny::column(width=12, htmltools::br()),
                                                                                         shiny::column(width=12,
                                                                                                       shiny::column(width=4),
                                                                                                       shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("PRO.cross.sample.variability.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                       shiny::column(width=4, shiny::downloadButton("PRO.cross.sample.variability.download", "Download", style="visibility:hidden;"))
                                                                                                       ), ## column 12
                                                                                         shiny::column(width=12, htmltools::br()),
                                                                                         shiny::column(width=6, 
                                                                                                       htmltools::h3('Histogram of expressed lipid numbers',style='text-align: center;'),
                                                                                                       plotly::plotlyOutput(outputId='PRO.lipid.number', height='100%') %>% shinycssloaders::withSpinner()
                                                                                                       ), ## column 6
                                                                                         shiny::column(width=6,
                                                                                                       htmltools::h3('Histogram of lipid amount',style='text-align: center;'),
                                                                                                       plotly::plotlyOutput(outputId='PRO.lipid.amount', height='100%') %>% shinycssloaders::withSpinner()
                                                                                                       ), ## column 6
                                                                                         shiny::column(width=6, 
                                                                                                       htmltools::h3('Density plot of lipid abundance distribution',style='text-align: center;'),
                                                                                                       plotly::plotlyOutput(outputId='PRO.lipid.distribution', height='100%') %>% shinycssloaders::withSpinner()
                                                                                                       ) ## column 6
                                                                                         ), ## tabPanel # Cross-sample variability
                                                                         ####################################
                                                                         ####  Dimensionality reduction  ####
                                                                         ####################################
                                                                         shiny::tabPanel(title='Dimensionality reduction',
                                                                                         htmltools::h3('Dimensionality reduction'),
                                                                                         htmltools::h6("Dimensionality reduction is common when dealing with large numbers of observations and/or large numbers of variables in lipids analysis.
                                                                                         It transforms data from a high-dimensional space into a low-dimensional space so that to retain vital properties of the original data and close to its intrinsic dimension.
                                                                                                       Three dimensionality reduction methods are provided in this page, PCA, t-SNE, UMAP.",
                                                                                                       style="text-align:left;background-color:AliceBlue;border-left:8px solid LightSteelBlue;padding: 15px"),
                                                                                         htmltools::br(),
                                                                                         htmltools::div(id='PRO_dim_redu_reset_div',
                                                                                                        shiny::column(width=5,
                                                                                                                      htmltools::div(id='PRO_dim_redu_div',
                                                                                                                                     style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:340px",
                                                                                                                                     shiny::selectInput(inputId='PRO_dim_redu_method', 
                                                                                                                                                        label='Dimensionality reduction method:',
                                                                                                                                                        choices=c('PCA'='pca', 't-SNE'='tsne', 'UMAP'='umap'),
                                                                                                                                                        selected='pca', multiple=FALSE, width='80%'), #selectInput #PRO_dim_redu_method
                                                                                                                                     shiny::conditionalPanel(condition='input.PRO_dim_redu_method == "tsne"',
                                                                                                                                                             shiny::numericInput(inputId='PRO_tsne_perplexity', label='Perplexity:',
                                                                                                                                                                                 value=5, min=3, max=7, step=1, width='80%') %>% 
                                                                                                                                                               shinyhelper::helper(type="inline", title="Perplexity",
                                                                                                                                                                                   content=c("The perplexity may be considered as a knob that sets the number of effective nearest neighbours.The typical perplexity range between 5 and 50.")), ## helper (for numericInput PRO_tsne_perplexity)
                                                                                                                                                             shiny::numericInput(inputId='PRO_tsne_max_iter', label='Number of iterations:',
                                                                                                                                                                                 value=500, min=100, max=5000, step=100, width='80%') %>%
                                                                                                                                                               shinyhelper::helper(type="inline", title="Number of iterations", 
                                                                                                                                                                                   content=c("The number of iterations is the maximum number of iterations to perform.")) ## helper (for numericInput PRO_tsne_max_iter)
                                                                                                                                                             ), ## conditionalPanel (If the user chooses tsne)
                                                                                                                                     shiny::conditionalPanel(condition='input.PRO_dim_redu_method == "umap"',
                                                                                                                                                             shiny::numericInput(inputId='PRO_umap_n_neighbors', label='Number of neighbors:',
                                                                                                                                                                                 value=15, min=2, max=23, step=1, width='80%') %>%
                                                                                                                                                               shinyhelper::helper(type="inline", title="Number of neighbors", 
                                                                                                                                                                                   content=c("The number of neighbors (the number of neighbouring sample points), which is used for manifold approximation.
                                                                                                                                                                                             Larger values lead to more global views of the manifold, whilst smaller values result in more local data being preserved.
                                                                                                                                                                                             In general values should be in the range 2 to 100.")), ## helper (for numericInput PRO_umap_n_neighbors)
                                                                                                                                                             shiny::selectInput(inputId='PRO_umap_metric', label='Distance metric:',
                                                                                                                                                                                choices=c('Euclidean'='euclidean',
                                                                                                                                                                                          'Cosine'='cosine',
                                                                                                                                                                                          'Manhattan'='manhattan',
                                                                                                                                                                                          'Hamming'='hamming'),
                                                                                                                                                                                selected='euclidean', multiple=FALSE, width='80%') %>%
                                                                                                                                                               shinyhelper::helper(type="inline", title="Distance metric",
                                                                                                                                                                                   content=c("The distance metric is use to find nearest neighbors.")) ## helper (for selectInput PRO_umap_metric)
                                                                                                                                                             ) ## conditionalPanel (If the user chooses umap)
                                                                                                                                     ), ## div # PRO_dim_redu_div
                                                                                                                      ), ## column 5
                                                                                                        shiny::column(width=5,
                                                                                                                      htmltools::div(id='PRO_dim_redu2_div',
                                                                                                                                     style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:340px",
                                                                                                                                     shiny::selectInput(inputId='PRO_cluster_method', label='Clustering method:',
                                                                                                                                                        choices=c('K-means'='kmeans',
                                                                                                                                                                  'Partitioning around medoids (PAM)'='kmedoids',
                                                                                                                                                                  'Hierarchical clustering'='hclustering',
                                                                                                                                                                  'DBSCAN'='dbscan'),
                                                                                                                                                        selected='kmeans', multiple=FALSE, width='80%'), ## selectInput # PRO_cluster_method
                                                                                                                                     shiny::conditionalPanel(condition='input.PRO_cluster_method == "kmeans"',
                                                                                                                                                             shiny::sliderInput(inputId='PRO_kmeans_group', label='Number of groups:',
                                                                                                                                                                                value=2, min=2, max=10, step=1, width='80%') ## sliderInput # PRO_kmeans_group
                                                                                                                                                             ), ## conditionalPanel (If the user chooses kmeans)
                                                                                                                                     shiny::conditionalPanel(condition='input.PRO_cluster_method == "kmedoids"',
                                                                                                                                                             shiny::sliderInput(inputId='PRO_pam_group', label='Number of groups:', 
                                                                                                                                                                                value=2, min=2, max=10,  step=1, width='80%'), ## sliderInput # PRO_pam_group
                                                                                                                                                             shiny::selectInput(inputId='PRO_pam_metric', label='Distance metrics:',
                                                                                                                                                                                choices=c('Euclidean'='euclidean',
                                                                                                                                                                                          'Manhattan'='manhattan'),
                                                                                                                                                                                selected='euclidean', multiple=FALSE, width='80%') ## selectInput # PRO_pam_metric
                                                                                                                                                             ), ## conditionalPanel (If the user chooses kmedoids)
                                                                                                                                     shiny::conditionalPanel(condition='input.PRO_cluster_method == "hclustering"',
                                                                                                                                                             shiny::sliderInput(inputId='PRO_hclust_group', label='Number of groups:',
                                                                                                                                                                                value=2, min=2, max=10, step=1, width='80%'), ## sliderInput # PRO_hclust_group
                                                                                                                                                             shiny::selectInput(inputId='PRO_hclust_dist', label='Distance measure:',
                                                                                                                                                                                choices=c('Pearson'='pearson',
                                                                                                                                                                                          'Spearman'='spearman',
                                                                                                                                                                                          'Kendall'='kendall',
                                                                                                                                                                                          "Euclidean"="euclidean",
                                                                                                                                                                                          "Maximum"="maximum",
                                                                                                                                                                                          "Manhattan"="manhattan",
                                                                                                                                                                                          "Canberra"="canberra",
                                                                                                                                                                                          "Binary"="binary",
                                                                                                                                                                                          "Minkowski"="minkowski"),
                                                                                                                                                                                selected='pearson', multiple=FALSE, width='80%'), ## selectInput # PRO_hclust_dist
                                                                                                                                                             shiny::selectInput(inputId='PRO_hclust_hclust', label='Clustering method:',
                                                                                                                                                                                choices=c('Complete'='complete',
                                                                                                                                                                                          'Single'='single',
                                                                                                                                                                                          'Median'='median',
                                                                                                                                                                                          'Average'='average',
                                                                                                                                                                                          "Ward.D"="ward.D",
                                                                                                                                                                                          "Ward.D2"="ward.D2",
                                                                                                                                                                                          "WPGMA"="mcquitty",
                                                                                                                                                                                          "WOGMC"="median",
                                                                                                                                                                                          "UPGMC"="centroid"),
                                                                                                                                                                                selected='complete', multiple=FALSE, width='80%') ## selectInput # PRO_hclust_hclust
                                                                                                                                                             ), ## conditionalPanel (If the user chooses hclustering)
                                                                                                                                     shiny::conditionalPanel(condition='input.PRO_cluster_method == "dbscan"',
                                                                                                                                                             shiny::numericInput(inputId='PRO_dbscan_eps', label='Epsilon:',
                                                                                                                                                                                 min=0.1, value=0.5, step=0.1, width='80%'), ## numericInput # PRO_dbscan_eps
                                                                                                                                                             shiny::numericInput(inputId='PRO_dbscan_minPts', label='minPts:',
                                                                                                                                                                                 value=1, min=1, max=22, width='80%') ## numericInput # PRO_dbscan_minPts
                                                                                                                                                             ) ## conditionalPanel (If the user chooses dbscan)
                                                                                                                                     ) ## div # PRO_dim_redu2_div
                                                                                                                      ) ## column 5
                                                                                                        ), ## div # PRO_dim_redu_reset_div
                                                                                         shiny::column(width=2,
                                                                                                       htmltools::br(),
                                                                                                       htmltools::br(),
                                                                                                       htmltools::br(),
                                                                                                       htmltools::br(),
                                                                                                       htmltools::br(),
                                                                                                       htmltools::br(),
                                                                                                       shiny::actionButton(inputId='PRO_dim_redu_start', label='Submit', icon=shiny::icon('play'), width='60%'), #PRO_dim_redu_start
                                                                                                       htmltools::br(),
                                                                                                       htmltools::br(),
                                                                                                       shiny::actionButton(inputId='PRO_dim_redu_reset', label='Reset', icon=shiny::icon('redo'), width='60%'), #actionButton #PRO_dim_redu_reset
                                                                                                       ), ## column 2
                                                                                         shiny::column(width=12, htmltools::br()),
                                                                                         htmltools::div(id='PRO_dim_redu_result_div',
                                                                                                        style='display:none;',
                                                                                                        htmltools::div(id='PRO_dim_redu_result_pca_div',style='display:none;',
                                                                                                                      shiny::column(width=12,style='border: solid 2px grey;',
                                                                                                                                    htmltools::br(),
                                                                                                                                    htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                                    '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                    The PCA plot visually simplifies and discerns patterns within complex lipidomic data, effectively reducing multidimensional variables to principal components.
                                                                                                                                                    The distinct separation or overlap of the groups reflects the underlying differences or similarities.
                                                                                                                                                    <br>
                                                                                                                                                    <br />
                                                                                                                                                    The scree plot is a common method for determining the number of PCs to be retained.
                                                                                                                                                    The "elbow" of the graph indicates all components to the left of this point can explain most variability of the samples.
                                                                                                                                                    </h4>
                                                                                                                                                    <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                         <ul>
                                                                                                                                                         <li>
                                                                                                                                                         Hover the mouse on the plot to view the corresponding detailed information.
                                                                                                                                                         </li>
                                                                                                                                                         <li>
                                                                                                                                                         For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.
                                                                                                                                                         </li>
                                                                                                                                                         </ul>
                                                                                                                                                    </h4>',
                                                                                                                                                    '</div>'), ## HTML
                                                                                                                                    shiny::column(width=12,htmltools::br()),
                                                                                                                                    shiny::column(width=12,
                                                                                                                                                  shiny::column(width=4),
                                                                                                                                                  shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("PRO.dim.redu.pca.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                                  shiny::column(width=4, shiny::downloadButton("PRO.dim.redu.pca.download", "Download", style="visibility:hidden;"))
                                                                                                                                                  ), ## column 12
                                                                                                                                    shiny::column(width=12,
                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                htmltools::h3('PCA plot',style='text-align:center;'),
                                                                                                                                                                plotly::plotlyOutput(outputId='PRO.pca.plot') %>% shinycssloaders::withSpinner(), #plotlyOutput #PRO.pca.biplot
                                                                                                                                                                ), ## column 6
                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                htmltools::h3('PCA scree plot',style='text-align: center;'),
                                                                                                                                                                plotly::plotlyOutput(outputId='PRO.pca.screeplot') %>% shinycssloaders::withSpinner(), #plotlyOutput #PRO.pca.screeplot
                                                                                                                                                                ) ## column 6
                                                                                                                                                  ), ## column 12
                                                                                                                                    shiny::column(width=12,htmltools::br()),
                                                                                                                                    shiny::column(width=6,
                                                                                                                                                  htmltools::h3('Table of PCA rotated data',style='text-align: center;'),
                                                                                                                                                  DT::dataTableOutput(outputId='PRO.pca.rotated.data') %>% shinycssloaders::withSpinner()
                                                                                                                                                  ), ## column 6
                                                                                                                                    shiny::column(width=6,
                                                                                                                                                  htmltools::h3('Table of PCA contribution table',style='text-align: center;'),
                                                                                                                                                  DT::dataTableOutput(outputId='PRO.pca.contrib.table') %>% shinycssloaders::withSpinner()
                                                                                                                                                  ), ## column 6
                                                                                                                                    shiny::column(width=12,htmltools::br())
                                                                                                                                    ), ## column 12
                                                                                                                      shiny::column(width=12,htmltools::br()),
                                                                                                                      shiny::column(width=12, style='border: solid 2px grey;',
                                                                                                                                    htmltools::br(),
                                                                                                                                    htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                                    '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                     The correlation circle plot illustrates the relationship between top N individual features (lipid species) and principal components (PCs).
                                                                                                                                                     It displays how all the variables are interrelated, with those positively correlated positioned in the same quadrant and negatively correlated ones located diametrically across the origin of the plot.
                                                                                                                                                     <br>
                                                                                                                                                     <br />
                                                                                                                                                     The feature contribution histogram offers an in-depth view of how individual features (lipid species) contribute to a user-selected principal component, such as PC1, PC2, or a combination thereof (PC1+PC2).
                                                                                                                                                     It allows users to identify which features influence the chosen principal component more.
                                                                                                                                                     </h4>
                                                                                                                                                     <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                           <ul>
                                                                                                                                                           <li>
                                                                                                                                                           Adjust the slider above each plot to choose the desired number of top features to display.
                                                                                                                                                           </li>
                                                                                                                                                           <li>
                                                                                                                                                           Hover the mouse on the plot to view the corresponding detailed information.
                                                                                                                                                           </li>
                                                                                                                                                           <li>
                                                                                                                                                           For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.
                                                                                                                                                           </li>
                                                                                                                                                           </ul>
                                                                                                                                                     </h4>',
                                                                                                                                                     '</div>'), ## HTML
                                                                                                                                    shiny::column(width=12,htmltools::br()),
                                                                                                                                    shiny::column(width=12,style='padding:0px;',
                                                                                                                                                  htmltools::div(style='height: 125px;padding:19px;margin-bottom:20px;background-color: #ecf0f1;border: 1px solid transparent;border-radius: 4px;',
                                                                                                                                                                 shiny::column(width=1),
                                                                                                                                                                 shiny::column(width=4,
                                                                                                                                                                               shiny::sliderInput(inputId='PRO_pca_variable_topN', label='top N feature:',
                                                                                                                                                                                                  min=1, max=30, value=10, step=1, width='100%')
                                                                                                                                                                               ),## column 4
                                                                                                                                                                 shiny::column(width=2),
                                                                                                                                                                 shiny::column(width=4,
                                                                                                                                                                               shiny::selectInput(inputId='PRO_pca_contrib_PC', label='Contribution of features to principal component:',
                                                                                                                                                                                                  choices=c('Component 1'=1,
                                                                                                                                                                                                            'Component 2'=2,
                                                                                                                                                                                                            'Component 1 & Component 2'= '1_2'),
                                                                                                                                                                                                  selected='1_2', multiple=FALSE)
                                                                                                                                                                               ),## column 4
                                                                                                                                                                 shiny::column(width=1)
                                                                                                                                                                 ) ## div
                                                                                                                                                  ),## column 12
                                                                                                                                    shiny::column(width=12,
                                                                                                                                                  shiny::column(width=4),
                                                                                                                                                  shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("PRO.dim.redu.pca.topN.download.start", "Download PDF", icon=shiny::icon("download"))),
                                                                                                                                                  shiny::column(width=4, shiny::downloadButton("PRO.dim.redu.pca.topN.download", "Download", style="visibility:hidden;"))
                                                                                                                                                  ), ## column 12
                                                                                                                                    shiny::column(width=6,
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  htmltools::h3('PCA correlation circle plot',style='text-align: center;'),
                                                                                                                                                  plotly::plotlyOutput(outputId='PRO.pca.variable') %>% shinycssloaders::withSpinner() #plotlyOutput #PRO.pca.variable
                                                                                                                                                  ), ## column 6
                                                                                                                                    shiny::column(width=6,
                                                                                                                                                  htmltools::br(),
                                                                                                                                                  htmltools::h3('Feature contribution histogram',style='text-align: center;'),
                                                                                                                                                  plotly::plotlyOutput(outputId='PRO.pca.contrib') %>% shinycssloaders::withSpinner() #plotlyOutput #PRO.pca.contrib
                                                                                                                                                  ), ## column 6
                                                                                                                                    shiny::column(width=12,htmltools::br())
                                                                                                                                    )## column 12
                                                                                                                      ), ## div # PRO_dim_redu_result_pca_div
                                                                                                        htmltools::div(id='PRO_dim_redu_result_tsne_div', style='display:none;',
                                                                                                                      shiny::column(width=12, style='border: solid 2px grey;',
                                                                                                                                    htmltools::br(),
                                                                                                                                    htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                                    '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                     The t-SNE plot visually simplifies and discerns patterns within complex lipidomic data, effectively reducing multidimensional variables to principal components.
                                                                                                                                                     The distinct separation or overlap of the groups reflects the underlying differences or similarities.
                                                                                                                                                     </h4>
                                                                                                                                                     <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                     <ul>
                                                                                                                                                     <li>
                                                                                                                                                     Hover the mouse on the plot to view the corresponding detailed information.
                                                                                                                                                     </li>
                                                                                                                                                     <li>
                                                                                                                                                     For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.
                                                                                                                                                     </li>
                                                                                                                                                     </ul>
                                                                                                                                                     </h4>',
                                                                                                                                                     '</div>'),
                                                                                                                                    shiny::column(width=12,htmltools::br()),
                                                                                                                                    shiny::column(width=12,
                                                                                                                                                  shiny::column(width=4),
                                                                                                                                                  shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("PRO.dim.redu.tsne.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                                  shiny::column(width=4, shiny::downloadButton("PRO.dim.redu.tsne.download", "Download", style="visibility:hidden;"))
                                                                                                                                    ), ## column 12
                                                                                                                                    shiny::column(width=12,
                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                htmltools::h3('t-SNE plot',style='text-align: center;'),
                                                                                                                                                                plotly::plotlyOutput(outputId='PRO.tsne.plot', height='400px') %>% shinycssloaders::withSpinner()
                                                                                                                                                  ), ## column 6
                                                                                                                                                  shiny::column(width=6,
                                                                                                                                                                htmltools::h3('Table of t-SNE data',style='text-align: center;'),
                                                                                                                                                                DT::dataTableOutput(outputId='PRO.tsne.table') %>% shinycssloaders::withSpinner()
                                                                                                                                                  ) ## column 6
                                                                                                                                    ) ## column 12
                                                                                                                                    )## column 12
                                                                                                                      ), ## div # PRO_dim_redu_result_tsne_div
                                                                                                        htmltools::div(id='PRO_dim_redu_result_umap_div',style='display:none;',
                                                                                                                       shiny::column(width=12,style='border: solid 2px grey;',
                                                                                                                                     htmltools::br(),
                                                                                                                                     htmltools::HTML('<div style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px">',
                                                                                                                                                     '<h4 style="text-align:left; line-height: 25px;">
                                                                                                                                                      The UMAP plot visually simplifies and discerns patterns within complex lipidomic data, effectively reducing multidimensional variables to principal components.
                                                                                                                                                      The distinct separation or overlap of the groups reflects the underlying differences or similarities.
                                                                                                                                                      </h4>
                                                                                                                                                      <h4 style="font-style:italic; font-weight: bolder; font-size:17px; line-height: 30px; color: #CD5C5C">
                                                                                                                                                      <ul>
                                                                                                                                                      <li>
                                                                                                                                                      Hover the mouse on the plot to view the corresponding detailed information.
                                                                                                                                                      </li>
                                                                                                                                                      <li>
                                                                                                                                                      For figure manipulation, please refer to <a href="https://lipidsig.bioinfomics.org/FAQ/?FAQ10" target="_blank" style="color: darkblue;">FAQ</a>.
                                                                                                                                                      </li>
                                                                                                                                                      </ul>
                                                                                                                                                      </h4>',
                                                                                                                                                      '</div>'),
                                                                                                                                     shiny::column(width=12,htmltools::br()),
                                                                                                                                     shiny::column(width=12,
                                                                                                                                                   shiny::column(width=4),
                                                                                                                                                   shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("PRO.dim.redu.umap.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                                                                   shiny::column(width=4, shiny::downloadButton("PRO.dim.redu.umap.download", "Download", style="visibility:hidden;"))
                                                                                                                                     ), ## column 12
                                                                                                                                     shiny::column(width=12,
                                                                                                                                                   shiny::column(width=6,
                                                                                                                                                                 htmltools::h3('UMAP plot',style='text-align: center;'),
                                                                                                                                                                 plotly::plotlyOutput(outputId='PRO.umap.plot', height='400px') %>% shinycssloaders::withSpinner()
                                                                                                                                                   ), ## column 6
                                                                                                                                                   shiny::column(width=6,
                                                                                                                                                                 htmltools::h3('Table of UMAP data',style='text-align: center;'),
                                                                                                                                                                 DT::dataTableOutput(outputId='PRO.umap.table') %>% shinycssloaders::withSpinner()
                                                                                                                                                   ) ## column 6
                                                                                                                                                   ) ## column 12
                                                                                                                                     ) ## column 12
                                                                                                                       ) ## div # PRO_dim_redu_result_umap_div
                                                                                                        ) ## div # PRO_dim_redu_result_div
                                                                                         ), #tabPanel #Dimensionality reduction
                                                                         ###############################
                                                                         ####  Correlation heatmap  ####
                                                                         ###############################
                                                                         shiny::tabPanel(title='Correlation heatmap',
                                                                                         htmltools::h3('Correlation heatmap'),
                                                                                         htmltools::h6(htmltools::p("Correlation heatmaps illustrate the correlation between lipid samples or characteristics and depict the patterns in each group. The correlation can be calculated by Pearson or Spearman. The correlation coefficient is clustered depending on the user-defined method and distance. Furthermore, users have to select a lipid characteristic to display the heatmap. Two heatmaps will be shown by lipid samples or by characteristics.",
                                                                                                                    style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                         htmltools::br(),
                                                                                         htmltools::div(id='PRO_corr_heatmap_style_div',
                                                                                                        style="text-align:justify;background-color:HoneyDew;padding:15px;border-radius:10px;height:95px",
                                                                                                        htmltools::div(id='PRO_corr_heatmap_reset_div',
                                                                                                                       shiny::column(width=2,
                                                                                                                                     shiny::selectInput(inputId='PRO_corr_heatmap_method', label='Correlation method:',
                                                                                                                                                        choices=c('Pearson'='pearson',
                                                                                                                                                                  'Spearman'='spearman'),
                                                                                                                                                        selected='spearman', multiple=FALSE) ## selectInput # PRO_corr_heatmap_method
                                                                                                                                     ), ## column 2
                                                                                                                       shiny::column(width=2,
                                                                                                                                     shiny::selectInput(inputId='PRO_corr_heatmap_dist', label='Distance measure:',
                                                                                                                                                        choices=c('Pearson'='pearson',
                                                                                                                                                                  'Spearman'='spearman',
                                                                                                                                                                  'Kendall'='kendall',
                                                                                                                                                                  "Euclidean"="euclidean",
                                                                                                                                                                  "Maximum"="maximum",
                                                                                                                                                                  "Manhattan"="manhattan",
                                                                                                                                                                  "Canberra"="canberra",
                                                                                                                                                                  "Binary"="binary",
                                                                                                                                                                  "Minkowski"="minkowski"),
                                                                                                                                                        selected='maximum', multiple=FALSE) ## selectInput # PRO_corr_heatmap_dist
                                                                                                                                     ), ## column 2
                                                                                                                       shiny::column(width=2,
                                                                                                                                     shiny::selectInput(inputId='PRO_corr_heatmap_hclust', label='Clustering method:',
                                                                                                                                                        choices=c('Complete'='complete',
                                                                                                                                                                  'Single'='single',
                                                                                                                                                                  'Median'='median',
                                                                                                                                                                  'Average'='average',
                                                                                                                                                                  "Ward.D"="ward.D",
                                                                                                                                                                  "Ward.D2"="ward.D2",
                                                                                                                                                                  "WPGMA"="mcquitty",
                                                                                                                                                                  "WOGMC"="median",
                                                                                                                                                                  "UPGMC"="centroid"),
                                                                                                                                                        selected='average', multiple=FALSE) ## selectInput # PRO_corr_heatmap_hclust
                                                                                                                                     ), ## column 2
                                                                                                                       shiny::column(width=3, shiny::uiOutput("PRO.corr.heatmap.char.select")), #column
                                                                                                                       ), ## div # PRO_corr_heatmap_reset_div
                                                                                                        shiny::column(width=3,
                                                                                                                      htmltools::br(),
                                                                                                                      shiny::actionButton(inputId='PRO_corr_heatmap_reset', label='Reset', icon=shiny::icon('redo'), width='40%'), #actionButton #PRO_corr_heatmap_reset
                                                                                                                      shiny::actionButton(inputId='PRO_corr_heatmap_start', label='Submit', icon=shiny::icon('play'), width='40%') #actionButton #PRO_corr_heatmap_start
                                                                                                                      ) ## column 3 
                                                                                                        ), #div #PRO_corr_heatmap_style_div
                                                                                         htmltools::br(),
                                                                                         htmltools::div(id='PRO_corr_heatmap_result_div',
                                                                                                        style='display:none;',
                                                                                                        shiny::column(width=12,htmltools::br()),
                                                                                                        shiny::column(width=5),
                                                                                                        shiny::column(width=12,
                                                                                                               shiny::column(width=4),
                                                                                                               shiny::column(width=4, style='padding:0px;text-align:center;', shiny::actionButton("PRO.corr.heatmap.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                               shiny::column(width=4, shiny::downloadButton("PRO.corr.heatmap.download", "Download", style="visibility:hidden;"))
                                                                                                               ), ## column 12
                                                                                                        shiny::column(width=12,htmltools::br()),
                                                                                                        shiny::column(width=6,
                                                                                                                      htmltools::h4('By samples'),
                                                                                                                      iheatmapr::iheatmaprOutput(outputId='PRO.corr.heatmap.sample', height='520px') %>% shinycssloaders::withSpinner(),
                                                                                                                      htmltools::br()
                                                                                                                      ), ## column 6
                                                                                                        shiny::column(width=6,
                                                                                                                      htmltools::h4('By lipid characteristics'),
                                                                                                                      iheatmapr::iheatmaprOutput(outputId='PRO.corr.heatmap.class', height='520px') %>% shinycssloaders::withSpinner(),
                                                                                                                      htmltools::br()
                                                                                                                      ) ## column 6
                                                                                                        ) ## div # PRO_corr_heatmap_result_div
                                                                                         ), ## tabPanel # Correlation heatmap
                                                                         ##########################################
                                                                         ####  Lipid characteristics analysis  ####
                                                                         ##########################################
                                                                         shiny::tabPanel(title='Lipid characteristics profiling',
                                                                                         htmltools::h3('Lipid characteristics profiling'),
                                                                                         htmltools::h6(htmltools::p("In this page, users can discover lipid abundance over specific lipid characteristics by scrolling dropdown menu.
                                                                                                                    Lipids will be firstly classified by the selected characteristics from ‘Lipid characteristics’ table uploaded by users. Next,
                                                                                                                    the lipid abundance will be shown in bar plot, which depicts the lipid abundance level of each sample within each group (e.g., PE, PC) of selected characteristics (e.g., class).
                                                                                                                    Additionally, a stacked horizontal bar chart reveals the percentage of characteristics in each sample.
                                                                                                                    For instance, if users select class as lipid characteristics from the dropdown menu, the stacked bar chart will tell users the percentage of TAG, ST, SM etc. of each sample,
                                                                                                                    the variability of percentage between samples can also be obtained from this plot.",
                                                                                                              style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 15px")),
                                                                                         htmltools::br(),
                                                                                         shiny::column(width=12,
                                                                                                       shiny::column(width=4),
                                                                                                       shiny::column(width=4,uiOutput('PRO.lipid.char')),
                                                                                                       shiny::column(width=4)),
                                                                                         shiny::column(width=12,
                                                                                                       shiny::column(width=4),
                                                                                                       shiny::column(width=4, style='padding:0px;padding-left: 50px;', shiny::actionButton("PRO.lipid.char.download.start", "Download PDF and table", icon=shiny::icon("download"))),
                                                                                                       shiny::column(width=4, shiny::downloadButton("PRO.lipid.char.download", "Download", style="visibility:hidden;"))),
                                                                                         
                                                                                         htmltools::br(),
                                                                                         htmltools::br(),
                                                                                         shiny::column(width=12,
                                                                                                       shiny::column(width=1),
                                                                                                       shiny::column(width=10,
                                                                                                                     htmltools::h3('Bar plot classified by selected characteristic', style='text-align:center;'),
                                                                                                                     plotly::plotlyOutput(outputId='PRO.lipid.char.barplot', height='500px') %>% shinycssloaders::withSpinner(),
                                                                                                                     htmltools::br(),
                                                                                                                     htmltools::br(),
                                                                                                                     htmltools::h3('Stacked horizontal bar chart of lipid class composition', style='text-align:center;'),
                                                                                                                     plotly::plotlyOutput(outputId='PRO.lipid.char.composition', height='600px') %>% shinycssloaders::withSpinner(),
                                                                                                                     htmltools::br(),
                                                                                                                     htmltools::br()), ## column 10
                                                                                                       shiny::column(width=1)
                                                                                                       ) ## column 12
                                                                                         ) ## tabPanel # Lipid characteristics analysis
                                                                         ) ## tabsetPanel
                                                      ) ## div # PRO_tabPanel_div
                                       )## column 12
                         ) ## column 12
         ) ## fluidRow
) ## tabPanel
