
tabPanel(title = h4(icon('home')),
         div(style = 'font-size:0px;',
         tags$p(strong('What is LipidSig?',style = 'font-size:24px;')),
         tags$p(strong('LipidSig'),' is the first web-based platform which integrates a comprehensive analysis for streamlined data mining of lipidomic datasets.',style = 'font-size:20px;'),
         tags$p('The user-friendly interface provides five main functions, namely ',
                actionLink("link_to_help_Profiling", strong("Profiling"),style = "color: darkblue;background-color: white;"),
                ',',
                actionLink("link_to_help_DE", strong("Differential expression"),style = "color: darkblue;background-color: white;"),
                ',',
                actionLink("link_to_help_ML", strong("Machine learning"),style = "color: darkblue;background-color: white;"),
                ',',
                actionLink("link_to_help_Correlation", strong("Correlation"),style = "color: darkblue;background-color: white;"),
                ' and ',
                actionLink("link_to_help_Network", strong("Network"),style = "color: darkblue;background-color: white;"),
                ' for assessment of lipid effects on biological mechanisms.',
                'The five functions provide unique aspects to analyze the lipidome profiling data based on different characteristics including lipid class, chain length, unsaturation, hydroxyl group, and fatty acid composition.',
                'In summary, LipidSig enables users to perform intensive lipid analysis and create interactive plots with downloadable images and corresponding tables.', style = 'font-size:20px;')),
         tags$p(strong('LipidSig is free and open to all users and provides downloadable example datasets in each analysis section.'),style = 'color: red;font-size:20px;text-align: center;'),
         fluidRow(
           introBox(
                    div(class='landing-page-box',
                        div('Profiling', class = 'landing-page-box-title'),
                        div(class = 'landing-page-icon', style= 'background-image: url("home/Profiling.png");background-size: auto 100%; background-position: center; background-repeat: no-repeat; '),
                        actionButton('jump_to_Profiling', NULL, class='landing-page-button')
                        ),
                    data.step = 1,
                    data.intro = h6(' '),
                    data.position = 'bottom-right-aligned'),
           div(sytle="width:10%;"),
           introBox(
                    div(class='landing-page-box',
                        div('Differential expression', class = 'landing-page-box-title'),
                        div(class = 'landing-page-icon', style= 'background-image: url("home/DE.png");background-size: 80%; background-position: center; background-repeat: no-repeat; '),
                        actionButton('jump_to_DE', NULL, class='landing-page-button')
                        ),
                    data.step = 2,
                    data.intro = h6(' ')),
           div(style = 'width:1%;'),
           introBox(
                    div(class='landing-page-box',
                        div('Machine learning', class = 'landing-page-box-title'),
                        div(class = 'landing-page-icon', style= 'background-image: url("home/ML.png");background-size: auto 100%; background-position: center; background-repeat: no-repeat; '),
                        actionButton('jump_to_ML', NULL, class='landing-page-button')
                        ),
                    data.step = 3,
                    data.intro = h6(' ')),
           introBox(
                    div(class='landing-page-box',
                        div('Correlation', class = 'landing-page-box-title'),
                        div(class = 'landing-page-icon', style= 'background-image: url("home/Correlation.png");background-size: auto 70%; background-position: center; background-repeat: no-repeat; '),
                        actionButton('jump_to_Correlation', NULL, class='landing-page-button')
                    ),
                    data.step = 4,
                    data.intro = h6('')
           ),
           introBox(
                    div(class='landing-page-box',
                        div('Network', class = 'landing-page-box-title'),
                        div(class = 'landing-page-icon', style= 'background-image: url("home/Network.png");background-size: auto 80%; background-position: center; background-repeat: no-repeat; '),
                        actionButton('jump_to_Network', NULL, class='landing-page-button')
                    ),
                    data.step = 5,
                    data.intro = h6('')
           ),
           column(width = 12,br()),
           column(width = 12, 
                  div(h6(strong('Citation:')),
                      h6('Lin WJ, Shen PC, Liu HC, Cho YC, Hsu MK, Lin IC, Chen FH, Yang JC, Ma WL, Cheng WC. LipidSig: a web-based tool for lipidomic data analysis. Nucleic Acids Res. 2021 Jul 2;49(W1):W336-W345. doi: 10.1093/nar/gkab419. PMID: 34048582.'),
                      style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 8px"
                      )
                  ),
           column(width = 12,br()),
           column(width = 12,style='text-align:center;',img(src='home/home.png', width='850px',height='100%')), 
           column(width = 12,br()),
           column(width = 12,br()),
           column(width = 12,br()),
           column(width = 12,br())
         )
)
         