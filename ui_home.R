shiny::tabPanel(title=htmltools::HTML("<h4 style='font-size:18px;padding-right:0px;padding-top:20.5px;padding-bottom:20.5px;'>Home</h4>"),
         value='Home',
         htmltools::div(style='font-size:0px; background-color: PowderBlue;border-left: 8px solid Teal;padding: 15px',
             htmltools::tags$p(htmltools::strong('What is LipidSig?', style='font-size:24px; line-height: 30px;')),
             htmltools::tags$p(htmltools::strong('LipidSig'),' is the first web-based platform which integrates a comprehensive analysis for streamlined data mining of lipidomic datasets.', style='font-size:20px;'),
             htmltools::HTML('<h4 style="font-size:20px;">
                              The user-friendly interface provides two helper functions,
                              <a href="javascript:void(0)" onclick="switchTab(\'Data_Check\')" style="color: darkblue;">Data Check</a>,
                              <a href="javascript:void(0)" onclick="switchTab(\'ID_conversion\')" style="color: darkblue;">ID Conversion</a>, and six analytical functions:
                              <a href="javascript:void(0)" onclick="switchTab(\'Profiling\')" style="color: darkblue;">Profiling</a>,
                              <a href="javascript:void(0)" onclick="switchTab(\'Differential expression\')" style="color: darkblue;">Differential Expression</a>,
                              <a href="javascript:void(0)" onclick="switchTab(\'Enrichment\')" style="color: darkblue;">Enrichment</a>,
                              <a href="javascript:void(0)" onclick="switchTab(\'ML\')" style="color: darkblue;">Machine Learning</a>,
                              <a href="javascript:void(0)" onclick="switchTab(\'Correlation\')" style="color: darkblue;">Correlation</a> and
                              <a href="javascript:void(0)" onclick="switchTab(\'Network\')" style="color: darkblue;">Network</a>.
                              <br>
                              The two helper functions offer format verification for user-uploaded data, map to 9 resource IDs, and automatically assign 29 lipid characteristics based on the uploaded features.
                              <br>
                              The six analytical functions are designed for the assessment of lipid effects on biological mechanisms and provide unique aspects to analyze the lipidome profiling data based on different characteristics, including lipid class, chain length, unsaturation, hydroxyl group, and fatty acid composition.
                              Users can perform intensive lipid analysis and create interactive plots with downloadable images and corresponding tables.
                              <br>
                              Suggest browsers: Chrome, Edge, and Firefox.
                              </h4>')),
         HTML('<script src="sweetalert2/dist/sweetalert2.min.js"></script>
              <script src="https://cdn.jsdelivr.net/npm/sweetalert2@latest"></script>'),
         HTML('<script type="text/javascript">Swal.fire({
  title: "News",
  width: 600,
  html: `<h4 style="text-align: left;font-size: 20px;line-height: 1.1;margin: 0px;">
  We are excited to share that LipidSig 2.0 providing a stronger and more efficient foundation for lipidomics analysis.
  <br>Highlights of this update:
  <ul>
  <li>Several bugs have been fixed, improving stability and overall user experience.</li>
  <li>If you discover any new bugs or issues, please contact us so we can address them promptly.</li>
  </ul>
  Thank you for your continued support!</h4>
  `,
  icon: "info"
});</script>'),
         htmltools::tags$p(htmltools::strong('LipidSig is free and open to all users and provides downloadable example datasets in each analysis section.'), style='color: red;font-size:20px;text-align: center; padding-top:15px;'),
         shiny::column(width=6, style='padding:0px;',
                       htmltools::div(
                         htmltools::div(style='display: inline-block;width:290px;height:62px;border-radius:10px;margin-top: 5px;margin-bottom: 5px;padding: 0px;border: 3px solid #DDDDDD',
                                        htmltools::HTML('<a href="javascript:void(0)" onclick="switchTab(\'Data_Check\')">
                                                         <div class="home_block">
                                                         <span class="name">Data Check</span>
                                                         </div></a>')),
                         htmltools::div(style='display: inline-block;width:290px;height:62px;border-radius:10px;margin-top: 5px;margin-bottom: 5px;padding: 0px;border: 3px solid #DDDDDD',
                                        htmltools::HTML('<a href="javascript:void(0)" onclick="switchTab(\'Profiling\')">
                                                         <div class="home_block">
                                                         <span class="name">Profiling</span>
                                                        </div></a>')),
                         htmltools::div(style='display: inline-block;width:290px;height:62px;border-radius:10px;margin-top: 5px;margin-bottom: 5px;padding: 0px;border: 3px solid #DDDDDD',
                                        htmltools::HTML('<a href="javascript:void(0)" onclick="switchTab(\'Differential expression\')">
                                                         <div class="home_block">
                                                         <span class="name">Differential Expression</span>
                                                        </div></a>')),
                         htmltools::div(style='display: inline-block;width:290px;height:62px;border-radius:10px;margin-top: 5px;margin-bottom: 5px;padding: 0px;border: 3px solid #DDDDDD',
                                        htmltools::HTML('<a href="javascript:void(0)" onclick="switchTab(\'Enrichment\')">
                                                         <div class="home_block">
                                                         <span class="name">Enrichment</span>
                                                        </div></a>')),
                         htmltools::div(style='display: inline-block;width:290px;height:62px;border-radius:10px;margin-top: 5px;margin-bottom: 5px;padding: 0px;border: 3px solid #DDDDDD',
                                        htmltools::HTML('<a href="javascript:void(0)" onclick="switchTab(\'ML\')">
                                                         <div class="home_block">
                                                         <span class="name">Machine Learning</span>
                                                        </div></a>')),
                         htmltools::div(style='display: inline-block;width:290px;height:62px;border-radius:10px;margin-top: 5px;margin-bottom: 5px;padding: 0px;border: 3px solid #DDDDDD',
                                        htmltools::HTML('<a href="javascript:void(0)" onclick="switchTab(\'Correlation\')">
                                                         <div class="home_block">
                                                         <span class="name">Correlation</span>
                                                        </div></a>')),
                         htmltools::div(style='display: inline-block;width:290px;height:62px;border-radius:10px;margin-top: 5px;margin-bottom: 5px;padding: 0px;border: 3px solid #DDDDDD',
                                        htmltools::HTML('<a href="javascript:void(0)" onclick="switchTab(\'Network\')">
                                                         <div class="home_block">
                                                         <span class="name">Network</span>
                                                         </div></a>')),
                         htmltools::div(style='display: inline-block;width:290px;height:62px;border-radius:10px;margin-top: 5px;margin-bottom: 5px;padding: 0px;border: 3px solid #DDDDDD',
                                        htmltools::HTML('<a href="javascript:void(0)" onclick="switchTab(\'ID_conversion\')">
                                                         <div class="home_block">
                                                         <span class="name">ID Conversion</span>
                                                        </div></a>'))
                         )
                       ),
         shiny::column(width=6,
                       htmltools::div(style='display: inline-block;width:100%;border-radius:10px;height:325px;margin: 5px;padding: 0px;border: 2px solid black',
                                      htmltools::div(htmltools::h4("News & Updates", style='color: white;margin: 0px;text-align: center;padding-top: 13px;'), style="height:50px;background-color: #2c3e50;;padding: 0px;border-top-left-radius:7px;border-top-right-radius:7px;"),
                                      htmltools::div(htmltools::HTML('<ul style="margin: 0px;padding-left: 24px;padding-top: 5px;">
                                                                      <li style="font-size: 20px;">2025.08.20 Powered by LipidSigR.</li>
                                                                      <li style="font-size: 20px;">2025.03.10 Publih LipidSigR in Bioinform Adv.</li>
                                                                      <li style="font-size: 20px;">2024.09.03 Release package LipidSigR on <a href="https://github.com/TMSWCChenglab/LipidSigR/" target="_blank">GitHub</a>.</li>
                                                                      <li style="font-size: 20px;">2024.07.05 Publih LipidSig 2.0 in Nucleic Acids Res.</li>
                                                                      <li style="font-size: 20px;">2023 12.01 Launch LipidSig 2.0</li>
                                                                      <li style="font-size: 20px;">2021.10.08 Improve the user experience of LipidSig.</li>
                                                                      <li style="font-size: 20px;">2021.07.02 Publish LipidSig in Nucleic Acids Res.</li>
                                                                      <li style="font-size: 20px;">2021.03.01 Launch Machine Learning feature.</li>
                                                                      <li style="font-size: 20px;">2021.01.01 Launch LipidSig!</li>
                                                                      </ul>'),
                          style="height:270px;background-color: #f0f2f3;;padding: 0px;border-bottom-left-radius:7px;border-bottom-right-radius:7px;"))
                       ),
         shiny::column(width=12, htmltools::br()),
         shiny::column(width=12, style='padding:0px;',
                       htmltools::div(htmltools::h6(htmltools::strong('Citation:')),
                                      htmltools::HTML('<h6>Chia-Hsin Liu, Pei-Chun Shen, Wen-Jen Lin, Hsiu-Cheng Liu, Meng-Hsin Tsai, Tzu-Ya Huang, I-Chieh Chen, Yo-Liang Lai, Yu-De Wang, Mien-Chie Hung, Wei-Chung Cheng, LipidSig 2.0: integrating lipid characteristic insights into advanced lipidomics data analysis, Nucleic Acids Research, Volume 52, Issue W1, 5 July 2024, Pages W390–W397, doi: <a href="https://doi.org/10.1093/nar/gkae335" target="_blank">10.1093/nar/gkae335</a>. PMID: 38709887.</h6>
                                                       <h6>Chia-Hsin Liu, Pei-Chun Shen, Wen-Jen Lin, Hsiu-Cheng Liu, Meng-Hsin Tsai, Yo-Liang Lai, Yu-De Wang, Mien-Chie Hung, Wei-Chung Cheng, LipidSigR: a R-based solution for integrated lipidomics data analysis and visualization, Bioinformatics Advances, Volume 5, Issue 1, 2025, vbaf047, doi: <a href="https://doi.org/10.1093/bioadv/vbaf047" target="_blank">10.1093/bioadv/vbaf047</a>. PMID: 40110562.</h6>'),
                                      style="text-align: left;background-color: AliceBlue;border-left: 8px solid LightSteelBlue;padding: 8px")
                       ),
         shiny::column(width=12, htmltools::br()),
         shiny::column(width=12, style='padding:0px;',
                htmltools::div(htmltools::h3('Schematic representation of LipidSig'),
                               style="text-align:center;padding: 8px")),
         shiny::column(width=12, style='text-align:center;', shiny::img(src='Description/home.webp', style='border: 2px #ccc solid;padding:20px; border-radius: 10px;', width='850px',height='100%'))
) ## tabPanel
