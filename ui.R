
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinyFeedback)
library(shinycssloaders)
library(shinydashboard)
library(DT)
library(crosstalk)
library(plotly)
library(tidyverse)
library(shinyhelper)
library(visNetwork)
library(pathview)
library(V8)
library(hwordcloud)
library(reactable)
library(iheatmapr)
library(rintrojs)
library(forcats)

#### Set width and margin in CSS ####
body.css <- "
#body_div {
width: 1200px; 
margin: 15px auto; 
}
h6 { /* Header 4 */
    font-size: 18px;
    line-height: 1.5;
  color: black;
}
h1 { /* Header 4 */
    /font-family: 'Lobster', cursive;
  font-weight: 250;
  line-height: 1.1;
  color:midnightblue;
}
#PRO_tabPanel_div {
height: 1000px;
}
#ML_tabPanel_div {
height: 3000px;
}
#DE_tabPanel_div {
height: 2000px;
}
#CORR_tabPanel_div {
height: 1000px;
}
.landing-page-box { width:19%; height:32vh; background-color:white;margin-right:1%; 
                    border: 3px solid #DDDDDD; margin-bottom: 2px; float: left; 
                    transition: 0.5s ease; position: relative; object-fit: scale-down;}
                    
.landing-page-box:hover, .landing-page-box-about:hover {-webkit-transform: scale(1.05); 
                          -ms-transform: scale(1.05); transform: scale(1.05); }
.landing-page-icon {width:100%; height:26vh; background-color: white;
                     border: 0 ; position: absolute; object-fit: scale-down; }
.landing-page-box-title {font-size: 20px; text-align:center; color: darkblue;
                          font-weight: bold; background-color: none; width:100%; 
                          max-height: 40px; margin-top: 1vh;}
.landing-page-button { position: absolute; top:0; width: 100%; 
                        height: 100%; opacity: 0;}
"
# timeoutSeconds <- 5
# inactivity <- sprintf("function idleTimer() {
# var t = setTimeout(logout, %s);
# window.onmousemove = resetTimer; // catches mouse movements
# window.onmousedown = resetTimer; // catches mouse movements
# window.onclick = resetTimer;     // catches mouse clicks
# window.onscroll = resetTimer;    // catches scrolling
# window.onkeypress = resetTimer;  //catches keyboard actions
# 
# function logout() {
# Shiny.setInputValue('timeOut', '%ss')
# }
# 
# function resetTimer() {
# clearTimeout(t);
# t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
# }
# }
# idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)

# js_code <- "$(document).on('shiny:value', function(event) {
#   
#   if (event.target.id === 'PRO.num.of.expressed.lipid') {
#     window.scrollBy(0,400);
#   }
#   if (event.target.id === 'DE.pair.detect') {
#     window.scrollBy(0,600);
#   }
#   if (event.target.id === 'ML.ROC.all') {
#     window.scrollBy(0,400);
#   }
# });"



##### Shiny User Interface #####
shinyUI(fluidPage(

  #includeScript("www/scrolldown.js"),
  #tags$script(js_code),
  #### CSS/shinythemes/shinyjs contorl ####
  tags$style(body.css),
  #tags$script(inactivity),
  # tags$head(tags$style(
  #   type="text/css",
  #   "#DE.species.enrichment.kegg.pathview img {max-width: 100%; width: 100%; height: auto}"
  # )),
  theme = shinytheme("flatly"),
  shinyjs::useShinyjs(),

  #### Navbar page ####
  div(id = 'body_div',
      ## Application title
      titlePanel("LipidSig: a web-based tool for lipidomic data analysis"),

      # NavbarPage
      navbarPage(title = h4('LipidSig'),
                 id = 'narbarpage',
                 
                 ##################
                 #####        #####
                 #####  Home  #####
                 #####        #####
                 ##################
                 
                 source("ui_home.R",  local = TRUE)$value,
                 
                 #######################
                 #####             #####
                 #####  Profiling  #####
                 #####             #####
                 #######################
                 
                 source("ui_Profiling.R",  local = TRUE)$value,
                 
                 #####################################
                 #####                           #####
                 #####  Differential Expression  #####
                 #####                           #####
                 #####################################
                 
                 source("ui_DE.R",  local = TRUE)$value, #tabPanel #Differential expression
                 
                 ##############################
                 #####                    #####
                 #####  Machine Learning  #####
                 #####                    #####
                 ##############################
                 
                 source("ui_ML.R",  local = TRUE)$value,
                 
                 #########################
                 #####               #####
                 #####  Correlation  #####
                 #####               #####
                 #########################
                 
                 source("ui_Correlation.R",  local = TRUE)$value, #tabPanel #Correlation
                 
                 #####################
                 #####           #####
                 #####  Network  #####
                 #####           #####
                 #####################
                 
                 source("ui_Network.R",  local = TRUE)$value, #tabPanel #Network
                 
                 ###########################
                 #####                 #####
                 #####  Documentation  #####
                 #####                 #####
                 ###########################
                 
                 source("ui_Documentation.R",  local = TRUE)$value, #tabPanel #Documentation
                 
                 ###########################
                 #####                 #####
                 #####       FAQ       #####
                 #####                 #####
                 ###########################
                 
                 source("ui_FAQ.R",  local = TRUE)$value #tabPanel #Documentation
                 
                 ) #navbarPage #narbarpage
      ) #div #body_div
)) # shinyUI




