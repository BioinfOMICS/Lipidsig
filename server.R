
library(shiny)
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

source('www/functions/data_check.R')
source('www/functions/data_summary.R')
source('www/functions/data_process.R')
source('www/functions/submit_check.R')
source('www/functions/PCA.R')
source('www/functions/PCA_contrib.R')
source('www/functions/PCA_variable.R')
source('www/functions/PLSDA.R')
source('www/functions/tsne.R')
source('www/functions/UMAP.R')
source('www/functions/Species2Char.R')
source('www/functions/Profiling/exp_profilling.R')
source('www/functions/Profiling/corr_heatmap.R')
source('www/functions/Profiling/exp_compo_by_lipidinfo.R')
source('www/functions/DE/DE_species_2.R')
source('www/functions/DE/Hclustering.R')
source('www/functions/DE/Sig_lipid_feature.R')
source('www/functions/DE/DE_char_2.R')
source('www/functions/DE/Enrichment.R')
source('www/functions/DE/DE_pathview_function.R')
source('www/functions/DE/DE_sub_char_2.R')
source('www/functions/DE/DE_sub_char_plot_2.R')
source('www/functions/DE/lipid_char_table_gather.R')
source('www/functions/ML/evalution_plot.R')
source('www/functions/ML/feature_plot.R')
#source('www/functions/ML/ML_data_process.R')
source('www/functions/ML/ML_data_process_1.R')
source('www/functions/ML/ML_data_process_2.R')
source('www/functions/ML/ML_final.R')
source('www/functions/ML/probability_plot.R')
source('www/functions/ML/PR_plot_all.R')
source('www/functions/ML/PR_plot.R')
source('www/functions/ML/ROC_plot_all.R')
source('www/functions/ML/ROC_plot.R')
source('www/functions/ML/SHAP_dependence_plot.R')
source('www/functions/ML/SHAP_forceplot.R')
source('www/functions/ML/SHAP.R')
source('www/functions/ML/cor_network.R')
source('www/functions/ML/corr_network.R')
source('www/functions/ML/model_for_net.R')
#source('www/functions/ML/SHAP_sample.R')
source('www/functions/Correlation/Clin_Cor_heatmap.R')
#source('www/functions/Correlation/Clin_DE_2_heatmap.R')
#source('www/functions/Correlation/Clin_LogR_heatmap.R')
source('www/functions/Correlation/Clin_LR_heatmap.R')
#source('www/functions/Correlation/Clin_ROC_heatmap.R')
source('www/functions/Network/annotation_human.r')
source('www/functions/Network/lipid_related_gene_network.r')
source('www/functions/Network/get_reactome_path.r')
source('www/functions/Network/reactome_network.r')


shinyServer(function(input, output, session){
    
    #### shinyhelper control #### 
    shinyhelper::observe_helpers()
    
    #### variables ####
    variables <- reactiveValues(
        #### Profiling ####
        PRO.exp.data.demo = NULL,
        PRO.lipid.char.tab.demo = NULL,
        
        PRO.exp.data.user = NULL,
        PRO.lipid.char.tab.user = NULL,
        PRO.sample.count.user = NULL,
        PRO.exp.user.col1 = NULL,
        PRO.lipid.char.user.col1 = NULL,
        
        PRO.pca.result = NULL,
        PRO.plsda.result = NULL,
        PRO.tsne.result = NULL,
        PRO.umap.result = NULL,
        
        PRO.corr.heatmap.result = NULL,
        
        #### DE ####
        DE.exp.data.demo = NULL,
        DE.group.info.demo = NULL, 
        DE.lipid.char.tab.demo = NULL, 
    
        DE.exp.data.user = NULL,
        DE.group.info.user = NULL, 
        DE.lipid.char.tab.user = NULL, 
        DE.sample.count.user = NULL,
        DE.group.count.user = NULL,
        DE.pair.detect.user = NULL,
        
        DE.exp.user.col1 = NULL,
        DE.group.user.col1 = NULL, 
        DE.group.user.col2 = NULL, 
        DE.group.user.col3 = NULL, 
        DE.group.user.col4 = NULL,
        DE.lipid.char.user.col1 = NULL,
        
        DE.species.2 = NULL, 
        DE.species.pca.result = NULL,
        DE.species.plsda.result = NULL,
        DE.species.tsne.result = NULL,
        DE.species.hclustering = NULL,
        DE.sig.lipid.feature = NULL, 
        DE.enrichment = NULL,
        DE.lipid.gene.path = NULL,
        DE.pathway.gene.list = NULL,
        
        DE.char.2 = NULL, 
        DE.class.pca.result = NULL,
        DE.class.plsda.result = NULL,
        DE.class.tsne.result = NULL,
        DE.class.hclustering = NULL,
        DE.class.hclustering = NULL, 
        DE.sub.char.2 = NULL, 
        DE.sub.char.plot.2 = NULL,
        DE.class.split.char = NULL, 
        
        #### ML ####
        ML.exp.data.demo = NULL,
        ML.cond.tab.demo = NULL,
        ML.lipid.char.tab.demo = NULL,
        
        ML.exp.data.user = NULL,
        ML.cond.tab.user = NULL, 
        ML.lipid.char.tab.user = NULL,
        
        ML.exp.user.col1 = NULL,
        ML.cond.user.col1 = NULL,
        ML.lipid.char.user.col1 = NULL,
        
        ML.add.var = NULL,
        ML.data.process = NULL,
        ML.results = NULL,
        ML.SHAP.result = NULL,
        ML.visnetwork = NULL,
        
        #### Correlation ####
        #CORR.exp.data.demo.cate = NULL,
        CORR.exp.data.demo.cont = NULL, 
        CORR.exp.data.user = NULL,
        #CORR.cond.tab.demo.cate = NULL,
        CORR.cond.tab.demo.cont = NULL, 
        CORR.cond.tab.user = NULL,
        #CORR.adj.tab.demo.cate = NULL,
        CORR.adj.tab.demo.cont = NULL,
        CORR.adj.tab.user = NULL,
        #CORR.lipid.char.demo.cate = NULL,
        CORR.lipid.char.demo.cont = NULL,
        CORR.lipid.char.user = NULL,
        
        CORR.exp.user.col1 = NULL,
        CORR.cond.user.col1 = NULL,
        CORR.adj.user.col1 = NULL,
        CORR.lipid.char.user.col1 = NULL,
        
        #CORR.species.DE.result = NULL,
        #CORR.species.ROC.result = NULL,
        #CORR.species.logistic.result = NULL,
        CORR.species.corr.result = NULL,
        CORR.species.linear.result = NULL,
        #CORR.class.DE.result = NULL,
        #CORR.class.ROC.result = NULL,
        #CORR.class.logistic.result = NULL,
        CORR.class.corr.result = NULL,
        CORR.class.linear.result = NULL,
        
        #### Network ####
        NET.lipid.gene.path = NULL, 
        NET.all.interaction = NULL, 
        NET.all.node = NULL, 
        NET.complex = NULL, 
        NET.edge = NULL, 
        NET.node = NULL, 
        NET.node.id.for.complex = NULL,
        NET.uniprot = NULL,
        NET.enrichment.result = NULL, 
        NET.enrichment.visnetwork = NULL,
        NET.reactome.result = NULL, 
        NET.reactome.visnetwork = NULL
        
    ) #variables <- reactiveValues
    
    ###########################
    ###########################
    ######               ######
    ######   Home Page   ######
    ######               ######
    ###########################
    ###########################
    
    source(file = "server_home.R",
           local = TRUE,
           encoding = "UTF-8")
    
    ################################
    ################################
    ######                    ######
    ######   Profiling Page   ######
    ######                    ######
    ################################
    ################################
    
    source(file = "server_Profiling.R",
           local = TRUE,
           encoding = "UTF-8")
    
    #############################################
    #############################################
    ######                                 ######
    ######   Differential Expressed Page   ######
    ######                                 ######
    #############################################
    #############################################
    
    source(file = "server_DE.R",
           local = TRUE,
           encoding = "UTF-8")
    
    #######################################
    #######################################
    ######                           ######
    ######   Machine Learning Page   ######
    ######                           ######
    #######################################
    #######################################
    
    source(file = "server_ML.R",
           local = TRUE,
           encoding = "UTF-8")
    
    ##################################
    ##################################
    ######                      ######
    ######   Correlation Page   ######
    ######                      ######
    ##################################
    ##################################
    
    source(file = "server_Correlation.R",
           local = TRUE,
           encoding = "UTF-8")
    
    ##############################
    ##############################
    ######                  ######
    ######   Network Page   ######
    ######                  ######
    ##############################
    ##############################
    
    source(file = "server_Network.R",
           local = TRUE,
           encoding = "UTF-8")
    
    ####################################
    ####################################
    ######                        ######
    ######   Documentation Page   ######
    ######                        ######
    ####################################
    ####################################
    
    source(file = "server_Documentation.R",
           local = TRUE,
           encoding = "UTF-8")
    
    session$allowReconnect(TRUE)
    
}) #shinyServer







