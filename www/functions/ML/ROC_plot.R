

ROC_plot <- function(data1,data2, feature_n){
  require(tidyverse)
  require(stringr)
  require(plotly)
  ROC_plot_data <- data1 %>% filter(feature_num==feature_n)
  cv_ROC_AUC <- data1[c(1,2,3,4,7)] %>% filter(feature_num==feature_n) %>% 
    unique() %>% .$ROC_AUC
  cv_ROC_AUC <- t.test(cv_ROC_AUC, mu=0.5)
  lower95 <- cv_ROC_AUC$conf.int[1] %>% round(3)
  upper95 <- cv_ROC_AUC$conf.int[2] %>% round(3)
  AUC_pvalue <- cv_ROC_AUC$p.value
  AUC_pvalue <- formatC(AUC_pvalue, digits = 2, format = "e")
  mean_AUC <- cv_ROC_AUC$estimate %>% round(3)
  
  ROC_plot<- rbind(data1[3:6], data2[c(3,4,6,7)]) %>% 
    filter(feature_num==feature_n) %>%
    mutate(cv=ifelse(cv_fold=='mean','1','0'))
  ROC_plot_1 <- ROC_plot %>% filter(cv_fold!="mean")
  ROC_plot_2 <- ROC_plot %>% filter(cv_fold=="mean")
  ROC_plot<-plot_ly(ROC_plot_1,x=~(1-specificity),y =~sensitivity,hoverinfo=NULL)%>%
    add_trace(data=ROC_plot_1,color=~cv_fold,colors ="gray",hoverinfo = "text",
              mode="lines",type='scatter',
              text = ~paste("cv_fold :", cv_fold))%>%
    add_trace(data=ROC_plot_2,name="mean",x=~(1-specificity),y =~sensitivity,line = list(color = "red"),
              hoverinfo = "text",mode="lines",type='scatter',
              text = ~paste("cv_fold :", cv_fold,"<br>AUC = ",
                            paste0(as.character(mean_AUC),'(',as.character(lower95),'-',as.character(upper95),')'))) %>%
    add_lines(x = 0:1, y = 0:1, line = list(color = 'black', width = 2, dash = 'dash'),showlegend = FALSE) %>%
    layout(xaxis = list(title ="1-specificity"),
           legend = list(title=list(text="cv_fold"),y=0.5,x=1.1,font = list(size = 9)),
           title =list(size =15,y=0.99,x=0.1,
                       text=str_c('ROC curve for ',as.character(feature_n),' feature model'))) %>%
    add_annotations(
      x=0.6,y=0.15,xref = "x",yref = "y",
      text = str_c('AUC=', as.character(mean_AUC),'(',as.character(lower95), '-',
                   as.character(upper95),')\n','pvalue=',as.character(AUC_pvalue)),
      xanchor = 'left',showarrow = F)
  
  return(list(ROC_plot_data,ROC_plot))
}


