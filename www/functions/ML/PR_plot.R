

PR_plot <- function(data1,data2, feature_n){
  require(tidyverse)
  require(stringr)
  PR_plot_data <- data1 %>% filter(feature_num==feature_n)
  cv_PR_AUC <- data1[c(1,2,3,4,7)] %>% filter(feature_num==feature_n) %>% 
    unique() %>% .$PR_AUC
  cv_PR_AUC <- t.test(cv_PR_AUC, mu=0.5)
  lower95 <- cv_PR_AUC$conf.int[1] %>% round(3)
  upper95 <- cv_PR_AUC$conf.int[2] %>% round(3)
  AUC_pvalue <- cv_PR_AUC$p.value
  AUC_pvalue <- formatC(AUC_pvalue, digits = 2, format = "e")
  mean_AUC <- cv_PR_AUC$estimate %>% round(3)
  
  PR_plot <- rbind(data1[3:6], data2[c(3,4,8,9)]) %>% 
    filter(feature_num==feature_n) %>%
    mutate(cv=ifelse(cv_fold=='mean','1','0')) 
  PR_plot_1 <- PR_plot %>% filter(cv_fold!="mean")
  PR_plot_2 <- PR_plot %>% filter(cv_fold=="mean")
  PR_plot <- plot_ly(PR_plot_1,x=~recall,y =~precision,hoverinfo = NULL)%>%
    add_lines(color =~cv_fold,colors = "gray",hoverinfo = "text",
              text = ~paste("cv_fold :", cv_fold))%>%
    add_lines(data=PR_plot_2,name="mean",x=~recall,y =~precision,line = list(color = "red"),hoverinfo = "text",
              text = ~paste("cv_fold :", cv_fold,"<br>",
                            paste0(as.character(mean_AUC),'(',as.character(lower95),'-',as.character(upper95),')'))) %>%
    layout(legend = list(title=list(text="cv_fold"),y=0.5,x=1.1,font = list(size = 12)),
           title = list(size =15,y=0.99,x=0.1,
                        text=str_c('PR curve for ',as.character(feature_n) ,' feature model'))) %>%
    add_annotations(
      x=0.2,y=0.3,xref = "x",yref = "y",
      text = str_c('AUC=', as.character(mean_AUC),'(',as.character(lower95), '-',
                   as.character(upper95),')\n','pvalue=',as.character(AUC_pvalue)),
      xanchor = 'left',showarrow = F)
  
  return(list(PR_plot_data,PR_plot))
}


