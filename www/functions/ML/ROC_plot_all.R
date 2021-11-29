

ROC_plot_all <- function(data1,data2){
  require(tidyverse)
  require(stringr)
  require(plotly)
  total_feature <- sort(unique(data1$feature_num)) %>% as.character()
  AUC_label <- c(length(total_feature))
  for(a in 1:length(total_feature)){
    cv_ROC_AUC <- data1[c(1,2,3,4,7)] %>% filter(feature_num==total_feature[a]) %>% 
      unique() %>% .$ROC_AUC
    cv_ROC_AUC <- t.test(cv_ROC_AUC, mu=0.5)
    lower95 <- cv_ROC_AUC$conf.int[1] %>% round(3)
    upper95 <- cv_ROC_AUC$conf.int[2] %>% round(3)
    AUC_pvalue <- cv_ROC_AUC$p.value
    AUC_pvalue <- formatC(AUC_pvalue, digits = 2, format = "e")
    mean_AUC <- cv_ROC_AUC$estimate %>% round(3)
    AUC_label[a] <- str_c('AUC=', as.character(mean_AUC),
                          ' (',as.character(lower95), '-',
                          as.character(upper95),')')
  }
  label <- c(length(total_feature))
  
  for(a in 1:length(total_feature)){
    if(str_length(total_feature[a])==1){
      label[a] <- str_c(str_pad(total_feature[a],width =5),' ', AUC_label[a])
    }else if(str_length(total_feature[a])==2){
      label[a] <- str_c(str_pad(total_feature[a],width =4),' ', AUC_label[a])
    }else{
      label[a] <- str_c(str_pad(total_feature[a],width =3),' ', AUC_label[a])
    }
  }
  #label <- str_c(str_pad(as.character(c(8:13)),width =3),' ', AUC_label)
  
  mean_AUC_plot <- data2[c(3,4,6,7)] %>% 
    mutate(feature_num=as.character(feature_num))
  mean_AUC_plot$feature_num_label <- NA
  mean_AUC_plot$feature_num_label1 <- NA
  for(a in 1:length(total_feature)){ 
    mean_AUC_plot$feature_num_label[which(mean_AUC_plot$feature_num == total_feature[a])] <-label[a]
    mean_AUC_plot$feature_num_label1[which(mean_AUC_plot$feature_num == total_feature[a])] <-AUC_label[a]
    
  }
  #mean_AUC_plot$feature_num <- as.numeric(mean_AUC_plot$feature_num)
  mean_AUC_plot <- plot_ly(mean_AUC_plot,x=~(1-specificity),y=~sensitivity)%>%
    add_lines(name = ~reorder(feature_num_label,as.numeric(feature_num)),hoverinfo = "text",
              text = ~paste("feature_num :", feature_num,"<br>",feature_num_label1)) %>%
    add_lines(x = 0:1, y = 0:1, line = list(color = 'black', width = 2, dash = 'dash'),showlegend = FALSE)%>%
    layout(xaxis = list(title ="1-specificity"),
           legend = list(title=list(text="Feature number"),y=0.5,x=1.1,font = list(size = 9)),
           title =list(size =15,y=0.99,x=0.1,text="ROC curve"))
  
  return(list(data2[-c(8,9)],mean_AUC_plot))
}

