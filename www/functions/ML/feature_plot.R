feature_plot <- function(data1, data2, feature_n, nfold=10){
  require(tidyverse)
  require(stringr)
  
  feature_loc <- which(names(data1[[1]])==as.character(feature_n))
  cv_sele_feature <- data1 %>% purrr::map(function(x){x[[feature_loc]]}) %>% 
    unlist %>% table()
  
  feature_freq_data <- data.frame(feature=names(cv_sele_feature),
                                  sele_freq=as.numeric(cv_sele_feature)/nfold) %>% 
    mutate(feature_num=feature_n) %>% 
    mutate(ranking_method=data2[1,1], 
           ML_method=data2[1,2]) %>% 
    #arrange(desc(sele_freq)) %>% 
    dplyr::select(ranking_method,ML_method,
                  feature_num, everything())
  
  
  if(nrow(feature_freq_data)>=10){
    plot_feature_num <- 10
  }else{
    plot_feature_num <- nrow(feature_freq_data)
  }
  #new
  feature_freq_data <- feature_freq_data %>% arrange(desc(sele_freq))
  #
  feature_freq_plot <- feature_freq_data[1:plot_feature_num,] %>%
    plot_ly(x=~(sele_freq*100),y=~reorder(feature, sele_freq),type = "bar", orientation = "h",
            marker = list(color = ~-sele_freq,colorscale="Blues"),
            text = ~paste("Feature :", feature,"<br>Selected frequency :", sele_freq*100,"%"),hoverinfo = "text") %>%
    layout(xaxis =list(title="Selected frequency (%)",showgrid=TRUE,nticks=20,showline=TRUE,mirror='all'),
           yaxis =list(title=""))
  
  feature_imp_data <- data2 %>% filter(feature_num==feature_n) %>% 
    group_by(feature) %>% 
    mutate(importance=mean(importance, na.rm=T)) %>% 
    dplyr::select(-cv_fold) %>% unique() %>% 
    mutate(ranking_method=data2[1,1], 
           ML_method=data2[1,2]) %>% 
    dplyr::select(ranking_method,ML_method,
                  feature_num, everything())
  if(nrow(feature_imp_data)>=10){
    plot_feature_num <- 10
  }else{
    plot_feature_num <- nrow(feature_imp_data)
  }
  #new
  feature_imp_data <- feature_imp_data %>% arrange(desc(importance))
  #
  feature_imp_plot <- feature_imp_data[1:plot_feature_num,] %>% 
    plot_ly(x=~importance,y=~reorder(feature, importance),type = "bar", orientation = "h",
            marker = list(color = ~-importance,colorscale="Blues"),
            text = ~paste("Feature :", feature,"<br>Average importance :",round(importance,2)),hoverinfo = "text") %>%
    layout(xaxis =list(title='Average importance',showgrid=TRUE,nticks=20,showline=TRUE,mirror='all'),
           yaxis =list(title=""))
  
  feature_imp_data$importance <- round(feature_imp_data$importance, 3)
  
  return(list(feature_freq_data, feature_freq_plot,
              feature_imp_data,feature_imp_plot))
}