

SHAP_sample <- function(shap_long,n_sample){
  require(tidyverse)
  require(ggpubr)
  require(fastshap)
  require(SHAPforxgboost)
  require(data.table)
  require(ggforce)
  require(plotly)
  feature_n <- length(unique(shap_long$variable))
  if(feature_n>10){topN <- 10}else{topN <- feature_n}
  
  SHAP_sample_plot <- shap_long %>% filter(ID==n_sample) %>% 
    mutate(abs_value=abs(shapley_value)) %>% 
    arrange(desc(abs_value)) %>% .[1:topN,] %>%
    plot_ly(x=~shapley_value,y=~reorder(variable, shapley_value), orientation = "h",type = "bar",
            hoverinfo = "text",marker = list(color = ~-shapley_value,colorscale="Blues"),
            text =~paste("Variable :", variable,"<br>Shapley_value :",round(shapley_value,2)))%>%
    layout(title = str_c('Feature importance for Sample ',as.character(n_sample)),
           xaxis =list(title="Shapley value",nticks=40,showline=TRUE,mirror='all'),
           yaxis =list(title="",tickfont = list(size = 8),zeroline = FALSE,zerolinewidth = 0))
  
  return(SHAP_sample_plot)
  
}



