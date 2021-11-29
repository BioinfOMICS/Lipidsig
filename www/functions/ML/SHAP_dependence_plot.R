

SHAP_dependence_plot <- function(shap_long,x,y,color_var){
  require(tidyverse)
  require(ggpubr)
  require(fastshap)
  require(SHAPforxgboost)
  require(data.table)
  require(ggforce)
  
  #shap_dependence_plot <- shap.plot.dependence(data_long = shap_long, 
  #                                             x = feature_name, 
  #                                             y = feature_name, 
  #                                             color_feature = feature_name) + 
  #  ggtitle(str_c('Dependence plot for ', feature_name))
  shap_dependence_plot <- data.frame(a=(shap_long %>% filter(variable==x) %>% .$raw_value),
                                     b=(shap_long %>% filter(variable==y) %>% .$shapley_value),
                                     c=(shap_long %>% filter(variable==color_var) %>% .$raw_value)) %>%
    plot_ly(x=~a,hoverinfo =NULL) %>%
    add_trace(y=~b,color = ~c,hoverinfo = "text",mode="markers",type='scatter',
              colors =~colorRampPalette(c("#E6E6FF" , "#0000FF"))(length(c)),
              text =~paste(x,":",a,"<br>Shapley_value :",round(b,3),
                           "<br>Feature value :",round(c,3))) %>%
    add_lines(y=~fitted(loess(b~a)),hoverinfo ="none",showlegend = FALSE) %>%
    colorbar(title = str_c(color_var,"\n(Feature value)")) %>%
    layout(title = 'Shap dependence plot',
           xaxis =list(title=x,nticks=20,showline=TRUE,mirror='all'),
           yaxis =list(title=str_c("Shapley value for ", y),zeroline = FALSE,zerolinewidth = 0))
  
  return(shap_dependence_plot)
  
}


