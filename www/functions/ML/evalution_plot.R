

evalution_plot <- function(data, method){
  require(tidyverse)
  require(ggpubr)
  require(ggrepel)
  require(stringr)
  evalution_data <- data %>% filter(index==method) %>% 
    group_by(feature_num) %>% 
    mutate(mean_value=round(mean(value),2)) %>% 
    arrange(feature_num, cv_fold)
  evalution_plot <- data %>% filter(index==method) %>% 
    group_by(feature_num) %>% 
    summarise(mean_value=round(mean(value),2),
              sd_value = round(sd(value),2)) %>%
    mutate(highest=ifelse(mean_value==max(.$mean_value), 'red', 'black')) %>% 
    ungroup() %>%
    plot_ly(x = ~feature_num, y =~mean_value, line = list(color='black'),
            marker = ~list(color=highest,size = 10),
            type='scatter',mode = 'lines',showlegend = FALSE,
            error_y = ~list(array = sd_value,color = '#000000'),
            text = ~paste("feature_num :", feature_num,"<br>mean :", mean_value,"<br>sd :", sd_value),hoverinfo = "text")%>%
    add_annotations(text = ~mean_value,xref = "x",yref = "y",showarrow = TRUE,
                    arrowhead = 4,arrowsize = .3,ax = -20,ay = 50) %>%
    layout(xaxis =list(title="Number of features",showgrid=TRUE,nticks=20,showline=TRUE,mirror='all'),
           yaxis =list(title="value (%)",range = c(0,100),nticks=15,mirror='all'),
           title =list(size =15,y=0.99,x=0.7,text= method))
  
  evalution_data$value <- round(evalution_data$value, 2)
  evalution_data$mean_value <- round(evalution_data$mean_value, 2)
  
  return(list(evalution_data,evalution_plot))
}


