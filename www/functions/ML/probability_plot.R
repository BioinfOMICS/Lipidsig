

probability_plot <- function(data, feature_n){
  require(tidyverse)
  require(ggpubr)
  require(ggbeeswarm)
  require(caret)
  require(stringr)
  data <- data %>% group_by(ID, feature_num) %>% 
    mutate(pred_prob=mean(pred_prob,na.rm=T)) %>% 
    dplyr::select(-3,-8) %>% unique() %>% 
    mutate(pred_label=ifelse(pred_prob>0.5,1,0))
  
  probability_plot <-  data %>% filter(feature_num==feature_n) %>% 
    mutate(true_label=as.factor(true_label)) %>% 
    plot_ly(x=~true_label,y=~pred_prob, type = 'violin', box = list(visible = F),points=F,showlegend=F,
            color =~true_label,colors=c("#132B43", "#56B1F7")) %>%
    add_markers(x=~jitter(as.numeric(paste(true_label))),y=~pred_prob,
                text = ~paste("Actual group :", true_label,"<br>Predicted probability :", round(pred_prob,2)),hoverinfo = "text")%>%
    layout(xaxis =list(zeroline = F,title="Actual group"),
           yaxis =list(zeroline = F,title="Predicted probabilities"),
           title =list(size =15,y=0.99,x=0.1,text="Average sample probability in all CVs")) 
  
  cm_data <-  data %>% filter(feature_num==feature_n)
  
  cm <- confusionMatrix(as.factor(cm_data$true_label), as.factor(cm_data$pred_label))
  
  cm_d <- as.data.frame(cm$table) %>% 
    group_by(Reference) %>% 
    mutate(pct=round(Freq/sum(Freq),2))
  
  cm_plot <- cm_d %>% 
    mutate(label=str_c(as.character(Freq),' (',as.character(pct),')')) %>% 
    ggplot(aes(x = Reference , y =  Prediction, fill = pct))+
    geom_tile()+
    scale_fill_gradient(low = "white", high = "#08306b")+
    theme_bw()+
    labs(x='Actual group', y='Predicted group',title='Confusion matrix')+
    geom_text(aes(label = label,color = pct > 0.5))+
    scale_color_manual(guide = FALSE, values = c("black", "white"))+
    guides(fill=F)+
    coord_equal()
  cm_plot <- ggplotly(cm_plot)
  
  cm_data$pred_prob <- round(cm_data$pred_prob, 3)
  
  return(list(cm_data, probability_plot, cm_plot, data))
}



