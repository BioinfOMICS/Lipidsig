

SHAP <- function(data, best_model, best_model_feature,  ML_method ,feature_n, nsim){
  require(tidyverse)
  require(ggpubr)
  require(fastshap)
  require(SHAPforxgboost)
  require(data.table)
  require(ggforce)
  require(ranger)
  require(plotly)
  
  std1 <- function(x){
    return ((x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }
  
  
  which_model <- which(names(best_model)==as.character(feature_n))
  model <- best_model[[which_model]]
  
  best_model_feature <- colnames(data)%in%best_model_feature[[which_model]]
  best_model_feature[1] <- T
  data <- data[best_model_feature]
  
  if(feature_n>10){topN <- 10}else{topN <- feature_n}
  
  if(ML_method=='xgboost'){
    
    shap_values <- shap.values(xgb_model = model, X_train = as.matrix(data[-1]))
    shap_score <- shap_values$shap_score
    
    shap_long <- shap.prep(xgb_model = model, X_train = as.matrix(data[-1]), top_n=NULL)
    
  }else if(ML_method=='Random_forest'){
    
    pred <- function(object, newdata) {
      predict(object, data = newdata)$predictions
    } #RF
    
    shap_score <- explain(
      model,
      X = data[-1],
      nsim = nsim,
      pred_wrapper = pred
    )
  }else if(ML_method=='SVM'){
    
    pred <- function(object, newdata) {
      attributes(predict(object,newdata =newdata, probability=TRUE))$probabilities[,2]
    } #SVM
    
    shap_score <- explain(
      model,
      X = data[-1],
      nsim = nsim,
      pred_wrapper = pred
    )
  }else if(ML_method %in% c('Lasso', 'Ridge', 'ElasticNet')){
    
    pred <- function(object, newdata) {
      predict(object, newx = as.matrix(newdata), type = 'response')[,1]
    } #Lasso, Ridge, ElasticNet
    
    shap_score <- explain(
      model,
      X = data[-1],
      nsim = nsim,
      pred_wrapper = pred
    )
  }
  
  shap_long <- shap_score %>%   
    gather(key='variable', value='value') %>%
    mutate(ID=rep(1:nrow(shap_score),ncol(shap_score)), rfvalue=unlist(data[-1])) %>% 
    group_by(variable) %>% 
    mutate(stdfvalue=std1(rfvalue)) %>% 
    mutate(mean_value=mean(abs(value))) %>% 
    mutate(variable=as.factor(variable)) %>%
    dplyr::select(ID,variable, value, rfvalue,stdfvalue, mean_value) %>% 
    as.data.table()
  
  mean_shapley_plot <- shap_long[,c(2,6)] %>% as.data.frame %>% unique() %>% 
    arrange(desc(mean_value)) %>% .[1:topN,]%>%
    plot_ly(x=~mean_value,y=~reorder(variable,mean_value),type = "bar",orientation = "h",
            hoverinfo = "text",marker = list(color = ~-mean_value,colorscale="Blues"),
            text =~paste("Feature :", variable,"<br>Mean shapley value :",round(mean_value,2)))%>%
    layout(title = "SHAP feature importance",
           xaxis =list(title="mean(|Shapley value|)",nticks=15,showline=TRUE,mirror='all'),
           yaxis =list(title=" "))
  #plot.summary.wrap2 <- function(shap_score, X, top_n, dilute = FALSE){
  #  if(missing(top_n)) top_n <- dim(X)[2]
  #  if(!top_n%in%c(1:dim(X)[2])) stop('supply correct top_n')
  #  shap_long2 <- shap.prep(shap_contrib = shap_score, X_train = X, top_n = top_n)
  #  plot.summary <- function(data_long,x_bound = NULL,dilute = FALSE,scientific = FALSE,my_format = NULL){
  #    
  #    if (scientific){label_format = "%.1e"} else {label_format = "%.3f"}
  #    if (!is.null(my_format)) label_format <- my_format
  #    # check number of observations
  #    N_features <- setDT(data_long)[,uniqueN(variable)]
  #    if (is.null(dilute)) dilute = FALSE
  #    
  #    nrow_X <- nrow(data_long)/N_features # n per feature
  #    if (dilute!=0){
  #      # if nrow_X <= 10, no dilute happens
  #      dilute <- ceiling(min(nrow_X/10, abs(as.numeric(dilute)))) # not allowed to dilute to fewer than 10 obs/feature
  #      set.seed(1234)
  #      data_long <- data_long[sample(nrow(data_long),
  #                                    min(nrow(data_long)/dilute, nrow(data_long)/2))] # dilute
  #    }
  #    
  #    x_bound <- if (is.null(x_bound)) max(abs(data_long$value))*1.1 else as.numeric(abs(x_bound))
  #    
  #    #new
  #    feature_rank <- data_long[,c('variable', 'mean_value')] %>% unique() %>% 
  #      arrange(mean_value) %>% .$variable
  #    data_long$variable <- factor(data_long$variable, levels = feature_rank)
  #    
  #    #data_long <- data_long %>% group_by(variable) %>% mutate(dist=max(value)-min(value))
  #    #data_long$variable <- fct_reorder(data_long$variable, data_long$dist, max)
  #    plot1 <- ggplot(data = data_long) +
  #      coord_flip(ylim = c(-x_bound, x_bound)) +
  #      geom_hline(yintercept = 0) + # the y-axis beneath
  #      ggforce::geom_sina(aes(x = variable, y = value, color = stdfvalue),
  #                         method = "counts", maxwidth = 0.7, alpha = 0.7) +
  #      geom_text(data = unique(data_long[, c("variable", "mean_value")]),
  #                aes(x = variable, y=-Inf, label = sprintf(label_format, mean_value)),
  #                size = 3, alpha = 0.7,
  #                hjust = -0.2,
  #                fontface = "bold") + # bold
  #      scale_color_gradient(low="#0000FF", high="#FF0000",
  #                           breaks=c(0,1), labels=c(" Low","High "),
  #                           guide = guide_colorbar(barwidth = 12, barheight = 0.3)) +
  #      theme_bw() +
  #      theme(axis.line.y = element_blank(),
  #            axis.ticks.y = element_blank(), # remove axis line
  #            legend.position="bottom",
  #            legend.title=element_text(size=10),
  #            legend.text=element_text(size=8),
  #            axis.title.x= element_text(size = 10)) +
  #      labs(y = "Shapley value (impact on model output)", x = "", color = "Feature value  ")
  #    plot1 <- ggplotly(plot1) %>%
  #      style(traces =0:10,hoverinfo="text",
  #            text=paste("Sample ID :",data_long$ID,"\nFeature :",data_long$variable,
  #                       "\nShapley value :",round(data_long$value,3),"\nMean shapley value :",round(data_long$mean_value,3))) %>% 
  #      layout(title = 'Summary plot')
  #    return(plot1)
  #  }
  #  plot.summary(shap_long2)
  #}
  require(xgboost)
  all_shapley_plot <- shap.plot.summary.wrap2(shap_score = shap_score,X = as.matrix(data[-1]), top_n = topN)
  shap_long <- as.data.frame(shap_long)
  colnames(shap_long)[c(3,4,5,6)] <- c('shapley_value', 'raw_value', 'normalized_value', 'mean_shapley_value')
  
  #shap_long$shapley_value <- round(shap_long$shapley_value, 3)
  #shap_long$raw_value <- round(shap_long$raw_value,)
  
  return(list(shap_score, shap_long, mean_shapley_plot,all_shapley_plot))
  
}

