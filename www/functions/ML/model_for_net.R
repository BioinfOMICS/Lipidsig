

model_for_net <- function(data, ML_method, varimp_method, best_model, 
                          best_model_feature,feature_num, nsim){
  if(varimp_method=='Algorithm-based'){
    num <- which(as.numeric(names(best_model))==feature_num)
    if(ML_method=='Random_forest'){
      varimp <- best_model[[num]]$variable.importance
      varImp_result <- data.frame(ML_method='Random_forest', feature=names(varimp),importance=varimp)
    }else if(ML_method=='SVM'){
      varimp <- coef(best_model[[num]])[-1]
      varImp_result <- data.frame(ML_method='SVM', feature=names(varimp),importance=varimp)
    }else if(ML_method=='Lasso'){
      varimp <- coef(best_model[[num]],s = 'lambda.min') %>% as.matrix() %>% as.data.frame()%>% .[-1, ,drop=F]
      varImp_result <- data.frame(ML_method='Lasso', feature=rownames(varimp),importance=varimp[[1]])
    }else if(ML_method=='Ridge'){
      varimp <- coef(best_model[[num]],s = 'lambda.min') %>% as.matrix() %>% as.data.frame()%>% .[-1, ,drop=F]
      varImp_result <- data.frame(ML_method='Ridge', feature=rownames(varimp),importance=varimp[[1]])
    }else if(ML_method=='ElasticNet'){
      varimp <- coef(best_model[[num]],s = 'lambda.min') %>% as.matrix() %>% as.data.frame()%>% .[-1, ,drop=F]
      varImp_result <- data.frame(ML_method='ElasticNet', feature=rownames(varimp),importance=varimp[[1]])
    }else if(ML_method=='xgboost'){
      varimp <- xgb.importance(best_model[[num]]$feature_names,best_model[[num]])
      varImp_result <- data.frame(ML_method='xgboost', feature=varimp$Feature,importance=varimp$Gain)
    }
  }else{
    SHAP <- function(data, best_model, best_model_feature,  
                     ML_method ,feature_n, nsim){
      require(tidyverse)
      require(ggpubr)
      require(fastshap)
      require(SHAPforxgboost)
      require(data.table)
      require(ggforce)
      require(ranger)
      
      std1 <- function(x){
        return ((x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
      }
      
      
      which_model <- which(names(best_model)==as.character(feature_n))
      model <- best_model[[which_model]]
      
      best_model_feature <- colnames(data)%in%best_model_feature[[which_model]]
      best_model_feature[1] <- T
      data <- data[best_model_feature]
      
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
      
      shap_long <- as.data.frame(shap_long)
      colnames(shap_long)[c(3,4,5,6)] <- c('shapley_value', 'raw_value', 'normalized_value', 'mean_shapley_value')
      return(list(shap_score, shap_long))
      
    }
    varImp_result <- SHAP(data=data, best_model=best_model, 
                          best_model_feature=best_model_feature,
                          ML_method=ML_method ,feature_n=feature_num, 
                          nsim=nsim)[[2]]
    
    varImp_dir <- varImp_result %>% mutate(dir=shapley_value*raw_value) %>% 
      group_by(variable) %>% 
      summarise(dir=sum(dir))
    varImp_result <- varImp_result[c(2,6)] %>% unique() %>% 
      left_join(varImp_dir,by='variable') %>% 
      mutate(mean_shapley_value=ifelse(dir>0, mean_shapley_value, -mean_shapley_value))
    
    varImp_result <- data.frame(ML_method=ML_method, feature=varImp_result$variable,
                                importance=varImp_result$mean_shapley_value) %>% unique()
    
  }
  
  return(varImp_result)
}

