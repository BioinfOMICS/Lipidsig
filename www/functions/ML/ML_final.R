

ML_final <- function(ML_data,ranking_method, ML_method, 
                     split_prop, nfold, alpha){
  require(tidyverse)
  require(parsnip)
  require(tidymodels)
  require(ranger)
  require(caret)
  require(e1071)
  require(pROC)
  require(glmnet)
  require(xgboost)
  require(data.table)
  
  n_ROC <- 300
  feature_selection <- function(data, ranking_method, topN){
    
    if(ranking_method=='p_value'){
      result <- data %>% mutate(group=ifelse(group==0,'group0', 'group1')) %>% 
        gather(key = feature, value = value, -group) %>% 
        group_by(group, feature) %>% 
        summarise(value = list(value)) %>% 
        spread(group, value) %>% 
        group_by(feature) %>% 
        mutate(p_value = tryCatch(t.test(unlist(group0), unlist(group1), var.equal = T)$p.value,
                                  error=function(e){NA})) %>% 
        dplyr::select(feature, p_value)
      
      result <- result %>% arrange(p_value)  %>% .[1:topN,1] %>% unlist()
      return(result) 
      
    }else if(ranking_method=='pvalue_FC'){
      
      result <- data %>% mutate(group=ifelse(group==0,'group0', 'group1')) %>% 
        gather(key = feature, value = value, -group) %>% 
        group_by(group, feature) %>% 
        summarise(value = list(value)) %>% 
        spread(group, value) %>% 
        group_by(feature) %>% 
        mutate(pvalue_FC = -log10(tryCatch(t.test(unlist(group0), unlist(group1), var.equal = T)$p.value,
                                           error=function(e){NA}))*abs(mean(unlist(group0),na.rm=T)/mean(unlist(group1), na.rm=T))) %>% 
        dplyr::select(feature, pvalue_FC)
      
      
      result <- result %>% arrange(desc(pvalue_FC)) %>% .[1:topN,1] %>% unlist()
      
      return(result) 
      
    }else if(ranking_method=='ROC'){
      
      result <- data[-1] %>% 
        gather(key = feature, value = value) %>% 
        group_by(feature) %>% 
        summarise(value = list(value)) %>%
        mutate(group=list(data[[1]])) %>% 
        group_by(feature) %>% 
        mutate(auc = tryCatch(as.numeric(roc(unlist(group), unlist(value),quiet=T)$auc),
                              error=function(e){NA})) %>% 
        dplyr::select(feature, auc) %>% 
        arrange(desc(auc)) %>% .[1:topN,1,drop=T]
      
      return(result)
      
    }else if(ranking_method=='Random_forest'){
      model <- ranger(group ~ ., data = data, num.trees = 500, importance='impurity', probability = T)
      result <- model$variable.importance %>% 
        sort(decreasing = T) %>% names()
      return(result[1:topN])
    }else if(ranking_method=='SVM'){
      model <- svm(group~., kernel='linear', data=data, type='C-classification', probability=TRUE)
      result <- coef(model)[-1] %>% abs() %>% sort(decreasing = T) %>% names()
      return(result[1:topN])
    }else if(ranking_method=='Lasso'){
      model <- cv.glmnet(x =as.matrix(data[-1]), 
                         y = data[[1]], 
                         alpha = 1,  # lasso
                         family = "binomial",nfolds = 5,maxit=1000)
      
      coef <- coef(model,s = 'lambda.min') %>% as.matrix() %>% as.data.frame()%>% .[-1, ,drop=F]
      
      result <- coef[[1]]
      names(result) <- rownames(coef)
      result <- result %>% abs() %>% sort(decreasing = T) %>% names()
      
      return(result[1:topN])
    }else if(ranking_method=='Ridge'){
      model <- cv.glmnet(x =as.matrix(data[-1]), 
                         y = data[[1]], 
                         alpha = 0, nlambda=50,  # lasso
                         family = "binomial",nfolds = 5,maxit=100000)
      
      coef <- coef(model,s = 'lambda.min') %>% as.matrix() %>% as.data.frame()%>% .[-1, ,drop=F]
      result <- coef[[1]]
      names(result) <- rownames(coef)
      result <- result %>% abs() %>% sort(decreasing = T) %>% names()
      return(result[1:topN])    
    }else if(ranking_method=='ElasticNet'){
      model <- cv.glmnet(x =as.matrix(data[-1]), 
                         y = data[[1]], 
                         alpha = alpha,  # lasso
                         family = "binomial",nfolds = 5,maxit=1000)
      
      coef <- coef(model,s = 'lambda.min') %>% as.matrix() %>% as.data.frame()%>% .[-1, ,drop=F]
      result <- coef[[1]]
      names(result) <- rownames(coef)
      result <- result %>% abs() %>% sort(decreasing = T) %>% names()
      return(result[1:topN])
    }
  }
  
  ML_main <- function(train_data, test_data, ML_method){
    
    if(ML_method=='Random_forest'){
      model <- ranger(group ~ ., data = train_data, num.trees = 500, importance='impurity', probability = F)
      
      pred_prob <- predict(model, test_data[-1])$predictions
      
      model_result <- data.frame(ML_method=ML_method, ID=rownames(test_data), true_label=test_data[[1]],
                                 pred_prob=pred_prob) %>% 
        mutate(pred_label=ifelse(pred_prob>0.5,1,0))
      
      varimp <- model$variable.importance
      varImp_result <- data.frame(ML_method=ML_method, feature=names(varimp),importance=varimp)
    }else if(ML_method=='SVM'){
      
      model <- svm(group~., kernel='linear', data=train_data, type='C-classification', probability=T)
      
      pred_prob <- predict(model, test_data[-1], probability = T) %>% attributes() %>% 
        .$probabilities %>% .[,2]
      
      model_result <- data.frame(ML_method=ML_method, ID=rownames(test_data), true_label=test_data[[1]],
                                 pred_prob=pred_prob) %>% 
        mutate(pred_label=ifelse(pred_prob>0.5,1,0))
      
      varimp <- coef(model)[-1] %>% abs() %>% 
        sort(decreasing = T)
      varImp_result <- data.frame(ML_method=ML_method, feature=names(varimp),importance=varimp)
    }else if(ML_method=='Lasso'){
      
      model <- cv.glmnet(x =as.matrix(train_data[-1]), 
                         y = train_data[[1]], 
                         alpha = 1,  # lasso
                         family = "binomial",nfolds = 5,maxit=1000)
      
      
      pred_prob <- predict(model, s='lambda.min', newx=as.matrix(test_data[-1]), type="response")[,1]
      
      model_result <- data.frame(ML_method=ML_method, ID=rownames(test_data), true_label=test_data[[1]],
                                 pred_prob=pred_prob) %>% 
        mutate(pred_label=ifelse(pred_prob>0.5,1,0))
      
      varimp <- coef(model,s = 'lambda.min') %>% as.matrix() %>% as.data.frame()%>% .[-1, ,drop=F]
      varImp_result <- data.frame(ML_method=ML_method, feature=rownames(varimp),importance=abs(varimp[[1]]))
    }else if(ML_method=='Ridge'){
      model <- cv.glmnet(x =as.matrix(train_data[-1]), 
                         y = train_data[[1]], 
                         alpha = 0,  # lasso
                         family = "binomial",nfolds = 5,maxit=100000,
                         nlambda=50)
      
      
      pred_prob <- predict(model, s='lambda.min', newx=as.matrix(test_data[-1]), type="response")[,1]
      
      model_result <- data.frame(ML_method=ML_method, ID=rownames(test_data), true_label=test_data[[1]],
                                 pred_prob=pred_prob) %>% 
        mutate(pred_label=ifelse(pred_prob>0.5,1,0))
      
      varimp <- coef(model,s = 'lambda.min') %>% as.matrix() %>% as.data.frame()%>% .[-1, ,drop=F]
      varImp_result <- data.frame(ML_method=ML_method, feature=rownames(varimp),importance=abs(varimp[[1]]))
    }else if(ML_method=='ElasticNet'){
      model <- cv.glmnet(x =as.matrix(train_data[-1]), 
                         y = train_data[[1]], 
                         alpha = alpha,  # lasso
                         family = "binomial",nfolds = 5,maxit=1000)
      
      
      pred_prob <- predict(model, s='lambda.min', newx=as.matrix(test_data[-1]), type="response")[,1]
      
      model_result <- data.frame(ML_method=ML_method, ID=rownames(test_data), true_label=test_data[[1]],
                                 pred_prob=pred_prob) %>% 
        mutate(pred_label=ifelse(pred_prob>0.5,1,0))
      
      varimp <- coef(model,s = 'lambda.min') %>% as.matrix() %>% as.data.frame()%>% .[-1, ,drop=F]
      varImp_result <- data.frame(ML_method=ML_method, feature=rownames(varimp),importance=abs(varimp[[1]]))
    }else if(ML_method=='xgboost'){
      
      cv_model <- xgb.cv(data = as.matrix(train_data[-1]), 
                         nrounds = 100, 
                         max_depth = 3,
                         label = train_data[[1]],
                         nfold = 5,
                         verbose = FALSE,
                         prediction = TRUE,
                         objective = "binary:logistic",
                         early_stopping_rounds = 10) 
      
      cv_nround_min <- cv_model$best_iteration
      
      model <- xgboost(data = as.matrix(train_data[-1]), 
                       label = train_data[[1]], nrounds = cv_nround_min,
                       max_depth = 3,
                       objective = "binary:logistic",
                       verbose=0)
      
      pred_prob <- predict(model, as.matrix(test_data[-1]))
      
      model_result <- data.frame(ML_method=ML_method, ID=rownames(test_data), true_label=test_data[[1]],
                                 pred_prob=pred_prob) %>% 
        mutate(pred_label=ifelse(pred_prob>0.5,1,0))
      
      varimp <- xgb.importance(colnames(test_data[-1]),model)
      varImp_result <- data.frame(ML_method=ML_method, feature=varimp$Feature,importance=varimp$Gain)
    }
    
    cm <- confusionMatrix(as.factor(model_result$true_label), factor(model_result$pred_label, levels = c(0,1)),positive = '1')
    cm_result <-rbind(data.frame(index=names(cm$overall)[1], value=cm$overall[1]),
                      data.frame(index=names(cm$byClass), 
                                 value=cm$byClass)) %>% 
      mutate(value=value*100) %>% 
      mutate(ML_method=ML_method) %>% 
      dplyr::select(ML_method, everything())
    
    two_class_roc <- data.frame(truth=model_result$true_label, 
                                Class1=model_result$pred_prob) %>% 
      mutate(truth=ifelse(truth==1, 'Class1', 'Class2')) %>% 
      mutate(truth=as.factor(truth))
    
    ROC_auc <- roc_auc(two_class_roc, truth, Class1)[[1,3]]
    ROC_curve <- roc_curve(two_class_roc, truth, Class1)
    PR_auc <- pr_auc(two_class_roc, truth, Class1)[[1,3]]
    PR_curve <- pr_curve(two_class_roc, truth, Class1)
    
    ROC_result <- data.frame(ML_method=ML_method,
                             sensitivity=ROC_curve$sensitivity,
                             specificity=ROC_curve$specificity,
                             ROC_AUC=ROC_auc)
    
    PR_result <- data.frame(ML_method=ML_method,
                            precision=PR_curve$precision,
                            recall=PR_curve$recall,
                            PR_AUC=PR_auc)
    
    
    ROC_plot <- roc(model_result$true_label, model_result$pred_prob, quiet = T)
    thershold <- c(seq(0,1,length.out = n_ROC))
    
    meanROC <- coords(ROC_plot,thershold ,ret = c("threshold", "specificity", "sensitivity",
                                                  "precision", "recall"), transpose = F)
    
    meanROC_result <- meanROC %>% 
      mutate(ML_method=ML_method) %>% 
      dplyr::select(ML_method, everything())
    
    return(list(model_result,cm_result, ROC_result, PR_result,meanROC_result,varImp_result,model))
  }
  
  
  ML <- function(data, ranking_method, ML_method, split_prop){
    feature_num <- (ncol(data)-1)
    sele_feature_num <- c(2,3,5,10,20,50,100)
    if(feature_num<100){
      sele_feature_num <- c(sele_feature_num[sele_feature_num<feature_num],
                            feature_num)
    }
    
    cv <- mc_cv(data, prop = split_prop, times = nfold, strata = NULL)
    
    print('Data split done')
    num <- nrow(analysis(cv$splits[[1]]))
    num1 <- num+1
    num2 <- num+2
    
    cv_model_result <- data.frame(ML_method=rep(NA, 110000),
                                  ID=rep(NA, 110000),
                                  true_label=rep(NA, 110000),
                                  pred_prob=rep(NA, 110000),
                                  pred_label=rep(NA, 110000),
                                  cv_fold=rep(NA, 110000),
                                  feature_num=rep(NA, 110000))
    
    cv_cm_result <-  data.frame(ML_method=rep(NA, 3000),
                                index=rep(NA, 3000),
                                value=rep(NA, 3000),
                                cv_fold=rep(NA, 3000),
                                feature_num=rep(NA, 3000))
    
    cv_ROC_result<- data.frame(ML_method=rep(NA, 110000),
                               sensitivity=rep(NA, 110000),
                               specificity=rep(NA, 110000),
                               ROC_AUC=rep(NA, 110000),
                               cv_fold=rep(NA, 110000),
                               feature_num=rep(NA, 110000))
    
    cv_PR_result<- data.frame(ML_method=rep(NA, 110000),
                              precision=rep(NA, 110000),
                              recall=rep(NA, 110000),
                              PR_AUC=rep(NA, 110000),
                              cv_fold=rep(NA, 110000),
                              feature_num=rep(NA, 110000))
    cv_meanROC_result<- data.frame(ML_method=rep(NA, 110000),
                                   threshold=rep(NA, 110000),
                                   sensitivity=rep(NA, 110000),
                                   specificity=rep(NA, 110000),
                                   precision=rep(NA, 110000),
                                   recall=rep(NA, 110000),
                                   cv_fold=rep(NA, 110000),
                                   feature_num=rep(NA, 110000))
    
    cv_varimp_result<- data.frame(ML_method=rep(NA, 30000),
                                  feature=rep(NA, 30000),
                                  importance=rep(NA, 30000),
                                  cv_fold=rep(NA, 30000),
                                  feature_num=rep(NA, 30000))
    
    m=0
    best_model <- list(length(sele_feature_num))
    best_model_feature <- list(length(sele_feature_num))
    best_ROC_PR <- numeric(length(sele_feature_num))
    
    cv_feature_save <- list(nfold)
    for(a in 1:nfold){
      print(str_c('CV fold ', as.character(a), ' done'))
      train_data <- assessment(cv$splits[[a]])
      test_data <- analysis(cv$splits[[a]])
      
      feature_save <- list(length(sele_feature_num))
      for(b in 1:length(sele_feature_num)){
        sele_feature <- feature_selection(train_data, ranking_method, sele_feature_num[b])
        
        feature_save[[b]] <- sele_feature
        
        feature_loc <- colnames(train_data) %in% sele_feature
        feature_loc[1] <- T
        
        ML_result <- ML_main(train_data[feature_loc],test_data[feature_loc],ML_method)
        model_result <- ML_result[[1]] %>% 
          mutate(cv_fold=a,feature_num=sele_feature_num[b])
        cm_result <- ML_result[[2]] %>% 
          mutate(cv_fold=a,feature_num=sele_feature_num[b])
        ROC_result <- ML_result[[3]] %>% 
          mutate(cv_fold=a,feature_num=sele_feature_num[b])
        
        PR_result <- ML_result[[4]] %>% 
          mutate(cv_fold=a,feature_num=sele_feature_num[b])
        
        meanROC_result <- ML_result[[5]] %>% 
          mutate(cv_fold=a,feature_num=sele_feature_num[b])
        
        varimp_result <- ML_result[[6]] %>% 
          mutate(cv_fold=a,feature_num=sele_feature_num[b])
        
        model_ROC_PR <- ML_result[[3]]$ROC_AUC[1]+ML_result[[4]]$PR_AUC[1]
        
        if(model_ROC_PR>best_ROC_PR[b]){
          best_model[[b]] <- ML_result[[7]]
          best_model_feature[[b]] <- sele_feature
          best_ROC_PR[b] <- model_ROC_PR
        }
        
        
        
        
        cv_model_result[(1+num*m):(num+num*m),] <- model_result
        cv_cm_result[(1+12*m):(12+12*m),] <- cm_result
        
        cv_ROC_result[(1+num2*m):(num2*m+nrow(ROC_result)),] <- ROC_result
        cv_PR_result[(1+num1*m):(num1*m+nrow(PR_result)),] <- PR_result
        cv_meanROC_result[(1+n_ROC*m):(n_ROC+n_ROC*m),] <- meanROC_result
        
        cv_varimp_result[(1+100*m):(100*m+nrow(varimp_result)),] <- varimp_result
        
        m=m+1
      }
      names(feature_save) <- as.character(sele_feature_num)
      cv_feature_save[[a]] <- feature_save
    }
    
    print('Cross validation done')
    
    cv_model_result <- cv_model_result %>% mutate(ranking_method=ranking_method) %>% 
      dplyr::select(ranking_method, ML_method, cv_fold, feature_num, everything()) %>% 
      filter(!is.na(ML_method))
    cv_cm_result <- cv_cm_result %>% mutate(ranking_method=ranking_method) %>% 
      dplyr::select(ranking_method, ML_method, cv_fold, feature_num, everything()) %>% 
      filter(!is.na(ML_method))
    
    cv_ROC_result <- cv_ROC_result %>% mutate(ranking_method=ranking_method) %>% 
      dplyr::select(ranking_method, ML_method, cv_fold, feature_num, everything()) %>% 
      filter(!is.na(ML_method))
    
    cv_PR_result <- cv_PR_result %>% mutate(ranking_method=ranking_method) %>% 
      dplyr::select(ranking_method, ML_method, cv_fold, feature_num, everything()) %>% 
      filter(!is.na(ML_method))
    
    cv_meanROC_result <- cv_meanROC_result %>%
      filter(!is.na(threshold)) %>% 
      group_by(feature_num, threshold) %>% 
      summarise(sensitivity=mean(sensitivity,na.rm=T),
                specificity=mean(specificity,na.rm=T),
                precision=mean(precision,na.rm=T),
                recall=mean(recall,na.rm=T)) %>% 
      mutate(ranking_method=ranking_method,ML_method=ML_method, cv_fold='mean') %>% 
      dplyr::select(ranking_method, ML_method, cv_fold,feature_num, everything())
    
    cv_varimp_result <- cv_varimp_result %>% mutate(ranking_method=ranking_method) %>% 
      dplyr::select(ranking_method, ML_method, cv_fold, feature_num, everything()) %>% 
      filter(!is.na(ML_method))
    
    names(best_model) <- as.character(sele_feature_num)
    names(best_model_feature) <- as.character(sele_feature_num)
    
    return(list(cv_model_result,cv_cm_result, 
                cv_ROC_result,cv_PR_result,
                cv_meanROC_result,cv_feature_save,
                cv_varimp_result,best_model,
                best_model_feature))
    
  }
  
  
  result <- ML(ML_data, ranking_method, ML_method, split_prop = split_prop)
  
  return(result)
}






