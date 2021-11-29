
# load('/media/md1200/analysis/Script/lipid_web/Corr_functions/Corr_categorical_DEMO.RData')
# Clin_LogR_heatmap(exp_data, data_transform = T, lipid_char_table=NULL, 
#                   char_var=NULL, condition_table, adjusted_table = NULL, 
#                   adjust_p_method = 'BH', 
#                   sig_stat = 'p', sig_pvalue = 0.05, 
#                   heatmap_col='ln_OR', distfun='pearson', hclustfun='average',
#                   path=file.path('~/Desktop/2', 'heatmap_sig_DE_metabolites.pdf'))

Clin_LogR_heatmap <- function(exp_data, data_transform = T, lipid_char_table=NULL, 
                              char_var=NULL, condition_table, adjusted_table=NULL,
                              adjust_p_method = 'BH', 
                              sig_stat = 'p.adj', sig_pvalue = 0.05, 
                              heatmap_col='ln_OR',distfun='spearman', hclustfun='average'){
  require(tidyverse)
  require(stringr)
  require(gplots)
  require(iheatmapr)
  source('www/functions/Species2Char.R')
  
  if((!is.null(lipid_char_table))&&(!is.null(char_var))){
    exp_data <- Species2Char(exp_data, lipid_char_table=lipid_char_table, char_var = char_var)
  }
  
  colnames(exp_data)[1] <- 'feature'
  exp_data <- exp_data %>% gather(-feature, key='sample_name', value='value') %>% 
    spread(key='feature', value='value') %>% 
    arrange(sample_name)
  
  condition_table <- condition_table %>% arrange(sample_name)
  
  if(data_transform==T){
    exp_data[-1] <- log10(exp_data[-1])
    #condition_table[-1] <- log10(condition_table[-1])
  }
  
  exp_data[-1] <- scale(exp_data[-1]) %>% as.data.frame()
  if(!is.null(adjusted_table)){
    adjusted_table <- adjusted_table %>% arrange(sample_name)
  }
  
  Clin_LogR_table_all <- list(length = (ncol(condition_table)-1))
  Clin_LogR_table_sig <- list(length = (ncol(condition_table)-1))
  #---------linear regression-----------------------------
  
  for(a in 2:ncol(condition_table)){
    feature <- character((ncol(exp_data)-1))
    ln_OR <- numeric((ncol(exp_data)-1))
    OR <- numeric((ncol(exp_data)-1))
    OR_upper95 <- numeric((ncol(exp_data)-1))
    OR_lower95 <- numeric((ncol(exp_data)-1))
    
    z_statistic <- numeric((ncol(exp_data)-1))
    p_value <- numeric((ncol(exp_data)-1))
    AIC <- numeric((ncol(exp_data)-1))
    
    
    for(b in 2:ncol(exp_data)){
      
      feature[b-1] <- colnames(exp_data)[b]
      
      if(!is.null(adjusted_table)){
        data_lr <- cbind(condition_table[a],exp_data[b], adjusted_table[-1])
      }else{
        data_lr <- cbind(condition_table[a],exp_data[b])
      }
      
      colnames(data_lr)[1] <- c('clin_var')
      
      lr <- tryCatch(
        glm(clin_var~., family ='binomial' ,data=data_lr,control=list(maxit=50)),
        error=function(e){NULL},
        warning=function(w){NULL})
      
      if(!is.null(lr)){
        lr_summary <- summary(lr)
        coef <- lr_summary$coefficients
        ln_OR[b-1] <- coef[2,1]
        OR[b-1] <- exp(coef[2,1])
        z_statistic[b-1] <- coef[2,3]
        p_value[b-1] <- coef[2,4]
        AIC[b-1] <- lr_summary$aic
        
        CI <- tryCatch(
          confint.default(lr),
          error=function(e){NULL},
          warning=function(w){NULL})
        if(!is.null(CI)){
          OR_upper95[b-1] <- exp(CI[2,2])
          OR_lower95[b-1] <- exp(CI[2,1])
        }else{
          OR_upper95[b-1] <- NA
          OR_lower95[b-1] <- NA
        }
        
        
      }else{
        OR[b-1] <- NA
        z_statistic[b-1] <- NA
        p_value[b-1] <- NA
        OR_upper95[b-1] <- NA
        OR_lower95[b-1] <- NA
        AIC[b-1] <- NA
        
      }
    }
    
    Clin_LogR_table_all[[a-1]] <- data.frame(clin_factor=colnames(condition_table)[a],
                                             feature=feature, method='Logistic Regression',
                                             ln_OR=ln_OR,OR=OR, OR_lower95=OR_lower95,OR_upper95=OR_upper95,
                                             z_statistic=z_statistic,p_value=p_value, 
                                             p_adj=p.adjust(p_value, method = adjust_p_method, n = length(p_value)))
    
    
    
  }
  
  Clin_LogR_table_all <- Reduce(rbind, Clin_LogR_table_all)
  
  Clin_LogR_table_all <- Clin_LogR_table_all %>% 
    mutate(sig_p=ifelse(p_value<sig_pvalue, 'yes','no'),
           sig_p_adj=ifelse(p_adj<sig_pvalue, 'yes','no'))
  
  if(sig_stat=='p'){
    Clin_LogR_table_sig <- Clin_LogR_table_all %>% 
      filter(sig_p=='yes')
  }else if(sig_stat=='p.adj'){
    Clin_LogR_table_sig <- Clin_LogR_table_all %>% 
      filter(sig_p_adj=='yes')
  }
  
  #---------heatmap-----------------------------
  if(length(unique(Clin_LogR_table_sig[[2]]))>1){
    max_colcex <- max(str_length(Clin_LogR_table_sig[[2]]))
    if(max_colcex<4){
      max_colcex <- 4
    }
    if(heatmap_col=='ln_OR'){
      LogR.mat <- Clin_LogR_table_sig %>% 
        dplyr::select(clin_factor, feature, ln_OR) %>% 
        spread(feature, ln_OR) %>%
        column_to_rownames(var = 'clin_factor') %>%
        as.matrix()
    }else if(heatmap_col=='z_statistic'){
      LogR.mat <- Clin_LogR_table_sig %>% 
        dplyr::select(clin_factor, feature, z_statistic) %>% 
        spread(feature, z_statistic) %>%
        column_to_rownames(var = 'clin_factor') %>%
        as.matrix()
    }
    
    
    LogR.mat[is.na(LogR.mat)] <- 0
    
    cb_grid <- setup_colorbar_grid(y_length =0.6,x_start = 1,y_start = 0.4)
    if(distfun%in%c("pearson","kendall","spearman")){
      col_dend <- hclust(as.dist(1-cor(LogR.mat, method=distfun)),method = hclustfun)
      row_dend <- hclust(as.dist(1-cor(t(LogR.mat), method=distfun)),method = hclustfun)
    }else{
      col_dend <- hclust(dist(t(LogR.mat), method=distfun),method = hclustfun)
      row_dend <- hclust(dist(LogR.mat, method=distfun),method = hclustfun)
    }
    hm <- iheatmap(LogR.mat,colorbar_grid = cb_grid) %>%
      add_col_labels(side="bottom",font=list(size=3.5)) %>%
      add_row_labels(side="left",font=list(size=20)) %>%
      add_col_dendro(col_dend,side="top",reorder =T,size = 0.1) %>%
      add_row_dendro(row_dend,side="right",reorder =T,size = 0.1)
    reorder_LogR.mat <- LogR.mat[rev(row_dend$order),col_dend$order]
    
  }
  
  if((!is.null(lipid_char_table))&&(!is.null(char_var))){
    colnames(Clin_LogR_table_all)[2] <- char_var
    colnames(Clin_LogR_table_sig)[2] <- char_var
  }
  
  return(list(Clin_LogR_table_all=Clin_LogR_table_all,
              Clin_LogR_table_sig=Clin_LogR_table_sig,
              Clin_LogR_table_plot=hm, 
              Clin_LogR_reorder_mat=reorder_LogR.mat))
  
}
