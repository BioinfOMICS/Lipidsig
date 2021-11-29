# load('/media/md1200/analysis/Script/lipid_web/Corr_functions/Corr_categorical_DEMO.RData')
# Clin_ROC_heatmap(exp_data,lipid_char_table=lipid_char_table,condition_table=condition_table)

Clin_ROC_heatmap <- function(exp_data, data_transform = T, lipid_char_table=NULL, 
                             char_var=NULL, condition_table, 
                             sig_auc = 0.5,
                             distfun='spearman', hclustfun='average'){
  require(tidyverse)
  require(stringr)
  require(ggthemes)
  require(plotly)
  require(ggpubr)
  require(gplots)
  require(pROC)
  #require(package)
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
  Clin_ROC_table_all <- list(length = (ncol(condition_table)-1))
  Clin_ROC_table_sig <- list(length = (ncol(condition_table)-1))
  #---------ROC-----------------------------
  for(a in 2:ncol(condition_table)){
    feature <- character((ncol(exp_data)-1))
    roc_auc <- numeric((ncol(exp_data)-1))
    roc_auc_lower95 <- numeric((ncol(exp_data)-1))
    roc_auc_upper95 <- numeric((ncol(exp_data)-1))
    
    for(b in 2:ncol(exp_data)){
      
      feature[b-1] <- colnames(exp_data[b])
      ROC_CI <- tryCatch(
        roc(condition_table[[a]], exp_data[[b]], ci = T)$ci,
        error=function(e){NULL})
      
      if(!is.null(ROC_CI)){
        roc_auc[b-1] <- ROC_CI[[2]]
        roc_auc_lower95[b-1] <- ROC_CI[[1]]
        roc_auc_upper95[b-1] <- ROC_CI[[3]]
        
      }else{
        roc_auc[b-1] <- NA
        roc_auc_lower95[b-1] <- NA
        roc_auc_upper95[b-1] <- NA
      }
    }
    Clin_ROC_table_all[[a-1]] <- data.frame(clin_factor=colnames(condition_table)[a],
                                            feature=feature, method='ROC analysis',
                                            roc_auc=roc_auc, roc_auc_lower95=roc_auc_lower95,
                                            roc_auc_upper95=roc_auc_upper95)
    
  }
  Clin_ROC_table_all <- Reduce(rbind, Clin_ROC_table_all)
  
  Clin_ROC_table_all <- Clin_ROC_table_all %>% 
    mutate(sig_auc=ifelse(roc_auc>sig_auc, 'yes','no'))
  Clin_ROC_table_sig <- Clin_ROC_table_all %>% 
    filter(sig_auc=='yes')
  #---------heatmap-----------------------------
  if(length(unique(Clin_ROC_table_sig[[2]]))>1){
    max_colcex <- max(str_length(Clin_ROC_table_sig[[2]]))
    max_rowcex <- max(str_length(Clin_ROC_table_sig[[1]]))
    
    if(max_colcex<4){
      max_colcex <- 4
    }
    if(max_rowcex<4){
      max_colcex <- 4
    }
    ROC.mat <- Clin_ROC_table_sig %>% 
      dplyr::select(clin_factor, feature, roc_auc) %>% 
      spread(feature, roc_auc) %>%
      column_to_rownames(var = 'clin_factor') %>%
      as.matrix()
    
    ROC.mat[is.na(ROC.mat)] <- 0.5
    
    cb_grid <- setup_colorbar_grid(y_length =0.6,x_start = 1,y_start = 0.4)
    if(distfun%in%c("pearson","kendall","spearman")){
      col_dend <- hclust(as.dist(1-cor(ROC.mat, method=distfun)),method = hclustfun)
      row_dend <- hclust(as.dist(1-cor(t(ROC.mat), method=distfun)),method = hclustfun)
    }else{
      col_dend <- hclust(dist(t(ROC.mat), method=distfun),method = hclustfun)
      row_dend <- hclust(dist(ROC.mat, method=distfun),method = hclustfun)
    }
    hm <- iheatmap(ROC.mat,colors = colorRampPalette(c("white", "red"))(n = 300) ,colorbar_grid = cb_grid) %>%
      add_col_labels(side="bottom",font=list(size=2)) %>%
      add_row_labels(side="left",font=list(size=20)) %>%
      add_col_dendro(col_dend,side="top",reorder =T,size = 0.1) %>%
      add_row_dendro(row_dend,side="right",reorder =T,size = 0.1) 
    reorder_ROC_mat <- ROC.mat[rev(row_dend$order),col_dend$order]
  }
  
  if((!is.null(lipid_char_table))&&(!is.null(char_var))){
    colnames(Clin_ROC_table_all)[2] <- char_var
    colnames(Clin_ROC_table_sig)[2] <- char_var
  }
  
  
  return(list(Clin_ROC_table_all=Clin_ROC_table_all,
              Clin_ROC_table_sig=Clin_ROC_table_sig,
              Clin_ROC_table_plot=hm, 
              Clin_ROC_reorder_mat=reorder_ROC_mat))
}
