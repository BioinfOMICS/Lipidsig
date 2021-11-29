
# load('/media/md1200/analysis/Script/lipid_web/Corr_functions/Corr_continuous_DEMO.RData')
# Clin_Cor_heatmap(exp_data,condition_table=condition_table,lipid_char_table=lipid_char_table)

Clin_Cor_heatmap <- function(exp_data, #data_transform = T, 
                             #lipid_char_table=NULL, char_var=NULL, 
                             condition_table, 
                             test = 'pearson', adjust_p_method = 'BH', 
                             sig_stat = 'p.adj', sig_pvalue = 0.05, sig_cor_coef = 0.3,
                             heatmap_col='statistic', distfun='spearman', hclustfun='average'){
  require(tidyverse)
  require(stringr)
  require(iheatmapr)
  #source('/media/md1200/analysis/Script/lipid_web/Species2Char.R')
  
  # if((!is.null(lipid_char_table))&&(!is.null(char_var))){
  #   exp_data <- Species2Char(exp_data, lipid_char_table=lipid_char_table, char_var = char_var)
  # }
    CHAR <- colnames(exp_data)[1]
    colnames(exp_data)[1] <- 'feature'
    exp_data <- exp_data %>% gather(-feature, key='sample_name', value='value') %>% 
      spread(key='feature', value='value') %>% 
      arrange(sample_name)
    
    condition_table <- condition_table %>% arrange(sample_name)
    # if(data_transform==T){
    #   exp_data[-1] <- log10(exp_data[-1])
    #   #condition_table[-1] <- log10(condition_table[-1])
    # }
    Clin_Cor_table_all <- list(length = (ncol(condition_table)-1))
    Clin_Cor_table_sig <- list(length = (ncol(condition_table)-1))
    #---------Cor-----------------------------
    for(a in 2:ncol(condition_table)){
      feature <- character((ncol(exp_data)-1))
      cor_coef <- numeric((ncol(exp_data)-1))
      p_value <- numeric((ncol(exp_data)-1))
      statistic <- numeric((ncol(exp_data)-1))
      for(b in 2:ncol(exp_data)){
        feature[b-1] <- colnames(exp_data)[b]
        cor_test <- tryCatch(
          cor.test(exp_data[[b]],condition_table[[a]], method=test),
          error=function(e){NULL})
        if(!is.null(cor_test)){
          statistic[b-1] <- cor_test$statistic
          p_value[b-1] <- cor_test$p.value
          cor_coef[b-1] <- cor_test$estimate
        }else{
          statistic[b-1] <- NA
          p_value[b-1] <- NA
          cor_coef[b-1] <- NA
        }
      }
      
      Clin_Cor_table_all[[a-1]] <- data.frame(clin_factor=colnames(condition_table)[a],
                                              feature=feature, method=test,
                                              cor_coef=cor_coef, statistic=statistic, p_value=p_value,
                                              p_adj=p.adjust(p_value, method = adjust_p_method, n = length(p_value)))
      
    }
    Clin_Cor_table_all <- Reduce(rbind, Clin_Cor_table_all)
    
    Clin_Cor_table_all <- Clin_Cor_table_all %>% 
      mutate(sig_p=ifelse(abs(cor_coef)>sig_cor_coef & p_value<sig_pvalue, 'yes','no'),
             sig_p_adj=ifelse(abs(cor_coef)>sig_cor_coef & p_adj<sig_pvalue, 'yes','no'))
    
    if(sig_stat=='p'){
      Clin_Cor_table_sig <- Clin_Cor_table_all %>% 
        filter(sig_p=='yes')
    }else if(sig_stat=='p.adj'){
      Clin_Cor_table_sig <- Clin_Cor_table_all %>% 
        filter(sig_p_adj=='yes')
    }
    
    #---------heatmap-----------------------------
    if(length(unique(Clin_Cor_table_sig[[2]]))>1){
      max_colcex <- max(str_length(Clin_Cor_table_sig[[2]]))
      max_rowcex <- max(str_length(Clin_Cor_table_sig[[1]]))
      
      if(max_colcex<4){
        max_colcex <- 4
      }
      if(max_rowcex<4){
        max_rowcex <- 4
      }
      if(heatmap_col=='cor_coef'){
        Cor.mat <- Clin_Cor_table_sig %>% 
          dplyr::select(clin_factor, feature, cor_coef) %>% 
          spread(feature, cor_coef) %>%
          column_to_rownames(var = 'clin_factor') %>%
          as.matrix()
      }else if(heatmap_col=='statistic'){
        Cor.mat <- Clin_Cor_table_sig %>% 
          dplyr::select(clin_factor, feature, statistic) %>% 
          spread(feature, statistic) %>%
          column_to_rownames(var = 'clin_factor') %>%
          as.matrix()
      }
      
      
      Cor.mat[is.na(Cor.mat)] <- 0
      if(sum(is.na(Cor.mat))==0 & nrow(Cor.mat) >= 2 & ncol(Cor.mat) >= 2){
      cb_grid <- setup_colorbar_grid(y_length =0.6,x_start = 1,y_start = 0.4)
      if(distfun%in%c("pearson","kendall","spearman")){
        col_dend <- hclust(as.dist(1-cor(Cor.mat, method=distfun)),method = hclustfun)
        row_dend <- hclust(as.dist(1-cor(t(Cor.mat), method=distfun)),method = hclustfun)
      }else{
        col_dend <- hclust(dist(t(Cor.mat), method=distfun),method = hclustfun)
        row_dend <- hclust(dist(Cor.mat, method=distfun),method = hclustfun)
      }
      
      heatmap_color_scale <- function(data){
        data <- round(data,3)
        if(max(data)<=0 & min(data)<0){
          over_median <- min(data)/2
          if(max(data)<over_median){
            color <-  colorRampPalette(c("#157AB5","#92c5de"))(n = 1000)
          }else{
            color_rank <- round(max(data)/(min(data))*1000)
            color_scale <- colorRampPalette(c("#0571b0","#92c5de","white"))(n = 1000)
            color <- color_scale[color_rank:1000]
          }
        }else if(min(data)>=0 & max(data)>0){
          over_median <- max(data)/2
          if(min(data)>over_median){
            color <-  colorRampPalette(c("#f4a582", "#ca0020"))(n = 1000)
          }else{
            color_rank <- round(min(data)/(max(data))*1000)
            color_scale <- colorRampPalette(c("white","#f4a582", "#ca0020"))(n = 1000)
            color <- color_scale[color_rank:1000]
          }
        }
        return(color)
      }
      
      if(min(Cor.mat)>=0){
        hm <- iheatmap(Cor.mat,colors = heatmap_color_scale(Cor.mat),colorbar_grid = cb_grid) %>%
          add_col_dendro(col_dend,side="top",reorder =T,size=0.1) %>%
          add_row_dendro(row_dend,side="right",reorder =T,size=0.1)
      }else if(max(Cor.mat)<=0){
        hm <- iheatmap(Cor.mat,colors = heatmap_color_scale(Cor.mat),colorbar_grid = cb_grid) %>%
          add_col_dendro(col_dend,side="top",reorder =T,size=0.1) %>%
          add_row_dendro(row_dend,side="right",reorder =T,size=0.1)
      }else{
        hm <- iheatmap(Cor.mat,colorbar_grid = cb_grid) %>%
          add_col_dendro(col_dend,side="top",reorder =T,size=0.1) %>%
          add_row_dendro(row_dend,side="right",reorder =T,size=0.1)
      }
      if(max(nchar(colnames(Cor.mat)))<10){
        text_size <- 0.1
      }else if(max(nchar(colnames(Cor.mat)))<20 & max(nchar(colnames(Cor.mat)))>=10){
        text_size <- 0.2
      }else if(max(nchar(colnames(Cor.mat)))<30 & max(nchar(colnames(Cor.mat)))>=20){
        text_size <- 0.3
      }else if(max(nchar(colnames(Cor.mat)))<40 & max(nchar(colnames(Cor.mat)))>=30){
        text_size <- 0.4
      }else{
        text_size <- 0.5
      }
      if(nrow(Cor.mat)<50){
        hm <- hm %>% add_row_labels(font=list(size=8))
      }
      if(ncol(Cor.mat)<50){
        hm <- hm %>%add_col_labels(side="bottom",font=list(size=8),size= text_size)
      }
      
      reorder_Cor.mat <- Cor.mat[rev(row_dend$order),col_dend$order]
      }else{
        hm = NULL
        reorder_Cor.mat = NULL
      }
      
    }else{
      hm = NULL
      reorder_Cor.mat = NULL
    }
    
    # if((!is.null(lipid_char_table))&&(!is.null(char_var))){
    #   colnames(Clin_Cor_table_all)[2] <- char_var
    #   colnames(Clin_Cor_table_sig)[2] <- char_var
    # }
    colnames(Clin_Cor_table_all)[2] <- CHAR
    colnames(Clin_Cor_table_sig)[2] <- CHAR
  
  return(list(Clin_Cor_table_all = Clin_Cor_table_all,
              Clin_Cor_table_sig = Clin_Cor_table_sig,
              Clin_Cor_table_plot = hm, 
              Clin_Cor_reorder_mat = reorder_Cor.mat))
  
}