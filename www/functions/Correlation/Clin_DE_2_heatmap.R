# load('/media/md1200/analysis/Script/lipid_web/Corr_functions/Corr_categorical_DEMO.RData')
# Clin_DE_2_heatmap(exp_data,lipid_char_table=lipid_char_table,condition_table=condition_table)

Clin_DE_2_heatmap <- function(exp_data, data_transform = T, lipid_char_table=NULL, 
                              char_var=NULL, condition_table, 
                              paired = F, test = 't.test', adjust_p_method = 'BH', 
                              sig_stat = 'p.adj', sig_pvalue = 0.05, sig_FC = 1,
                              heatmap_col='log2FC',distfun='spearman', hclustfun='average'){
  require(tidyverse)
  require(stringr)
  require(ggthemes)
  require(plotly)
  require(ggpubr)
  require(gplots)
  require(iheatmapr)
  source('www/fucntions/Species2Char.R')
  DE_species_2 <- function(exp_data, data_transform = T, group_info, paired = F, test = 't.test', 
                           adjust_p_method = 'BH', sig_stat = 'p.adj', sig_pvalue = 0.05, sig_FC = 2){
    
    require(tidyverse)
    require(ggthemes)
    require(plotly)
    require(ggpubr)
    
    
    colnames(exp_data)[1] <- 'feature'
    
    if(paired == T){
      exp_data_ga <- exp_data %>% 
        gather(sample_name, value, -1) %>% 
        left_join(group_info, by = 'sample_name') %>% 
        arrange(group, pair, feature)
    }else if(paired == F){
      exp_data_ga <- exp_data %>% 
        gather(sample_name, value, -1) %>% 
        left_join(group_info, by = 'sample_name')
    }
    
    
    if(data_transform == T & test == 't.test'){
      
      exp_data_trans_ga <- exp_data_ga
      exp_data_trans_ga[,3] <- log10(exp_data_trans_ga[,3])
      
      exp_data_tab <- exp_data_ga %>% 
        group_by(feature, group) %>%
        summarise(value = list(value)) %>%
        spread(group, value) %>%
        group_by(feature) %>%
        mutate(mean_ctrl = mean(unlist(ctrl), na.rm = T),
               mean_exp = mean(unlist(exp), na.rm = T), 
               method = 't-test',
               FC = mean_exp/mean_ctrl, 
               log2FC = log2(FC))
      
      exp_data_p <- exp_data_trans_ga %>% 
        group_by(feature, group) %>%
        summarise(value = list(value)) %>%
        spread(group, value) %>% 
        group_by(feature) %>% 
        mutate(p_value = tryCatch(t.test(unlist(ctrl), unlist(exp), paired = paired)$p.value,
                                  error=function(e){NA}),
               t_statistic=tryCatch(t.test(unlist(ctrl), unlist(exp), paired = paired)$statistic,
                                    error=function(e){NA}))
      
      exp_data_p <- exp_data_p %>% ungroup() %>%  mutate(p_adj=p.adjust(exp_data_p$p_value, method = adjust_p_method))
      
      
      exp_data_stat <- exp_data_tab[-2:-3] %>% 
        right_join(exp_data_p[-2:-3], by = 'feature') %>% 
        mutate(sig_p = ifelse(p_value < sig_pvalue & abs(log2(FC)) > log2(sig_FC), 'yes', 'no'), 
               sig_p_adj = ifelse(p_adj < sig_pvalue & abs(log2(FC)) > log2(sig_FC), 'yes', 'no'))
      
      
    }else if(data_transform == F & test == 't.test'){
      
      exp_data_stat <- exp_data_ga %>% 
        group_by(feature, group) %>%
        summarise(value = list(value)) %>%
        spread(group, value) %>%
        group_by(feature) %>%
        mutate(mean_ctrl = mean(unlist(ctrl), na.rm = T),
               mean_exp = mean(unlist(exp), na.rm = T), 
               method = 't-test',
               FC = mean_exp/mean_ctrl,
               log2FC = log2(FC),
               p_value = tryCatch(t.test(unlist(ctrl), unlist(exp), paired = paired)$p.value,
                                  error=function(e){NA}),
               t_statistic=tryCatch(t.test(unlist(ctrl), unlist(exp), paired = paired)$statistic,
                                    error=function(e){NA}))
      
      exp_data_stat <- exp_data_stat %>% ungroup() %>%  mutate(p_adj=p.adjust(exp_data_stat$p_value, method = adjust_p_method)) %>% 
        mutate(sig_p = ifelse(p_value < sig_pvalue & abs(log2(FC)) > log2(sig_FC), 'yes', 'no'), 
               sig_p_adj = ifelse(p_adj < sig_pvalue & abs(log2(FC)) > log2(sig_FC), 'yes', 'no'))
      
      
      exp_data_stat <- exp_data_stat %>% dplyr::select(-2,-3)
    }else if(data_transform == T & test == 'wilcox.test'){
      
      exp_data_trans_ga <- exp_data_ga
      exp_data_trans_ga[,3] <- log10(exp_data_trans_ga[,3])
      
      exp_data_tab <- exp_data_ga %>% 
        group_by(feature, group) %>%
        summarise(value = list(value)) %>%
        spread(group, value) %>%
        group_by(feature) %>%
        mutate(mean_ctrl = mean(unlist(ctrl), na.rm = T),
               mean_exp = mean(unlist(exp), na.rm = T), 
               method = 'wilcoxon test',
               FC = mean_exp/mean_ctrl, 
               log2FC = log2(FC))
      
      exp_data_p <- exp_data_trans_ga %>% 
        group_by(feature, group) %>%
        summarise(value = list(value)) %>%
        spread(group, value) %>%
        group_by(feature) %>%
        mutate(p_value = tryCatch(wilcox.test(unlist(ctrl), unlist(exp), paired = paired)$p.value,
                                  error=function(e){NA}),
               w_statistic = tryCatch(wilcox.test(unlist(ctrl), unlist(exp))$statistic,
                                      error=function(e){NA}))
      
      exp_data_p <- exp_data_p %>% ungroup() %>%  mutate(p_adj=p.adjust(exp_data_p$p_value, method = adjust_p_method))
      
      exp_data_stat <- exp_data_tab[-2:-3] %>% 
        right_join(exp_data_p[-2:-3], by = 'feature') %>% 
        mutate(sig_p = ifelse(p_value < sig_pvalue & abs(log2(FC)) > log2(sig_FC), 'yes', 'no'), 
               sig_p_adj = ifelse(p_adj < sig_pvalue & abs(log2(FC)) > log2(sig_FC), 'yes', 'no'))
      
      
    }else if(data_transform == F & test == 'wilcox.test'){
      
      exp_data_stat <- exp_data_ga %>% 
        group_by(feature, group) %>%
        summarise(value = list(value)) %>%
        spread(group, value) %>%
        group_by(feature) %>%
        mutate(mean_ctrl = mean(unlist(ctrl), na.rm = T),
               mean_exp = mean(unlist(exp), na.rm = T), 
               method = 'wilcoxon test',
               FC = mean_exp/mean_ctrl,
               log2FC = log2(FC),
               p_value = tryCatch(wilcox.test(unlist(ctrl), unlist(exp), paired = paired)$p.value,
                                  error=function(e){NA}),
               w_statistic = tryCatch(wilcox.test(unlist(ctrl), unlist(exp))$statistic,
                                      error=function(e){NA}))
      
      exp_data_stat <- exp_data_stat %>% ungroup() %>%  mutate(p_adj=p.adjust(exp_data_stat$p_value, method = adjust_p_method)) %>% 
        mutate(sig_p = ifelse(p_value < sig_pvalue & abs(log2(FC)) > log2(sig_FC), 'yes', 'no'), 
               sig_p_adj = ifelse(p_adj < sig_pvalue & abs(log2(FC)) > log2(sig_FC), 'yes', 'no'))
      
      
      
      exp_data_stat <- exp_data_stat %>% dplyr::select(-2,-3)
      
    }
    
    
    #### Significant lipid ####
    if(sig_stat == 'p'){
      sig.diff.exp <- exp_data_stat %>%
        filter(sig_p == 'yes') %>% 
        distinct(feature, .keep_all = T)
    }else if(sig_stat == 'p.adj'){
      sig.diff.exp <- exp_data_stat %>%
        filter(sig_p_adj == 'yes') %>% 
        distinct(feature, .keep_all = T)
    }
    return(list(DE_species_table_all = exp_data_stat, 
                DE_species_table_sig = sig.diff.exp))
    
  } #function
  
  
  if((!is.null(lipid_char_table))&&(!is.null(char_var))){
    exp_data <- Species2Char(exp_data, lipid_char_table=lipid_char_table, char_var = char_var)
  }
  
  condition_table[condition_table==1] <- 'exp'
  condition_table[condition_table==0] <- 'ctrl'
  Clin_DE_table_all <- list(length = (ncol(condition_table)-1))
  Clin_DE_table_sig <- list(length = (ncol(condition_table)-1))
  clin_var <- vector(length = (ncol(condition_table)-1))
  #---------DE-----------------------------
  
  for(a in 2:ncol(condition_table)){
    
    k <- condition_table[c(1,a)]
    k <- k %>% mutate(label_name=sample_name, pair=NA)
    clin_var[a-1] <- colnames(k)[2]
    colnames(k)[2] <- 'group'
    non_na_group <- k[(!is.na(k$group)),] %>% .$sample_name
    
    k1 <- k %>% filter(sample_name%in%non_na_group)
    non_na_group2 <- colnames(exp_data)%in%non_na_group
    non_na_group2[1] <- T
    exp_data2 <- exp_data[non_na_group2]
    
    DE_clin <- DE_species_2(exp_data2, data_transform = data_transform, group_info=k1, 
                            paired = paired, test = test, adjust_p_method = adjust_p_method, 
                            sig_stat = sig_stat, sig_pvalue = sig_pvalue, sig_FC = sig_FC)
    
    Clin_DE_table_all[[a-1]] <- DE_clin[[1]] %>% mutate(clin_factor=clin_var[a-1])
    Clin_DE_table_sig[[a-1]] <- DE_clin[[2]] %>% mutate(clin_factor=clin_var[a-1])
    
    
  }
  Clin_DE_table_all <- Reduce(rbind, Clin_DE_table_all) %>% 
    dplyr::select(clin_factor, everything())
  
  Clin_DE_table_sig <- Reduce(rbind, Clin_DE_table_sig) %>% 
    dplyr::select(clin_factor, everything())
  
  #---------heatmap-----------------------------
  
  if(length(unique(Clin_DE_table_sig[[2]]))>1){
    max_colcex <- max(str_length(Clin_DE_table_sig[[2]]))
    max_rowcex <- max(str_length(Clin_DE_table_sig[[1]]))
    
    if(max_colcex<4){
      max_colcex <- 4
    }
    if(max_rowcex<4){
      max_rowcex <- 4
    }
    if(heatmap_col=='log2FC'){
      FC.mat <- Clin_DE_table_sig %>% 
        dplyr::select(clin_factor, feature, log2FC) %>% 
        spread(feature, log2FC) %>%
        column_to_rownames(var = 'clin_factor') %>%
        as.matrix()
    }else if(heatmap_col=='t_statistic'){
      FC.mat <- Clin_DE_table_sig %>% 
        dplyr::select(clin_factor, feature, t_statistic) %>% 
        spread(feature, t_statistic) %>%
        column_to_rownames(var = 'clin_factor') %>%
        as.matrix()
    }
    
    
    FC.mat[is.na(FC.mat)] <- 0
    
    cb_grid <- setup_colorbar_grid(y_length =0.6,x_start = 1,y_start = 0.4)
    if(distfun%in%c("pearson","kendall","spearman")){
      col_dend <- hclust(as.dist(1-cor(FC.mat, method=distfun)),method = hclustfun)
      row_dend <- hclust(as.dist(1-cor(t(FC.mat), method=distfun)),method = hclustfun)
    }else{
      col_dend <- hclust(dist(t(FC.mat), method=distfun),method = hclustfun)
      row_dend <- hclust(dist(FC.mat, method=distfun),method = hclustfun)
    }
    hm <- iheatmap(FC.mat,colorbar_grid = cb_grid) %>%
      add_col_labels(side="bottom",font=list(size=5)) %>%
      add_row_labels(side="left",font=list(size=20)) %>%
      add_col_dendro(col_dend,side="top",reorder =T,size = 0.1) %>%
      add_row_dendro(row_dend,side="right",reorder =T,size = 0.1)
    reorder_FC.mat <- FC.mat[rev(row_dend$order),col_dend$order]
    
  }
  
  if((!is.null(lipid_char_table))&&(!is.null(char_var))){
    colnames(Clin_DE_table_all)[2] <- char_var
    colnames(Clin_DE_table_sig)[2] <- char_var
  }
  
  
  return(list(Clin_DE_table_all=Clin_DE_table_all,
              Clin_DE_table_sig=Clin_DE_table_sig,
              Clin_DE_table_plot = hm, 
              Clin_DE_reorder_mat = reorder_FC.mat))
  
}
