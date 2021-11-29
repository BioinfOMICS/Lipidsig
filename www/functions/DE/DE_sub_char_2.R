DE_sub_char_2 <- function(exp_data, data_transform=T, lipid_char_table, split_var, char_var, 
                          group_info, paired=F, sig_pvalue=0.05, sig_FC=0, 
                          exclude_var_missing=T, missing_pct_limit=50,
                          replace_zero=T, zero2what='min', xmin=0.5,
                          replace_NA=T, NA2what='min', ymin=0.5,
                          pct_transform=T,
                          trans_type='log',
                          centering=F, scaling=F){
  require(tidyverse)
  source('www/functions/Species2Char.R')
  source('www/functions/data_process.R')
  source('www/functions/DE/DE_char_2.R')
  
  split_var_member <- lipid_char_table %>% filter(!is.na(eval(parse(text = char_var)))) %>% 
    .[[split_var]] %>% unique() %>% na.omit()
  
  exp_data <- exp_data %>% left_join(lipid_char_table[c('feature', split_var)], by='feature')
  
  result_table1 <- list()
  result_table2 <- list()
  result_table3 <- list()
  result_table4 <- list()
  
  for(var_member in 1:length(split_var_member)){
    split_exp_data <- exp_data %>% filter(eval(parse(text = split_var))==split_var_member[var_member]) %>% dplyr::select(-split_var)
    split_lipid_char_table <- lipid_char_table %>% filter(eval(parse(text = split_var))==split_var_member[var_member])
    
    spec2char <- Species2Char(split_exp_data, split_lipid_char_table, char_var)
    
    if(nrow(spec2char)==0){
      print('no')
      next()
    }
    
    exp_data_trans <- data_process(spec2char, exclude_var_missing, missing_pct_limit,
                                   replace_zero, zero2what, xmin,
                                   replace_NA, NA2what, ymin,
                                   pct_transform,
                                   data_transform=F, trans_type,
                                   centering, scaling)
    
    if(is.null(exp_data_trans)){
      print('no')
      next()
    }
    
    result <- DE_char_2(exp_data_trans, data_transform=data_transform, 
                        #lipid_char_table=split_lipid_char_table, char_var=char_var,
                        group_info=group_info, paired=paired, 
                        sig_pvalue=sig_pvalue, sig_FC=sig_FC)
    
    result[[1]][split_var] <- split_var_member[var_member]
    result[[2]][split_var] <- split_var_member[var_member]
    result[[3]][split_var] <- split_var_member[var_member]
    result[[4]][split_var] <- split_var_member[var_member]
    
    result_table1[[var_member]] <- result[[1]] %>% dplyr::select(split_var, everything())
    result_table2[[var_member]] <- result[[2]] %>% dplyr::select(split_var, everything())
    result_table3[[var_member]] <- result[[3]] %>% dplyr::select(split_var, everything())
    result_table4[[var_member]] <- result[[4]] %>% dplyr::select(split_var, everything())
    
    
  }
  result_table1 <- Reduce(rbind, result_table1)
  result_table2 <- Reduce(rbind, result_table2)
  result_table3 <- Reduce(rbind, result_table3)
  result_table4 <- Reduce(rbind, result_table4)
  
  return(list(result_table1, result_table2, result_table3, result_table4))
}
