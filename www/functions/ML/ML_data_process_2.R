ML_data_process_2 <- function(ML_data, exclude_var_missing=T,
                              missing_pct_limit=50,
                              replace_zero=T, zero2what='min', xmin=0.5,
                              replace_NA=T, NA2what='min', ymin=0.5,
                              pct_transform=T,
                              data_transform=T, trans_type='log',
                              centering=F,
                              scaling=F){
  
  source('/media/md1200/analysis/Script/lipid_web/data_process.R')
  #new
  combo <- unique(ML_data[[1]]$type)
  char_data <- list()
  for(var_num in 1:length(combo)){
    trans_data <- ML_data[[1]] %>% filter(type==combo[var_num]) %>% dplyr::select(-type)
    char_data[[var_num]] <- data_process(trans_data, exclude_var_missing=exclude_var_missing,
                                         missing_pct_limit=missing_pct_limit,
                                         replace_zero=F, zero2what, xmin,
                                         replace_NA=replace_NA, NA2what=NA2what, ymin=ymin,
                                         pct_transform=pct_transform,
                                         data_transform=F, trans_type=F,
                                         centering=F, scaling=F)
  }
  ML_data[[1]] <- Reduce(rbind, char_data)
  ML_data[[1]] <- data_process(ML_data[[1]], exclude_var_missing=F,
                               missing_pct_limit=missing_pct_limit,
                               replace_zero=F, zero2what, xmin,
                               replace_NA=F, NA2what=NA2what, ymin=ymin,
                               pct_transform=F,
                               data_transform, trans_type,
                               centering, scaling)
  data <- ML_data[[1]]
  rownames(data) <- data$feature
  ML_data[[2]] <- data[-1] %>% t() %>% as.data.frame() %>% mutate(group=ML_data[[2]]$group) %>% 
    dplyr::select(group, everything())
  return(ML_data)
}