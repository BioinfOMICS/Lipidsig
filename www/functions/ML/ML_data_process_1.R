ML_data_process_1 <- function(exp_data, group_info, lipid_char_table, char_var){
  
  source('/media/md1200/analysis/Script/lipid_web/Species2Char_20210113.R')
  
  if((!is.null(lipid_char_table))&&(!is.null(char_var))){
    char_num <- length(char_var)
    char_data <- list()
    for(a in 1:char_num){
      #new
      char_data[[a]] <- Species2Char(exp_data, lipid_char_table, char_var[a]) %>% mutate(type=char_var[a])
      char_data[[a]][[1]] <- str_c(char_var[a],'_',char_data[[a]][[1]])
      colnames(char_data[[a]])[1] <- 'feature'
      
    }
    #new
    exp_data2 <- exp_data %>% mutate(type='species')
    data_raw <- rbind(Reduce(rbind, char_data), exp_data2)
    #new
    data <- data_raw[-ncol(data_raw)]
    rownames(data) <- data[[1]]
    data <- data[-1] %>% t() %>% as.data.frame() %>% 
      mutate(sample_name=colnames(exp_data)[-1])
  }else{
    data_raw <- exp_data
    #new 
    data_raw <- data_raw %>% mutate(type='species')
    rownames(exp_data) <- exp_data[[1]]
    data <- exp_data[-1] %>% t() %>% as.data.frame() %>% 
      mutate(sample_name=colnames(exp_data)[-1])
  }
  
  data <- data %>% left_join(group_info, by='sample_name') %>% 
    dplyr::select(-sample_name) %>% dplyr::select(group, everything())
  
  print('Data process done')
  return(list(data_raw, data))
}