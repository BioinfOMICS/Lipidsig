

ML_data_process <- function(exp_data, group_info, lipid_char_table, char_var){
  Species2Char <- function(exp_data, lipid_char_table, char_var){
    require(tidyverse)
    require(stringr)
    
    exp_data[-1][is.na(exp_data[-1])] <- 0
    
    if(str_detect(char_var, 'FA_')){
      max_chain_num <- str_split(lipid_char_table[[char_var]],  pattern = ',') %>% 
        map_dbl(length) %>% max()
      
      suppressWarnings(
        transform_table <- exp_data %>% left_join(lipid_char_table[c('feature', char_var)], by='feature') %>% 
          separate(eval(parse(text = char_var)), letters[1:max_chain_num]) %>% 
          gather(letters[1:max_chain_num], key='key', value = 'value') %>% 
          filter(!is.na(value))
      )
      if(nrow(transform_table)==0){
        transform_table <- data.frame()
      }else{
        transform_table <- transform_table %>% 
          dplyr::select(-feature,-key) %>% 
          aggregate(. ~ value, ., sum)
        transform_table[[1]] <- as.character(transform_table[[1]])
        colnames(transform_table)[1] <- char_var
      }
      
    }else{
      transform_table <- exp_data %>% left_join(lipid_char_table[c('feature', char_var)], by='feature') %>% 
        dplyr::select(-1)
      transform_table <- transform_table[!is.na(transform_table[[char_var]]),]
      if(nrow(transform_table)==0){
        transform_table <- data.frame()
      }else{
        transform_table <- transform_table %>% 
          aggregate(as.formula(str_c('. ~ ',char_var)), ., sum)
      }
    }
    
    #if(pct_transform==T  && nrow(transform_table)!=0){}
    transform_table[-1] <- map2(transform_table[-1], colSums(transform_table[-1], na.rm = T), ~.x/.y*100) %>% 
      as.data.frame()
    
    transform_table[-1][transform_table[-1]==0] <- NA
    
    return(transform_table)
  }
  if((!is.null(lipid_char_table))&&(!is.null(char_var))){
    char_num <- length(char_var)
    char_data <- list()
    for(a in 1:char_num){
      char_data[[a]] <- Species2Char(exp_data, lipid_char_table, char_var[a])
      char_data[[a]][[1]] <- str_c(char_var[a],'_',char_data[[a]][[1]])
      colnames(char_data[[a]])[1] <- 'feature'
    }
    data_raw <- rbind(Reduce(rbind, char_data), exp_data)
    data <- data_raw
    rownames(data) <- data[[1]]
    data <- data[-1] %>% t() %>% as.data.frame() %>% 
      mutate(sample_name=colnames(exp_data)[-1])
  }else{
    data_raw <- exp_data
    rownames(exp_data) <- exp_data[[1]]
    data <- exp_data[-1] %>% t() %>% as.data.frame() %>% 
      mutate(sample_name=colnames(exp_data)[-1])
  }
  
  data <- group_info %>% left_join(data, by='sample_name') %>% 
    dplyr::select(-sample_name)
  
  print('Data process done')
  return(list(data_raw, data))
}

