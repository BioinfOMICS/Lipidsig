
lipid_char_table_gather <- function(lipid_char_table, char_var){
  
  require(tidyverse)
  
  lipid_char <- lipid_char_table %>% 
    dplyr::select(feature, all_of(char_var))
  colnames(lipid_char) <- c('feature', 'characteristic')
  
  lipid_char_ga <- lipid_char %>%
    tidyr::separate(col = characteristic, into = c('c1', 'c2'), sep = ',', 
                    remove = T, convert = T, fill = 'right') %>% 
    gather(C, Value, -1) %>% 
    filter(!is.na(Value)) %>%
    dplyr::select(-C)
  colnames(lipid_char_ga) <- c('feature', char_var)
  
  return(lipid_char_table_ga = lipid_char_ga)
  
} #function