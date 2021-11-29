

cor_network <- function(exp_transform_table, lipid_char_table,
                        sig_feature, node_col,
                        cor_method, edge_cutoff){
  
  rownames(exp_transform_table) <- exp_transform_table$feature
  edge_table <- exp_transform_table %>% filter(feature %in% sig_feature) %>% 
    dplyr::select(-1) %>% t() %>% cor(method = cor_method, use = 'pairwise.complete.obs') %>% 
    as.data.frame() %>% mutate(from=rownames(.)) %>% 
    gather(-from, key='to', value='cor_coef') %>% 
    filter(abs(cor_coef)>=edge_cutoff)
  
  node_table <- data.frame(feature=sig_feature, node_col=node_col) %>% 
    filter(feature%in%c(edge_table$from, edge_table$to))
  if(!is.null(lipid_char_table)){
    node_table <- node_table %>% left_join(lipid_char_table, by='feature')
  }
  return(list(node_table, edge_table))
}