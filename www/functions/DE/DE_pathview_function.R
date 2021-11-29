
pathview_function <- function(lipid_class, path, lipid_gene_path, pathway_gene_list){
  require(tidyverse)
  require(pathview)
  #load('/home/william/lipid_web/mapping_table/pathview_function.RData')
  #lipid_gene_path <- readRDS('/media/md1200/analysis/Script/lipid_web/Shiny/test1/test1.v3/www/mapping_table/lipid_gene_path.rds')
  #pathway_gene_list <- readRDS('/media/md1200/analysis/Script/lipid_web/Shiny/test1/test1.v3/www/mapping_table/pathway_gene_list.rds')
  
  kegg_cpd <- lipid_gene_path[c(1,2,4,7,8)] %>% filter(key_name%in%lipid_class) %>% 
    filter(DB=='KEGG') %>% unique()
  kegg_cpd <- kegg_cpd[complete.cases(kegg_cpd),]
  kegg_path <- sort(table(na.omit(kegg_cpd$path_id)),decreasing = T)
  pathview_path <- kegg_path %>% names() %>% str_sub(start = 5)
  
  
  cpd <- lipid_gene_path %>% filter(key_name%in%lipid_class) %>% 
    filter(DB=='KEGG') %>% .$ID
  
  lipid_class_list <- unique(cpd)
  
  if (length(lipid_class_list)==0) {
    stop('Lipid class name not found')
  }
  pathview_cpd <- rep(1,length(lipid_class_list))
  
  names(pathview_cpd) <- lipid_class_list
  
  setwd(path)
  
  for(a in 1:length(kegg_path)){
    
    pathway_gene <- which(str_sub(names(pathway_gene_list),start = 5)==pathview_path[a])
    
    pathway_gene <- pathway_gene_list[[pathway_gene]] %>% str_sub(start = 10)
    
    pathview_gene <- rep(0, length(pathway_gene))
    names(pathview_gene) <- pathway_gene  
    
    tryCatch({
      pathview(gene.data = pathview_gene, cpd.data=pathview_cpd, pathway.id  = pathview_path[a], species = "hsa", 
               out.suffix = "pathview",discrete=list(cpd=T),
               limit = list(cpd = c(0,1)), bins = list(cpd = 1), 
               mid = list(cpd = "red"),low = list(cpd = "white", gene='#bfffbf'),
               plot.col.key=F, kegg.native = T,same.layer=T) #kegg.dir='C:/Users/user/Desktop/KEGG/pathview/
    }, error = function(e){message(e)})
    
  }
  
  
  return(kegg_cpd)
  
}
