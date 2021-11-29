

annotation <- function(genelist, ref.genelist, database = 'KEGG', sig_pvalue = 0.05,
                       environmentDir='/media/md1200/analysis/Script/function-annotation/environment/'){
  
  require(topGO)
  require(tidyverse)
  source('/media/md1200/analysis/Script/lipid_web/Network_functions/function_code.r')
  dataDir = file.path(environmentDir, "data")
  
  ## plot Gene ontology ##
  if(database %in% c('BP', 'CC', 'MF')){
    
    enrich.res <- plotGO(genelist, onts = database, sig_pvalue = sig_pvalue, orgdb = "org.Hs.eg.db")
    
    plot.tab <- enrich.res$sig.term %>% 
      dplyr::select('term'='Term', 'nGene'='Significant', 'pvalue' = 'classicFisher', m.log.p, gene_name, 'term_ID'='GO.ID')
    
  }else if(database %in% c('KEGG', 'REACTOME')){
    
    if(database == 'KEGG'){
      sourcefile <- read.delim(file.path(dataDir, 'Pathway_KEGG_clean.txt'), 
                               header = T, stringsAsFactors = F, check.names = F)
    }else if(database == 'REACTOME'){
      sourcefile <- read.delim(file.path(dataDir, 'Pathway_Reactome_clean.txt'), 
                               header = T, stringsAsFactors = F, check.names = F)
    }
    
    enrich.res <- pathway(genelist, sourcefile, sig_pvalue = sig_pvalue, ref.genelist)
    
    plot.tab <- enrich.res$sig.term %>% 
      dplyr::select('term'='pathway', 'nGene'='input.gene', pvalue, m.log.p, gene_name)
  } 
  
  return(list(all_term = enrich.res$all.term, 
              sig_term = enrich.res$sig.term, 
              plot_table = plot.tab))
  
} #function annotation()
