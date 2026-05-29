####################################
# author:
# name: annotation_human.R
# created date: 2026/03/24
# revise date:
# description: A unified wrapper function for gene set enrichment analysis supporting multiple
#              annotation databases. Dispatches to plotGO() for Gene Ontology analyses (BP, CC, MF)
#              or to pathway() for pathway-based analyses (KEGG, REACTOME). For GO analyses,
#              results are retrieved directly via topGO; for pathway analyses, the corresponding
#              pre-built gene-pathway mapping file is loaded from the data directory and passed
#              to pathway() for Fisher's exact test enrichment. Significant results are
#              harmonized into a unified plot_table format with consistent column names
#              (term, nGene, pvalue, m.log.p, gene_name) for downstream visualization.
# input: genelist      (character vector; query gene symbols for enrichment),
#        ref.genelist  (character vector; reference background gene list, used for KEGG/REACTOME only),
#        database      (character; annotation database to use, one of "BP", "CC", "MF", "KEGG", "REACTOME"; default "KEGG"),
#        sig_pvalue    (numeric; p-value cutoff for significant terms; default 0.05),
#        environmentDir (character; base directory path containing the data/ subfolder
#                        with pathway mapping files; default "www/environment/")
# output: list(all_term, sig_term, plot_table)
#         - all_term   : data frame of all tested terms (full results from plotGO or pathway)
#         - sig_term   : data frame of significant terms filtered by sig_pvalue
#         - plot_table : harmonized data frame of significant terms with columns:
#                        term, nGene, pvalue, m.log.p, gene_name
#                        (additionally includes term_ID for GO analyses)
####################################
annotation <- function(genelist, ref.genelist, database='KEGG', sig_pvalue=0.05,
                       environmentDir='www/environment/'){
  
  dataDir=file.path(environmentDir, "data")
  ## plot Gene ontology ##
  if(database %in% c('BP', 'CC', 'MF')){
    enrich.res <- plotGO(genelist, onts=database, sig_pvalue=sig_pvalue, orgdb="org.Hs.eg.db")
    plot.tab <- enrich.res$sig.term %>% 
      dplyr::select('term'='Term', 'nGene'='Significant', 'pvalue'='classicFisher', m.log.p, gene_name, 'term_ID'='GO.ID')
  }else if(database %in% c('KEGG', 'REACTOME')){
    if(database == 'KEGG'){
      sourcefile <- utils::read.delim(
        file.path(dataDir, 'Pathway_KEGG_clean.txt'), header=TRUE, 
        stringsAsFactors=FALSE, check.names=FALSE)
    }else if(database == 'REACTOME'){
      sourcefile <- utils::read.delim(
        file.path(dataDir, 'Pathway_Reactome_clean.txt'), header=TRUE, 
        stringsAsFactors=FALSE, check.names=FALSE)
    }
    enrich.res <- pathway(genelist, sourcefile, sig_pvalue=sig_pvalue, ref.genelist)
    
    plot.tab <- enrich.res$sig.term %>% 
      dplyr::select('term'='pathway', 'nGene'='input.gene', pvalue, m.log.p, gene_name)
  } 
  return(list(all_term=enrich.res$all.term, 
              sig_term=enrich.res$sig.term, 
              plot_table=plot.tab))
} #function annotation()
