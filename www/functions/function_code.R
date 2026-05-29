####################################
# author:
# name: function_code.R
# created date: 2026/03/24
# revise date:
# Inner SubFunction descriptions:
# pathway: Performs pathway enrichment analysis for a given gene list against a user-supplied
#          pathway-gene mapping file using Fisher's exact test. Input genes are matched to
#          pathways requiring at least two annotated genes. For each qualifying pathway,
#          a 2x2 contingency table is constructed from input gene count, pathway gene count,
#          and reference genome gene count, and a one-sided Fisher's exact test p-value is
#          computed. Results are returned as both full and significant pathway tables,
#          each including gene counts, p-value, -log10(p-value), and annotated gene names.
# input: genelist    (character vector; query gene symbols for enrichment),
#        sourcefile  (matrix or data frame; pathway-gene mapping with columns: [1] ID, [2] pathway, [3] gene),
#        sig_pvalue  (numeric; p-value cutoff for significant pathways; default 0.05),
#        ref.genelist (character vector; reference background gene list for Fisher's exact test)
# output: list(all.term, sig.term)
#         - all.term : data frame of all tested pathways with input.gene, path.gene, pvalue,
#                      m.log.p, and gene_name columns, sorted by ascending p-value
#         - sig.term : subset of all.term filtered by pvalue < sig_pvalue
# plotGO: Performs Gene Ontology (GO) enrichment analysis for a given gene list using the
#         topGO package with the classic Fisher's exact test algorithm. Genes are mapped to
#         GO terms via annFUN.org using gene symbols, and a topGOdata object is constructed
#         with a minimum node size of 2. All GO terms are tested and results are filtered
#         by significance (p-value < sig_pvalue) and minimum significant gene count (≥ 2).
#         For each significant GO term, the annotated significant gene names are extracted
#         and collapsed into a comma-separated string. P-values reported as "< 1e-30" are
#         replaced with 1e-31 for numeric handling.
# input: genelist   (character vector; query gene symbols; duplicates and NAs are removed),
#        onts       (character; GO ontology to test, one of "BP", "MF", "CC"; default "BP"),
#        sig_pvalue (numeric; p-value cutoff for significant GO terms; default 0.05),
#        orgdb      (character; Bioconductor organism annotation package name; default "org.Hs.eg.db")
# output: list(all.term, sig.term)
#         - all.term : data frame of all tested GO terms with GO.ID, Term, Annotated, Significant,
#                      Expected, classicFisher, and m.log.p columns
#         - sig.term : subset of all.term filtered by classicFisher < sig_pvalue and Significant ≥ 2,
#                      with an additional gene_name column containing comma-separated significant gene symbols
####################################

#######################################
#######################################
######                           ######
######                           ######
######   pathway function code   ######
######                           ######
######                           ######
#######################################
#######################################

pathway <- function(genelist, sourcefile, sig_pvalue=0.05, ref.genelist){
  
  ###################################
  ###                             ###
  ###   Check invalid parameter   ###
  ###                             ###
  ###################################
  
  ### Parameter: genelist
  genelist <- unique(genelist)
  if(is.null(genelist) || !is.vector(genelist)){  
    stop("The parameter, genelist, must be a character vector.")
  }
  ### Parameter: sourcefile
  if(is.null(sourcefile)){
    stop("The parameter, sourcefile, must be specified a file name (a character string) or a full path of the source file (a character string).")
  }else{
    myfile <- as.matrix(sourcefile)
  }
  
  ### Parameter: pvalue
  if(sig_pvalue <= 0){
    stop("The parameter, sig_pvalue, must be in the range between 0 (greater than zero) and 1")
  }
  ###################################
  ###                             ###
  ###        Main function        ###
  ###                             ###
  ###################################
  
  ## Matching
  matched <- myfile[,3] %in% genelist ## input gene list
  if(sum(matched)<2){	
    warning("No enough gene-to-pathway results found (at least two gene-to-pathway information) so that cannot output results!")	
  }else{
    pathway_table <- myfile[which(matched==1),]
    unique_pathway_list <- unique(pathway_table[,2])
    idx_2 <- NULL
    for(i in 1:length(unique_pathway_list)){
      if(sum(pathway_table[,2] %in% unique_pathway_list[i]) >= 2){ ## The pathway contain at least two genes
        idx_2 <- c(idx_2, which(pathway_table[,2] %in% unique_pathway_list[i]))	
      }
    }
    
    if(is.null(idx_2)){	
      warning("No pathway containing at least two genes so that cannot output results!")
    }else{
      
      out_file <- pathway_table[idx_2,]
      input.genecount <- out_file %>% 
        as.data.frame() %>%
        dplyr::group_by(pathway) %>% 
        dplyr::summarise(input.gene=dplyr::n()) %>% 
        dplyr::arrange(desc(input.gene))
      input.genecount$pathway <- as.character(input.genecount$pathway)
      
      path.genecount <- myfile %>% 
        as.data.frame() %>% 
        dplyr::filter(pathway %in% input.genecount$pathway) %>% 
        dplyr::filter(gene %in% ref.genelist) %>% 
        dplyr::group_by(pathway) %>% 
        dplyr::summarise(path.gene=dplyr::n()) %>% 
        dplyr::arrange(desc(path.gene))
      path.genecount$pathway <- as.character(path.genecount$pathway)
      
      fisher.tab <- input.genecount %>% 
        dplyr::left_join(path.genecount, by='pathway')
      fisher.tab$pvalue <- NA
      ref.gene <- length(ref.genelist)
      for(m in 1:nrow(fisher.tab)){
        
        contigency_table <- matrix(c(fisher.tab$input.gene[m], length(genelist)-fisher.tab$input.gene[m], 
                                     fisher.tab$path.gene[m], ref.gene-fisher.tab$path.gene[m]), nrow=2)
        fisher.tab$pvalue[m] <- fisher.test(contigency_table)[[1]]
      }
      fisher.tab$m.log.p <- round(-log10(fisher.tab$pvalue), 2)
      out_file_gene <- out_file %>% 
        as.data.frame() %>%
        dplyr::group_by(pathway) %>%
        dplyr::summarise(gene_name=paste(gene, collapse=","))
      out_file_gene$pathway <- as.character(out_file_gene$pathway)
      all.path <- fisher.tab %>% 
        dplyr::left_join(out_file_gene, by='pathway') %>% 
        dplyr::arrange(pvalue)
      sig.path <- all.path %>% dplyr::filter(pvalue < sig_pvalue)
      return(list(all.term=all.path, sig.term=sig.path))
    }
  }
  
} #function pathway()

#######################################
#######################################
######                           ######
######                           ######
######    plotGO function code   ######
######                           ######
######                           ######
#######################################
#######################################

plotGO <- function(genelist, onts='BP', sig_pvalue=0.05, orgdb="org.Hs.eg.db"){
  
  ###################################
  ###                             ###
  ###   Check invalid parameter   ###
  ###                             ###
  ###################################
  ### Parameter: twocolmat
  genelist <- unique(genelist)
  genelist <- genelist[!is.na(genelist)]
  if(is.null(genelist) || !is.vector(genelist)){
    stop("The parameter, genelist, must be a character vector.")
  }else if(length(genelist)<2){
    stop("The number of the genelist must be greater than 1.")
  }
  
  ### Function: annFUN.org
  if(!exists("annFUN.org")){	
    stop("Please install and load the package, topGO, first.")
  }
  
  ###################################
  ###                             ###
  ###        Main function        ###
  ###                             ###
  ###################################
  
  GOfunct <- function(onts="BP", orgdb="org.Hs.eg.db"){ 
    
    y=topGO::annFUN.org(whichOnto=onts, mapping=orgdb, ID="symbol")
    allgenes=unique(unlist(y))
    mygenelist=rep(0,length(allgenes))
    mygenelist[allgenes%in%genelist]=1
    if(sum(mygenelist)==0){
      warning("There is no genes found in annFUN.org of topGO")
    }
    names(mygenelist) <- allgenes
    mygenelist <- as.factor(mygenelist)
    levels(mygenelist) <- c(0, 1)  
    myGOdata <- new("topGOdata", ontology=onts, allGenes=mygenelist, nodeSize=2, annot=annFUN.org, mapping=orgdb, ID="symbol")
    totalnodes <- length(myGOdata@graph@nodes)
    resultFisher <- topGO::runTest(myGOdata, algorithm="classic", statistic="fisher")
    allRes <- topGO::GenTable(myGOdata, classicFisher=resultFisher, orderBy="elimKS", ranksOf="classicFisher", topNodes=totalnodes, numChar=1000)
    allRes$classicFisher <- ifelse(allRes$classicFisher == "< 1e-30", 1e-31, allRes$classicFisher)
    allRes$classicFisher <- as.numeric(allRes$classicFisher)
    allRes$m.log.p <- round(-log10(allRes$classicFisher), 2)
    sigRes <- allRes %>% 
      dplyr::filter(classicFisher < sig_pvalue, Significant >= 2) %>%
      dplyr::mutate(gene_name=NA)
    for (g in 1:nrow(sigRes)){
      ann.score <- sig.gene <- gname <- gtext <- NULL
      ann.score <- topGO::scoresInTerm(myGOdata, sigRes$GO.ID[g], use.names=TRUE) %>% 
        as.data.frame() %>% 
        tibble::rownames_to_column(var="gene_name")
      colnames(ann.score)[2] <- "GO.ID"
      sig.gene <- subset(ann.score, ann.score$GO.ID == 2)
      gname <- sig.gene$gene_name %>% as.character
      gtext <- paste(gname, collapse=",")
      sigRes$gene_name[g]=gtext
    }
    return(list(all.term=allRes, sig.term=sigRes))
  } #function GOfunct()
  RES <- GOfunct(onts, orgdb)
  return(list(all.term=RES$all.term, sig.term=RES$sig.term))
} #function plotGO()
