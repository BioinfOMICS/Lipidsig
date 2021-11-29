#######################################
#######################################
######                           ######
######                           ######
######   pathway function code   ######
######                           ######
######                           ######
#######################################
#######################################

pathway <- function(genelist, sourcefile, sig_pvalue = 0.05, ref.genelist){
  
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
  #sourcefile = as.character(sourcefile)
  if(is.null(sourcefile)){
    stop("The parameter, sourcefile, must be specified a file name (a character string) or a full path of the source file (a character string).")
    #}else if(!file.exists(sourcefile)){
    #  stop("Please make sure the sourcefile exists in the current working directory.")
  }else{
    
    myfile <- as.matrix(sourcefile)
    
    # ### Parameter: db
    # db = as.character(db)
    # all_db = unique(myfile[,1])
    # if(sum(all_db%in%db)==0 && db!="all" && db!="other"){	
    #   stop("The parameter, db, must be one of the database name (the first column)from the sourcefile.")	
    # }
    # 
    # if(db!="all"){
    #   if(db=="other"){
    #     db_idx = which(myfile[,1]%in%c("KEGG","BIOCARTA","PID","REACTOME"))
    #     myfile = myfile[-db_idx,]
    #   }else{
    #     db_idx = which(myfile[,1]%in%db)
    #     myfile = myfile[db_idx,]
    #   }
    # }
    # if(ncol(myfile) != 3){	
    #   stop("The sourcefile must be a three-column table file: \n 1st column is database names, 2nd column is a pathway names, 3rd column is gene names")	
    # }else if(nrow(myfile) < 1){	
    #   stop("The sourcefile must contain at least one gene-to-pathway information.")
    # }
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
    #unique_pathway_list <- unique(paste(pathway_table[,1], pathway_table[,2], sep="_")) #prevent the same pathway name from different database
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
        group_by(pathway) %>% 
        summarise(input.gene = n()) %>% 
        arrange(desc(input.gene))
      input.genecount$pathway <- as.character(input.genecount$pathway)
      
      path.genecount <- myfile %>% 
        as.data.frame() %>% 
        filter(pathway %in% input.genecount$pathway) %>% 
        filter(gene %in% ref.genelist) %>% 
        group_by(pathway) %>% 
        summarise(path.gene = n()) %>% 
        arrange(desc(path.gene))
      path.genecount$pathway <- as.character(path.genecount$pathway)
      
      fisher.tab <- input.genecount %>% 
        left_join(path.genecount, by = 'pathway')
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
        group_by(pathway) %>%
        summarise(gene_name = paste(gene, collapse = ","))
      out_file_gene$pathway <- as.character(out_file_gene$pathway)
      
      
      all.path <- fisher.tab %>% 
        left_join(out_file_gene, by = 'pathway') %>% 
        arrange(pvalue)
      
      sig.path <- all.path %>% 
        filter(pvalue < sig_pvalue)
      
      return(list(all.term = all.path, 
                  sig.term = sig.path))
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

plotGO <- function(genelist, onts = 'BP', sig_pvalue = 0.05, orgdb="org.Hs.eg.db"){
  
  ###################################
  ###                             ###
  ###   Check invalid parameter   ###
  ###                             ###
  ###################################
  
  #require(topGO)
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
    
    y = annFUN.org(whichOnto=onts, mapping=orgdb, ID="symbol")
    allgenes = unique(unlist(y))
    mygenelist=rep(0,length(allgenes))
    mygenelist[allgenes%in%genelist]=1
    if(sum(mygenelist)==0){
      warning("There is no genes found in annFUN.org of topGO")
    }
    names(mygenelist)=allgenes
    mygenelist=as.factor(mygenelist)
    levels(mygenelist)=c(0,1)  
    myGOdata = new("topGOdata", ontology=onts, allGenes=mygenelist, nodeSize=2, annot=annFUN.org, mapping=orgdb, ID="symbol")
    totalnodes = length(myGOdata@graph@nodes)
    resultFisher = runTest(myGOdata, algorithm="classic", statistic="fisher")
    allRes = GenTable(myGOdata, classicFisher=resultFisher, orderBy="elimKS", ranksOf="classicFisher", topNodes=totalnodes, numChar=1000)
    allRes$classicFisher <- ifelse(allRes$classicFisher == "< 1e-30", 1e-31, allRes$classicFisher)
    allRes$classicFisher <- as.numeric(allRes$classicFisher)
    allRes$m.log.p <- round(-log10(allRes$classicFisher), 2)
    
    sigRes <- allRes %>% 
      filter(classicFisher < sig_pvalue, Significant >= 2) %>%
      mutate(gene_name = NA)
    
    for (g in 1:nrow(sigRes)){
      
      ann.score=sig.gene=gname=gtext=NULL
      
      ann.score <- scoresInTerm(myGOdata, sigRes$GO.ID[g], use.names = TRUE) %>% 
        as.data.frame() %>% 
        tibble::rownames_to_column(var = "gene_name")
      colnames(ann.score)[2] <- "GO.ID"
      sig.gene <- subset(ann.score, ann.score$GO.ID == 2)
      gname <- sig.gene$gene_name %>% as.character
      gtext <- paste(gname, collapse = ",")
      sigRes$gene_name[g]=gtext
      
    }
    
    return(list(all.term = allRes, sig.term = sigRes))
    
    #allRes$gene_name=matrix(NA,nrow(allRes),1)
    # for (g in 1:nrow(allRes)){
    #   ann.score=sig.gene=gname=gtext=NULL
    #   ann.score <- scoresInTerm(myGOdata, allRes$GO.ID[g], use.names = TRUE) %>% as.data.frame %>% 
    #     tibble::rownames_to_column(var = "gene_name")
    #   colnames(ann.score)[2] <- "GO.ID"
    #   sig.gene <- subset(ann.score, ann.score$GO.ID == 2)
    #   gname <- sig.gene$gene_name %>% as.character
    #   gtext <- paste(gname, collapse = ",")
    #   allRes$gene_name[g]=gtext
    #   ##pc##
    #   allRes$classicFisher[g]=ifelse(allRes$classicFisher[g] == "< 1e-30", 31, round(-log10(as.numeric(allRes$classicFisher[g])), 2))
    # }
    # ##pc##
    # allRes$classicFisher <- as.numeric(allRes$classicFisher)
    # ##pc##allRes[,6] = round(-log10(as.numeric(allRes[,6])), 2)
    # 
    # allRes1 = allRes[which(allRes$Significant>=2& as.double(allRes$classicFisher)>=(-log10(0.05))),]
    # colnames(allRes1)[6] = colnames(allRes)[6] = "-log10(p-value)"
    # 
    # if(onts=="BP"){
    #   write.table(allRes, paste(filename, "_all_table.txt", sep=""), sep="\t", quote=F, row.names=F)
    #   write.table(allRes1, paste(filename, "_table.txt", sep=""), sep="\t", quote=F, row.names=F)
    # } else {
    #   write.table(allRes, paste(filename, paste(onts, "_all_table.txt", sep=""), sep="."), sep="\t", quote=F, row.names=F)
    #   write.table(allRes1, paste(filename, paste(onts, "_table.txt", sep=""), sep="."), sep="\t", quote=F, row.names=F)    
    # }
    # 
  } #function GOfunct()
  
  RES <- GOfunct(onts, orgdb)
  
  return(list(all.term = RES$all.term, 
              sig.term = RES$sig.term))
  
} #function plotGO()




