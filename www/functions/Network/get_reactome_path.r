

get_path <- function(lipid_class, path, max_path_length, 
                     All_interaction, All_node, complex, edge, node,
                     node_id_for_complex, Uniprot, lipid_gene_path){
  require(tidyverse)
  require(igraph)
  require(stringr)
  
  #### Function: add_member_complex ####
  add_member_complex <- function(data){
    require(tidyverse)
    complex_transform <- function(complex){
      complex_componunt <- str_split(complex$participants, '\\|')
      new_complex <- vector(mode='list', length = length(unlist(complex_componunt)))
      m <- 1
      for (p in 1:nrow(complex)) {
        for (q in 1:length(complex_componunt[[p]])) {
          new_complex[[m]] <- complex[p,] %>% mutate(second=complex_componunt[[p]][q])
          m <- m+1
        }
      }
      new_complex <- Reduce(rbind, new_complex)
      return(new_complex)
    }
    lipid_class_interaction <- vector(mode='list', length = length(data))
    
    for (a in 1:length(data)) {
      if (nrow(data[[a]])>0) {
        k <- c((data[[a]] %>% filter(`BIOPAX_TYPE.x`=='BiochemicalReaction') %>% .$first), 
               (data[[a]] %>% filter(`BIOPAX_TYPE.y`=='BiochemicalReaction') %>% .$second)) %>% unique()
        
        k <- rbind(data[[a]], Uniprot %>% mutate(first=purrr::map_chr(strsplit(V3, '\\['), function(x){x[1]})) %>% 
                     mutate(first=str_sub(first, end = -2)) %>% 
                     dplyr::select(1,3,6,9,10) %>% filter(V6 %in% k) %>% 
                     mutate(second=V6, interaction='control', name=str_c(first, interaction, second)) %>% 
                     mutate(BIOPAX_TYPE.x='Protein', UNIPROT_CHEBI.x=V1) %>% 
                     mutate(BIOPAX_TYPE.y='BiochemicalReaction', UNIPROT_CHEBI.y='') %>% 
                     mutate(pathway='', samename='no') %>% 
                     dplyr::select(first, second, interaction, name, BIOPAX_TYPE.x, UNIPROT_CHEBI.x,
                                   BIOPAX_TYPE.y, UNIPROT_CHEBI.y, pathway,  samename))
        k <- k[!duplicated(k[c(1,2)]),]
        
        k1 <- c((k %>% filter(`BIOPAX_TYPE.x`=='Complex') %>% .$first),
                (k %>% filter(`BIOPAX_TYPE.y`=='Complex') %>% .$second)) %>% unique()
        complex2 <- complex[!duplicated(complex[6]),] %>% filter(first%in%k1)
        
        if(nrow(complex2)>0){
          
          lipid_class_interaction[[a]] <- complex_transform(complex2) %>% 
            dplyr::select(6,7) %>% 
            mutate(interaction='contains') %>% 
            mutate(BIOPAX_TYPE='Complex', UNIPROT_CHEBI='') %>% 
            mutate(pathway='', samename='no') %>%
            merge(node_id_for_complex, by.x='second', by.y = 'entityReference.xref') %>% 
            mutate(second=displayName) %>% 
            mutate(name=str_c(first, interaction, second)) %>% 
            dplyr::select(first, second, interaction, name, BIOPAX_TYPE.x, UNIPROT_CHEBI.x,
                          BIOPAX_TYPE.y, UNIPROT_CHEBI.y, pathway,  samename) %>% 
            rbind(k)
          print(a)
        }else{
          lipid_class_interaction[[a]] <- k
          print(a)
        }
        
      }else{
        lipid_class_interaction[[a]] <- data[[a]]
        print(a)
      }
      
    }
    lipid_class_interaction <- Reduce(rbind, lipid_class_interaction)
    #deplet PC protein name in PE interaction
    lipid_ids <- All_node %>% filter(CHEBI %in%  lipid_gene_path$CHEBI_ID) %>% .$displayName
    lipid_class_interaction <- lipid_class_interaction %>% 
      mutate(samename=ifelse(first %in% lipid_ids & str_length(`UNIPROT_CHEBI.x`)>5, 'yes','no')) %>% 
      mutate(samename=ifelse(second %in% lipid_ids & str_length(`UNIPROT_CHEBI.y`)>5, 'yes',samename)) %>% 
      filter(samename=='no')
    return(lipid_class_interaction)
  } #add_member_complex
  
  #### Function: k.shortest.paths ####
  k.shortest.paths <- function(graph, from, to, k, max_path_length){
    
    # first shortest path
    k0 <- get.shortest.paths(graph,from,to, output='both')
    
    # number of currently found shortest paths
    kk <- 1
    
    # list of alternatives
    variants <- list()
    
    # shortest variants
    shortest.variants <- list(list(g=graph, path=k0$epath, vert=k0$vpath, dist=shortest.paths(graph,from,to)))
    
    # until k shortest paths are found
    while(kk<k){
      # take last found shortest path
      last.variant <- shortest.variants[[length(shortest.variants)]]              
      
      # calculate all alternatives
      variants <- calculate.variants(variants, last.variant, from, to)
      
      if(length(variants)==0){
        break()
      }
      # find shortest alternative
      sp <- select.shortest.path(variants)
      if(variants[[sp]]$variants$dist>max_path_length){
        break()
      }
      # add to list, increase kk, remove shortest path from list of alternatives
      shortest.variants[[length(shortest.variants)+1]] <- list(g=variants[[sp]]$g, path=variants[[sp]]$variants$path, vert=variants[[sp]]$variants$vert, dist=variants[[sp]]$variants$dist)
      kk <- kk+1
      x <- unlist( lapply( variants, function(x){x$variants$dist} ) )
      variants <- variants[-which(x == min(x))]
    }
    
    return(shortest.variants)
  } #k.shortest.paths
  
  
  #### calculate.variants ####
  # found all alternative routes
  calculate.variants <- function(variants, variant, from, to){
    # take graph from current path
    g <- variant$g
    
    # iterate through edges, removing one each iterations
    for (j in unlist(variant$path)){
      newgraph <- delete.edges(g, j) # remove adge
      sp <- suppressWarnings(get.shortest.paths(newgraph,from,to, output='both')) # calculate shortest path
      spd <- sp %>% .[[1]] %>% .[[1]] %>% length() # calculate length
      
      if ((sp %>% .[[1]] %>% .[[1]] %>% length())!=0){ # the the path is found
        if (!contains.path(variants, sp$vpath)) # add to list, unless it already contains the same path
        {
          variants[[length(variants)+1]] <- list(g=newgraph, variants=list(path=sp$epath, vert=sp$vpath, dist=spd-1))
        }
      }
    }
    return(variants)
  } #calculate.variants
  
  #### contains.path ####
  # does a list contain this path?
  contains.path <- function(variants, variant){
    return( any( unlist( lapply( variants, function(x){ identical(x$variant$vert,variant) } ) ) ) )
  } #contains.path
  
  #### select.shortest.path ####
  # which path from the list is the shortest?
  select.shortest.path <- function(variants){
    return( which.min( unlist( lapply( variants, function(x){x$variants$dist} ) ) ) )
  } #select.shortest.path
  
  
  
  All_CHEBI <- All_interaction[c(6,8)] %>% unlist() %>% unique()
  
  All_CHEBI_name <- All_node %>% filter(CHEBI!='') %>% .$displayName
  
  
  CHEBI_list <- lipid_gene_path %>% filter(key_name%in%lipid_class) %>% 
    .$CHEBI_ID %>% na.omit() %>% unique()
  
  #edge <- edge %>% filter((!from%in%wrong_path) & (!to %in%  wrong_path))
  CL <- CHEBI_list[CHEBI_list %in% All_CHEBI]
  print(str_c('Find ', as.character(length(CL)), ' ID in pathway'))
  
  CHEBI_list <- All_node %>% 
    filter(CHEBI%in%CHEBI_list) %>% 
    .$displayName %>% unique()
  
  
  if(length(CHEBI_list)<2){
    print('Less than 2 lipids found in pathway')
    k <- NULL
    All_path_record <- NULL
    return(list(k, All_path_record))
  }
  
  
  dup <- lipid_gene_path[c(1,6)] %>% filter(key_name%in%lipid_class) %>% 
    filter(!is.na(CHEBI_ID)) %>% 
    mutate(CHEBI_ID=as.character(CHEBI_ID)) %>% 
    unique() %>% left_join(All_node[c(4,9)], by=c('CHEBI_ID'='CHEBI')) %>% 
    unique()
  
  
  lipid_pair <- combn(CHEBI_list,2)
  dup_record <- c()
  d=1
  for(a in 1:ncol(lipid_pair)){
    dup_key <- dup %>% filter(displayName%in%lipid_pair[,a]) %>% 
      .$key_name
    if(length(unique(dup_key))==1){
      dup_record[d] <- a
      d <- d+1
    }
  }
  if(length(dup_record)!=0){
    lipid_pair <- lipid_pair[,-dup_record] %>% as.matrix()
  }
  print('Calculate all combinations done')
  
  
  All_pair_path <- vector(mode = 'list', length = ncol(lipid_pair))
  All_pair_path_record <- vector(mode = 'list', length = ncol(lipid_pair))
  
  for (a in 1:ncol(lipid_pair)) {
    two_lipid <- lipid_pair[,a]
    
    find_path1 <- graph_from_data_frame(edge, directed = T, vertices = node) %>% 
      k.shortest.paths(from = two_lipid[1], to = two_lipid[2], k = path, max_path_length=max_path_length) %>% 
      purrr::map(function(x){x[[3]] %>% .[[1]]})
    
    find_path2 <- graph_from_data_frame(edge, directed = T, vertices = node) %>% 
      k.shortest.paths(from = two_lipid[2], to = two_lipid[1], k = path, max_path_length=max_path_length) %>% 
      purrr::map(function(x){x[[3]] %>% .[[1]]})
    
    path_node <- list()
    
    if(length(find_path1[[1]])!=0 && 
       length(as_ids(find_path1[[1]]))<=max_path_length){
      for (b in 1:length(find_path1)) {
        cpdpath <- as_ids(find_path1[[b]])
        path_node[[b]] <- cpdpath
        names(path_node)[b] <- str_c(cpdpath[1],'_',cpdpath[length(cpdpath)],'_',as.character(b))
      }
    }
    path_length <- length(path_node)
    if(length(find_path2[[1]])!=0 && 
       length(as_ids(find_path2[[1]]))<=max_path_length){
      for (b in 1:length(find_path2)) {
        cpdpath <- as_ids(find_path2[[b]])
        path_node[[b+path_length]] <- cpdpath
        names(path_node)[b+path_length] <- str_c(cpdpath[1],'_',cpdpath[length(cpdpath)],'_',as.character(b))
      }
    }
    All_pair_path_record[[a]] <- path_node
    All_pair_path[[a]] <- unlist(path_node)
    
  }
  print('Find path done')
  
  if(length(unlist(All_pair_path))==0){
    print('No path found')
    k <- NULL
    All_path_record <- NULL
    return(list(reactome_path_table = k, All_path_record))
  }
  
  path_reaction <- All_node %>% filter(displayName %in% unlist(All_pair_path)) %>% 
    filter(BIOPAX_TYPE=='BiochemicalReaction') %>% .$displayName
  
  k <- All_interaction %>% filter(first %in% path_reaction | second %in% path_reaction) %>% 
    mutate(samename=ifelse(first %in% All_CHEBI_name & `BIOPAX_TYPE.x`!='SmallMolecule', 'yes','no')) %>% 
    mutate(samename=ifelse(second %in% All_CHEBI_name & `BIOPAX_TYPE.y`!='SmallMolecule', 'yes',samename)) %>% 
    filter(samename=='no')
  
  k <- add_member_complex(list(k)) %>% 
    mutate(interaction=ifelse(interaction%in%c('ACTIVATION', 'INHIBITION'), 'control',interaction))
  
  
  print('Add complex done')
  
  
  path_exist <- purrr::map_dbl(All_pair_path_record,  length)
  num <- 1
  if(sum(path_exist)==0){
    All_path_record <- NULL
  }else{
    All_pair_path_record <- All_pair_path_record[path_exist!=0]
    All_path_record <- vector(mode = 'list', length = length(All_pair_path_record))
    
    for(a in 1:length(All_pair_path_record)){
      for(b in 1:length(All_pair_path_record[[a]])){
        
        All_path_record[[num]] <- data.frame(path=names(All_pair_path_record[[a]])[b],
                                             path_node_seq=1:length(All_pair_path_record[[a]][[b]]),
                                             path_node=All_pair_path_record[[a]][[b]])
        num <- num+1
      }
    }
    All_path_record <- Reduce(rbind, All_path_record)
    All_path_record <- All_path_record %>% mutate(path_rank=str_extract(path, '\\d+$')) %>% 
      mutate(path=str_sub(path, end=-3)) %>% 
      dplyr::select(path,path_rank,everything())
    All_path_record <- All_path_record %>% left_join(unique(All_node[c(1,4,8,9)]), by=c('path_node'='displayName')) %>% 
      mutate(samename=ifelse(path_node %in% All_CHEBI_name & `BIOPAX_TYPE`!='SmallMolecule', 'yes','no')) %>% 
      filter(samename=='no') %>% dplyr::select(-samename)
    
  }
  return(list(reactome_path_table = k, 
              All_path_record))
  
}

