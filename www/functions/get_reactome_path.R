####################################
# author: pcshen
# name: get_reactome_path.R
# created date: 
# revise date: 2026/03/24
# description: Identifies and retrieves k-shortest reaction paths between lipid class pairs
#              in the Reactome pathway network using igraph-based graph traversal.
#              Lipid classes are first mapped to their ChEBI IDs and Reactome display names,
#              then all pairwise combinations are evaluated for bidirectional shortest paths
#              (up to k paths per pair, constrained by max_path_length).
#              Paths passing through BiochemicalReaction nodes are expanded to include
#              associated Protein interactions (via Uniprot) and Complex member relationships.
#              Nodes sharing display names with lipid entities but belonging to non-SmallMolecule
#              types are filtered out to avoid ambiguous cross-class contamination.
# input: lipid_class       (character vector; lipid class names to query),
#        path              (integer; maximum number of k-shortest paths per lipid pair),
#        max_path_length   (integer; maximum allowed path length in number of nodes),
#        All_interaction   (data frame; full Reactome interaction table with BIOPAX_TYPE and ChEBI columns),
#        All_node          (data frame; Reactome node table with displayName, CHEBI, and BIOPAX_TYPE columns),
#        complex           (data frame; complex membership table with participants and entity reference columns),
#        edge              (data frame; directed edge table with from/to columns for igraph construction),
#        node              (data frame; node attribute table for igraph construction),
#        node_id_for_complex (data frame; maps entityReference.xref to displayName for complex members),
#        Uniprot           (data frame; Uniprot-to-Reactome mapping with protein and reaction ID columns),
#        lipid_gene_path   (data frame; lipid class-to-ChEBI ID mapping table with key_name and CHEBI_ID columns)
# output: list(reactome_path_table, All_path_record)
#         - reactome_path_table : data frame of all interactions (edges) involved in the identified paths,
#                                 including expanded Protein and Complex relationships
#         - All_path_record     : data frame recording each path's node sequence, rank, and BIOPAX annotation;
#                                 NULL if no valid paths are found
####################################

####################################
# Inner SubFunction descriptions:
# add_member_complex   : For each interaction set, retrieves BiochemicalReaction nodes and appends
#                        controlling Protein interactions (from Uniprot) and Complex member expansions
#                        (via complex_transform). Filters out lipid display name ambiguities using
#                        CHEBI-based identity checks.
# k.shortest.paths     : Implements Yen's k-shortest paths algorithm on a directed igraph object.
#                        Iteratively removes edges from previously found shortest paths to discover
#                        alternative routes, stopping when k paths are found or max_path_length is exceeded.
# calculate.variants   : Enumerates alternative paths by removing one edge at a time from the current
#                        shortest path graph and recording newly discovered shortest paths not already in the list.
# contains.path        : Checks whether a given vertex path already exists in the current list of variants,
#                        used to avoid duplicate path recording in calculate.variants.
# select.shortest.path : Returns the index of the shortest path (by node count) from the current
#                        list of variant candidates.
####################################
get_path <- function(lipid_class, path, max_path_length, 
                     All_interaction, All_node, complex, edge, node,
                     node_id_for_complex, Uniprot, lipid_gene_path){
  #### Function: add_member_complex ####
  add_member_complex <- function(data){
    complex_transform <- function(complex){
      complex_componunt <- stringr::str_split(complex$participants, '\\|')
      new_complex <- vector(mode='list', length=length(unlist(complex_componunt)))
      m <- 1
      for (p in 1:nrow(complex)) {
        for (q in 1:length(complex_componunt[[p]])) {
          new_complex[[m]] <- complex[p,] %>% dplyr::mutate(second=complex_componunt[[p]][q])
          m <- m+1
        }
      }
      new_complex <- Reduce(rbind, new_complex)
      return(new_complex)
    }
    lipid_class_interaction <- vector(mode='list', length=length(data))
    
    for (a in 1:length(data)) {
      if (nrow(data[[a]])>0) {
        k <- c((data[[a]] %>% dplyr::filter(`BIOPAX_TYPE.x`=='BiochemicalReaction') %>% .$first), 
               (data[[a]] %>% dplyr::filter(`BIOPAX_TYPE.y`=='BiochemicalReaction') %>% .$second)) %>% unique()
        
        k <- rbind(data[[a]], Uniprot %>% 
                     dplyr::mutate(first=purrr::map_chr(strsplit(V3, '\\['), function(x){x[1]})) %>% 
                     dplyr::mutate(first=stringr::str_sub(first, end=-2)) %>% 
                     dplyr::select(1,3,6,9,10) %>% 
                     dplyr::filter(V6 %in% k) %>% 
                     dplyr::mutate(second=V6, interaction='control', name=stringr::str_c(first, interaction, second)) %>% 
                     dplyr::mutate(BIOPAX_TYPE.x='Protein', UNIPROT_CHEBI.x=V1) %>% 
                     dplyr::mutate(BIOPAX_TYPE.y='BiochemicalReaction', UNIPROT_CHEBI.y='') %>% 
                     dplyr::mutate(pathway='', samename='no') %>% 
                     dplyr::select(first, second, interaction, name, BIOPAX_TYPE.x, UNIPROT_CHEBI.x,
                                   BIOPAX_TYPE.y, UNIPROT_CHEBI.y, pathway,  samename))
        k <- k[!duplicated(k[c(1,2)]),]
        
        k1 <- c((k %>% dplyr::filter(`BIOPAX_TYPE.x`=='Complex') %>% .$first),
                (k %>% dplyr::filter(`BIOPAX_TYPE.y`=='Complex') %>% .$second)) %>% unique()
        complex2 <- complex[!duplicated(complex[6]),] %>% dplyr::filter(first%in%k1)
        if(nrow(complex2)>0){
          lipid_class_interaction[[a]] <- complex_transform(complex2) %>% 
            dplyr::select(6,7) %>% 
            dplyr::mutate(interaction='contains') %>% 
            dplyr::mutate(BIOPAX_TYPE='Complex', UNIPROT_CHEBI='') %>% 
            dplyr::mutate(pathway='', samename='no') %>%
            merge(node_id_for_complex, by.x='second', by.y='entityReference.xref') %>% 
            dplyr::mutate(second=displayName) %>% 
            dplyr::mutate(name=stringr::str_c(first, interaction, second)) %>% 
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
    lipid_ids <- All_node %>% dplyr::filter(CHEBI %in%  lipid_gene_path$CHEBI_ID) %>% .$displayName
    lipid_class_interaction <- lipid_class_interaction %>% 
      dplyr::mutate(samename=ifelse(first %in% lipid_ids & stringr::str_length(`UNIPROT_CHEBI.x`)>5, 'yes','no')) %>% 
      dplyr::mutate(samename=ifelse(second %in% lipid_ids & stringr::str_length(`UNIPROT_CHEBI.y`)>5, 'yes',samename)) %>% 
      dplyr::filter(samename=='no')
    return(lipid_class_interaction)
  } #add_member_complex
  
  #### Function: k.shortest.paths ####
  k.shortest.paths <- function(graph, from, to, k, max_path_length){
    # first shortest path
    k0 <- igraph::get.shortest.paths(graph,from,to, output='both')
    # number of currently found shortest paths
    kk <- 1
    # list of alternatives
    variants <- list()
    # shortest variants
    shortest.variants <- list(list(g=graph, path=k0$epath, vert=k0$vpath, dist=igraph::shortest.paths(graph,from,to)))
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
      newgraph <- igraph::delete_edges(g, j) # remove adge
      sp <- suppressWarnings(igraph::get.shortest.paths(newgraph,from,to, output='both')) # calculate shortest path
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
  
  All_CHEBI_name <- All_node %>% dplyr::filter(CHEBI!='') %>% .$displayName
  
  
  CHEBI_list <- lipid_gene_path %>% dplyr::filter(key_name%in%lipid_class) %>% 
    .$CHEBI_ID %>% na.omit() %>% unique()
  
  #edge <- edge %>% filter((!from%in%wrong_path) & (!to %in%  wrong_path))
  CL <- CHEBI_list[CHEBI_list %in% All_CHEBI]
  print(stringr::str_c('Find ', as.character(length(CL)), ' ID in pathway'))
  
  CHEBI_list <- All_node %>% 
    dplyr::filter(CHEBI%in%CHEBI_list) %>% 
    .$displayName %>% unique()
  if(length(CHEBI_list)<2){
    print('Less than 2 lipids found in pathway')
    k <- NULL
    All_path_record <- NULL
    return(list(k, All_path_record))
  }
  
  
  dup <- lipid_gene_path[c(1,6)] %>% 
    dplyr::filter(key_name%in%lipid_class) %>% 
    dplyr::filter(!is.na(CHEBI_ID)) %>% 
    dplyr::mutate(CHEBI_ID=as.character(CHEBI_ID)) %>% 
    unique() %>% dplyr::left_join(All_node[c(4, 9)], by=c('CHEBI_ID'='CHEBI')) %>% 
    unique()
  lipid_pair <- combn(CHEBI_list,2)
  dup_record <- c()
  d=1
  for(a in 1:ncol(lipid_pair)){
    dup_key <- dup %>% 
      dplyr::filter(displayName%in%lipid_pair[,a]) %>% 
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
  
  
  All_pair_path <- vector(mode='list', length=ncol(lipid_pair))
  All_pair_path_record <- vector(mode='list', length=ncol(lipid_pair))
  
  for (a in 1:ncol(lipid_pair)) {
    two_lipid <- lipid_pair[,a]
    find_path1 <- igraph::graph_from_data_frame(edge, directed=TRUE, vertices=node) %>% 
      k.shortest.paths(from=two_lipid[1], to=two_lipid[2], k=path, max_path_length=max_path_length) %>% 
      purrr::map(function(x){x[[3]] %>% .[[1]]})
    
    find_path2 <- igraph::graph_from_data_frame(edge, directed=TRUE, vertices=node) %>% 
      k.shortest.paths(from=two_lipid[2], to=two_lipid[1], k=path, max_path_length=max_path_length) %>% 
      purrr::map(function(x){x[[3]] %>% .[[1]]})
    
    path_node <- list()
    
    if(length(find_path1[[1]])!=0 && 
       length(igraph::as_ids(find_path1[[1]])) <= max_path_length){
      for (b in 1:length(find_path1)) {
        cpdpath <- igraph::as_ids(find_path1[[b]])
        path_node[[b]] <- cpdpath
        names(path_node)[b] <- stringr::str_c(cpdpath[1],'_',cpdpath[length(cpdpath)],'_',as.character(b))
      }
    }
    path_length <- length(path_node)
    if(length(find_path2[[1]]) !=0 && 
       length(igraph::as_ids(find_path2[[1]])) <= max_path_length){
      for (b in 1:length(find_path2)) {
        cpdpath <- igraph::as_ids(find_path2[[b]])
        path_node[[b+path_length]] <- cpdpath
        names(path_node)[b+path_length] <- stringr::str_c(cpdpath[1],'_',cpdpath[length(cpdpath)],'_',as.character(b))
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
    return(list(reactome_path_table=k, All_path_record))
  }
  
  path_reaction <- All_node %>% 
    dplyr::filter(displayName %in% unlist(All_pair_path)) %>% 
    dplyr::filter(BIOPAX_TYPE=='BiochemicalReaction') %>% .$displayName
  
  k <- All_interaction %>% 
    dplyr::filter(first %in% path_reaction | second %in% path_reaction) %>% 
    dplyr::mutate(samename=ifelse(first %in% All_CHEBI_name & `BIOPAX_TYPE.x`!='SmallMolecule', 'yes', 'no')) %>% 
    dplyr::mutate(samename=ifelse(second %in% All_CHEBI_name & `BIOPAX_TYPE.y`!='SmallMolecule', 'yes', samename)) %>% 
    dplyr::filter(samename=='no')
  
  k <- add_member_complex(list(k)) %>% 
    dplyr::mutate(interaction=ifelse(interaction%in%c('ACTIVATION', 'INHIBITION'), 'control',interaction))
  print('Add complex done')
  path_exist <- purrr::map_dbl(All_pair_path_record,  length)
  num <- 1
  if(sum(path_exist)==0){
    All_path_record <- NULL
  }else{
    All_pair_path_record <- All_pair_path_record[path_exist!=0]
    All_path_record <- vector(mode='list', length=length(All_pair_path_record))
    
    for(a in 1:length(All_pair_path_record)){
      for(b in 1:length(All_pair_path_record[[a]])){
        
        All_path_record[[num]] <- data.frame(path=names(All_pair_path_record[[a]])[b],
                                             path_node_seq=1:length(All_pair_path_record[[a]][[b]]),
                                             path_node=All_pair_path_record[[a]][[b]])
        num <- num+1
      }
    }
    All_path_record <- Reduce(rbind, All_path_record)
    All_path_record <- All_path_record %>% 
      dplyr::mutate(path_rank=stringr::str_extract(path, '\\d+$')) %>% 
      dplyr::mutate(path=stringr::str_sub(path, end=-3)) %>% 
      dplyr::select(path,path_rank, dplyr::everything())
    All_path_record <- All_path_record %>% 
      dplyr::left_join(unique(All_node[c(1,4,8,9)]), by=c('path_node'='displayName')) %>% 
      dplyr::mutate(samename=ifelse(path_node %in% All_CHEBI_name & `BIOPAX_TYPE`!='SmallMolecule', 'yes','no')) %>% 
      dplyr::filter(samename=='no') %>% 
      dplyr::select(-samename)
  }
  return(list(reactome_path_table=k, 
              All_path_record))
}