

lipid_related_gene_network <- function(sig_term, node = 30, similarity_cutoff = 0.5){
  
  require(tidyverse)
  require(visNetwork)
  require(scales)
  
  if(nrow(sig_term) < node){
    
    sig_tab <- sig_term
    
  }else{
    
    sig_tab <- sig_term[1:node,]
    
  }
  
  network <- combn(sig_tab$term, 2) %>%
    t() %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    left_join(sig_tab[, c('term', 'gene_name')], by = c('V1' = 'term')) %>% 
    left_join(sig_tab[, c('term', 'gene_name')], by = c('V2' = 'term'))
  
  colnames(network) <- c('from', 'to', 'gene1', 'gene2')
  network$nIntersect <- NA
  network$similarity <- NA
  
  for(i in 1:nrow(network)){
    
    GENE1 <- unlist(str_split(network$gene1[i], ','))
    GENE2 <- unlist(str_split(network$gene2[i], ','))
    co_gene <- unique(intersect(GENE1, GENE2))
    #co_mean <- (length(GENE1)+length(GENE2))/2
    co_min <- min(c(length(GENE1),length(GENE2)))
    
    network$nIntersect[i] <- length(co_gene)
    network$similarity[i] <- round(length(co_gene)/co_min, digits = 2)
  }
  
  network2 <- network %>% filter(similarity >= similarity_cutoff)
  
  nodes <- data.frame(id = sig_tab$term,
                      label = sig_tab$term,
                      nGene = sig_tab$nGene,
                      size = rescale(sig_tab$nGene, to = c(10, 40)),
                      shape = 'dot',
                      font.size = (node+10)/2,
                      title = paste0("<p><b>", sig_tab$term,"</b><br>number of gene = ", sig_tab$nGene, "<br>p-value = ", sig_tab$pvalue,"</p>"), 
                      color = sig_tab$m.log.p,
                      m.log.p = sig_tab$m.log.p,
                      shadow = FALSE,
                      stringsAsFactors = F)
  nodes <- arrange(nodes,color)
  n_color <- length(unique(nodes$color))
  unique_color <- unique(nodes$color)
  for (i in 1:nrow(nodes)) {
    for (a in 1:n_color) {
      if (nodes$color[i]==unique_color[a]){nodes$color[i] <- colorRampPalette(c("#F0F8FF" , "#4169E1"))(n_color)[a]}
    }
  }
  
  edges <- network2 %>%
    dplyr::select(from, to, similarity) %>% 
    mutate(#length = 100, 
      width = rescale(similarity, to = c(1, 8)),
      title = paste0("Similarity = ", network2$similarity), 
      dashes = FALSE, 
      smooth = FALSE, 
      shadow = FALSE)
  
  ledges_n <- c(1,1+round(nrow(nodes)/4),1+round(2*nrow(nodes)/4),
                1+round(3*nrow(nodes)/4),nrow(nodes))
  ledges <- data.frame(color = c("white",nodes$color[ledges_n],
                                 "white",rep("black",5)), 
                       label = c("-log10(p)",
                                 as.character(nodes$m.log.p[ledges_n]),
                                 "Gene\ncount",
                                 as.character(arrange(nodes,nGene)$nGene[ledges_n])),
                       shape =c("box",rep("dot",5),"box",rep("dot",5)),
                       size = c(30,rep(20,5),30,
                                rescale(arrange(nodes,nGene)$nGene, to = c(10, 40))[ledges_n]),
                       font.size =c(35,rep(30,5),35,rep(25,5)))
  
  p <-visNetwork(nodes, edges, width = "100%",height = "600px") %>% 
    visLayout(randomSeed = 12) %>%
    visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T))%>%
    visEdges(color = list(color = "#DDDDDD",highlight = "#C62F4B")) %>%
    visPhysics(solver = 'barnesHut', 
               stabilization = T, 
               barnesHut = list(gravitationalConstant = -1000)
    ) %>%
    visIgraphLayout(layout = "layout_in_circle") %>%
    visInteraction(navigationButtons = TRUE) %>% 
    visEvents(dragEnd ="function () {this.setOptions( { physics: false } );}") %>%
    visLegend(useGroups = F,addNodes=ledges,width = 0.15) 
  
  
  return(list(network = p, node_tab = nodes, edge_tab = edges))
  
} #function


