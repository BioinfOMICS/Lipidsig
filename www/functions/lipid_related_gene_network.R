####################################
# author: pcshen
# name: lipid_related_gene_network.R
# created date: 
# revise date: 2026/03/24
# description: Constructs an interactive gene-term similarity network for lipid-related enrichment
#              results using visNetwork. Up to a specified number of top significant terms are
#              selected, and all pairwise term combinations are evaluated for shared gene overlap.
#              Term-to-term similarity is calculated as the proportion of shared genes relative to
#              the smaller gene set (overlap coefficient), and edges are retained only when
#              similarity meets or exceeds the specified cutoff.
#              Nodes are sized by gene count and colored by -log10(p-value) along a gradient
#              from light blue (#F0F8FF) to royal blue (#4169E1). Edge width reflects similarity
#              strength. A legend encoding both -log10(p-value) and gene count scales is included.
# input: sig_term         (data frame; enrichment result table with columns: term, gene_name, nGene,
#                          pvalue, m.log.p; rows pre-sorted by significance),
#        node             (integer; maximum number of top terms to include in the network; default 30),
#        similarity_cutoff (numeric; minimum overlap coefficient to retain an edge; default 0.5)
# output: list(network, node_tab, edge_tab)
#         - network  : visNetwork interactive HTML widget with circular layout and legend
#         - node_tab : data frame of nodes with id, label, size, color, and tooltip columns
#         - edge_tab : data frame of edges with from, to, similarity, width, and tooltip columns
####################################
lipid_related_gene_network <- function(sig_term, node=30, similarity_cutoff=0.5){
  if(nrow(sig_term) < node){
    sig_tab <- sig_term
  }else{
    sig_tab <- sig_term[1:node,]
  }
  
  network <- combn(sig_tab$term, 2) %>%
    t() %>% 
    as.data.frame(stringsAsFactors=FALSE) %>% 
    dplyr::left_join(sig_tab[, c('term', 'gene_name')], by=c('V1'='term')) %>% 
    dplyr::left_join(sig_tab[, c('term', 'gene_name')], by=c('V2'='term'))
  colnames(network) <- c('from', 'to', 'gene1', 'gene2')
  network$nIntersect <- NA
  network$similarity <- NA
  for(i in 1:nrow(network)){
    GENE1 <- unlist(stringr::str_split(network$gene1[i], ','))
    GENE2 <- unlist(stringr::str_split(network$gene2[i], ','))
    co_gene <- unique(intersect(GENE1, GENE2))
    co_min <- min(c(length(GENE1),length(GENE2)))
    network$nIntersect[i] <- length(co_gene)
    network$similarity[i] <- round(length(co_gene)/co_min, digits=2)
  }
  
  network2 <- network %>% dplyr::filter(similarity >= similarity_cutoff)
  
  nodes <- data.frame(id=sig_tab$term,
                      label=sig_tab$term,
                      nGene=sig_tab$nGene,
                      size=scales::rescale(sig_tab$nGene, to=c(10, 40)),
                      shape='dot',
                      font.size=(node+10)/2,
                      title=paste0("<p><b>", sig_tab$term,"</b><br>number of gene=", sig_tab$nGene, "<br>p-value=", sig_tab$pvalue,"</p>"), 
                      color=sig_tab$m.log.p,
                      m.log.p=sig_tab$m.log.p,
                      shadow=FALSE,
                      stringsAsFactors=FALSE)
  nodes <- dplyr::arrange(nodes,color)
  n_color <- length(unique(nodes$color))
  unique_color <- unique(nodes$color)
  for (i in 1:nrow(nodes)) {
    for (a in 1:n_color) {
      if (nodes$color[i]==unique_color[a]){
        nodes$color[i] <- grDevices::colorRampPalette(c("#F0F8FF" , "#4169E1"))(n_color)[a]
      }
    }
  }
  edges <- network2 %>%
    dplyr::select(from, to, similarity) %>% 
    dplyr::mutate(
      width=scales::rescale(similarity, to=c(1, 8)),
      title=paste0("Similarity=", network2$similarity), 
      dashes=FALSE, 
      smooth=FALSE, 
      shadow=FALSE)
  
  ledges_n <- c(1, 1+round(nrow(nodes)/4), 1+round(2*nrow(nodes)/4), 1+round(3*nrow(nodes)/4),nrow(nodes))
  ledges <- data.frame(color=c("white",nodes$color[ledges_n],
                                 "white",rep("black",5)), 
                       label=c("-log10(p)",
                                 as.character(nodes$m.log.p[ledges_n]),
                                 "Gene\ncount",
                                 as.character(dplyr::arrange(nodes, nGene)$nGene[ledges_n])),
                       shape =c("box",rep("dot",5),"box",rep("dot",5)),
                       size=c(30,rep(20,5),30,
                              scales::rescale(dplyr::arrange(nodes, nGene)$nGene, to=c(10, 40))[ledges_n]),
                       font.size =c(35,rep(30,5),35,rep(25,5)))
  
  p <-visNetwork::visNetwork(nodes, edges, width="100%",height="600px") %>% 
    visNetwork::visLayout(randomSeed=12) %>%
    visNetwork::visOptions(highlightNearest=list(enabled=TRUE, degree=1, hover=TRUE))%>%
    visNetwork::visEdges(color=list(color="#DDDDDD",highlight="#C62F4B")) %>%
    visNetwork::visPhysics(solver='barnesHut', 
               stabilization=TRUE, 
               barnesHut=list(gravitationalConstant=-1000)) %>%
    visNetwork::visIgraphLayout(layout="layout_in_circle") %>%
    visNetwork::visInteraction(navigationButtons=TRUE) %>% 
    visNetwork::visEvents(dragEnd ="function () {this.setOptions( { physics: false } );}") %>%
    visNetwork::visLegend(useGroups=FALSE,addNodes=ledges,width=0.15) 
  return(list(network=p, node_tab=nodes, edge_tab=edges))
  
} #function


