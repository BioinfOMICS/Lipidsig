

reactome_network <- function(reactome_path_table, all_path_record){
  
  require(tidyverse)
  require(visNetwork)
  require(scales)
  require(data.table)
  
  lipid_class <- str_split(all_path_record$path, pattern = '_') %>% unlist() %>% unique()
    
  
  
  node.x <- reactome_path_table %>%
    dplyr::select('id' = 'first', 'group' = 'BIOPAX_TYPE.x')
  node.y <- reactome_path_table %>% 
    dplyr::select('id' = 'second', 'group' = 'BIOPAX_TYPE.y')
  
  node.all <- rbindlist(l = list(node.x, node.y), use.names = T, fill = T) %>% 
    distinct() %>% 
    mutate(type = ifelse(id %in% lipid_class, 'LipidClass', group))
  
  
  #BiochemicalReaction, SmallMolecule, Protein, Complex, PhysicalEntity
  nodes <- node.all %>% 
    mutate(label = id, 
           shape = ifelse(group == 'Protein', 'dot', 
                          ifelse(group == 'SmallMolecule', 'diamond', 
                                 ifelse(group == 'Complex', 'triangle', 
                                        ifelse(group == 'BiochemicalReaction', 'square', 
                                               ifelse(group == 'PhysicalEntity', 'triangleDown', 'triangleDown'))))), 
           color = ifelse(type == 'Protein', '#66CDAA', 
                          ifelse(type == 'SmallMolecule', '#87CEFA', 
                                 ifelse(type == 'Complex', '#E6E6FA', 
                                        ifelse(type == 'BiochemicalReaction', '#FFDAB9', 
                                               ifelse(type == 'PhysicalEntity', '#DDDDDD', 
                                                      ifelse(type == 'LipidClass', '#FFB6C1', '#DDDDDD')))))),
           size = ifelse(type == 'Protein', 15, 
                         ifelse(type == 'SmallMolecule', 20, 
                                ifelse(type == 'Complex', 15, 
                                       ifelse(type == 'BiochemicalReaction', 18, 
                                              ifelse(type == 'PhysicalEntity', 15, 
                                                     ifelse(type == 'LipidClass', 30, 15)))))),
           shadow = FALSE) %>% 
    dplyr::select(-type)
  
  edges <- reactome_path_table %>% 
    mutate(dashes = ifelse(interaction %in% c('right', 'left'), FALSE, TRUE), 
           arrows = 'to', 
           smooth = FALSE, 
           shadow = FALSE) %>%
    dplyr::select('from' = 'first', 'to' = 'second', dashes, arrows, smooth, shadow)
  
  # size_ledges_n <- c(1,1+round(nrow(nodes)/4),1+round(2*nrow(nodes)/4),
  #                    1+round(3*nrow(nodes)/4),nrow(nodes))
  # color_ledges_n <- c(1,
  #                     which(nodes$color==unique(nodes$color)[round(length(unique(nodes$color))/4)])[1],
  #                     which(nodes$color==unique(nodes$color)[round(2*length(unique(nodes$color))/4)])[1],
  #                     which(nodes$color==unique(nodes$color)[round(3*length(unique(nodes$color))/4)])[1],
  #                     which(nodes$color==unique(nodes$color)[length(unique(nodes$color))])[1])
  # ledges <- data.frame(color = c("white",nodes$color[color_ledges_n],
  #                                "white",rep("black",5)), 
  #                      label = c("Color\n-log10(p)",
  #                                as.character(nodes$m.log.p[color_ledges_n]),
  #                                "Size\nnGene",
  #                                as.character(arrange(nodes,nGene)$nGene[size_ledges_n])),
  #                      shape =c("box",rep("dot",5),"box",rep("dot",5)),
  #                      size = c(30,rep(20,5),30,
  #                               rescale(arrange(nodes,nGene)$nGene, to = c(10, 40))[size_ledges_n]),
  #                      font.size =c(35,rep(30,5),35,rep(25,5)))
  
  p <- visNetwork(nodes, edges) %>% 
    visLayout(randomSeed = 500) %>%
    visPhysics(solver = 'barnesHut', 
               stabilization = T, 
               barnesHut = list(gravitationalConstant = -5000)) %>% 
    visInteraction(navigationButtons = TRUE) %>% 
    visEvents(dragEnd ="function () {this.setOptions( { physics: false } );}") %>% 
    visEdges(color = list(color = "#DDDDDD",highlight = "#C62F4B")) %>%
    visOptions(highlightNearest = list(enabled = T, degree = 1, hover = F), 
               selectedBy = "group",
               nodesIdSelection = TRUE)
  
  return(list(network = p, node_tab = nodes, edge_tab = edges))
  
}

