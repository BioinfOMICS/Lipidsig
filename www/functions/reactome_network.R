####################################
# author: pcshen
# name: reactome_network.R
# created date: 
# revise date: 2026/03/24
# description: Constructs an interactive Reactome pathway network visualization from a pathway
#              relationship table and a lipid-path record. Nodes represent biological entities
#              (Protein, SmallMolecule, Complex, BiochemicalReaction, PhysicalEntity) and lipid
#              classes identified from the path record, each styled with distinct shapes, colors,
#              and sizes. Edges represent directional interactions, with dashed lines indicating
#              non-directional relationships. The network is rendered using visNetwork with
#              Barnes-Hut physics, nearest-neighbor highlighting, and group/node ID selection.
# input: reactome_path_table (data frame with columns: first, second, BIOPAX_TYPE.x, BIOPAX_TYPE.y, interaction),
#        all_path_record (data frame with column: path, containing underscore-separated lipid class strings)
# output: list(network, node_tab, edge_tab)
#         - network  : visNetwork interactive HTML widget
#         - node_tab : data frame of nodes with label, shape, color, size, and group columns
#         - edge_tab : data frame of edges with from, to, dashes, arrows, smooth, and shadow columns
####################################
reactome_network <- function(reactome_path_table, all_path_record){
  lipid_class <- stringr::str_split(all_path_record$path, pattern='_') %>% unlist() %>% unique()
  node.x <- reactome_path_table %>%
    dplyr::select('id'='first', 'group'='BIOPAX_TYPE.x')
  node.y <- reactome_path_table %>% 
    dplyr::select('id'='second', 'group'='BIOPAX_TYPE.y')
  node.all <- data.table::rbindlist(l=list(node.x, node.y), use.names=TRUE, fill=TRUE) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(type=ifelse(id %in% lipid_class, 'LipidClass', group))
  #BiochemicalReaction, SmallMolecule, Protein, Complex, PhysicalEntity
  nodes <- node.all %>% 
    dplyr::mutate(label=id, 
                  shape=ifelse(group == 'Protein', 'dot', 
                               ifelse(group == 'SmallMolecule', 'diamond', 
                                      ifelse(group == 'Complex', 'triangle', 
                                             ifelse(group == 'BiochemicalReaction', 'square', 
                                                    ifelse(group == 'PhysicalEntity', 'triangleDown', 'triangleDown'))))), 
                  color=ifelse(type == 'Protein', '#66CDAA', 
                               ifelse(type == 'SmallMolecule', '#87CEFA', 
                                      ifelse(type == 'Complex', '#E6E6FA', 
                                             ifelse(type == 'BiochemicalReaction', '#FFDAB9', 
                                                    ifelse(type == 'PhysicalEntity', '#DDDDDD', 
                                                           ifelse(type == 'LipidClass', '#FFB6C1', '#DDDDDD')))))),
                  size=ifelse(type == 'Protein', 15, 
                              ifelse(type == 'SmallMolecule', 20, 
                                     ifelse(type == 'Complex', 15, 
                                            ifelse(type == 'BiochemicalReaction', 18, 
                                                   ifelse(type == 'PhysicalEntity', 15, 
                                                          ifelse(type == 'LipidClass', 30, 15)))))),
                  shadow=FALSE) %>% 
    dplyr::select(-type)
  
  edges <- reactome_path_table %>% 
    dplyr::mutate(dashes=ifelse(interaction %in% c('right', 'left'), FALSE, TRUE), 
                  arrows='to', 
                  smooth=FALSE, 
                  shadow=FALSE) %>%
    dplyr::select('from'='first', 'to'='second', dashes, arrows, smooth, shadow)
  p <- visNetwork::visNetwork(nodes, edges) %>% 
    visNetwork::visLayout(randomSeed=500) %>%
    visNetwork::visPhysics(solver='barnesHut', 
                           stabilization=TRUE, 
                           barnesHut=list(gravitationalConstant=-5000)) %>% 
    visNetwork::visInteraction(navigationButtons=TRUE) %>% 
    visNetwork::visEvents(dragEnd ="function () {this.setOptions( { physics: false } );}") %>% 
    visNetwork::visEdges(color=list(color="#DDDDDD",highlight="#C62F4B")) %>%
    visNetwork::visOptions(highlightNearest=list(enabled=TRUE, degree=1, hover=FALSE), 
                           selectedBy="group",
                           nodesIdSelection=TRUE)
  
  return(list(network=p, node_tab=nodes, edge_tab=edges))
  
}