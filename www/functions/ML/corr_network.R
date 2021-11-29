

corr_network <- function(node_table, edge_table){
  require(scales)
  require(visNetwork)
  node_table <- node_table %>%
    dplyr::select(feature, node_col)
  colnames(node_table)[which(colnames(node_table)=="node_col")] <- "color"
  colnames(node_table)[which(colnames(node_table)=="feature")] <- "id"
  
  node_table <- node_table %>% 
    mutate(lebel = id,
           shpae = "diamond",
           title = paste0("<p><b>",id))
  node_table <- arrange(node_table,color)
  nodes <- node_table
  n_color <- length(unique(node_table$color))
  unique_color <- unique(node_table$color)
  if(min(node_table$color)>0){
    for (i in 1:nrow(node_table)) {
      for (a in 1:n_color) {
        if (node_table$color[i]==unique_color[a]){
          node_table$color[i] <- colorRampPalette(c("white" , "red"))(n_color)[a]
        }
      }
    }  
  }else if(max(node_table$color)<0){
    for (i in 1:nrow(node_table)) {
      for (a in 1:n_color) {
        if (node_table$color[i]==unique_color[a]){
          node_table$color[i] <- colorRampPalette(c("blue" , "white"))(n_color)[a]
        }
      }
    }  
  }else{
    for (i in 1:nrow(node_table)) {
      for (a in 1:n_color) {
        if (node_table$color[i]==unique_color[a]){
          node_table$color[i] <- colorRampPalette(c("blue","white" , "red"))(n_color)[a]
        }
      }
    }  
  }
  
  
  #colnames(edge_table)[which(colnames(edge_table)=="cor_coef")] <- "width"
  edge_table <- edge_table %>%
    mutate(width = abs(cor_coef)) %>%
    dplyr::select(from, to, width, cor_coef) %>% 
    mutate(width = rescale(width, to = c(1, 5)),
           dashes = FALSE, 
           smooth = FALSE, 
           shadow = FALSE,
           color = ifelse(cor_coef > 0, '#FFDDAA', '#CCBBFF'),
           title = paste0("<p><b>",edge_table$from," vs ",edge_table$to,"</b><br>cor_coef = ",  round(edge_table$cor_coef,5),"</p>"))
  edge_table <-  edge_table[-which(edge_table$from==edge_table$to),]
  
  color_ledges_n <- c(1,
                      which(nodes$color==unique(nodes$color)[round(length(unique(nodes$color))/4)])[1],
                      which(nodes$color==unique(nodes$color)[round(2*length(unique(nodes$color))/4)])[1],
                      which(nodes$color==unique(nodes$color)[round(3*length(unique(nodes$color))/4)])[1],
                      which(nodes$color==unique(nodes$color)[length(unique(nodes$color))])[1])
  
  ledges <- data.frame(color = c("white",node_table$color[color_ledges_n]), 
                       label = c("Feature\nimportance",
                                 as.character(round(nodes$color[color_ledges_n],3))),
                       shape =c("box",rep("dot",5)),
                       size = c(30,rep(20,5)),
                       font.size =c(35,rep(30,5)))
  
  
  visNet <- visNetwork(node_table, edge_table) %>% 
    visLayout(randomSeed = 12) %>%
    visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T))%>%
    visEdges(color = list(highlight = "#C62F4B")) %>%
    visPhysics(solver = 'barnesHut', 
               stabilization = T, 
               barnesHut = list(gravitationalConstant = -1000)) %>%
    visIgraphLayout(layout = "layout_nicely") %>%
    visInteraction(navigationButtons = TRUE)  %>% 
    visEvents(dragEnd ="function () {this.setOptions( { physics: false } );}") %>%
    visLegend(useGroups = F,addNodes=ledges,width = 0.15) 
  return(visNetwork_plot = visNet)
}

