

PCA_contrib <- function(pca,n_PC,top_n_feature){
  require(tidyverse)
  require(stringr)
  require(factoextra)
  
  feature_contrib <- fviz_contrib(pca, choice = "var", addlabels=T,axes = n_PC, top = top_n_feature,
                                  title=str_c('Contribution of Top',as.character(top_n_feature),
                                              ' features to PC-', str_c(as.character(n_PC), collapse = ',')))
  feature_contrib <- ggplotly(feature_contrib)
  feature_contrib$x$data[[1]]$text <- paste("Feature :",feature_contrib$x$layout$xaxis$categoryarray,
                                            "\nContributions :",round(feature_contrib$x$data[[1]]$y,3),"%")
  feature_contrib$x$data[[2]]$text <- paste("yintercept :",round(feature_contrib$x$data[[2]]$y[1],3))
  return(feature_contrib)
}