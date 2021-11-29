

PCA_variable <- function(pca,top_n_feature){
  require(tidyverse)
  require(stringr)
  require(factoextra)
  
  eig.val <- get_eigenvalue(pca)    
  
  pca_variable <- fviz_pca_var(pca, col.var="contrib",select.var= list(contrib = top_n_feature),
                               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                               repel = TRUE, title='Variables - PCA',
                               xlab=str_c("PC1 (",as.character(round(eig.val[1,2],1))  ,'%)'), 
                               ylab=str_c("PC2 (",as.character(round(eig.val[2,2],1))  ,'%)'))
  
  pca_variable$data <- arrange(pca_variable$data,contrib)
  pca_variable_ggplotly <- ggplotly(pca_variable)
  for (i in 1:top_n_feature) {
    pca_variable_ggplotly <- pca_variable_ggplotly %>%
      add_annotations(data = pca_variable$data[i,],x = ~x, y = ~y,text="",
                      arrowcolor = pca_variable_ggplotly$x$data[[i+1]]$line$color,
                      showarrow = TRUE,
                      axref='x', ayref='y',
                      ax = 0,
                      ay = 0)
    pca_variable_ggplotly$x$data[[i+1]]$text <- paste("X :",round(pca_variable$data$x[i],3),
                                                      "\nY :",round(pca_variable$data$y[i],3),
                                                      "\nName :",pca_variable$data$name[i]) 
  }
  pca_variable_ggplotly$x$data[[top_n_feature+2]]$hoverinfo <- "none"
  pca_variable <-  pca_variable_ggplotly 
  return(pca_variable)
} #function PCA_variable()





