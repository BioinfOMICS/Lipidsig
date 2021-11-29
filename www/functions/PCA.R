

PCA <- function(exp_transform_table, group_info = NULL, sig_feature = NULL,
                scaling=T, centering=T, cluster_method='kmeans', 
                group_num = NULL, var1 = NULL, var2 = NULL,
                insert_ref_group=NULL,ref_group=NULL){
  require(tidyverse)
  require(stringr)
  require(cluster)
  require(dbscan)
  require(factoextra)
  require(plotly)
  if(!is.null(insert_ref_group) & !is.null(ref_group) &!is.null(group_info)){
    exp_raw_name <- ref_group[-which(insert_ref_group%in%ref_group)]
    group_info$group[which(group_info$group=='ctrl')] <-  insert_ref_group
    group_info$group[which(group_info$group=='exp')] <-  exp_raw_name
  }
  if(!is.null(group_num)){
    if( group_num<1 ||  group_num>10){
      #stop('Group number must be between 1 and 10')
      return(NULL)
    }
  }
  
  if(!is.null(sig_feature)){
    exp_transform_table <- exp_transform_table %>% 
      filter(eval(parse(text = colnames(exp_transform_table)[1]))%in%sig_feature)
  }
  
  num <- apply(exp_transform_table[-1], 1, FUN = function(x){length(unique(x))})
  exp_transform_table <- exp_transform_table[(num!=1),]
  exp_transform_table <- exp_transform_table[!is.infinite(rowSums(exp_transform_table[-1], na.rm = T)),]
  
  pca_table <- exp_transform_table[-1] %>% t() %>% as.data.frame()
  
  colnames(pca_table) <- exp_transform_table[[1]]
  
  
  pca_table <- na.omit(pca_table)
  
  if(nrow(pca_table)<6){
    #stop('Samples must be larger than 6')
    return(NULL)
  }
  if(ncol(pca_table)<2){
    #stop('Samples must be larger than 6')
    return(NULL)
  }
  pca <- prcomp(pca_table, scale = scaling, center = centering)
  
  #output pca data
  pca_rotated_data <- as.data.frame(pca$x) %>% mutate(sample_name=colnames(exp_transform_table)[-1])
  
  
  #grouping
  if(cluster_method=='kmeans'){
    cluster_group <- kmeans(pca$x, centers = group_num)$cluster
  }else if(cluster_method=='kmedoids'){
    cluster_group <- pam(pca$x, k = group_num, metric = var1)$cluster #euclidean manhattan
  }else if(cluster_method=='hclustering'){
    if(var1 %in% c('pearson','spearman','kendall')){
      dist.fun=function(x){
        x=t(x)
        cor.mat=cor(x,method=var1,use = 'complete.obs')
        cor.mat=(1-cor.mat)
        cor.dist=as.dist(cor.mat)
        return(cor.dist)
      }
    }else{
      dist.fun=function(x) dist(x, method=var1)
    }
    cluster_group <- hclust(dist.fun(pca$x), method = var2)
    cluster_group <- cutree(cluster_group, k=group_num) 
  }else if(cluster_method=='dbscan'){
    cluster_group <- dbscan(pca$x, eps = var1, minPts = var2)$cluster
    cluster_group <- ifelse(cluster_group>9,9,cluster_group) %>% as.character()
    cluster_group <- ifelse(cluster_group=='0','noise',cluster_group)
  }else if(cluster_method=='group_info'){
    
    group_order <- map_dbl(rownames(pca_table), function(x){which(x==group_info$sample_name)})
    
    cluster_group <- group_info[group_order,] %>% .$group
    group_num <- length(unique(cluster_group))
  }
  
  
  pca_rotated_data <- pca_rotated_data %>% mutate(group=cluster_group) %>% 
    dplyr::select(sample_name, group, everything())
  if(!is.null(insert_ref_group) & !is.null(ref_group)){
    exp_raw_name <- ref_group[-which(insert_ref_group%in%ref_group)]
    pca_rotated_data$group[which(pca_rotated_data$group=='ctrl')] <-  insert_ref_group
    pca_rotated_data$group[which(pca_rotated_data$group=='exp')] <-  exp_raw_name
  }
  if(T){
    
    
    #pca contribution table
    pca_contrib_table <- get_pca_var(pca)$contrib %>% as.data.frame() %>% 
      mutate(feature=rownames(.)) %>% dplyr::select(feature, everything())
    
    PC <- ncol(pca_contrib_table)-1
    colnames(pca_contrib_table)[2:ncol(pca_contrib_table)] <- str_c('PC',as.character(1:PC))
    
    #pca plot*5
    color <- c("#00AFBB", "#E7B800", "#FC4E07","#42B540FF","#BB3099","#EE0099","#0000AC","#868686FF",'#00468BFF','black')
    
    if(cluster_method %in% c('dbscan')){
      pca_biplot <- fviz_pca_ind(pca,label = "none", habillage = cluster_group, 
                                 palette = color[1:length(unique(cluster_group))],
                                 addEllipses = F,invisible="quali",
                                 title='PCA')
      biplot <- ggplotly(pca_biplot)
      for (i in 1:length(biplot$x$data)){
        if (!is.null(biplot$x$data[[i]]$name)){
          biplot$x$data[[i]]$name =  gsub("\\(","",str_split(biplot$x$data[[i]]$name,",")[[1]][1])
        }
        if(i<=length(unique(cluster_group))){
          biplot$x$data[[i]]$text<-paste("x :",round(biplot$x$data[[i]]$x,3),
                                         "\ny :",round(biplot$x$data[[i]]$y,3),
                                         "\nGroups :",biplot$x$data[[i]]$name,
                                         "\nSample name :",pca_biplot$data[which(pca_biplot$data$Groups==biplot$x$data[[i]]$name),]$name)
        }
      }
      pca_biplot <- biplot
      
      
    }else{
      pca_biplot <- fviz_pca_ind(pca,label = "none", habillage = cluster_group, 
                                 palette = color[1:group_num],
                                 addEllipses = T,invisible="quali",
                                 title='PCA')
      biplot <- ggplotly(pca_biplot)
      for (i in 1:length(biplot$x$data)){
        if (!is.null(biplot$x$data[[i]]$name)){
          biplot$x$data[[i]]$name =  gsub("\\(","",str_split(biplot$x$data[[i]]$name,",")[[1]][1])
        }
        if (i<=group_num){
          biplot$x$data[[i]]$text<-paste("Dim1 :",round(biplot$x$data[[i]]$x,3),
                                         "\nDim2 :",round(biplot$x$data[[i]]$y,3),
                                         "\nGroups :",biplot$x$data[[i]]$name,
                                         "\nSample name :",pca_biplot$data[which(pca_biplot$data$Groups==biplot$x$data[[i]]$name),]$name)
        }else if(i>=group_num+1 & i <= 2*group_num){
          biplot$x$data[[i]]$text<-paste("\nGroups :",biplot$x$data[[i]]$name)
        }
      }
      for (i in 1:(2*group_num)) {
        rev_n <- rev(1:(2*group_num))
        if(i==1){rev_list<-list(biplot$x$data[[rev_n[i]]])}
        else{rev_list<-c(rev_list,list(biplot$x$data[[rev_n[i]]]))}
      }
      biplot$x$data <- rev_list
      pca_biplot <- biplot
    }
    
    pca_screeplot <- fviz_screeplot(pca, ncp=10, main="Scree plot for Top10 PCs",
                                    xlab="Principle components (PCs)", 
                                    ylab='Explained variance (%)'#,
                                    #addlabels = TRUE)
    )
    pca_screeplot <- ggplotly(pca_screeplot) %>% 
      style(traces = 1:2,hoverinfo="text",
            text=paste("Principle components :",pca_screeplot$data$dim,
                       "\nExplained variance :",round(pca_screeplot$data$eig,3),"%"))
    
  }
  return(list(pca, pca_rotated_data, pca_contrib_table,
              pca_biplot, pca_screeplot))
  
}



