

UMAP <- function(exp_transform_table, group_info = NULL, sig_feature = NULL,
                 n_neighbors=15, scale=T,metric='euclidean', cluster_method, 
                 group_num = NULL, var1 = NULL, var2 = NULL, 
                 insert_ref_group = NULL, ref_group = NULL){
  
  require(tidyverse)
  require(stringr)
  require(cluster)
  require(dbscan)
  require(uwot)
  require(factoextra)
  
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
  
  umap_table <- exp_transform_table[-1] %>% t() %>% as.data.frame()
  
  colnames(umap_table) <- exp_transform_table[[1]]
  
  
  umap_table <- na.omit(umap_table)
  if(nrow(umap_table) < n_neighbors){
    #stop('Samples must be larger than n_neighbors')
    return(NULL)
  }
  if(nrow(umap_table) < 2){
    #stop('Samples must be larger than n_neighbors')
    return(NULL)
  }
  if(ncol(umap_table)<2){
    #stop('Samples must be larger than 6')
    return(NULL)
  }
  
  umap_result <- umap(umap_table,n_neighbors=n_neighbors,
                      scale=scale, metric=metric)
  
  umap_data <- as.data.frame(umap_result) %>% mutate(sample_name=colnames(exp_transform_table)[-1])
  
  colnames(umap_data)[1:2] <- c('UMAP-1', 'UMAP-2')
  
  #grouping
  if(cluster_method=='kmeans'){
    cluster_group <- kmeans(umap_result, centers = group_num)$cluster
  }else if(cluster_method=='kmedoids'){
    cluster_group <- pam(umap_result, k = group_num, metric = var1)$cluster #euclidean manhattan
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
    cluster_group <- hclust(dist.fun(umap_result), method = var2)
    cluster_group <- cutree(cluster_group, k=group_num) 
  }else if(cluster_method=='dbscan'){
    cluster_group <- dbscan(umap_result, eps = var1, minPts = var2)$cluster
    cluster_group <- ifelse(cluster_group>9,9,cluster_group) %>% as.character()
    cluster_group <- ifelse(cluster_group=='0','noise',cluster_group)
  }else if(cluster_method=='group_info'){
    
    group_order <- map_dbl(rownames(umap_table), function(x){which(x==group_info$sample_name)})
    
    cluster_group <- group_info[group_order,] %>% .$group
    group_num <- length(unique(cluster_group))
    
  }
  
  cluster_group <- as.character(cluster_group)
  umap_data <- umap_data %>% mutate(group=cluster_group) %>% 
    dplyr::select(sample_name, group, everything())
  
  #tsne plot*5
  color <- c("#00AFBB", "#E7B800", "#FC4E07","#42B540FF","#BB3099","#EE0099","#0000AC","#868686FF",'#00468BFF','black')
  if(cluster_method %in% c('dbscan')){
    
    umap_plot <-  fviz_cluster(list(data = as.data.frame(umap_result), 
                                    cluster = cluster_group),
                               palette = color[1:length(unique(cluster_group))], 
                               geom = "point",
                               stand=F,
                               ellipse=F,
                               ggtheme = theme_bw(),
                               show.clust.cent=F,
                               xlab='UMAP-1',
                               ylab='UMAP-2',
                               main = "UMAP",
                               legend.title='Groups')
    umap_plot <- ggplotly(umap_plot)
    for (i in 1:length(umap_plot$x$data)){
      if (!is.null(umap_plot$x$data[[i]]$name)){
        umap_plot$x$data[[i]]$name =  gsub("\\(","",str_split(umap_plot$x$data[[i]]$name,",")[[1]][1])
      }
      if(i<=length(unique(cluster_group))){
        umap_plot$x$data[[i]]$text<-paste("x :",round(umap_plot$x$data[[i]]$x,3),
                                          "\ny :",round(umap_plot$x$data[[i]]$y,3),
                                          "\nGroups :",umap_plot$x$data[[i]]$name,
                                          "\nSample name :",umap_data[which(umap_data$group==umap_plot$x$data[[i]]$name),]$sample_name)
      }
    }
    
  }else{
    umap_plot <- fviz_cluster(list(data = as.data.frame(umap_result), 
                                   cluster = cluster_group),
                              palette = color[1:group_num], 
                              geom = "point",
                              stand=F,
                              ellipse=T,
                              ellipse.type = 'norm', 
                              ggtheme = theme_bw(),
                              show.clust.cent=F,
                              xlab='UMAP-1',
                              ylab='UMAP-2',
                              main = "UMAP",
                              legend.title='Groups')
    umap_plot <- ggplotly(umap_plot)
    for (i in 1:length(umap_plot$x$data)){
      if (!is.null(umap_plot$x$data[[i]]$name)){
        umap_plot$x$data[[i]]$name =  gsub("\\(","",str_split(umap_plot$x$data[[i]]$name,",")[[1]][1])
      }
      if(i<=group_num){
        umap_plot$x$data[[i]]$text<-paste("x :",round(umap_plot$x$data[[i]]$x,3),
                                          "\ny :",round(umap_plot$x$data[[i]]$y,3),
                                          "\nGroups :",umap_plot$x$data[[i]]$name,
                                          "\nSample name :",umap_data[which(umap_data$group==umap_plot$x$data[[i]]$name),]$sample_name)
      }else if(i>=group_num+1 & i <= 2*group_num){
        umap_plot$x$data[[i]]$text<-paste("Groups :",umap_plot$x$data[[i]]$name) 
      }
    }
    for (i in 1:(2*group_num)) {
      rev_n <- rev(1:(2*group_num))
      if(i==1){rev_list<-list(umap_plot$x$data[[rev_n[i]]])}
      else{rev_list<-c(rev_list,list(umap_plot$x$data[[rev_n[i]]]))}
    }
    umap_plot$x$data<- rev_list
  }
  return(list(umap_data, umap_plot))
}




