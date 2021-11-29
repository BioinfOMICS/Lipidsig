

tsne <- function(exp_transform_table, group_info = NULL, sig_feature = NULL,
                 pca=T, perplexity=5,max_iter=500, cluster_method, 
                 group_num = NULL, var1 = NULL, var2 = NULL, 
                 insert_ref_group = NULL, ref_group = NULL){
  
  require(tidyverse)
  require(stringr)
  require(cluster)
  require(dbscan)
  require(Rtsne)
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
  
  tsne_table <- exp_transform_table[-1] %>% t() %>% as.data.frame()
  
  colnames(tsne_table) <- exp_transform_table[[1]]
  
  
  tsne_table <- na.omit(tsne_table)
  if(nrow(tsne_table) <= (3*perplexity)){
    #stop('Samples must be larger than 3*perplexity')
    return(NULL)
  }
  if(nrow(tsne_table) <4){
    #stop('Samples must be larger than 3*perplexity')
    return(NULL)
  }
  if(ncol(tsne_table)<2){
    #stop('Samples must be larger than 6')
    return(NULL)
  }
  
  tsne <- Rtsne(tsne_table,check_duplicates=F , pca=pca,
                perplexity=perplexity, verbose=TRUE, 
                max_iter = max_iter,theta=0)
  
  tsne_data <- as.data.frame(tsne$Y) %>% mutate(sample_name=colnames(exp_transform_table)[-1])
  
  colnames(tsne_data)[1:2] <- c('tsne1', 'tsne2')
  
  #grouping
  if(cluster_method=='kmeans'){
    cluster_group <- kmeans(tsne$Y, centers = group_num)$cluster
  }else if(cluster_method=='kmedoids'){
    cluster_group <- pam(tsne$Y, k = group_num, metric = var1)$cluster #euclidean manhattan
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
    cluster_group <- hclust(dist.fun(tsne$Y), method = var2)
    cluster_group <- cutree(cluster_group, k=group_num) 
  }else if(cluster_method=='dbscan'){
    cluster_group <- dbscan(tsne$Y, eps = var1, minPts = var2)$cluster
    cluster_group <- ifelse(cluster_group>9,9,cluster_group) %>% as.character()
    cluster_group <- ifelse(cluster_group=='0','noise',cluster_group)
  }else if(cluster_method=='group_info'){
    
    group_order <- map_dbl(rownames(tsne_table), function(x){which(x==group_info$sample_name)})
    
    cluster_group <- group_info[group_order,] %>% .$group
    group_num <- length(unique(cluster_group))
    
  }
  
  cluster_group <- as.character(cluster_group)
  tsne_data <- tsne_data %>% mutate(group=cluster_group) %>% 
    dplyr::select(sample_name, group,everything())
  
  #tsne plot*5
  color <- c("#00AFBB", "#E7B800", "#FC4E07","#42B540FF","#BB3099","#EE0099","#0000AC","#868686FF",'#00468BFF','black')
  if(cluster_method %in% c('dbscan')){
    
    tsne_plot <- fviz_cluster(list(data = as.data.frame(tsne$Y),
                                   cluster = cluster_group),
                              palette = color[1:length(unique(cluster_group))], 
                              geom = "point",
                              stand=F,
                              ellipse=F,
                              ggtheme = theme_bw(),
                              show.clust.cent=F,
                              xlab='tsne-1',
                              ylab='tsne-2',
                              main = "t-SNE",
                              legend.title = "Groups")
    tsne_plot <- ggplotly(tsne_plot)
    for (i in 1:length(tsne_plot$x$data)){
      if (!is.null(tsne_plot$x$data[[i]]$name)){
        tsne_plot$x$data[[i]]$name =  gsub("\\(","",str_split(tsne_plot$x$data[[i]]$name,",")[[1]][1])
      }
      if(i<=length(unique(cluster_group))){
        tsne_plot$x$data[[i]]$text<-paste("x :",round(tsne_plot$x$data[[i]]$x,3),
                                          "\ny :",round(tsne_plot$x$data[[i]]$y,3),
                                          "\nGroups :",tsne_plot$x$data[[i]]$name,
                                          "\nSample name :",tsne_data[which(tsne_data$group==tsne_plot$x$data[[i]]$name),]$sample_name)
      }
    }
    
  }else{
    tsne_plot <- fviz_cluster(list(data = as.data.frame(tsne$Y),
                                   sample_id = tsne_data$sample_name,
                                   cluster = cluster_group),
                              palette = color[1:group_num], 
                              geom = "point",
                              stand=F,
                              ellipse=T,
                              ellipse.type = 'norm', 
                              ggtheme = theme_bw(),
                              show.clust.cent=F,
                              xlab='tsne-1',
                              ylab='tsne-2',
                              main = "t-SNE",
                              legend.title = "Groups")
    tsne_plot <- ggplotly(tsne_plot)
    for (i in 1:length(tsne_plot$x$data)){
      if (!is.null(tsne_plot$x$data[[i]]$name)){
        tsne_plot$x$data[[i]]$name =  gsub("\\(","",str_split(tsne_plot$x$data[[i]]$name,",")[[1]][1])
      }
      if(i<=group_num){
        tsne_plot$x$data[[i]]$text<-paste("x :",round(tsne_plot$x$data[[i]]$x,3),
                                          "\ny :",round(tsne_plot$x$data[[i]]$y,3),
                                          "\nGroups :",tsne_plot$x$data[[i]]$name,
                                          "\nSample name :",tsne_data[which(tsne_data$group==tsne_plot$x$data[[i]]$name),]$sample_name)
      }else if(i>=group_num+1 & i <= 2*group_num){
        tsne_plot$x$data[[i]]$text<-paste("\nGroups :",tsne_plot$x$data[[i]]$name)
      }
    }
    for (i in 1:(2*group_num)) {
      rev_n <- rev(1:(2*group_num))
      if(i==1){rev_list<-list(tsne_plot$x$data[[rev_n[i]]])}
      else{rev_list<-c(rev_list,list(tsne_plot$x$data[[rev_n[i]]]))}
    }
    tsne_plot$x$data <- rev_list
    
  }
  
  return(list(tsne_data, tsne_plot))
}




