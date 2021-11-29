

PLSDA <- function(exp_transform_table, group_info = NULL, sig_feature = NULL, ncomp = 2, scaling = T,
                  cluster_method, group_num = NULL, var1 = NULL, var2 = NULL, 
                  insert_ref_group = NULL, ref_group = NULL){
  
  require(tidyverse)
  require(mixOmics)
  require(ggforce)
  require(ggthemes)
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
  
  colnames(exp_transform_table)[1] <- 'feature'
  
  if(sum(!colnames(exp_transform_table)%in%group_info$sample_name)!=1){
    #stop("Sample name not matched in the exp_transform_table and group_info!")
    return(NULL)
  }
  
  if(!is.null(sig_feature)){
    exp_transform_table <- exp_transform_table %>% 
      filter(eval(parse(text = colnames(exp_transform_table)[1]))%in%sig_feature)
  }
  
  num <- apply(exp_transform_table[-1], 1, FUN = function(x){length(unique(x))})
  exp_transform_table <- exp_transform_table[(num!=1),]
  exp_transform_table <- exp_transform_table[!is.infinite(rowSums(exp_transform_table[-1], na.rm = T)),]
  
  group_info <- group_info %>% filter(sample_name %in% colnames(exp_transform_table)[-1])
  
  if(!is.null(insert_ref_group) & !is.null(ref_group) &!is.null(group_info)){
    Y <- ifelse(group_info$group==insert_ref_group,1,2)
  }else{
    Y <- ifelse(group_info$group=='ctrl',1,2)
  }
  
  names(Y) <- group_info$sample_name
  rownames(exp_transform_table) <- exp_transform_table$feature
  
  X <- exp_transform_table[c('feature', group_info$sample_name)] %>% 
    dplyr::select(-1) %>% 
    t()
  
  if(nrow(X)<6){
    #stop('Samples must be larger than 6')
    return(NULL)
  }
  if(ncol(X)<2){
    #stop('Samples must be larger than 6')
    return(NULL)
  }
  
  
  plsda.res <- plsda(X, Y, ncomp, scaling)
  
  
  X.variate <- plsda.res$variates$X
  X.loading <- plsda.res$loadings$X
  
  
  
  
  #grouping
  if(cluster_method=='kmeans'){
    cluster_group <- kmeans(X.variate, centers = group_num)$cluster
  }else if(cluster_method=='kmedoids'){
    cluster_group <- pam(X.variate, k = group_num, metric = var1)$cluster #euclidean manhattan
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
    cluster_group <- hclust(dist.fun(X.variate), method = var2)
    cluster_group <- cutree(cluster_group, k=group_num) 
  }else if(cluster_method=='dbscan'){
    cluster_group <- dbscan(X.variate, eps = var1, minPts = var2)$cluster
    cluster_group <- ifelse(cluster_group>9,9,cluster_group) %>% as.character()
    cluster_group <- ifelse(cluster_group=='0','noise',cluster_group)
  }else if(cluster_method=='group_info'){
    group_order <- map_dbl(rownames(X.variate), function(x){which(x==group_info$sample_name)})
    
    cluster_group <- group_info[group_order,] %>% .$group
    group_num <- length(unique(cluster_group))
    
  }
  
  cluster_group <- as.character(cluster_group)
  
  color <- c("#00AFBB", "#E7B800", "#FC4E07","#42B540FF","#BB3099","#EE0099","#0000AC","#868686FF",'#00468BFF','black')
  if(cluster_method %in% c('dbscan')){
    
    sample.plot <-  fviz_cluster(list(data = as.data.frame(X.variate), 
                                      cluster = cluster_group),
                                 palette = color[1:length(unique(cluster_group))], 
                                 geom = "point",
                                 stand=F,
                                 ellipse=F,
                                 ggtheme = theme_bw(),
                                 show.clust.cent=F,
                                 xlab='PLSDA-1',
                                 ylab='PLSDA-2',
                                 main = "PLSDA",
                                 legend.title='Groups')
    sample_plot <- ggplotly(sample.plot)
    for (i in 1:length(sample_plot$x$data)){
      if (!is.null(sample_plot$x$data[[i]]$name)){
        sample_plot$x$data[[i]]$name =  gsub("\\(","",str_split(sample_plot$x$data[[i]]$name,",")[[1]][1])
      }
      if(i<=length(unique(cluster_group))){
        sample_plot$x$data[[i]]$text<-paste("x :",round(sample_plot$x$data[[i]]$x,3),
                                            "\ny :",round(sample_plot$x$data[[i]]$y,3),
                                            "\nGroups :",sample_plot$x$data[[i]]$name,
                                            "\nSample name :",sample.plot$data[which(sample.plot$data$cluster==sample_plot$x$data[[i]]$name),]$name)
      }
    }
    sample.plot <- sample_plot
  }else{
    sample.plot <- fviz_cluster(list(data = as.data.frame(X.variate), 
                                     cluster = cluster_group),
                                palette = color[1:group_num], 
                                geom = "point",
                                stand=F,
                                ellipse=T,
                                ellipse.type = 'norm', 
                                ggtheme = theme_bw(),
                                show.clust.cent=F,
                                xlab='PLSDA-1',
                                ylab='PLSDA-2',
                                main = "PLSDA",
                                legend.title='Groups')
    sample_plot <- ggplotly(sample.plot)
    for (i in 1:length(sample_plot$x$data)){
      if (!is.null(sample_plot$x$data[[i]]$name)){
        sample_plot$x$data[[i]]$name =  gsub("\\(","",str_split(sample_plot$x$data[[i]]$name,",")[[1]][1])
      }
      if (i<=group_num){
        sample_plot$x$data[[i]]$text<-paste("x :",round(sample_plot$x$data[[i]]$x,3),
                                            "\ny :",round(sample_plot$x$data[[i]]$y,3),
                                            "\nGroups :",sample_plot$x$data[[i]]$name,
                                            "\nSample name :",sample.plot$data[which(sample.plot$data$cluster==sample_plot$x$data[[i]]$name),]$name)
      }else if(i>=group_num+1 & i <= 2*group_num){
        sample_plot$x$data[[i]]$text<-paste("Groups :",sample_plot$x$data[[i]]$name)
      }
    }
    for (i in 1:(2*group_num)) {
      rev_n <- rev(1:(2*group_num))
      if(i==1){rev_list<-list(sample_plot$x$data[[rev_n[i]]])}
      else{rev_list<-c(rev_list,list(sample_plot$x$data[[rev_n[i]]]))}
    }
    sample_plot$x$data <- rev_list
    sample.plot <- sample_plot
  }
  
  
  rown <- rownames(X.variate)
  
  X.variate <- X.variate %>% as.data.frame() %>% 
    mutate(sample_name=rown) %>% 
    mutate(group=cluster_group) %>% 
    dplyr::select(sample_name, group, everything())
  colnames(X.variate)[3:4] <- c('PLSDA1', 'PLSDA2')
  
  X.loading <- X.loading %>% as.data.frame()
  colnames(X.loading) <- c('PLSDA1', 'PLSDA2')
  
  variable.tab <- plotVar(plsda.res, comp = 1:2, var.names = T, plot = F)
  
  creat_circle_data<-function(r,x=0,y=0){
    circle.x<-seq(x-r,x+r,length.out =150)
    circle.y <-c(sqrt(r^2-circle.x^2),-sqrt(r^2-circle.x^2))
    circle.x=c(circle.x,seq(x+r,x-r,length.out =150))
    circle <-data.frame(circle.x,circle.y) 
  }
  circle_data_1 <-creat_circle_data(1)
  circle_data_2 <-creat_circle_data(0.5)
  
  variable.plot <- plot_ly(x=~circle.x,y=~circle.y,hoverinfo="none") %>%
    add_trace(data=circle_data_1,type="scatter",mode="lines",hoverinfo="none",showlegend = FALSE,
              line = list(color = 'black', width = 2)) %>%
    add_trace(data=circle_data_2,type="scatter",mode="lines",hoverinfo="none", showlegend = FALSE,
              line = list(color = 'black', width = 2)) %>%
    add_trace(data = variable.tab,x=~x,y=~y,type="scatter",mode="markers",hoverinfo="text",showlegend = FALSE,
              marker = list(color = "red", size = 5),
              text=~paste("x :",round(x,3),
                          "\ny :",round(y,3),
                          "\nfeature :",variable.tab$names)) %>%
    layout(xaxis=list(title = "PLSDA-1"),
           yaxis=list(title = "PLSDA-2"),
           title = 'Variables - PLSDA')                                      
  return(list(X.variate, X.loading, sample.plot, variable.plot))
  
}


