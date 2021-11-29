#Hclustering (DE_result_table, exp_table, group_info, lipid_var, distfun, hclustfun)
#plot_multi_heatmap(exp.mat, rowGroup=NULL, colGroup=NULL, scale='none', out.mat=FALSE, plotPATH)

#dis=c('pearson','spearman','euclidean','binary','maximum','manhattan','canberra','minkowski','kendall')
#met=c('ward.D','ward.D2','single','complete','average','mcquitty','median','centroid')

Hclustering <- function(exp_data, DE_result_table, group_info, 
                        lipid_char_table = NULL, char_var = NULL, 
                        distfun = 'pearson', hclustfun = 'complete',
                        insert_ref_group=NULL,ref_group=NULL){
  
  require(tidyverse)
  require(iheatmapr)
  
  if((ncol(exp_data)-1) != nrow(group_info)) stop('Please check the number of samples are equal in the "exp_data" and "group_info"!')
  
  colnames(exp_data)[1] <- 'feature'
  colnames(DE_result_table)[1] <- 'feature'
  
  rownames(exp_data) <- NULL
  exp.mat.all <- exp_data %>% 
    dplyr::select(feature, group_info$sample_name) %>%
    column_to_rownames(var = 'feature') %>% 
    as.matrix()
  colnames(exp.mat.all) <- group_info$label_name
  
  #exp.mat.all[is.na(exp.mat.all)] <- 0
  
  exp.mat.sig <- exp_data %>% 
    dplyr::select(feature, group_info$sample_name) %>%
    filter(feature %in% DE_result_table$feature) %>%
    column_to_rownames(var = 'feature') %>% 
    as.matrix()
  colnames(exp.mat.sig) <- group_info$label_name
  
  #exp.mat[is.na(exp.mat)] <- 0
  
  
  # ## distance function
  # if(distfun %in% c('pearson','spearman','kendall')){
  #   dist.fun=function(x){
  #     x=t(x)
  #     cor.mat=cor(x,method=distfun,use = 'complete.obs')
  #     cor.mat=(1-cor.mat)
  #     cor.dist=as.dist(cor.mat)
  #     return(cor.dist)
  #   }
  # }else{
  #   dist.fun=function(x) dist(x, method=distfun)
  # }
  # 
  # ## hclust function
  # clust.fun=function(x) hclust(x,method = hclustfun)
  
  if(!is.null(insert_ref_group) & !is.null(ref_group)){
    exp_raw_name <- ref_group[-which(insert_ref_group==ref_group)]
    group_info$group[which(group_info$group=='ctrl')] <-  insert_ref_group
    group_info$group[which(group_info$group=='exp')] <-  exp_raw_name
  }
  
  colGroup <- data.frame(Sample = group_info$group, stringsAsFactors = F)
  
  if(!is.null(lipid_char_table) & !is.null(char_var)){
    
    rowGroup.all <- exp.mat.all %>% 
      as.data.frame() %>%
      rownames_to_column(var = 'feature') %>%
      dplyr::select(feature) %>% 
      left_join(lipid_char_table, by = 'feature') %>% 
      dplyr::select(all_of(char_var))
    
    rowGroup.sig <- exp.mat.sig %>% 
      as.data.frame() %>%
      rownames_to_column(var = 'feature') %>%
      dplyr::select(feature) %>% 
      left_join(lipid_char_table, by = 'feature') %>% 
      dplyr::select(all_of(char_var))
    
  }
  
  heatmap_color_scale <- function(data){
    data <- round(data,3)
    if(max(data)<=0 & min(data)<0){
      over_median <- min(data)/2
      if(max(data)<over_median){
        color <-  colorRampPalette(c("#157AB5","#92c5de"))(n = 1000)
      }else{
        color_rank <- round(max(data)/(min(data))*1000)
        color_scale <- colorRampPalette(c("#0571b0","#92c5de","white"))(n = 1000)
        color <- color_scale[color_rank:1000]
      }
    }else if(min(data)>=0 & max(data)>0){
      over_median <- max(data)/2
      if(min(data)>over_median){
        color <-  colorRampPalette(c("#f4a582", "#ca0020"))(n = 1000)
      }else{
        color_rank <- round(min(data)/(max(data))*1000)
        color_scale <- colorRampPalette(c("white","#f4a582", "#ca0020"))(n = 1000)
        color <- color_scale[color_rank:1000]
      }
    }
    return(color)
  }
  #### all exp ####
  if(nrow(exp.mat.all) >= 2 & ncol(exp.mat.all) >= 2 & sum(is.na(exp.mat.all))==0){
    
    if(max(nchar(rownames(exp.mat.all)))<10){
      all_row_text_size <- 0.1
    }else if(max(nchar(rownames(exp.mat.all)))>=10 & max(nchar(rownames(exp.mat.all)))<20){
      all_row_text_size <- 0.2
    }else if(max(nchar(rownames(exp.mat.all)))>=20 & max(nchar(rownames(exp.mat.all)))<30){
      all_row_text_size <- 0.3
    }else if(max(nchar(rownames(exp.mat.all)))>=30 & max(nchar(rownames(exp.mat.all)))<40){
      all_row_text_size <- 0.4
    }else {
      all_row_text_size <- 0.5
    }
    if(max(nchar(colnames(exp.mat.all)))<10){
      all_col_text_size <- 0.1
    }else if(max(nchar(colnames(exp.mat.all)))>=10 & max(nchar(colnames(exp.mat.all)))<20){
      all_col_text_size <- 0.2
    }else if(max(nchar(colnames(exp.mat.all)))>=20 & max(nchar(colnames(exp.mat.all)))<30){
      all_col_text_size <- 0.3
    }else if(max(nchar(colnames(exp.mat.all)))>=30 & max(nchar(colnames(exp.mat.all)))<40){
      all_col_text_size <- 0.4
    }else {
      all_col_text_size <- 0.5
    }
    
    if(!is.null(lipid_char_table) & !is.null(char_var)){
      exp.mat.all <- sweep(exp.mat.all, 1, rowMeans(exp.mat.all, na.rm = T))
      exp.mat.all <- sweep(exp.mat.all, 1, apply(exp.mat.all, 1, sd, na.rm = T), "/")
      if(sum(is.na(exp.mat.all))>0){
        exp.mat.all <- exp.mat.all[-which(is.na(exp.mat.all[,2])),]
      }
      cb_grid <- setup_colorbar_grid(y_length =0.6,x_start = 1,y_start = 0.4)
      if(distfun %in% c("pearson","kendall","spearman")){
        col_dend <- hclust(as.dist(1-cor(exp.mat.all, method=distfun)),method = hclustfun)
        row_dend <- hclust(as.dist(1-cor(t(exp.mat.all), method=distfun)),method = hclustfun)
      }else{
        col_dend <- hclust(dist(t(exp.mat.all), method=distfun),method = hclustfun)
        row_dend <- hclust(dist(exp.mat.all, method=distfun),method = hclustfun)
      }
      if(min(exp.mat.all)>=0 || max(exp.mat.all)<=0){
        if(ncol(exp.mat.all)<=50){
          heatmap.all <- iheatmap(exp.mat.all,colors = heatmap_color_scale(exp.mat.all),colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_labels(side="bottom",size=all_col_text_size) %>%
            add_col_annotation(annotation = colGroup,side="top",show_colorbar = F) %>%
            add_col_dendro(col_dend,side="top",reorder =T) %>%
            add_row_annotation(annotation = rowGroup.all,side="right",show_colorbar = F) %>%
            add_row_dendro(row_dend,side="right",reorder =T)
        }else{
          heatmap.all <- iheatmap(exp.mat.all,colors = heatmap_color_scale(exp.mat.all),colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_annotation(annotation = colGroup,side="top",show_colorbar = F) %>%
            add_col_dendro(col_dend,side="top",reorder =T) %>%
            add_row_annotation(annotation = rowGroup.all,side="right",show_colorbar = F) %>%
            add_row_dendro(row_dend,side="right",reorder =T)
        }
        if(nrow(exp.mat.all)<=50){ heatmap.all <- heatmap.all %>% add_row_labels(side="left",size=all_row_text_size) }
      }else{
        if(ncol(exp.mat.all)<=50){
          heatmap.all <- iheatmap(exp.mat.all,colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_labels(side="bottom",size=all_col_text_size) %>%
            add_col_annotation(annotation = colGroup,side="top",show_colorbar = F) %>%
            add_col_dendro(col_dend,side="top",reorder =T) %>%
            add_row_annotation(annotation = rowGroup.all,side="right",show_colorbar = F) %>%
            add_row_dendro(row_dend,side="right",reorder =T)
        }else{
          heatmap.all <- iheatmap(exp.mat.all,colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_annotation(annotation = colGroup,side="top",show_colorbar = F) %>%
            add_col_dendro(col_dend,side="top",reorder =T) %>%
            add_row_annotation(annotation = rowGroup.all,side="right",show_colorbar = F) %>%
            add_row_dendro(row_dend,side="right",reorder =T) 
        }
        if(nrow(exp.mat.all)<=50){ heatmap.all <- heatmap.all %>% add_row_labels(side="left",size=all_row_text_size) }
      }
      
      reorder.data.all<-exp.mat.all[rev(row_dend$order),col_dend$order] 
      
    }else{
      
      exp.mat.all <- sweep(exp.mat.all, 1, rowMeans(exp.mat.all, na.rm = T))
      exp.mat.all <- sweep(exp.mat.all, 1, apply(exp.mat.all, 1, sd, na.rm = T), "/")
      if(sum(is.na(exp.mat.all))>0){
        exp.mat.all <- exp.mat.all[-which(is.na(exp.mat.all[,2])),]
      }
      cb_grid <- setup_colorbar_grid(y_length =0.6,x_start = 1,y_start = 0.4)
      if(distfun %in% c("pearson","kendall","spearman")){
        col_dend <- hclust(as.dist(1-cor(exp.mat.all, method=distfun)),method = hclustfun)
        row_dend <- hclust(as.dist(1-cor(t(exp.mat.all), method=distfun)),method = hclustfun)
      }else{
        col_dend <- hclust(dist(t(exp.mat.all), method=distfun),method = hclustfun)
        row_dend <- hclust(dist(exp.mat.all, method=distfun),method = hclustfun)
      }
      if(min(exp.mat.all)>0 || max(exp.mat.all)<0){
        if(ncol(exp.mat.all)<=50){
          heatmap.all <- iheatmap(exp.mat.all,colors = heatmap_color_scale(exp.mat.all),colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_labels(size=all_col_text_size) %>%
            add_col_annotation(annotation = colGroup,show_colorbar = F) %>%
            add_col_dendro(col_dend,reorder =T) %>%
            add_row_dendro(row_dend,side="right",reorder =T) 
        }else{
          heatmap.all <- iheatmap(exp.mat.all,colors = heatmap_color_scale(exp.mat.all),colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_annotation(annotation = colGroup,show_colorbar = F) %>%
            add_col_dendro(col_dend,reorder =T) %>%
            add_row_dendro(row_dend,side="right",reorder =T)
        }
        if(nrow(exp.mat.all)<=50){ heatmap.all <- heatmap.all %>% add_row_labels(side="left",size=all_row_text_size) }
      }else{
        if(ncol(exp.mat.all)<=50){
          heatmap.all <- iheatmap(exp.mat.all,colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_labels(size=all_col_text_size) %>%
            add_col_annotation(annotation = colGroup,show_colorbar = F) %>%
            add_col_dendro(col_dend,reorder =T) %>%
            add_row_dendro(row_dend,side="right",reorder =T)
        }else{
          heatmap.all <- iheatmap(exp.mat.all,colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_annotation(annotation = colGroup,show_colorbar = F) %>%
            add_col_dendro(col_dend,reorder =T) %>%
            add_row_dendro(row_dend,side="right",reorder =T)
        }
        if(nrow(exp.mat.all)<=50){ heatmap.all <- heatmap.all %>% add_row_labels(side="left",size=all_row_text_size) }
      }
      
      reorder.data.all<-exp.mat.all[rev(row_dend$order),col_dend$order] 
      
    }
    
  }else{
    
    heatmap.all <- NULL
    reorder.data.all <- NULL
  }
  
  
  #### sig exp ####
  if(nrow(exp.mat.sig) >= 2 & ncol(exp.mat.sig) >= 2 & sum(is.na(exp.mat.all))==0){
    
    if(max(nchar(rownames(exp.mat.sig)))<10){
      sig_row_text_size <- 0.1
    }else if(max(nchar(rownames(exp.mat.sig)))>=10 & max(nchar(rownames(exp.mat.sig)))<20){
      sig_row_text_size <- 0.2
    }else if(max(nchar(rownames(exp.mat.sig)))>=20 & max(nchar(rownames(exp.mat.sig)))<30){
      sig_row_text_size <- 0.3
    }else if(max(nchar(rownames(exp.mat.sig)))>=30 & max(nchar(rownames(exp.mat.sig)))<40){
      sig_row_text_size <- 0.4
    }else {
      sig_row_text_size <- 0.5
    }
    if(max(nchar(colnames(exp.mat.sig)))<10){
      sig_col_text_size <- 0.1
    }else if(max(nchar(colnames(exp.mat.sig)))>=10 & max(nchar(colnames(exp.mat.sig)))<20){
      sig_col_text_size <- 0.2
    }else if(max(nchar(colnames(exp.mat.sig)))>=20 & max(nchar(colnames(exp.mat.sig)))<30){
      sig_col_text_size <- 0.3
    }else if(max(nchar(colnames(exp.mat.sig)))>=30 & max(nchar(colnames(exp.mat.sig)))<40){
      sig_col_text_size <- 0.4
    }else {
      sig_col_text_size <- 0.5
    }
    
    if(!is.null(lipid_char_table) & !is.null(char_var)){
      exp.mat.sig <- sweep(exp.mat.sig, 1, rowMeans(exp.mat.sig, na.rm = T))
      exp.mat.sig <- sweep(exp.mat.sig, 1, apply(exp.mat.sig, 1, sd, na.rm = T), "/")
      cb_grid <- setup_colorbar_grid(y_length =0.6,x_start = 1,y_start = 0.4)
      if(distfun %in% c("pearson","kendall","spearman")){
        col_dend <- hclust(as.dist(1-cor(exp.mat.sig, method=distfun)),method = hclustfun)
        row_dend <- hclust(as.dist(1-cor(t(exp.mat.sig), method=distfun)),method = hclustfun)
      }else{
        col_dend <- hclust(dist(t(exp.mat.sig), method=distfun),method = hclustfun)
        row_dend <- hclust(dist(exp.mat.sig, method=distfun),method = hclustfun)
      }
      if(min(exp.mat.sig)>0 || max(exp.mat.sig)<0){
        if(ncol(exp.mat.sig)<=50){
          heatmap.sig <- iheatmap(exp.mat.sig,colors = heatmap_color_scale(exp.mat.sig),colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_labels(size= sig_col_text_size) %>%
            add_col_annotation(annotation = colGroup,show_colorbar = F) %>%
            add_col_dendro(col_dend,reorder =T) %>%
            add_row_annotation(annotation = rowGroup.sig,show_colorbar = F) %>%
            add_row_dendro(row_dend,side="right",reorder =T)
        }else{
          heatmap.sig <- iheatmap(exp.mat.sig,colors = heatmap_color_scale(exp.mat.sig),colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_annotation(annotation = colGroup,show_colorbar = F) %>%
            add_col_dendro(col_dend,reorder =T) %>%
            add_row_annotation(annotation = rowGroup.sig,show_colorbar = F) %>%
            add_row_dendro(row_dend,side="right",reorder =T)
        }
        if(nrow(exp.mat.sig)<=50){ heatmap.sig <- heatmap.sig %>% add_row_labels(side="left",size=sig_row_text_size) }
      }else{
        if(ncol(exp.mat.sig)<=50){
          heatmap.sig <- iheatmap(exp.mat.sig,colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_labels(size=sig_col_text_size) %>%
            add_col_annotation(annotation = colGroup,show_colorbar = F) %>%
            add_col_dendro(col_dend,reorder =T) %>%
            add_row_annotation(annotation = rowGroup.sig,show_colorbar = F) %>%
            add_row_dendro(row_dend,side="right",reorder =T)
        }else{
          heatmap.sig <- iheatmap(exp.mat.sig,colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_annotation(annotation = colGroup,show_colorbar = F) %>%
            add_col_dendro(col_dend,reorder =T) %>%
            add_row_annotation(annotation = rowGroup.sig,show_colorbar = F) %>%
            add_row_dendro(row_dend,side="right",reorder =T)
        }
        if(nrow(exp.mat.sig)<=50){ heatmap.sig <- heatmap.sig %>% add_row_labels(side="left",size=sig_row_text_size) }
      }
      reorder.data.sig<-exp.mat.sig[rev(row_dend$order),col_dend$order]
      
    }else{
      
      exp.mat.sig <- sweep(exp.mat.sig, 1, rowMeans(exp.mat.sig, na.rm = T))
      exp.mat.sig <- sweep(exp.mat.sig, 1, apply(exp.mat.sig, 1, sd, na.rm = T), "/")
      cb_grid <- setup_colorbar_grid(y_length =0.6,x_start = 1,y_start = 0.4)
      if(distfun %in% c("pearson","kendall","spearman")){
        col_dend <- hclust(as.dist(1-cor(exp.mat.sig, method=distfun)),method = hclustfun)
        row_dend <- hclust(as.dist(1-cor(t(exp.mat.sig), method=distfun)),method = hclustfun)
      }else{
        col_dend <- hclust(dist(t(exp.mat.sig), method=distfun),method = hclustfun)
        row_dend <- hclust(dist(exp.mat.sig, method=distfun),method = hclustfun)
      }
      
      if(min(exp.mat.sig)>0 || max(exp.mat.sig)<0){
        if(ncol(exp.mat.sig)<=50){
          heatmap.sig <- iheatmap(exp.mat.sig,colors = heatmap_color_scale(exp.mat.sig),colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_labels(size=sig_col_text_size) %>%
            add_col_annotation(annotation = colGroup,show_colorbar = F) %>%
            add_col_dendro(col_dend,reorder =T) %>%
            add_row_dendro(row_dend,side="right",reorder =T) 
        }else{
          heatmap.sig <- iheatmap(exp.mat.sig,colors = heatmap_color_scale(exp.mat.sig),colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_annotation(annotation = colGroup,show_colorbar = F) %>%
            add_col_dendro(col_dend,reorder =T) %>%
            add_row_dendro(row_dend,side="right",reorder =T) 
        }
        if(nrow(exp.mat.sig)<=50){ heatmap.sig <- heatmap.sig %>% add_row_labels(side="left",size=sig_row_text_size) }
      }else{
        if(ncol(exp.mat.sig)<=50){
          heatmap.sig <- iheatmap(exp.mat.sig,colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_labels(size=sig_col_text_size) %>%
            add_col_annotation(annotation = colGroup,show_colorbar = F) %>%
            add_col_dendro(col_dend,reorder =T) %>%
            add_row_dendro(row_dend,side="right",reorder =T) 
        }else{
          heatmap.sig <- iheatmap(exp.mat.sig,colorbar_grid = cb_grid,scale = "rows") %>%
            add_col_annotation(annotation = colGroup,show_colorbar = F) %>%
            add_col_dendro(col_dend,reorder =T) %>%
            add_row_dendro(row_dend,side="right",reorder =T)
        }
        if(nrow(exp.mat.sig)<=50){ heatmap.sig <- heatmap.sig %>% add_row_labels(side="left",size=sig_row_text_size) }
      }
      reorder.data.sig<-exp.mat.sig[rev(row_dend$order),col_dend$order]
      
    }
    
  }else{
    
    heatmap.sig <- NULL
    reorder.data.sig <- NULL
  }
  
  return(list(all.lipid = heatmap.all, sig.lipid = heatmap.sig,
              all.lipid.data = reorder.data.all, sig.lipid.data = reorder.data.sig))
  
} #function
