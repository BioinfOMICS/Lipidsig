

corr_heatmap <- function(exp_data,corr_method="pearson",
                         distfun = "maximum", hclustfun='average'){
  require(tidyverse)
  require(stringr)
  require(iheatmapr)
  require(rstatix)
  #####################################
  #####################################
  ### Correlation heatmap by Sample ###
  #####################################
  #####################################
  
  ## Sample correlation coefficients ##
    sample_exp_data <- subset(exp_data, select = -feature)
    sample_corr_coef <- cor(sample_exp_data, method=corr_method, use="pairwise.complete.obs")
    
    ## Sample correlation p-value ##
    cor.test.p <- function(x,method){
      FUN <- function(x, y,method) cor.test(x=x, y=y,method=method,na.action = "na.exclude")[["p.value"]]
      z <- outer(
        colnames(x), 
        colnames(x), 
        Vectorize(function(i,j) FUN(x[,i], x[,j],method=method))
      )
      dimnames(z) <- list(colnames(x), colnames(x))
      z
    }
    sample_corr_p <- cor.test.p(x=sample_exp_data,method = corr_method)
    
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
    
    ## Sample Correlation heatmap ##
    cb_grid <- setup_colorbar_grid(y_length =0.6,x_start = 1,y_start = 0.5)
    
    if(sum(is.na(sample_corr_coef))==0 & nrow(sample_corr_coef)>=2 & ncol(sample_corr_coef)>=2){
      if(distfun%in%c("pearson","kendall","spearman")){
        col_dend <- hclust(as.dist(1-cor(sample_exp_data, method=distfun)),method = hclustfun)
      }else{
        col_dend <- hclust(dist(t(sample_exp_data), method=distfun),method = hclustfun)
      }
      if(min(sample_corr_coef)>=0 || max(sample_corr_coef)<=0){
        sample_hm <- iheatmap(sample_corr_coef,colors = heatmap_color_scale(sample_corr_coef),colorbar_grid = cb_grid) %>%
          add_col_dendro(col_dend,side="top",reorder =T,size=0.1) %>%
          add_row_dendro(col_dend,side="right",reorder =T,size=0.1)
      }else{
        sample_hm <- iheatmap(sample_corr_coef,colorbar_grid = cb_grid) %>%
          add_col_dendro(col_dend,side="top",reorder =T,size=0.1) %>%
          add_row_dendro(col_dend,side="right",reorder =T,size=0.1)
      }
      if(nrow(sample_corr_coef)<50){
        sample_hm <- sample_hm %>% add_row_labels(font=list(size=10))
      }
      if(ncol(sample_corr_coef)<50){
        sample_hm <- sample_hm %>%add_col_labels(side="bottom",font=list(size=10))
      }
      
      ## reorder Sample correlation matrix ##
      reorder_sample_corr_coef <- apply(sample_corr_coef[,col_dend$order],2,rev)
    }else{
      sample_hm <- NULL
      reorder_sample_corr_coef <- NULL
    }
    
    #####################################
    #####################################
    ### Correlation heatmap by lipids ###
    #####################################
    #####################################  
    
    ## Lipids correlation coefficients ##
    lipids_exp_data <- exp_data %>% gather(-feature, key='sample_name', value='value') %>% 
      spread(key='feature', value='value') 
    lipids_exp_data <- subset(lipids_exp_data, select = -sample_name)
    lipids_corr_coef <- cor(lipids_exp_data, method=corr_method, use="pairwise.complete.obs")
    
    ## Lipids correlation p-value ##
    lipids_corr_p <- cor.test.p(x=lipids_exp_data,method = corr_method)
    
    ## Lipids Correlation heatmap ##
    if(sum(is.na(lipids_exp_data))==0 & nrow(lipids_exp_data)>=2 & ncol(lipids_exp_data)>=2){
      if(distfun%in%c("pearson","kendall","spearman")){
        col_dend <- hclust(as.dist(1-cor(lipids_exp_data, method=distfun)),method = hclustfun)
      }else{
        col_dend <- hclust(dist(t(lipids_exp_data), method=distfun),method = hclustfun)
      }
      if(min(lipids_corr_coef)>0 | max(lipids_corr_coef)<0){
        lipids_hm <- iheatmap(lipids_corr_coef,colors = heatmap_color_scale(sample_corr_coef),colorbar_grid = cb_grid) %>%
          add_col_dendro(col_dend,side="top",reorder =T,size=0.1) %>%
          add_row_dendro(col_dend,side="right",reorder =T,size=0.1)
      }else{
        lipids_hm <- iheatmap(lipids_corr_coef,colorbar_grid = cb_grid) %>%
          add_col_dendro(col_dend,side="top",reorder =T,size=0.1) %>%
          add_row_dendro(col_dend,side="right",reorder =T,size=0.1)
      }
      if(nrow(lipids_corr_coef)<50){
        lipids_hm <- lipids_hm %>% add_row_labels(font=list(size=10))
      }
      if(ncol(lipids_corr_coef)<50){
        lipids_hm <- lipids_hm %>%add_col_labels(side="bottom",font=list(size=10))
      }
      
      
      ## reorder Lipids correlation matrix ##
      reorder_lipids_corr_coef <- apply(lipids_corr_coef[,col_dend$order],2,rev)
    }else{
      lipids_hm <- NULL
      reorder_lipids_corr_coef <- NULL
    }
  
  
  return(list(sample_corr_coef = sample_corr_coef,
              sample_corr_p = sample_corr_p,
              sample_hm = sample_hm,
              reorder_sample_corr_coef = reorder_sample_corr_coef,
              lipids_corr_coef = lipids_corr_coef,
              lipid_corr_p =  lipids_corr_p,
              lipids_hm = lipids_hm,
              reorder_lipids_corr_coef = reorder_lipids_corr_coef))
}





