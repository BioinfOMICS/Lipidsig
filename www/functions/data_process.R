

data_process <- function(exp_data, exclude_var_missing=T, 
                         missing_pct_limit=50,
                         replace_zero=T, zero2what='min', xmin=0.5,
                         replace_NA=T, NA2what='min', ymin=0.5,
                         pct_transform=T,
                         data_transform=T,trans_type='log',
                         centering=F,
                         scaling=F){
  require(tidyverse)
  require(stringr)
  exp_data2 <- exp_data[-1]
  #replace 0 with: min, specfic num
  if(replace_zero==T){
    if(is.numeric(zero2what)){
      exp_data2[exp_data2==0] <- zero2what
    }else if(zero2what=='min'){
      for(a in 1:nrow(exp_data2)){
        num <- unlist(exp_data2[a,])[unlist(exp_data2[a,])!=0]
        exp_data2[a,][exp_data2[a,]==0] <- xmin*min(num, na.rm = T)
      }
    }else if(zero2what=='NA'){
      exp_data2[exp_data2==0] <- NA
    }
    exp_data <- cbind(exp_data[1],exp_data2)
  }
  
  
  
  exp_data2 <- exp_data[-1]
  
  #exclude_var_missing
  if(exclude_var_missing==T){
    missing_pct <- apply(exp_data2, 1, function(x){ sum(is.na(x)) / length(x) })
    maintain_var <- missing_pct*100 < missing_pct_limit
    exp_data <- exp_data[maintain_var,]
  }
  if(nrow(exp_data)==0){
    #stop('no species remains')
    return(NULL)
  }
  
  
  exp_data2 <- exp_data[-1]
  
  
  #replace NA with: min, mean, median, specfic num
  if(replace_NA==T){
    if(NA2what=='min'){
      for(a in 1:nrow(exp_data2)){
        exp_data2[a,][is.na(exp_data2[a,])] <- ymin*min(unlist(exp_data2[a,]), na.rm = T)
      }
    }else if(NA2what=='mean'){
      for(a in 1:nrow(exp_data2)){
        exp_data2[a,][is.na(exp_data2[a,])] <- mean(unlist(exp_data2[a,]), na.rm = T)
      }
    }else if(NA2what=='median'){
      for(a in 1:nrow(exp_data2)){
        exp_data2[a,][is.na(exp_data2[a,])] <- median(unlist(exp_data2[a,]), na.rm = T)
      }
    }else if(is.numeric(NA2what)){
      exp_data2[is.na(exp_data2)] <- NA2what
    }
    exp_data <- cbind(exp_data[1],exp_data2)
  }
  
  if(pct_transform==T){
    exp_data[-1] <- map2(exp_data[-1], colSums(exp_data[-1], na.rm = T), ~.x/.y*100) %>% 
      as.data.frame()
  }
  
  #data_transform
  if(data_transform==T){
    if(trans_type=='log'){
      exp_data[-1] <- log10(exp_data[-1])
    }
  }
  
  #centering
  if(centering==T){
    exp_data[-1] <- exp_data[-1] %>% t() %>% scale(scale = F) %>% t() %>% as.data.frame()
  }
  
  #scaling
  if(scaling==T){
    exp_data[-1] <- exp_data[-1] %>% t() %>% scale() %>% t() %>% as.data.frame()
  }
  
  return(exp_data)
} #function: data_process()




