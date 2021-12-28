submit_check <- function(transform_data=NULL,sig_data=NULL,check_NA=T,feature_num=2,sample_num=2,sig_count=NULL){
  library(dplyr)
  library(tidyr)
  library(stringr)
  if(!is.null(transform_data)){
    trans_sd <- apply(transform_data[,-1],1,function(x) sd(x,na.rm=T))
    remove_count <- sum(na.omit(trans_sd)==0)
    if(nrow(transform_data)-remove_count<feature_num){
      feature_number <- tags$li('Less than ',feature_num,' features')
    }else{
      feature_number <- NULL
    }
    
    if(ncol(transform_data)-1<sample_num){
      sample_number <- tags$li('Less than ',sample_num,' samples')
    }else{
      sample_number <- NULL
    }
  }else{
    feature_number <- NULL
    sample_number <- NULL
  }
  if(!is.null(sig_data) & !is.null(sig_count)){
    if(nrow(sig_data)<sig_count){
      sig <- tags$li(paste('Less than', sig_count ,'significant lipid features'))
    }else{
      sig <- NULL
    }
  }else{
    sig <- NULL
  }
  if(check_NA==T & !is.null(transform_data)){
    if(sum(is.na(transform_data))>0){
      na_check <- tags$li('The lipids name (features) can not contains NA')
    }else{
      na_check <- NULL
    }
  }else{
    na_check <- NULL
  }
  print_text <- tags$ol(
    feature_number,
    sample_number,
    sig,
    na_check
  )
  if(length(grep("li",as.character(print_text)))==0){
    check_all_right <- TRUE 
  }else{
    check_all_right <- FALSE
  }
  return(list(check_all_right,print_text))
}
