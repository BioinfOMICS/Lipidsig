data_summary <- function(exp_data=NULL,group_info=NULL,remove_na=T,remove_na_pct=NULL,
                         fill_na=T,fill_na_method=NULL,fill_na_Multiply=NULL,
                         PCT_tr=T,log_tr=T,scaling=NULL){
  require(shiny)
  number_of_sample <- tags$p(paste("The number of sample after data processing :",ncol(exp_data)-1))
  
  if(!is.null(group_info)){
    number_of_group <- tags$p("The number of group : 2")
    if(ncol(group_info)==4){
      if(sum(is.na(group_info[,4]))==0){
        pair <- tags$p("Data is  paired")
      }else{
        pair <- tags$p("Data is not paired")
      }
    }else{
      pair <- tags$p("Data is not paired")
    }
  }else{
    number_of_group <- NULL
    pair <- NULL
  }
  trans_data <- exp_data
  trans_data[trans_data==0] <- NA
  all_na_remove_count <- sum(apply(is.na(trans_data),1,sum)==ncol(trans_data[,-1]))
  if(all_na_remove_count>0){
    all_na_remove <- tags$p("Remove the lipids name (features) contains all NA or zero ",
                        tags$text("(Remove",all_na_remove_count,"rows)",style="font-size: 16px;color:red;"))
    trans_data <- trans_data[-which(apply(is.na(trans_data),1,sum)==ncol(trans_data[,-1])),]
  }else{
    all_na_remove <- NULL
  }
  if(remove_na==T){
    remove_count <- sum(apply(is.na(trans_data[,-1]),1,sum)>=ncol(trans_data[,-1])* as.numeric(remove_na_pct)/100)
    if(remove_count>0){
      na_remove <- tags$p(paste0("Remove the lipids name (features) have more than ",as.numeric(remove_na_pct),"% missing values"),
                          tags$text("(Remove",remove_count,"rows)",style="font-size: 16px;color:red;"))
      number_of_feature <- tags$p(paste("The number of feature after data processing :",nrow(exp_data)-remove_count-all_na_remove_count))
    }else{
      na_remove <- tags$p(paste0("Remove the lipids name (features) have more than ",as.numeric(remove_na_pct),"% missing values"))
      number_of_feature <- tags$p(paste("The number of feature after data processing :",nrow(exp_data)-all_na_remove_count))
    }
  }else{
    na_remove <- tags$p("Did not remove the lipids name (features) with many missing values.",style="color:#A9A9A9;")
    number_of_feature <- tags$p(paste("The number of feature :",nrow(exp_data)-all_na_remove_count))
  }
  if(fill_na==T){
    if(fill_na_method=='min'){
      na_fill <- tags$p(paste0("Replace missing value with the minimum value of data * ",fill_na_Multiply))
    }else{
      na_fill <- tags$p(paste0("Fill missing value with the ",fill_na_method," value of data"))
    }
  }else{
    na_fill <- tags$p("Did not Fill missing value",style="color:#A9A9A9;")
  }
  if(log_tr==T){
    tr_PCT <- tags$p("Log10 transformation")
  }else{
    tr_PCT <- tags$p("Did not have Log10 transformation",style="color:#A9A9A9;")
  }
  if(PCT_tr==T){
    tr_log <- tags$p("Data transformed into percentage.")
  }else{
    tr_log <- tags$p("Did not transform into percentage.",style="color:#A9A9A9;")
  }
  if(!is.null(scaling)){
    if(scaling==T){
      sca <- tags$p("Data scaling (Standardsation).")
    }else{
      sca <- tags$p("Did not have scaling (Standardsation).",style="color:#A9A9A9;")
    }
  }else{
    sca <- NULL
  }
  
  return_div <-tags$div(
    tags$p(strong("Data summary"),style="font-size: 20px;"),
    tags$ul(
      all_na_remove,
      na_remove,
      number_of_sample,
      number_of_feature,
      number_of_group,
      pair,
      na_fill,
      tr_PCT,
      tr_log,
      sca,
      style="font-size: 16px;"
    ),
    style="font-size: 0px;"
  )
  
  return(return_div)
}