 data_check <- function(data=NULL,exp_data=NULL,data_type,page=NULL,file_path=NULL,group_name=NULL,remove_na=T,remove_na_pct=NULL){
  library(dplyr)
  library(tidyr)
  library(stringr)
  require(shiny)
  if(!is.null(exp_data) & data_type=="exp"){
    title <- "Check Expression Data"
    if(grepl('.csv$', file_path) | grepl('.tsv$', file_path)){
      if(class(exp_data[,1])=="character"){
        feature_name <- tags$p(icon("check"),"The first column must contain a list of lipids names (features)",style="font-size: 16px;")
      }else{
        feature_name <- tags$p(icon("times"),"The first column must contain a list of lipids names (features)",style="font-size: 16px;color:red;")
      }
      if(nrow(exp_data)==length(unique(exp_data[,1]))){
        feature_uni <- tags$p(icon("check"),"The lipids name (features) must be unique",style="font-size: 16px;")
      }else{
        feature_uni <- tags$p(icon("times"),"The lipids name (features) must be unique",style="font-size: 16px;color:red;")
      }
      if(ncol(exp_data)==2){
        if(class(exp_data[,1])=="character" & class(exp_data[,-1])%in%c("numeric","integer")){
          column_type <- tags$p(icon("check"),"First column type must be 'character',others must be 'numeric'",style="font-size: 16px;")
          if(sum(exp_data[,2]<0,na.rm = T)>0){
            variable <- tags$p(icon("times"),"Variable must greater than zero",style="font-size: 16px;color:red;")
          }else{
            variable <- tags$p(icon("check"),"Variable must greater than zero",style="font-size: 16px;")
          }
        }else{
          column_type <-tags$p(icon("times"),"First column type must be 'character',others must be 'numeric'",style="font-size: 16px;color:red;")
          variable <- NULL
        }
      }else{
        if(class(exp_data[,1])=="character" & sum(sapply(exp_data[,-1], class)%in%c("numeric","integer"))==ncol(exp_data[,-1])){
          column_type <- tags$p(icon("check"),"First column type must be 'character',others must be 'numeric'",style="font-size: 16px;")
          if(sum(exp_data[,-1]<0,na.rm = T)>0){
            variable <- tags$p(icon("times"),"Variable must greater than zero",style="font-size: 16px;color:red;")
          }else{
            variable <- tags$p(icon("check"),"Variable must greater than zero",style="font-size: 16px;")
          }
        }else{
          column_type <-tags$p(icon("times"),"First column type must be 'character',others must be 'numeric'",style="font-size: 16px;color:red;")
          variable <- NULL
        } 
      }
      if(page=="DE" | page=="Profiling"){
        if(ncol(exp_data)>3){
          sample_number <- tags$p(icon("check"),"At least 2 samples.",style="font-size: 16px;")
          if(ncol(exp_data)==4){
            sample_warning <- tags$p(icon("exclamation"),"Only 3 samples will not show p-value.",style="font-size: 16px;color:#ffad33;")
          }else{
            sample_warning <- NULL
          }
        }else if(ncol(exp_data)==3){
          sample_number <- tags$p(icon("check"),"At least 2 samples.",style="font-size: 16px;")
          sample_warning <- tags$p(icon("exclamation"),"Only 2 samples will not show p-value.",style="font-size: 16px;color:#ffad33;")
        }else{
          sample_number <- tags$p(icon("times"),"At least 2 samples.",style="font-size: 16px;color:red;")
          sample_warning <- NULL
        }
        trans_data <- exp_data
        trans_data[trans_data==0] <- NA
        if(remove_na==T){
          remove_count <- sum(apply(is.na(trans_data[,-1]),1,sum)>=ncol(trans_data[,-1])* as.numeric(remove_na_pct)/100)
        }else{
          remove_count <- sum(apply(is.na(trans_data[,-1]),1,sum)==ncol(trans_data[,-1]))
        }
        if(nrow(exp_data)-remove_count>=5){
          feature_number <- tags$p(icon("check"),"The number of lipids names (features) must be more than 5 after data processing.",style="font-size: 16px;")
        }else{
          feature_number <- tags$p(icon("times"),"The number of lipids names (features) must be more than 5 after data processing.",style="font-size: 16px;color:red;")
        } 
      }else if(page=="ML"){
        sample_warning <- NULL
        if(ncol(exp_data)>=61){
          sample_number <- tags$p(icon("check"),"At least 60 samples.",style="font-size: 16px;")
        }else{
          sample_number <- tags$p(icon("times"),"At least 60 samples.",style="font-size: 16px;color:red;")
        }
        trans_data <- exp_data
        trans_data[trans_data==0] <- NA
        if(remove_na==T){
          remove_count <- sum(apply(is.na(trans_data[,-1]),1,sum)>=ncol(trans_data[,-1])* as.numeric(remove_na_pct)/100)
        }else{
          remove_count <- sum(apply(is.na(trans_data[,-1]),1,sum)==ncol(trans_data[,-1]))
        }
        if(nrow(exp_data)-remove_count>=10){
          feature_number <- tags$p(icon("check"),"The number of lipids names (features) must be more than 5 after data processing.",style="font-size: 16px;")
        }else{
          feature_number <- tags$p(icon("times"),"The number of lipids names (features) must be more than 5 after data processing.",style="font-size: 16px;color:red;")
        } 
      }else if(page=="Correlation"){
        sample_warning <- NULL
        if(ncol(exp_data)>=11){
          sample_number <- tags$p(icon("check"),"At least 10 samples.",style="font-size: 16px;")
        }else{
          sample_number <- tags$p(icon("times"),"At least 10 samples.",style="font-size: 16px;color:red;")
        }
        trans_data <- exp_data
        trans_data[trans_data==0] <- NA
        if(remove_na==T){
          remove_count <- sum(apply(is.na(trans_data[,-1]),1,sum)>=ncol(trans_data[,-1])* as.numeric(remove_na_pct)/100)
        }else{
          remove_count <- sum(apply(is.na(trans_data[,-1]),1,sum)==ncol(trans_data[,-1]))
        }
        if(nrow(exp_data)-remove_count>=5){
          feature_number <- tags$p(icon("check"),"The number of lipids names (features) must be more than 5 after data processing.",style="font-size: 16px;")
        }else{
          feature_number <- tags$p(icon("times"),"The number of lipids names (features) must be more than 5 after data processing.",style="font-size: 16px;color:red;")
        } 
      }
      return_div <- tags$div(
        tags$p(strong(title),style="font-size: 20px;"),
        tags$ul(
          feature_name,
          feature_uni,
          column_type,
          variable,
          sample_number,
          sample_warning,
          feature_number
        ),
        style="font-size: 0px;"
      )
    }else{
      return_div <- tags$div(
        tags$p(strong(title),style="font-size: 20px;"),
        tags$ul(
          tags$p(icon("times"),"Upload your data table in .csv/.tsv.",style="font-size: 16px;color:red;")
        ),
        style="font-size: 0px;"
      )
    }
    
  }else if(!is.null(data) & data_type=="group"){
    title<- "Check Group information"
    if(grepl('.csv$', file_path) | grepl('.tsv$', file_path)){
      if(ncol(data)==4){
        if(sum(sapply(data[,1:3],class)!="character")==0){
          if("pair" %in% colnames(data)){
            if(which(colnames(data)=="pair")==4){
              column_name <- tags$p(icon("check"),"The column must arrange in order of sample_name, label_name, group, pair(optional).",style="font-size: 16px;")
              column_type <- tags$p(icon("check"),"The first 3 columns must be characters",style="font-size: 16px;")
            }else{
              column_name <- tags$p(icon("times"),"The column must arrange in order of sample_name, label_name, group, pair(optional).",style="font-size: 16px;color:red;")
              column_type <- tags$p(icon("times"),"The first 3 columns must be characters",style="font-size: 16px;color:red;")
            }
          }else{
            column_name <- tags$p(icon("check"),"The column must arrange in order of sample_name, label_name, group, pair(optional).",style="font-size: 16px;")
            column_type <- tags$p(icon("check"),"The first 3 columns must be characters",style="font-size: 16px;")
          }
        }else{
          column_name <- tags$p(icon("times"),"The column must arrange in order of sample_name, label_name, group, pair(optional).",style="font-size: 16px;color:red;")
          column_type <- tags$p(icon("times"),"The first 3 columns must be characters",style="font-size: 16px;color:red;")
        }
        if(sum(!is.na(data[,4]))==0){
          check_pair <- tags$p(icon("check"),"Each pair must have a specific number, staring from 1 to N. Cannot have NA, blank, or skip numbers.",style="font-size: 16px;")
        }else if(sum(table(data[,4])!=2)==0 & sum(is.na(data[,4]))==0){
          check_pair <- tags$p(icon("check"),"Each pair must have a specific number, staring from 1 to N. Cannot have NA, blank, or skip numbers.",style="font-size: 16px;")
        }else{
          check_pair <- tags$p(icon("times"),"Each pair must have a specific number, staring from 1 to N. Cannot have NA, blank, or skip numbers.",style="font-size: 16px;color:red;")
        }
        if(sum(data[,1]%in%colnames(exp_data))==nrow(data) & sum(data[,1]%in%colnames(exp_data))==ncol(exp_data[,-1])){
          sample_name <- tags$p(icon("check"),"‘sample_name’ must same as the name of samples of ‘Lipid expression data’",style="font-size: 16px;")
        }else{
          sample_name <- tags$p(icon("times"),"‘sample_name’ must same as the name of samples of ‘Lipid expression data’",style="font-size: 16px;color:red;")
        }
        if(length(unique(data[,3]))==2){
          if(sum(table(data[,3])>=1)==2){
            group <- tags$p(icon("check"),"The column ‘group’ only can have 2 groups, and ≥ 1 sample for each group.",style="font-size: 16px;") 
          }else{
            group <- tags$p(icon("times"),"The column ‘group’ only can have 2 groups, and ≥ 1 sample for each group.",style="font-size: 16px;color:red;") 
          }
        }else{
          group <- tags$p(icon("times"),"The column ‘group’ only can have 2 groups, and ≥ 1 sample for each group.",style="font-size: 16px;color:red;")
        }
        if(group_name %in% data[,3] ){
          group.name <- tags$p(icon("check"),"The reference group entered by users must be included in the uploaded ‘Group Information’ table.",style="font-size: 16px;")
        }else{
          group.name <- tags$p(icon("times"),"The reference group entered by users must be included in the uploaded ‘Group Information’ table.",style="font-size: 16px;color:red;")
        }
      }else if(ncol(data)==3){
        if("pair" %in% colnames(data)){
          column_name <- tags$p(icon("times"),"The column must arrange in order of sample_name, label_name, group, pair(optional).",style="font-size: 16px;color:red;")
        }else{
          column_name <- tags$p(icon("check"),"The column must arrange in order of sample_name, label_name, group, pair(optional).",style="font-size: 16px;")
        }  
        if(sum(sapply(data,class)!="character")==0){
          column_type <- tags$p(icon("check"),"The first 3 columns must be characters",style="font-size: 16px;")
        }else{
          column_type <- tags$p(icon("times"),"The first 3 columns must be characters",style="font-size: 16px;color:red;")
        }
        check_pair <- NULL
        if(sum(data[,1]%in%colnames(exp_data))==nrow(data) & sum(data[,1]%in%colnames(exp_data))==ncol(exp_data[,-1])){
          sample_name <- tags$p(icon("check"),"‘sample_name’ must same as the name of samples of ‘Lipid expression data’",style="font-size: 16px;")
        }else{
          sample_name <- tags$p(icon("times"),"‘sample_name’ must same as the name of samples of ‘Lipid expression data’",style="font-size: 16px;color:red;")
        }
        if(length(unique(data[,3]))==2){
          if(sum(table(data[,3])>=1)==2){
            group <- tags$p(icon("check"),"The column ‘group’ only can have 2 groups, and ≥ 1 sample for each group.",style="font-size: 16px;") 
          }else{
            group <- tags$p(icon("times"),"The column ‘group’ only can have 2 groups, and ≥ 1 sample for each group.",style="font-size: 16px;color:red;") 
          }
        }else{
          group <- tags$p(icon("times"),"The column ‘group’ only can have 2 groups, and ≥ 1 sample for each group.",style="font-size: 16px;color:red;")
        }
        if(group_name %in% data[,3] ){
          group.name <- tags$p(icon("check"),"The reference group entered by users must be included in the uploaded ‘Group Information’ table.",style="font-size: 16px;")
        }else{
          group.name <- tags$p(icon("times"),"The reference group entered by users must be included in the uploaded ‘Group Information’ table.",style="font-size: 16px;color:red;")
        }
      }else{
        column_name <- tags$p(icon("times"),"The column must arrange in order of sample_name, label_name, group, pair(optional).",style="font-size: 16px;color:red;")
        column_type <- NULL
        check_pair <- NULL
        sample_name <- NULL
        group <- NULL
        group.name <- NULL
      }
      
      if(sum(is.na(data))==0){
        no_na <- tags$p(icon("check"),"The first 3 columns cannot contain NA or blanks.",style="font-size: 16px;")
      }else{
        no_na <- tags$p(icon("times"),"The first 3 columns cannot contain NA or blanks.",style="font-size: 16px;color:red;")
      }
      return_div <- tags$div(
        tags$p(strong(title),style="font-size: 20px;"),
        tags$ul(
          column_name,
          column_type,
          check_pair,
          sample_name,
          group,
          group.name
        ),
        style="font-size: 0px;"
      )
    }else{
      return_div <- tags$div(
        tags$p(strong(title),style="font-size: 20px;"),
        tags$ul(
          tags$p(icon("times"),"Upload your data table in .csv/.tsv.",style="font-size: 16px;color:red;")
        ),
        style="font-size: 0px;"
      )
    }
    
  }else if(!is.null(data) &  data_type=="lipid_char"){
    title <- "Check lipid_char_table"
    if(grepl('.csv$', file_path) | grepl('.tsv$', file_path)){
      if(class(data[,1])=="character"){
        feature_name <- tags$p(icon("check"),"The first column must contain a list of lipids names (features).",style="font-size: 16px;")
      }else{
        feature_name <- tags$p(icon("times"),"The first column must contain a list of lipids names (features).",style="font-size: 16px;color:red;")
      }
      if(nrow(data)==length(unique(data[,1]))){
        feature_uni <- tags$p(icon("check"),"The lipids names (features) must be unique.",style="font-size: 16px;")
      }else{
        feature_uni <- tags$p(icon("times"),"The lipids names (features) must be unique.",style="font-size: 16px;color:red;")
      }
      total_text <- c("totallength","totaldb","totaloh")
      if(sum(c("class",total_text)%in%colnames(data))>0){
        if("class"%in%colnames(data)  &sum(total_text%in%colnames(data))==0){
          if(class(data[,"class"])=="character"){
            data_class <- tags$p(icon("check"),"The content of column “Class” must be characters",style="font-size: 16px;")
          }else{
            data_class <- tags$p(icon("times"),"The content of column “Class” must be characters",style="font-size: 16px;color:red;")
          }
        }else if(sum("class"%in%colnames(data))==0&sum(total_text%in%colnames(data))!=0){
          if(sum(total_text%in%colnames(data))==1){
            if(class(data[,total_text[which(total_text%in%colnames(data)==1)]])%in%c("integer","numeric")){
              data_class <- tags$p(icon("check"), paste("The content of column",paste(total_text[which(total_text%in%colnames(data)==1)],collapse=" " )," must be numeric"),style="font-size: 16px;")
            }else{
              data_class <- tags$p(icon("times"),paste("The content of column",paste(total_text[which(total_text%in%colnames(data)==1)],collapse=" " )," must be numeric"),style="font-size: 16px;color:red;")
            } 
          }else{
            if(sum(apply(data[,total_text[which(total_text%in%colnames(data)==1)]],2,class)%in%c("integer","numeric"))==length(which(total_text%in%colnames(data)==1))){
              data_class <- tags$p(icon("check"), paste("The content of column",paste(total_text[which(total_text%in%colnames(data)==1)],collapse=" " )," must be numeric"),style="font-size: 16px;")
            }else{
              data_class <- tags$p(icon("times"),paste("The content of column",paste(total_text[which(total_text%in%colnames(data)==1)],collapse=" " )," must be numeric"),style="font-size: 16px;color:red;")
            } 
          }
        }else{
          if(sum(total_text%in%colnames(data))==1){
            if(class(data[,"class"])=="character"&class(data[,total_text[which(total_text%in%colnames(data)==1)]])%in%c("integer","numeric")){
              data_class <- tags$p(icon("check"),paste("The content of column “Class” must be characters ,",paste("the content of column",total_text[which(total_text%in%colnames(data)==1)],collapse=" " ),"type must be numeric") ,style="font-size: 16px;")
            }else{
              data_class <- tags$p(icon("times"),paste("The content of column “Class” must be characters ,",paste("the content of column",total_text[which(total_text%in%colnames(data)==1)],collapse=" " ),"type must be numeric"),style="font-size: 16px;color:red;")
            }
          }else{
            if(class(data[,"class"])=="character"&sum(apply(data[,total_text[which(total_text%in%colnames(data)==1)]],2,class)%in%c("integer","numeric"))==length(which(total_text%in%colnames(data)==1))){
              data_class <- tags$p(icon("check"),paste("The content of column “Class” must be characters ,",paste("the content of column",total_text[which(total_text%in%colnames(data)==1)],collapse=" " ),"type must be numeric") ,style="font-size: 16px;")
            }else{
              data_class <- tags$p(icon("times"),paste("The content of column “Class” must be characters ,",paste("the content of column",total_text[which(total_text%in%colnames(data)==1)],collapse=" " ),"type must be numeric"),style="font-size: 16px;color:red;")
            } 
          }
        }
      }else{
        data_class <- NULL
      }
      if(nrow(data)==nrow(exp_data)){
        feature_number <- tags$p(icon("check"),"The row number of ‘Lipid characteristics’ table must same as ‘Lipid expression data’ table.",style="font-size: 16px;")
        if(sum(data[,1]%in%exp_data[,1])==nrow(data)){
          feature_content <- tags$p(icon("check"),"The lipids names (features) of ‘Lipid characteristics’ table must same as ‘Lipid expression data’ table.",style="font-size: 16px;")
        }else{
          feature_content <- tags$p(icon("times"),"The lipids names (features) of ‘Lipid characteristics’ table must same as ‘Lipid expression data’ table.",style="font-size: 16px;color:red;") 
        }
      }else{
        feature_number <- tags$p(icon("times"),"The row number of ‘Lipid characteristics’ table must same as ‘Lipid expression data’ table.",style="font-size: 16px;color:red;")
        feature_content <- NULL
      }
      if(ncol(dplyr::select(data,starts_with("FA_")))==0){
        check_FA <- tags$p(icon("exclamation"),"(OPTIONAL) Uploaded data does not contain column names starting with “FA_”",style="font-size: 16px;color:#ffad33;")
        check_FA_type <- NULL
      }else{
        FA_data <- data %>%dplyr::select(feature,starts_with("FA_"))
        FA_col <- grep("FA_",colnames(FA_data),value = TRUE)
        max_comma <- 0
        for(i in 1:length(FA_col)){
          col <- FA_col[i]
          comma_count <- max(str_count(FA_data[,col], ','),na.rm = T)
          if(comma_count>0){
            FA_data <- separate(FA_data,col,c(col,paste0(col,"_",1:comma_count)),",", convert = TRUE) 
          }
          if(comma_count>max_comma){max_comma <- comma_count}
        }
        FA_data <- FA_data%>%gather(lipid.category, lipid.category.value,-feature)
        if(max_comma>0){
          for (i in 1:max_comma) {
            select_name <- paste0("_",i)
            FA_data <-FA_data[-intersect(grep(select_name,FA_data[,"lipid.category"]),which(is.na(FA_data$lipid.category.value))),]
          }
        }
        if(class(FA_data$lipid.category.value)=="character"){
          check_FA <-  tags$p(icon("check"),"Uploaded data contains column names starting with “FA_”",style="font-size: 16px;")
          check_FA_type <- tags$p(icon("times"),"In the “FA_” related analyses, the values are positive integer or zero and separated by comma. i.e., 10,12,11",style="font-size: 16px;color:red;")
        }else if(sum(na.omit(as.numeric(FA_data$lipid.category.value))!=round(na.omit(as.numeric(FA_data$lipid.category.value))))==0 & min(na.omit(as.numeric(FA_data$lipid.category.value)))>=0){
          check_FA <-  tags$p(icon("check"),"Uploaded data contains column names starting with “FA_”",style="font-size: 16px;")
          check_FA_type <- tags$p(icon("check"),"In the “FA_” related analyses, the values are positive integer or zero and separated by comma. i.e., 10,12,11",style="font-size: 16px;")
        }else{
          check_FA <-  tags$p(icon("check"),"Uploaded data contains column names starting with “FA_”",style="font-size: 16px;")
          check_FA_type <- tags$p(icon("times"),"In the “FA_” related analyses, the values are positive integer or zero and separated by comma. i.e., 10,12,11",style="font-size: 16px;color:red;")
        }
      }
      
      return_div <- tags$div(
        tags$p(strong(title),style="font-size: 20px;"),
        tags$ul(
          feature_name,
          feature_uni,
          feature_number,
          feature_content,
          check_FA,
          check_FA_type,
          data_class
        ),
        style="font-size: 0px;"
      )
    }else{
      return_div <- tags$div(
        tags$p(strong(title),style="font-size: 20px;"),
        tags$ul(
          tags$p(icon("times"),"Upload your data table in .csv/.tsv.",style="font-size: 16px;color:red;")
        ),
        style="font-size: 0px;"
      )
    }
    
  }else if(!is.null(data) & data_type=="Continuous_condition"){
    title <- "Check Continuous condition"
    if(grepl('.csv$', file_path) | grepl('.tsv$', file_path)){
      if(class(data[,1])=="character"){
        sample_name <- tags$p(icon("check"),"The first column must be ‘sample_name’.",style="font-size: 16px;")
      }else{
        sample_name <- tags$p(icon("times"),"The first column must be ‘sample_name’.",style="font-size: 16px;color:red;")
      }
      if(ncol(data)==2){
        if(class(data[,1])=="character" & class(data[,-1])%in%c("numeric","integer")){
          column_type <- tags$p(icon("check"),"The columns ‘sample_name’ must be characters; the other columns must be numeric.",style="font-size: 16px;")
        }else{
          column_type <- tags$p(icon("times"),"The columns ‘sample_name’ must be characters; the other columns must be numeric.",style="font-size: 16px;color:red;")
        }
      }else{
        if(class(data[,1])=="character" & sum(sapply(data[,-1], class)%in%c("numeric","integer"))==ncol(data[,-1])){
          column_type <- tags$p(icon("check"),"The columns ‘sample_name’ must be characters; the other columns must be numeric.",style="font-size: 16px;")
        }else{
          column_type <- tags$p(icon("times"),"The columns ‘sample_name’ must be characters; the other columns must be numeric.",style="font-size: 16px;color:red;")
        } 
      }
      if(sum(data[,1]%in%colnames(exp_data))==nrow(data) & sum(data[,1]%in%colnames(exp_data))==ncol(exp_data[,-1])){
        sample_content <- tags$p(icon("check"),"‘sample_name’ must same as the name of samples of ‘Lipid expression data’",style="font-size: 16px;")
      }else{
        sample_content <- tags$p(icon("times"),"‘sample_name’ must same as the name of samples of ‘Lipid expression data’",style="font-size: 16px;color:red;")
      }
      if(ncol(data)>=3){
        conditions_number <- tags$p(icon("check"),"At least 2 conditions.",style="font-size: 16px;")
      }else{
        conditions_number <- tags$p(icon("times"),"At least 2 conditions.",style="font-size: 16px;color:red;")
      }
      if(nrow(data)>=3){
        samples_number <- tags$p(icon("check"),"At least 10 samples. ",style="font-size: 16px;")
      }else{
        samples_number <- tags$p(icon("times"),"At least 10 samples.",style="font-size: 16px;color:red;")
      }
      
      return_div <- tags$div(
        tags$p(strong(title),style="font-size: 20px;"),
        tags$ul(
          sample_name,
          column_type,
          sample_content,
          conditions_number,
          samples_number
        ),
        style="font-size: 0px;"
      )
    }else{
      return_div <- tags$div(
        tags$p(strong(title),style="font-size: 20px;"),
        tags$ul(
          tags$p(icon("times"),"Upload your data table in .csv/.tsv.",style="font-size: 16px;color:red;")
        ),
        style="font-size: 0px;"
      )
    }
    
  }else if(!is.null(data) & data_type=="adjusted"){
    title <- "Check adjusted table"
    if(grepl('.csv$', file_path) | grepl('.tsv$', file_path)){
      if(class(data[,1])=="character"){
        sample_name <- tags$p(icon("check"),"The first column must contain a list of lipids names (features).",style="font-size: 16px;")
        column_type <- tags$p(icon("check"),"The first column must be characters.",style="font-size: 16px;")
      }else{
        sample_name <- tags$p(icon("times"),"The first column must contain a list of lipids names (features).",style="font-size: 16px;color:red;")
        column_type <- tags$p(icon("times"),"The first column must be characters.",style="font-size: 16px;color:red;")
      }
      if(sum(data[,1]%in%colnames(exp_data))==nrow(data)&sum(data[,1]%in%colnames(exp_data))==ncol(exp_data[,-1])){
        sample_content <- tags$p(icon("check"),"The sample names of ‘Adjusted table’ must same as the sample names of ‘Lipid expression data’ table.",style="font-size: 16px;")
      }else{
        sample_content <- tags$p(icon("times"),"The sample names of ‘Adjusted table’ must same as the sample names of ‘Lipid expression data’ table.",style="font-size: 16px;color:red;")
      }
      
      return_div <- tags$div(
        tags$p(strong(title),style="font-size: 20px;"),
        tags$ul(
          sample_name,
          column_type,
          sample_content
        ),
        style="font-size: 0px;"
      )
    }else{
      return_div <- tags$div(
        tags$p(strong(title),style="font-size: 20px;"),
        tags$ul(
          tags$p(icon("times"),"Upload your data table in .csv/.tsv.",style="font-size: 16px;color:red;")
        ),
        style="font-size: 0px;"
      )
    }
    
  }else if(!is.null(data) & data_type=="ML_condition"){
    title <- "Check ML condition table "
    if(grepl('.csv$', file_path) | grepl('.tsv$', file_path)){
      if(ncol(data)==2){
        column_name <- tags$p(icon("check"),"The column must contain ‘sample_name’ (1st column), ‘group’ (2nd column)",style="font-size: 16px;")
        if(class(data[,1])=="character" & sum(data$group%in%c(0,1))==nrow(data)){
          column_type <- tags$p(icon("check"),"The columns ‘sample_name’ must be characters; the column ‘group’ must be numeric (only be 0 or 1).",style="font-size: 16px;")
          if(sum(table(data[,2])>=30)==2){
            group_number <- tags$p(icon("check"),"Each group must have more than 30 samples.",style="font-size: 16px;")
          }else{
            group_number <- tags$p(icon("times"),"Each group must have more than 30 samples.",style="font-size: 16px;color:red;")
          }
        }else{
          column_type <- tags$p(icon("times"),"The columns ‘sample_name’ must be characters; the column ‘group’ must be numeric (only be 0 or 1).",style="font-size: 16px;color:red;")
          group_number <- NULL
        }
      }else{
        column_name <- tags$p(icon("times"),"The column must contain ‘sample_name’ (1st column), ‘group’ (2nd column)",style="font-size: 16px;color:red;")
        column_type <- NULL
        group_number <- NULL
      }
      if(sum(data[,1]%in%colnames(exp_data))==nrow(data)&sum(data[,1]%in%colnames(exp_data))==ncol(exp_data[,-1])){
        sample_content <- tags$p(icon("check"),"‘sample_name’ must same as the name of samples of ‘Lipid expression data’",style="font-size: 16px;")
      }else{
        sample_content <- tags$p(icon("times")," ‘sample_name’ must same as the name of samples of ‘Lipid expression data’",style="font-size: 16px;color:red;")
      }
      
      return_div <- tags$div(
        tags$p(strong(title),style="font-size: 20px;"),
        tags$ul(
          column_name,
          column_type,
          group_number,
          sample_content
        ),
        style="font-size: 0px;"
      )
    }else{
      return_div <- tags$div(
        tags$p(strong(title),style="font-size: 20px;"),
        tags$ul(
          tags$p(icon("times"),"Upload your data table in .csv/.tsv.",style="font-size: 16px;color:red;")
        ),
        style="font-size: 0px;"
      )
    }
    
  }
  if(is.null(data)&is.null(exp_data)){
    if(data_type=="must_exp"){
      title <- "Check Expression Data"
      text <- tags$p(icon("radiation-alt"),"You need to uploaded Expression Data",style="font-size: 16px;color:red;") 
    }else if(data_type=="must_group"){
      title <- "Check Group information"
      text <- tags$p(icon("radiation-alt"),"You need to uploaded Group information",style="font-size: 16px;color:red;")
    }else if(data_type=="must_ML_condition"){
      title <- "Check Condition table"
      text <- tags$p(icon("radiation-alt"),"You need to uploaded Condition table",style="font-size: 16px;color:red;")
    }else if(data_type=="must_Continuous_condition"){
      title <- "Check Condition table"
      text <- tags$p(icon("radiation-alt"),"You need to uploaded Condition table",style="font-size: 16px;color:red;")
    }else if(data_type=="optional_lipid_char"){
      title <- "Check lipid_char_table"
      text <-  tags$p(icon("exclamation"),"Not uploaded",style="font-size: 16px;color:#ffad33;")
    }else if(data_type=="optional_adjusted"){
      title <- "Check adjusted table"
      text <-  tags$p(icon("exclamation"),"Not uploaded",style="font-size: 16px;color:#ffad33;")
    } 
    return_div <-tags$div(
      tags$p(strong(title),style="font-size: 20px;"),
      tags$ul(
        text
      ),
      style="font-size: 0px;"
    )
  }
  if(length(grep("times",as.character(return_div)))==0){
    check_all_right <- TRUE 
  }else{
    check_all_right <- FALSE
  }
  
  return(list(return_div,check_all_right))
}