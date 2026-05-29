####################################
# author: Hcliu
# name: dataCheck-otherpage.R
# created date: 2026/03/24
# revise date:
# description: Data validation functions for the LipidSig web application, organized into three
#              sequential steps and two auxiliary checks. Step 1 (check_utf8Format) verifies
#              UTF-8 encoding of all input tables before upload proceeds. Step 2 (inputFormat)
#              validates data frame structure, including first-column type, feature uniqueness,
#              sample count, and group/condition table format. Step 3 (data_check) performs
#              numeric value conversion, constant-value feature/sample removal, missing value
#              assessment, and lipid name recognition via rgoslin, then constructs a
#              SummarizedExperiment object for downstream analysis.
#              submit.check performs pre-submission validation to confirm minimum feature count,
#              significant lipid count, and absence of NA feature names before launching analysis.
#              check.despse validates a deSp SummarizedExperiment object for required metadata
#              fields, significance criteria, and transformation settings prior to differential
#              expression species visualization.
# input: abundance, group_info, condition_table, adjusted_table, analysis_type, nGroup
# output: varies by function — see individual function headers below
####################################

### step1. UTF-8 encoding check
# analysis_type =c("Profiling", "DE","ML", "Correlation")
# nGroup=c("two", "multiple") 2群/多群(只有DE會有多群)
# input: abundance, group_info, condition_table, adjusted_table, analysis_type, nGroup
# output: TRUE for correct, message for incorrect

# Profiling: check_utf8Format(profiling_abundance, group_info=NULL, condition_table=NULL, adjusted_table=NULL, analysis_type="Profiling", nGroup="two")
# DE: check_utf8Format(DE_abundance, DE_group_info, condition_table=NULL, adjusted_table=NULL, analysis_type="DE", nGroup="two")
# ML: check_utf8Format(ML_abundance, group_info=NULL, ML_condition_table, adjusted_table=NULL, analysis_type="ML", nGroup="two")
# correlation: check_utf8Format(corr_abundance, group_info=NULL, corr_condition_table, corr_adjusted_table, analysis_type="Correlation", nGroup="two")
check_utf8Format <- function(
    abundance, group_info=NULL, condition_table=NULL, adjusted_table=NULL,
    abundance_path, group_info_path=NULL, condition_table_path=NULL,
    adjusted_table_path=NULL, analysis_type="DE", nGroup="two",
    variables=NULL, session=NULL) {
  
  check_note <- .utf8Check(abundance)
  ## check group_info
  if (analysis_type == "DE"){
    check_note <- append(.utf8Check(group_info), check_note)
  }
  ## check condition_table
  if(analysis_type %in% c("ML", "Correlation")){
    check_note <- append(.utf8Check(condition_table), check_note)
  }
  ## adjusted_table
  if (analysis_type=="Correlation"){
    if (!is.null(adjusted_table)){
      check_note <- append(.utf8Check(adjusted_table), check_note)
    }
  }
  
  ## print note
  if (all(check_note)) {
    return(list(logical=TRUE, return_div=NULL))
  } else {
    url <- .reupload(
      abundance, group_info, condition_table, adjusted_table, abundance_path,
      group_info_path, condition_table_path, adjusted_table_path,
      analysis_type, nGroup, variables=variables, session=session)
    return_div <- shiny::tags$div(
      shiny::tags$p(
        shiny::icon("times"), "Incorrect input data format. Please visit the ", url, ". for a detailed examination.",
        style="font-size: 16px;color:red;"))
    return(list(logical=FALSE, return_div=return_div))
  }
}

### step2. data frame format & group info
# analysis_type =c("Profiling", "DE","ML", "Correlation")
# nGroup=c("two", "multiple") 2群/多群(只有DE會有多群)
# input: abundance, group_info, condition_table, adjusted_table, analysis_type, nGroup
# output: TRUE for correct, message for incorrect

# Profiling: inputFormat(profiling_abundance, group_info=NULL, condition_table=NULL, adjusted_table=NULL, analysis_type="Profiling", nGroup="two")
# DE: inputFormat(DE_abundance, DE_group_info, condition_table=NULL, adjusted_table=NULL, analysis_type="DE", nGroup="two")
# ML: inputFormat(ML_abundance, group_info=NULL, ML_condition_table, adjusted_table=NULL, analysis_type="ML", nGroup="two")
# correlation: inputFormat(corr_abundance, group_info=NULL, corr_condition_table, corr_adjusted_table, analysis_type="Correlation", nGroup="two")
inputFormat <- function(
    abundance, group_info=NULL, condition_table=NULL, adjusted_table=NULL,
    abundance_path, group_info_path=NULL, condition_table_path=NULL, adjusted_table_path=NULL,
    analysis_type="DE", nGroup="two",
    variables=NULL, session=NULL) {
  ## abundance first column is character
  check_note <- .check_all_character(abundance[1])
  ## no duplicates & NA in abundance features
  check_note <- append(.check_no_duplicates(abundance[1]), check_note)
  check_note <- append(.check_no_NA(abundance[1]), check_note)
  ## check samples number
  if (analysis_type %in% c("Profiling", "DE", "ML", "Correlation")) {
    sample_limit <- switch(
      analysis_type, "Profiling"=2, "DE"=2, "ML"=60,
      "Correlation"=10
    )
    check_note <- append(
      .check_colNum(abundance[-1], sample_limit), check_note)
  }
  ## check group_info
  if (analysis_type=="DE"){
    check_note <- .check_group_info(abundance, group_info, nGroup, check_note)
  }
  ## check condition_table
  if(analysis_type %in% c("ML", "Correlation")){
    check_note <- .check_condition_table(
      abundance, condition_table, analysis_type, check_note)
  }
  ## adjusted_table
  if (analysis_type=="Correlation"){
    if (!is.null(adjusted_table)){
      check_note <- .check_adjusted_table(abundance, adjusted_table, check_note)
    }
  }
  ## print note
  if (all(check_note)) {
    return(list(logical=TRUE, return_div=NULL))
  } else {
    url <- .reupload(abundance, group_info, condition_table, adjusted_table,
                               abundance_path, group_info_path, condition_table_path, adjusted_table_path,
                               analysis_type, nGroup, variables=variables, session=session)
    if(analysis_type=="DE") {
      return_div <- shiny::tags$div(
        shiny::tags$p(
          shiny::icon("times"), "Incorrect input data format. First, please double-check that you selected the appropriate 'Number of groups' from the drop-down menu.
               For a more detailed examination, please visit the ", url, ".",
          style="font-size: 16px;color:red;"))
    }else{
      return_div <- shiny::tags$div(
        shiny::tags$p(
          shiny::icon("times"), "Incorrect input data format. Please visit the ", url, ". for a detailed examination.",
          style="font-size: 16px;color:red;"))
    }
    return(list(logical=FALSE,
                return_div=return_div))
  }
}

### step3. numeric value ＆build SE object / ID conversion & Char_table / print messages
# missing values with constant values (i.e. all zero or all NA)
# features with a constant or single value across samples
# samples with a constant or single value across features
# missing value percentage
# number of unmatched lipids
data_check <- function(
    abundance, group_info=NULL, ref_group=NULL, condition_table=NULL, adjusted_table=NULL,
    abundance_path, group_info_path=NULL, condition_table_path=NULL, adjusted_table_path=NULL,
    analysis_type="DE", nGroup=NULL,
    variables=NULL, session=NULL) {
  title <- "Check input data"
  ## abundance first column names is "feature"
  colnames(abundance)[1] <- "feature"
  ## abundance values must numeric
  if (isFALSE(.check_all_numeric(abundance[-1])) ){
    abundance[-1] <- sapply(abundance[-1], as.numeric)
    convert_value <- shiny::tags$p(shiny::icon("exclamation"), "Converting character values of lipid abundance into numeric values.", style="font-size: 16px;color:blue;")
  } else {
    abundance[-1] <- sapply(abundance[-1], as.numeric)
    convert_value <- shiny::tags$p(shiny::icon("check"), "All lipid abundance values are numeric.", style="font-size: 16px;")
  }
  abundance[abundance==0] <- NA
  ## missing value percentage
  missing_percent <- round(mean(is.na(abundance[-1])) * 100, digits=2)
  if (missing_percent==0) {
    miss_value <- shiny::tags$p(shiny::icon("check"), "No missing values.", style="font-size: 16px;")
  } else {
    miss_value <- shiny::tags$p(shiny::icon("exclamation"), paste0(missing_percent, "% missing values."), style="font-size: 16px;color:blue;")
  }
  ## feature to row names
  abundance_raw <- abundance %>% tibble::remove_rownames() %>%
    tibble::column_to_rownames(var="feature")
  ## features with a constant or single value across samples
  abundance_rmR <- .rm_constant_feature(abundance_raw)
  if (nrow(abundance_rmR) < nrow(abundance_raw)) {
    rm_rowNum <- nrow(abundance_raw) - nrow(abundance_rmR)
    rm_feature <- shiny::tags$p(shiny::icon("exclamation"), paste0("Remove ", rm_rowNum, "/", nrow(abundance), " features with constant values (including all NAs and all zeros)."), style="font-size: 16px;color:blue;")
  } else {
    rm_feature <- shiny::tags$p(shiny::icon("check"), "No features with constant values (including all NAs and all zeros).", style="font-size: 16px;")
  }
  ## samples with a constant or single value across features
  abundance_filt <- .rm_constant_sample(abundance_rmR)
  if (ncol(abundance_filt) < ncol(abundance_raw)) {
    rm_colNum <- ncol(abundance_raw) - ncol(abundance_filt)
    sample_limit <- switch(
      analysis_type, "Profiling"=2, "DE"=2, "ML"=60, "Correlation"=10 )
    if (isTRUE(.check_colNum(abundance_filt[-1], sample_limit))) {
      rm_sample <- shiny::tags$p(shiny::icon("exclamation"), paste0("Remove ", rm_colNum, "/", nrow(abundance), " samples with constant values (including all NAs and all zeros)."), style="font-size: 16px;color:blue;")
    } else {
      ## after remove the samples is not enough for analysis
      rm_sample <- shiny::tags$p(shiny::icon("times"), paste0("Remove ", rm_colNum, "/", nrow(abundance), " samples with constant values (including all NAs and all zeros). Not enough samples for further analysis."), style="font-size: 16px;color:red;")
    }
  } else {
    rm_sample <- shiny::tags$p(shiny::icon("check"), "No samples with constant values (including all NAs and all zeros).", style="font-size: 16px;")
  }
  abundance_se <- abundance_filt %>% tibble::rownames_to_column(var="feature")
  
  ## group_info 1-3 column must be character
  if (!is.null(group_info)) {
    group_info[1:3] <- lapply(group_info[1:3], as.character)
    group_info_filt <- group_info %>%
      dplyr::filter(sample_name %in% colnames(abundance_filt))
  }
  ## id convert
  parse_lipid <- suppressMessages(rgoslin::parseLipidNames(lipidNames=abundance_se$feature))
  # filter lipid recognized by rgoslin
  paired_sample <- NULL
  if(analysis_type=='Profiling'){
    se_type <- 'profiling'
    secol <- NULL
    group_info <- NULL
  }else if(analysis_type=='DE'){
    se_type <- ifelse(nGroup=="two",'de_two','de_multiple')
    secol <- group_info
    paired_sample <- if(nGroup=="two"){ any(!is.na(group_info$pair)) }
  }else if(analysis_type=='ML'){
    se_type <- 'ml'
    secol <- condition_table
  }else if(analysis_type=='Correlation'){
    se_type <- 'corr'
    #secol <- merge(condition_table,adjusted_table)
    #secol <- dplyr::left_join(condition_table, adjusted_table)
    if(!is.null(adjusted_table)){
      secol <- merge(condition_table, adjusted_table, by="sample_name", sort=FALSE)
    }else{
      secol <- condition_table
    }
    
  }
  recognized_lipid <- parse_lipid$Original.Name[which(parse_lipid$Grammar != 'NOT_PARSEABLE')]
  abundance_se <- abundance_se %>% 
    dplyr::filter(feature %in% recognized_lipid)
  goslin_annotation <- parse_lipid %>% 
    dplyr::filter(Original.Name %in% recognized_lipid)
  if(any(parse_lipid$Grammar == 'NOT_PARSEABLE')){
    lipid_unmatched <- parse_lipid %>% 
      dplyr::filter(parse_lipid$Grammar == 'NOT_PARSEABLE') %>%
      dplyr::pull(Original.Name) %>% length()
    lipid_id_pct <- (lipid_unmatched/nrow(abundance_filt))*100
    url <- .reupload(abundance, group_info, condition_table, adjusted_table,
                     abundance_path, group_info_path, condition_table_path, adjusted_table_path,
                     analysis_type, nGroup, variables=variables, session=session)
    if ((nrow(abundance_filt)-lipid_unmatched) < 20 ) {
      id_convert <- shiny::tags$p(shiny::icon("times"), paste0("Remove ", lipid_unmatched, "/", nrow(abundance_filt)), " unrecognized lipid names. View the detailed information by verifying your input on the ", url, "There must be at least 20 features remaining for further analysis. For lipid species naming format, please refer to the", htmltools::HTML("<a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ12' target='_blank' style='color: darkblue;'>FAQ</a>."), style="font-size: 16px;color:red;")
      SE <- NULL
    } else {
      id_convert <- shiny::tags$p(shiny::icon("exclamation"), paste0("Remove ", lipid_unmatched, "/", nrow(abundance_filt)), " unrecognized lipid names. View the detailed information by verifying your input on the", url, style="font-size: 16px;color:blue;")
      SE <- LipidSigR::as_summarized_experiment(
        abundance_se, goslin_annotation, group_info=secol, 
        se_type=se_type, paired_sample=paired_sample)
    }
  }else{
    id_convert <- shiny::tags$p(shiny::icon("check"), "All lipid names are recognized.", style="font-size: 16px;")
    lipid_id_pct <- 0
    SE <- LipidSigR::as_summarized_experiment(
      abundance_se, goslin_annotation, group_info=secol, 
      se_type=se_type, paired_sample=paired_sample)
  }
  
  #> Encountered an error while parsing 'Chol 27:1;0': Expecting a single string value: [type=character; extent=4].
  
  
  return_div <- shiny::tags$div(
    shiny::tags$p(shiny::strong(title),style="font-size: 20px;"),
    shiny::tags$ul(
      convert_value, miss_value, rm_feature, rm_sample, id_convert),
    style="font-size: 0px;"
  )
  return(list(rawSE=SE, return_div=return_div, lipid_id_pct=lipid_id_pct))
}

### submit.check. pre-submission validation for downstream analysis
# Checks whether the input data meets the minimum requirements for a given analysis,
# including sufficient feature count, minimum number of significant lipids, and absence of NA in feature names.
# input: abundance, sig_result, check.NA, Nfeature, Nsig
# output: list(logic, message) — logic=TRUE if any check fails (blocking submission), message contains HTML error list
submit.check <- function(abundance=NULL, sig_result=NULL, check.NA=TRUE, Nfeature=2, Nsig=NULL){
  message <- feature.message <- Nsig.message <- NA.message <- NULL
  if(!is.null(abundance)){
    abundance_sd <- apply(abundance[,-1],1,function(x) sd(x, na.rm=TRUE))
    remove_count <- sum(na.omit(abundance_sd) == 0)
    if(nrow(abundance)-remove_count < Nfeature){
      feature.message <- htmltools::tags$li('Less than ',Nfeature,' features')
    }
  }
  if(!is.null(sig_result) & !is.null(Nsig)){
    if(nrow(sig_result) < Nsig){
      Nsig.message <- htmltools::tags$li(paste('Less than', Nsig ,'significant lipid features'))
    }
  }
  if(check.NA &  any(is.na(abundance))){
    NA.message <- htmltools::tags$li('The lipids name (features) can not contains NA')
  }
  if(!is.null(feature.message) | !is.null(Nsig.message) | !is.null(NA.message)){
    logic <- TRUE
    message <- htmltools::tags$ul(feature.message, Nsig.message,NA.message)
  }else{
    logic <- FALSE
  }
  return(list(logic=logic,message=message))
}

### check.despse. validate SummarizedExperiment object for differential expression species (deSp) analysis
# Verifies that the input SE object is a valid SummarizedExperiment constructed by as_summarized_experiment
# or an upstream analysis function, and that it contains the required metadata fields for deSp analysis,
# including significance criteria and transformation settings.
# input: deSp_se (SummarizedExperiment object), nGroups=c("two", "multiple")
# output: return_div (shiny HTML validation summary message)
check.despse <- function(deSp_se, nGroups=c('two', 'multiple')){
  tryCatch(
    {
      if (isFALSE(inherits(deSp_se, "SummarizedExperiment")) | 
          all(dim(deSp_se) == c(0, 0)) | 
          any(length(SummarizedExperiment::assays(deSp_se))==0,
              length(SummarizedExperiment::rowData(deSp_se))==0,
              length(SummarizedExperiment::colData(deSp_se))==0) | 
          isFALSE(all(c("assays", "colData", "elementMetadata") %in% slotNames(deSp_se)))) {
        check.se <- shiny::tags$p(shiny::icon("times"), 
                                  "The input must be a SummarizedExperiment object construct by as_summarized_experiment function or output from upstream analysis function.",
                                  style="font-size: 16px;color:red;")
      }else{
        check.se <- shiny::tags$p(shiny::icon("check"), 
                                  "The input must be a SummarizedExperiment object construct by as_summarized_experiment function or output from upstream analysis function.",
                                  style="font-size: 16px;")
      }
      check.metadata <- TRUE
      metadata_list <- switch(nGroups, 
                              two=c('all_deSp_result', 'sig_deSp_result', 'significant', 'p_cutoff', 'FC_cutoff', 'transform'),
                              multiple=c('all_deSp_result', 'sig_deSp_result', 'significant', 'p_cutoff', 'transform'))
      for (i in seq(metadata_list)) {
        if (is.null(S4Vectors::metadata(deSp_se)[[metadata_list[i]]])) {
          check.metadata <- FALSE
        }
      }
      if (!check.metadata) {
        check.metadata <- shiny::tags$p(shiny::icon("times"), 
                                        "Correct SummarizedExperiment metadata not found, please use output from upstream analysis function.",
                                        style="font-size: 16px;color:red;")
        de.condition <- NULL
        de.transformation <- NULL
      }else{
        check.metadata <- shiny::tags$p(shiny::icon("check"), 
                                        "Find the correct SummarizedExperiment metadata.",
                                        style="font-size: 16px;")
        if(nGroups == 'two'){
          de.condition <- shiny::tags$p(shiny::icon("check"), 
                                        paste0("Significant criteria: ",
                                               ifelse(S4Vectors::metadata(deSp_se)$significant=="pval",'P-value','Adjusted p-value'),' < ', S4Vectors::metadata(deSp_se)$p_cutoff,
                                               ' & Fold change (FC) > ',S4Vectors::metadata(deSp_se)$FC_cutoff),
                                        style="font-size: 16px;")
        }else{
          de.condition <- shiny::tags$p(shiny::icon("check"), 
                                        paste0("Significant criteria: ",
                                               ifelse(S4Vectors::metadata(deSp_se)$significant=="pval",'P-value','Adjusted p-value'),' < ', S4Vectors::metadata(deSp_se)$p_cutoff),
                                        style="font-size: 16px;")
        }
        if (S4Vectors::metadata(deSp_se)$transform=='none') {
          de.transformation <- shiny::tags$p(shiny::icon("check"), "The data not underwent any transformation.",style="font-size: 16px;")
        } else {
          de.transformation <- shiny::tags$p(shiny::icon("check"), paste0("The data underwent ", .transform_name(S4Vectors::metadata(deSp_se)$transform), " transformation."),style="font-size: 16px;") 
        }
      }
      return_div<- shiny::tags$div(
        shiny::tags$p(shiny::strong("Review the data on differential expression."),style="font-size: 20px;"),
        shiny::tags$ul(check.se, check.metadata, de.condition, de.transformation), 
        style="font-size: 0px;")
    },
    error = function(e) {
      return_div <- shiny::tags$div(
        shiny::tags$p(shiny::strong("Review the data on differential expression."),style="font-size: 20px;"),
        shiny::tags$ul(shiny::tags$p(shiny::icon("times"), 
                                     "The input must be a SummarizedExperiment object construct by as_summarized_experiment function or output from upstream analysis function.",
                                     style="font-size: 16px;color:red;"), 
                       shiny::tags$p(shiny::icon("times"), 
                                     "Correct SummarizedExperiment metadata not found, please use output from upstream analysis function.",
                                     style="font-size: 16px;color:red;")),
        style="font-size: 0px;")
      return(return_div)
    }
  )
  return(return_div)
} #Function: checkEnrichmentInput