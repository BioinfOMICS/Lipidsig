####################################
# author: Hcliu
# name: dataSummary.R
# created date: 2026/03/24
# revise date: 
# desciption: Summarizes the data processing steps applied to the SE object, including sample numbers, group information, missing value handling, normalization, and transformation.
# input: input: se, analysis_type, nGroup, exclude_missing, exclude_missing_pct, replace_na_method, replace_na_method_ref, normalization, transform
# output: return_div (shiny HTML summary message)

####################################

data_summary <- function(
    se, analysis_type="DE", nGroup="two", exclude_missing=TRUE,
    exclude_missing_pct=50,replace_na_method='min',
    replace_na_method_ref=0.5, normalization='Percentage',
    transform='log10'){
  ## data process
  processed_se <- LipidSigR::data_process(se, exclude_missing,
                                          exclude_missing_pct, replace_na_method, replace_na_method_ref,
                                          normalization, transform)
  abundance <- as.data.frame(SummarizedExperiment::assay(se)) %>%
    tibble::rownames_to_column("feature")
  abundance_process <- S4Vectors::metadata(processed_se)$processed_abund
  ## sample numbers
  sampleNum <- ncol(abundance[-1])
  sample_number <- shiny::tags$p(shiny::icon("caret-right"), paste0("Sample numbers: ", sampleNum), style="font-size: 16px;")
  ## group number & paired or not
  if (analysis_type=="DE") {
    group_info <- as.data.frame(SummarizedExperiment::colData(se))
    groupNum <- length(unique(group_info$group))
    group_number <- shiny::tags$p(shiny::icon("caret-right"), paste0("Group numbers: ", groupNum), style="font-size: 16px;")
    if (nGroup=="two"){
      ## not paired
      if (all(is.na(group_info$pair)) ) {
        is_paired <- shiny::tags$p(shiny::icon("caret-right"), paste0("Not paired samples.", groupNum), style="font-size: 16px;")
      } else { ## paired
        is_paired <- shiny::tags$p(shiny::icon("caret-right"), paste0("Paired samples.", groupNum), style="font-size: 16px;")
      }
    }
  } else if (analysis_type=="ML") {
    group_number <- shiny::tags$p(shiny::icon("caret-right"), "Group numbers: 2", style="font-size: 16px;")
  } else {
    group_number <- NULL
  }
  is_paired <- NULL
  ## data processing message
  ## lipid before
  lipid_before <- shiny::tags$p(shiny::icon("caret-right"), paste0("Numbers of lipid (feature) before data processing: ", nrow(abundance)), style="font-size: 16px;")
  ## missing value
  rm_rowNum <- nrow(abundance)-nrow(abundance_process)
  if (isTRUE(exclude_missing)) {
    exclude_missing <- shiny::tags$p(shiny::icon("caret-right"), paste0("Remove lipids (features) with more than ", as.numeric(exclude_missing_pct),"% missing values.", " (Remove ", rm_rowNum," rows)"), style="font-size: 16px;")
  } else {
    exclude_missing <- shiny::tags$p(shiny::icon("caret-right"), paste0("Lipids (features) with numerous missing values were not removed."), style="font-size: 16px;")
  }
  ## lipid after
  lipid_after <- shiny::tags$p(shiny::icon("caret-right"), paste0("Numbers of lipid (feature) after data processing: ", nrow(abundance_process)), style="font-size: 16px;")
  ## replace NA
  replaceNA <- shiny::tags$p(shiny::icon("caret-right"), paste0("Fill missing value with the ", replace_na_method, " value of data."), style="font-size: 16px;")
  replace_ref <- shiny::tags$p(shiny::icon("caret-right"), paste0("Reference for the method of replacing NA values: ", replace_na_method_ref), style="font-size: 16px;")
  ## normalization
  if (normalization=='none') {
    normalization <- shiny::tags$p(shiny::icon("caret-right"), "The data not underwent any normalization.", style="font-size: 16px;")
  } else {
    nor_name <- .normalization_name(normalization)
    normalization <- shiny::tags$p(shiny::icon("caret-right"), paste0("The data underwent ", nor_name), style="font-size: 16px;")
  }
  ## transform type
  if (transform=='none') {
    transformation <- shiny::tags$p(shiny::icon("caret-right"), "The data not underwent any transformation.", style="font-size: 16px;")
  } else {
    transform_name <- .transform_name(transform)
    transformation <- shiny::tags$p(shiny::icon("caret-right"), paste0("The data underwent ", transform_name, " transformation."), style="font-size: 16px;")
  }
  ## return message
  title <- "Data summary & data quality"
  return_div <- shiny::tags$div(
    shiny::tags$p(shiny::strong(title),style="font-size: 20px;"),
    shiny::tags$ul(
      sample_number, group_number, is_paired, lipid_before,
      exclude_missing, lipid_after, replaceNA, replace_ref, normalization,
      transformation),
    style="font-size: 0px;"
  )
  
  return(return_div)
}

## name convert
.transform_name <- function(transform){
  if (transform=="log10") {
    transform_name <- "Log 10"
  } else if (transform=="square") {
    transform_name <- "Square root"
  } else if (transform=="cube") {
    transform_name <- "Cube root"
  }
  return(transform_name=transform_name)
}

.normalization_name <- function(normalization){
  if (normalization=="Percentage") {
    nor_name <- "percentage normalization."
  } else if (normalization=="PQN") {
    nor_name <- "probabilistic quotient normalization."
  } else if (normalization=="Quantile") {
    nor_name <- "quantile normalization."
  } else if (normalization=="Sum") {
    nor_name <- "normalization by sum."
  } else if (normalization=="Median") {
    nor_name <- "normalization by median."
  } else if (normalization=="MeanCenter") {
    nor_name <- "mean centering."
  } else if (normalization=="AutoScaling") {
    nor_name <- "auto-scaling normalization."
  } else if (normalization=="ParetoScaling") {
    nor_name <- "Pareto scaling normalization."
  } else if (normalization=="RangeScaling") {
    nor_name <- "range-scaling normalization."
  } else if (normalization=="VastScaling") {
    nor_name <- "Vast Scaling normalization."
  } else if (normalization=="LevelScaling") {
    nor_name <- "level-scaling normalization."
  }
  return(nor_name=nor_name)
}