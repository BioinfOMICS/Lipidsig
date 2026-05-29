####################################
# author: Hcliu
# name: utils-dataCheck.R
# created date: 2026/03/24
# revise date: 
# desciption: data check web page
# input: exp_data, group_info, condition_table, adjusted_table, analysis_type, nGroup
# output:
####################################

## ============================================================================
## UTF-8 Encoding Check
## ============================================================================

.utf8Check <- function(data){
  bad <- lapply(data, function(x) {
    if (is.character(x)) which(!stringi::stri_enc_isutf8(x)) else integer(0)
  })
  return(all(sapply(bad, length) == 0))
}

## ============================================================================
## Column Name Validation
## ============================================================================

.check_colName <- function(df, col, strict=TRUE) {
  ## Check if dataframe column names match expected column names
  ## @param df: data frame to check
  ## @param col: expected column names (vector of strings)
  ## @param strict: if TRUE, requires exact match; if FALSE, allows subset match
  ## @return: TRUE if columns match criteria, FALSE otherwise
  if (strict==TRUE) {
    identical(colnames(df), col)
  } else {
    all(colnames(df) %in% col) && length(colnames(df)) == length(col)
  }
}


## ============================================================================
## Missing Value Checks
## ============================================================================

.check_no_NA <- function(df){
  ## Check if dataframe contains any NA values
  ## @param df: data frame to check
  ## @return: TRUE if no NAs present, FALSE otherwise
  if (anyNA(df)) {
    return(FALSE)
  }
  return(TRUE)
}


## ============================================================================
## Duplicate Value Checks
## ============================================================================

.check_no_duplicates <- function(x) {
  ## Check if vector contains any duplicate values
  ## @param x: vector to check
  ## @return: TRUE if no duplicates, FALSE otherwise
  if (any(duplicated(x))) {
    return(FALSE)
  }
  return(TRUE)
}


## ============================================================================
## Group Information Validation
## ============================================================================

.check_pair_group <- function(group_info){
  ## Validate pairing information in group_info dataframe
  ## For paired samples, pair column must be sequentially numbered from 1 to N
  ## @param group_info: dataframe with group information
  ## @return: TRUE if pairing is valid, FALSE otherwise
  if (ncol(group_info)==4) {
    if (isTRUE(all(is.na(group_info[[4]]))) ) {
      return(TRUE)
    } else {
      ## have pair samples
      pair_num <- 1:(nrow(group_info)/2)
      if (isTRUE(identical(as.integer(unique(group_info[[4]])), pair_num))) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  } else {
    return(FALSE)
  }
}

.check_groups <- function(group_info, nGroup){
  ## Validate number of groups and samples per group
  ## @param group_info: dataframe with group information (3rd column is group)
  ## @param nGroup: expected number of groups ("two" or "multiple")
  ## @return: TRUE if group structure is valid, FALSE otherwise
  if (ncol(group_info)>=3) { ## has group column
    ## > group_num
    if (nGroup=="two"){
      groupCol <- group_info[[3]][!is.na(group_info[[3]])]
      if (length(unique(groupCol))==2) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else if (nGroup=="multiple") {
      groupCol <- group_info[[3]][!is.na(group_info[[3]])]
      if (length(unique(groupCol))>2) {
        ## check for more than 2 sample in each group
        check_sample <- c()
        for (i in seq(unique(groupCol)) ) {
          if (nrow(group_info[groupCol==unique(groupCol)[i] , ]) > 1) {
            check_sample <- append(check_sample, TRUE)
          } else {
            check_sample <- append(check_sample, FALSE)
          }
        }
        if (all(check_sample)) {
          return(TRUE)
        } else {
          return(FALSE)
        } # end sample check
      } else {
        return(FALSE)
      }
    } # end multiple
  } else {
    return(FALSE)
  }
}


## ============================================================================
## Dimension Validation
## ============================================================================

.check_colNum <- function(df, col_num){
  ## Check if dataframe has at least col_num columns
  ## @param df: data frame to check
  ## @param col_num: minimum required number of columns
  ## @return: TRUE if ncol(df) >= col_num, FALSE otherwise
  if (ncol(df) >= col_num) {
    return(TRUE)
  }
  return(FALSE)
}

.check_rowNum <- function(df, row_num){
  ## Check if dataframe has at least row_num rows
  ## @param df: data frame to check
  ## @param row_num: minimum required number of rows
  ## @return: TRUE if nrow(df) >= row_num, FALSE otherwise
  if (nrow(df) >= row_num) {
    return(TRUE)
  }
  return(FALSE)
}


## ============================================================================
## Data Type Validation
## ============================================================================

.check_all_numeric <- function(df){
  ## Check if all columns in dataframe are numeric
  ## @param df: data frame to check
  ## @return: TRUE if all columns are numeric, FALSE otherwise
  if (all(sapply(df, is.numeric)) ){
    return(TRUE)
  }
  return(FALSE)
}

.check_any_integer64 <- function(df){
  ## Check if any column in dataframe is integer64 type
  ## @param df: data frame to check
  ## @return: TRUE if any integer64 column found, FALSE otherwise
  if (any(sapply(df, bit64::is.integer64)) ){
    return(TRUE)
  }
  return(FALSE)
}

.check_all_character <- function(df){
  ## Check if all columns in dataframe are character type
  ## @param df: data frame to check
  ## @return: TRUE if all columns are character, FALSE otherwise
  if (all(sapply(df, is.character)) ){
    return(TRUE)
  }
  return(FALSE)
}

.check_all_character_value <- function(df){
  ## Check if all column values are actual character values (not numeric strings)
  ## @param df: data frame to check
  ## @return: TRUE if all values are non-numeric characters, FALSE otherwise
  if (isTRUE(.check_all_character(df)) ){
    all(sapply(df, function(col) {
      non_numeric <- all(!grepl("^[+-]?\\d*\\.?\\d*$", col[!is.na(col)]))
      return(non_numeric)
    }) )
  } else {
    return(FALSE)
  }
}

.check_numeric_01 <- function(col){
  ## Check if all values in column are only 0 or 1 (binary)
  ## @param col: vector to check
  ## @return: TRUE if all values are 0 or 1, FALSE otherwise
  col <- as.numeric(col)
  if (all(col %in% c(0, 1)) ){
    return(TRUE)
  }
  return(FALSE)
}


## ============================================================================
## Constant/Zero Value Filtering
## ============================================================================

.rm_constant_sample <- function(data) {
  ## Remove samples (columns) with constant values or all zeros/NAs
  ## @param data: matrix or data frame with samples as columns
  ## @return: filtered data with constant-value columns removed
  constant_cols <- sapply(data, function(col) {
    all_NA0 <- all(col == 0 | is.na(col))
    constant_val <- length(unique(col[!is.na(col)])) <= 1 && length((col[!is.na(col)])) > 1
    any(constant_val, all_NA0)
  })
  data_filt <- data[, !constant_cols, drop = FALSE]
  return(data_filt=data_filt)
}

.rm_constant_feature <- function(data) {
  ## Remove features (rows) with constant values or all zeros/NAs
  ## @param data: matrix or data frame with features as rows
  ## @return: filtered data with constant-value rows removed
  constant_rows <- apply(data, 1, function(row) {
    all_NA0 <- all(row == 0 | is.na(row))
    constant_val <- length(unique(row[!is.na(row)])) <= 1 && length((row[!is.na(row)])) > 1
    any(constant_val, all_NA0)
  })
  data_filt <- data[!constant_rows, , drop = FALSE]
  return(data_filt=data_filt)
}


## ============================================================================
## Group Information Comprehensive Validation
## ============================================================================

.check_group_info <- function(abundance, group_info, nGroup, check_note){
  ## Comprehensive validation of group_info dataframe
  ## Checks column numbers, column names, duplicates, NAs, and group structure
  ## @param abundance: abundance dataframe with samples as columns (excluding feature column)
  ## @param group_info: group information dataframe with columns: sample_name, label_name, group, [pair]
  ## @param nGroup: expected number of groups ("two" or "multiple")
  ## @param check_note: vector of existing check results to append to
  ## @return: vector of logical check results
  ## correct column number
  if (ncol(group_info)==3 | ncol(group_info)==4) {
    ## abundance colnames match group_info sample_name
    check_note <- append(
      .check_colName(abundance[-1], group_info[[1]], strict=FALSE), check_note)
    ## group_info sample names no duplicate
    check_note <- append(.check_no_duplicates(group_info[1]), check_note)
    ## no NA value in "sample_name", "label_name", "group"
    check_note <- append(.check_no_NA(group_info[, 1:3]) , check_note)
    ## group number / samples
    if (nGroup=="two") {
      ## group_info column names correct
      check_note <- append(
        .check_colName(
          group_info, c("sample_name", "label_name", "group", "pair"), strict=TRUE),
        check_note)
      ## paired
      check_note <- append(.check_pair_group(group_info), check_note)
      check_note <- append(.check_groups(group_info, nGroup), check_note)
    } else if (nGroup=="multiple") {
      ## group_info column names correct
      check_note <- append(
        .check_colName(
          group_info, c("sample_name", "label_name", "group"), strict=TRUE),
        check_note)
      check_note <- append(.check_groups(group_info, nGroup), check_note)
    }
  } else {
    check_note <- append(FALSE, check_note)
  }
  return(check_note=check_note)
}


## ============================================================================
## Condition Table Comprehensive Validation
## ============================================================================

.check_condition_table <- function(
    abundance, condition_table, analysis_type, check_note) {
  ## Comprehensive validation of condition_table dataframe
  ## Validates structure, column names, data types, and sample matching based on analysis type
  ## @param abundance: abundance dataframe with samples as columns (excluding feature column)
  ## @param condition_table: condition table dataframe with format specific to analysis_type
  ## @param analysis_type: type of analysis ("ML" or "Correlation")
  ## @param check_note: vector of existing check results to append to
  ## @return: vector of logical check results
  ## correct column number
  if (isTRUE(.check_colNum(condition_table, 2)) ){
    ## sample_name must same as the name of samples of abundance
    check_note <- append(
      .check_colName(
        abundance[-1], condition_table[[1]], strict=FALSE), check_note)
    if (analysis_type=="ML"){
      ## The column must contain ‘sample_name’ (1st column), ‘group’ (2nd column)
      check_note <- append(
        .check_colName(
          condition_table, c("sample_name", "group"), strict=TRUE),
        check_note)
      ## if group column exist
      if (isTRUE(as.numeric(ncol(condition_table))==2) ) {
        ## The columns sample_name must be characters; the column group must be numeric (only be 0 or 1).
        check_note <- append(
          .check_all_character_value(condition_table[1]), check_note)
        check_note <- append(
          .check_numeric_01(condition_table[[2]]), check_note)
        ## Each group must have more than 30 samples.
        condition_table_noNA <- condition_table[!is.na(condition_table[[2]]), ]
        check_note <- append(
          .check_rowNum(condition_table_noNA[condition_table_noNA[[2]]==0, ], 30),
          check_note)
        check_note <- append(
          .check_rowNum(condition_table_noNA[condition_table_noNA[[2]]==1, ], 30),
          check_note)
      } else {
        check_note <- append(FALSE, check_note)
      }
    } else if (analysis_type=="Correlation"){
      ## The first column must be sample_name
      check_note <- append(
        .check_colName(
          condition_table[1], "sample_name", strict=TRUE), check_note)
      ## The columns sample_name must be characters; the other columns must be numeric.
      check_note <- append(
        .check_all_numeric(condition_table[-1]), check_note)
      check_note <- append(
        .check_all_character_value(condition_table[1]), check_note)
      check_note <- append(
        .check_no_NA(condition_table[-1]), check_note)
      ## At least 2 conditions
      #check_note <- append(.check_colNum(condition_table[-1], 2), check_note)
    }
  } else {
    check_note <- append(FALSE, check_note)
  }
  return(check_note=check_note)
}


## ============================================================================
## Adjusted Table Validation
## ============================================================================

.check_adjusted_table <- function(
    abundance, adjusted_table, check_note) {
  ## Comprehensive validation of adjusted_table dataframe
  ## Checks first column is sample_name (character type) and matches abundance samples
  ## @param abundance: abundance dataframe with samples as columns (excluding feature column)
  ## @param adjusted_table: adjusted table dataframe with sample_name as first column
  ## @param check_note: vector of existing check results to append to
  ## @return: vector of logical check results
  ## The first column must be sample_name
  check_note <- append(
    .check_colName(
      adjusted_table[1], "sample_name", strict=TRUE), check_note)
  ## The first column must be character
  check_note <- append(
    .check_all_character_value(adjusted_table[1]), check_note)
  ## sample_name must same as the name of samples of abundance
  check_note <- append(
    .check_colName(
      abundance[-1], adjusted_table[[1]], strict=FALSE), check_note)
  return(check_note=check_note)
}


## ============================================================================
## File Upload Utility for Data Check
## ============================================================================
## NOTE (integrated app): When variables/session are provided, .reupload()
## preloads the uploaded data into the Data Check tab's reactiveValues and
## updates the radio button to "Preload <analysis_type> data" so the user
## clearly knows where the data came from.  No disk I/O or database writes.

.reupload <- function(
    abundance, group_info=NULL, condition_table=NULL, adjusted_table=NULL,
    abundance_path, group_info_path=NULL, condition_table_path=NULL, adjusted_table_path=NULL,
    analysis_type="DE", nGroup=NULL,
    variables=NULL, session=NULL) {
  if (!is.null(variables) && !is.null(session)) {
    variables$Check.abundance    <- abundance
    variables$Check.group.info   <- group_info
    variables$Check.cond.tab     <- condition_table
    variables$Check.adj.tab      <- adjusted_table
    variables$Check.type         <- analysis_type
    variables$Check.Ngroups      <- nGroup
    variables$Check.preload.type <- analysis_type   ## triggers DataCheck observer

    label <- switch(analysis_type,
      "Profiling"   = "Preload Profiling data",
      "DE"          = "Preload DE data",
      "ML"          = "Preload ML data",
      "Correlation" = "Preload Correlation data",
      "Preload data"
    )
    shiny::updateRadioButtons(session, inputId = 'Check_type',
      choices  = c('Profiling'='Profiling', 'DE'='DE',
                   'ML'='ML', 'Correlation'='Correlation'),
      selected = analysis_type)
    shiny::updateRadioButtons(session, inputId = 'Check_data_source',
      choices  = c('Example dataset'   = 'Check_demo_data',
                   'Upload your data!' = 'Check_user_data',
                   setNames('Check_preload', label)),
      selected = 'Check_preload')
  }
  url <- htmltools::HTML(
    "<a href='javascript:void(0)' onclick='switchTab(\"Data_Check\")' style='color: darkblue;'>Data Check</a>"
  )
  return(url)
}
