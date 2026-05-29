####################################
# author: MHTsai
# name: dataCheck-page.R
# created date: 2024/01/03
# revise date: 2026/03/23 (Integrating source function versions.); 2024/04/02;
#              05/15 (20 lipids, multigroup sample check); 05/16 (revise due to df2SE update);
#              06/07 (add group_info less than 3 cols); 06/12 (add description for FAQ12)
# description: A comprehensive input data validation function for the LipidSig web application,
#              supporting four analysis types: Profiling, DE, ML, and Correlation.
#              Performs sequential checks on lipid abundance data including UTF-8 encoding
#              of feature and sample names, first-column character type, NA and duplicate
#              lipid names, numeric value conversion, constant-value feature/sample removal,
#              missing value percentage, and lipid name recognition via lipidNameConversion().
#              For DE analysis, additionally validates group_info for UTF-8 encoding, column
#              names, sample name matching, group count, duplicate sample/label names, NA values,
#              and paired sample numbering.
#              For ML analysis, additionally validates condition_table for UTF-8 encoding,
#              column names, sample name matching, binary group coding (0/1), and minimum
#              per-group sample count (≥ 30).
#              For Correlation analysis, additionally validates condition_table for UTF-8
#              encoding, sample name matching, numeric non-NA condition columns, and minimum
#              condition count (≥ 2); optionally validates adjusted_table if provided.
#              Errors (blocking) and warnings (non-blocking) are counted separately per table
#              and reported in both a detailed check div and a summary check div.
# input: exp_data         (data frame; lipid abundance table with feature names in the first column),
#        group_info       (data frame or NULL; required for DE; columns: sample_name, label_name, group, pair),
#        condition_table  (data frame or NULL; required for ML and Correlation; columns vary by analysis type),
#        adjusted_table   (data frame or NULL; optional for Correlation; columns: sample_name + covariate columns),
#        analysis_type    (character; one of "Profiling", "DE", "ML", "Correlation"; default "DE"),
#        nGroup           (character; one of "two", "multiple"; applicable for DE only; default "two")
# output: list(return_div, check_sum_div, nonParseable_lipid, exp_naming_error,
#              exp_data, group_info, condition_table, adjusted_table,
#              exp_data_process, lipid_char_process,
#              group_info_process, condition_table_process, adjusted_table_process)
#         - return_div            : shiny HTML div with detailed per-item check results for all uploaded tables
#         - check_sum_div         : shiny HTML div with error/warning count summary per table
#         - nonParseable_lipid    : data frame of unrecognized lipid names (NULL if all recognized)
#         - exp_naming_error      : data frame of lipids removed due to NA or duplicate names (NULL if none)
#         - exp_data              : original uploaded lipid abundance data (with UTF-8 corrections applied)
#         - group_info            : original group information table (with UTF-8 corrections applied; DE only)
#         - condition_table       : original condition table (with UTF-8 corrections applied; ML/Correlation only)
#         - adjusted_table        : original adjusted table (with UTF-8 corrections applied; Correlation only)
#         - exp_data_process      : post-processing lipid abundance data with only parseable lipids (NULL if none)
#         - lipid_char_process    : lipid characteristic annotation table from parseable SE rowData (NULL if none)
#         - group_info_process    : group_info filtered to samples present in exp_data (DE only; otherwise NULL)
#         - condition_table_process : condition_table filtered to samples present in exp_data (ML/Correlation only)
#         - adjusted_table_process  : adjusted_table filtered to samples present in exp_data (Correlation only)
####################################
### step1. data frame format
check_web <- function(
        exp_data, group_info=NULL, condition_table=NULL, adjusted_table=NULL,
        analysis_type="DE", nGroup="two"){

    ## count for error & warnings
    exp_error <- 0
    exp_warning <- 0
    ## Check exp_data characters are UTF-8 encoded
    if(isTRUE(.utf8Check(exp_data[1]))){
      exp_noUTF8 <- exp_data
      lipid_UTF8 <- NULL
      lipid_UTF8_message <- NULL
    }else{
      uft8Locale <- which(!stringi::stri_enc_isutf8(exp_data |> dplyr::select(1) |> dplyr::pull()))
      exp_noUTF8 <- exp_data[-uft8Locale,]
      exp_UTF8 <- exp_data[uft8Locale,]
      exp_data[uft8Locale, 1] <- 'Unknown feature'
      lipid_UTF8 <- shiny::tags$p(shiny::icon("times"), paste0("Lipid names (features) must be UTF-8 encoded text. Remove ", nrow(exp_UTF8), "/", nrow(exp_data), " lipids.") , style="font-size: 16px;color:red;")
      lipid_UTF8_message <- tags$blockquote(shiny::tags$p(shiny::icon("caret-right"), paste0("Replace the lipid name with 'Unknown feature' in the Raw data tab. The location of the lipid is as follows row : ", uft8Locale,'.'), style="font-size: 16px;"),style='border: none;margin: 0 0 10.5px;padding: 0 21px;')
      exp_error <- exp_error + 1
    }
    ## Check exp_data characters are UTF-8 encoded
    if(isTRUE(.utf8Check(colnames(exp_data)))){
      exp_noUTF8 <- exp_noUTF8
      sample_UTF8 <- NULL
      sample_UTF8_message <- NULL
    }else{
      uft8Locale <- which(!stringi::stri_enc_isutf8(colnames(exp_data)))
      exp_noUTF8 <- exp_noUTF8[, -uft8Locale]
      colnames(exp_data)[uft8Locale] <- 'Unknown sample'
      sample_UTF8 <- shiny::tags$p(shiny::icon("times"), paste0("Sample names must be UTF-8 encoded text.") , style="font-size: 16px;color:red;")
      sample_UTF8_message <- tags$blockquote(shiny::tags$p(shiny::icon("caret-right"), paste0("Replace the sample name with 'Unknown sample' in the Raw data tab."), style="font-size: 16px;"),style='border: none;margin: 0 0 10.5px;padding: 0 21px;')
      exp_error <- exp_error + 1
    }
    
    ## exp_data first column is character
    if (isTRUE(.check_all_character_value(exp_noUTF8[, 1]))) {
        exp_char <- shiny::tags$p(shiny::icon("check"), "The first column of lipid abundance data contains a list of lipid names (features).", style="font-size: 16px;")
    } else {
        exp_char <- shiny::tags$p(shiny::icon("times"), "The first column of lipid abundance data must contain a list of lipid names (features).", style="font-size: 16px;color:red;")
        exp_error <- exp_error + 1
    }
    
    ## no duplicates & NA in exp_data features
    if (isTRUE(.check_no_NA(exp_noUTF8[1])) ) {
        exp_noNA <- exp_noUTF8
        exp_NA <- NULL
        lipid_na <- shiny::tags$p(shiny::icon("check"), "Lipid names (features) contain no NAs.", style="font-size: 16px;")
    } else {
        exp_noNA <- exp_noUTF8[!is.na(exp_noUTF8[1]), ]
        exp_NA <- exp_noUTF8[is.na(exp_noUTF8[1]), ]
        lipid_na <- shiny::tags$p(shiny::icon("times"), paste0("Lipid names (features) must not contain NAs. Remove ", nrow(exp_NA), "/", nrow(exp_data), " lipids.") , style="font-size: 16px;color:red;")
        exp_error <- exp_error + 1
    }
    if (isTRUE(.check_no_duplicates(exp_noNA[1])) ) {
        lipid_dup <- shiny::tags$p(shiny::icon("check"), "All lipid names (features) are unique.", style="font-size: 16px;")
        exp_noDup <- exp_noNA
        exp_dupName <- NULL
    } else {
        exp_noDup <- exp_noNA %>% dplyr::distinct(exp_noNA[1], .keep_all = TRUE)
        exp_dupName <- exp_noNA[duplicated(exp_noNA[1]), ]
        lipid_dup <- shiny::tags$p(shiny::icon("times"), paste0("Lipid names (features) must be unique. Remove ", nrow(exp_dupName), "/", nrow(exp_data), " lipids."), style="font-size: 16px;color:red;")
        exp_error <- exp_error + 1
    }
    ## naming error features
    if (!is.null(exp_dupName) | !is.null(exp_NA)) {
        exp_naming_error <- rbind(exp_dupName, exp_NA)
    } else {
        exp_naming_error <- NULL
    }
    ## exp no NA & duplicate
    exp_data_check <- exp_noDup

    ### EXP check step 2 : ID conversion / construct SE ---------------------
    ## exp_data first column names is "feature"
    colnames(exp_data_check)[1] <- "feature"
    ## exp_data values must numeric
    if (isFALSE(.check_all_numeric(exp_data_check[-1]))){
        exp_data_check[-1] <- sapply(exp_data_check[-1], as.numeric)
        convert_value <- shiny::tags$p(shiny::icon("exclamation"), "Converting character values of lipid abundance into numeric values.", style="font-size: 16px;color:blue;")
        exp_warning <- exp_warning + 1
    } else if (isTRUE(.check_any_integer64(exp_data_check[-1])) ) {
        exp_data_check[-1] <- sapply(exp_data_check[-1], as.numeric)
        convert_value <- shiny::tags$p(shiny::icon("exclamation"), "Converting integer64 values of lipid abundance into numeric values.", style="font-size: 16px;color:blue;")
        exp_warning <- exp_warning + 1
    } else {
        exp_data_check <- exp_data_check
        exp_data_check[-1] <- sapply(exp_data_check[-1], as.numeric)
        convert_value <- shiny::tags$p(shiny::icon("check"), "All lipid abundance values are numeric.", style="font-size: 16px;")
    }
    ## feature to row names
    exp_data_raw <- exp_data_check %>% tibble::remove_rownames() %>% 
      tibble::column_to_rownames(var="feature")
    ## features with a constant or single value across samples
    exp_data_rmR <- .rm_constant_feature(exp_data_raw)
    if (nrow(exp_data_rmR) < nrow(exp_data_raw)) {
        rm_rowNum <- nrow(exp_data_raw) - nrow(exp_data_rmR)
        rm_feature <- shiny::tags$p(shiny::icon("exclamation"), paste0("Remove ", rm_rowNum, "/", nrow(exp_data_check), " features with constant values (including all NAs and all zeros)."), style="font-size: 16px;color:blue;")
        exp_warning <- exp_warning + 1
    } else {
        rm_feature <- shiny::tags$p(shiny::icon("check"), "No features with constant values (including all NAs and all zeros).", style="font-size: 16px;")
    }
    ## samples with a constant or single value across features
    exp_data_filt <- .rm_constant_sample(exp_data_rmR)
    if (ncol(exp_data_filt) < ncol(exp_data_raw)) {
        rm_colName <- paste(colnames(exp_data_raw)[!(colnames(exp_data_raw) %in% colnames(exp_data_filt))], collapse = ", ")
        rm_colNum <- ncol(exp_data_raw) - ncol(exp_data_filt)
        sample_limit <- switch(
            analysis_type, "Profiling" = 2, "DE" = 2, "ML" = 60, "Correlation" = 10)
        if (isTRUE(.check_colNum(exp_data_filt[-1], sample_limit))) {
            rm_sample <- shiny::tags$p(shiny::icon("exclamation"), paste0("Remove ", rm_colNum, "/", nrow(exp_data_check), " samples with constant values (including all NAs and all zeros). Remove sample: ", rm_colName, "."), style="font-size: 16px;color:blue;")
            exp_warning <- exp_warning + 1
        } else {
            ## after remove the samples is not enough for analysis
            rm_sample <- shiny::tags$p(shiny::icon("times"), paste0("Remove ", rm_colNum, "/", nrow(exp_data_check), " samples with constant values (including all NAs and all zeros). Remove sample: ", rm_colName, ". Not enough samples for further analysis."), style="font-size: 16px;color:red;")
            exp_error <- exp_error + 1
        }
    } else {
        rm_sample <- shiny::tags$p(shiny::icon("check"), "No samples with constant values (including all NAs and all zeros).", style="font-size: 16px;")
    }
    ## missing value percentage
    missing_percent <- round(mean(is.na(exp_data_filt[-1])) * 100, digits = 2)
    if (missing_percent==0) {
        miss_value <- shiny::tags$p(shiny::icon("check"), "No missing values.", style="font-size: 16px;")
    } else {
        miss_value <- shiny::tags$p(shiny::icon("exclamation"), paste0(missing_percent, "% missing values."), style="font-size: 16px;color:blue;")
        exp_warning <- exp_warning + 1
    }
    exp_data_se <- exp_data_filt %>% tibble::rownames_to_column(var="feature")
    ## group_info 1-3 column must be character
    if (!is.null(group_info)) {
        if (ncol(group_info) >= 3) {
            group_info[1:3] <- lapply(group_info[1:3], as.character)
        }
    }
    ## build SE
    #se <- df2SE(exp_data_se, group_info=NULL, ref_group=NULL,
    #            condition_table=NULL, adjusted_table=NULL)
    exp_mat <- exp_data_se %>% tibble::remove_rownames() %>% 
      tibble::column_to_rownames(var="feature")
    seCol <- colnames(exp_data)[-1]
    ## construct SE objects
    se <- SummarizedExperiment::SummarizedExperiment(
      assays=as.matrix(exp_mat),
      rowData=exp_data_se["feature"],
      colData=seCol,
      metadata=list(condition_table=NULL, adjusted_table=NULL))
    ## id convert
    idSE <- lipidNameConversion(se, exclude = FALSE)
    nonParseable_se <- idSE$nonParseableLipid
    parseable_se <- idSE$parseableLipid
    if (is.null(nonParseable_se)) {
        id_convert <- shiny::tags$p(shiny::icon("check"), "All lipid names are recognized.", style="font-size: 16px;")
        unmatched_names <- NULL
        lipid_unmatched <- 0
    } else {
        lipid_unmatched <- dim(nonParseable_se)[1]
        unmatched_names <- as.data.frame(SummarizedExperiment::rowData(nonParseable_se))
        rownames(unmatched_names) <- NULL
        # if (lipid_unmatched==nrow(exp_data_check)) {
        if ( (nrow(exp_data_check)-lipid_unmatched) < 20) {
            id_convert <- shiny::tags$p(shiny::icon("times"), paste0("Remove ", lipid_unmatched, "/", nrow(exp_data), " unrecognized lipid names. There must be at least 20 features remaining for further analysis. For lipid species naming format, please refer to the"), HTML("<a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ12' target='_blank' style='color: darkblue;'>FAQ</a>."), style="font-size: 16px;color:red;")
            exp_error <- exp_error + 1
        } else {
            id_convert <- shiny::tags$p(shiny::icon("exclamation"), paste0("Remove ", lipid_unmatched, "/", nrow(exp_data), " unrecognized lipid names. For lipid species naming format, please refer to the"), HTML("<a href='https://lipidsig.bioinfomics.org/FAQ/?FAQ12' target='_blank' style='color: darkblue;'>FAQ</a>."), style="font-size: 16px;color:blue;")
            exp_warning <- exp_warning + 1
        }
    }

    ## number of lipid available for analysis
    if (!is.null(parseable_se)){
        featureNum_aval <- shiny::tags$p(shiny::icon("caret-right"), paste0("Number of lipids (features) available for analysis: ", dim(parseable_se)[1]), style="font-size: 16px;")
    } else {
        featureNum_aval <- shiny::tags$p(shiny::icon("caret-right"), "Number of lipids (features) available for analysis: 0", style="font-size: 16px;")
    }

    ## for each analysis type
    if (analysis_type=="Profiling") {
    ## profiling --------------------------------------------------------------
        ## check samples number (ncol)
        if (isTRUE(.check_colNum(exp_data_check[-1], 2)) ) {
            sample_num <- shiny::tags$p(shiny::icon("check"), "Lipid abundance data contain at least 2 samples.", style="font-size: 16px;")
        } else {
            sample_num <- shiny::tags$p(shiny::icon("times"), "Lipid abundance data must contain at least 2 samples.", style="font-size: 16px;color:red;")
            exp_error <- exp_error + 1
        }
        ## return message
        return_div <- shiny::tags$div(
            shiny::tags$p(shiny::strong("Data check results of Profiling analysis"),style="font-size: 24px;"),
            shiny::tags$p(shiny::strong("Lipid abundance data"),style="font-size: 20px;"),
            shiny::tags$ul(
                exp_char, convert_value, miss_value, lipid_UTF8, lipid_UTF8_message, sample_UTF8, sample_UTF8_message, lipid_na, lipid_dup, rm_feature, id_convert, featureNum_aval, sample_num, rm_sample),
            style="font-size: 0px;"
        )
        ## return check summary
        if (exp_error==0 && exp_warning ==0) {
            exp_checks <- shiny::tags$p("No errors require correction.", style="font-size: 16px;")
            exp_warns <- NULL
        } else {
            exp_checks <- shiny::tags$p(paste0("Detect ", exp_error," error(s).") , style="font-size: 16px;color:red;")
            exp_warns <- shiny::tags$p(paste0("Detect ", exp_warning," warning(s).") , style="font-size: 16px;color:blue;")
        }
        check_sum_div <- shiny::tags$div(
            shiny::tags$p(shiny::strong("Data Check completed."),style="font-size: 24px;"),
            shiny::tags$p("A summary of the checking results for each input dataset is available below. Please correct any detected errors.
                          Warnings signify the steps taken in the data check process but do not impact the analysis. Scroll down for a detailed overview of all checks.
                          You will also find your uploaded data and the post-processing expression data.", style="font-size: 18px;"),
            shiny::tags$p(shiny::strong("Lipid abundance data"),style="font-size: 20px;"),
            shiny::tags$ul(exp_checks),
            shiny::tags$ul(exp_warns),
            style="font-size: 0px;"
        )
    } else if (analysis_type=="DE") {
    ## DE ---------------------------------------------------------------------
        group_info_error <- 0
        ## check samples number (ncol)
        if (isTRUE(.check_colNum(exp_data_check[-1], 2)) ) {
            sample_num <- shiny::tags$p(shiny::icon("check"), "Lipid abundance data contain at least 2 samples.", style="font-size: 16px;")
        } else {
            sample_num <- shiny::tags$p(shiny::icon("times"), "Lipid abundance data must contain at least 2 samples.", style="font-size: 16px;color:red;")
            exp_error <- exp_error + 1
        }
        ## Check group_info characters are UTF-8 encoded
        if(isTRUE(.utf8Check(group_info)) & isTRUE(.utf8Check(colnames(group_info)))){
          group_UTF8 <- NULL
          group_UTF8_message <- NULL
        }else{
          group_info <- group_info %>% as.data.frame() %>%
            apply(2, function(x){ifelse(stringi::stri_enc_isutf8(x), x, "Unknown")}) %>% 
            as.data.frame()
          uft8Locale <- which(!stringi::stri_enc_isutf8(colnames(group_info)))
          colnames(group_info)[uft8Locale] <- 'Unkown'
          group_UTF8 <- shiny::tags$p(shiny::icon("times"), paste0("Group information table must be UTF-8 encoded text.") , style="font-size: 16px;color:red;")
          group_UTF8_message <- tags$blockquote(shiny::tags$p(shiny::icon("caret-right"), paste0("Replace the non UTF-8 encoded text with 'Unknown' in the group information tab."), style="font-size: 16px;"),style='border: none;margin: 0 0 10.5px;padding: 0 21px;')
          group_info_error <- group_info_error + 1
        }
        ## exp_data colnames match group_info sample_name
        if (isTRUE(.check_colName(exp_data_check[-1], group_info[[1]], strict=FALSE)) ) {
            sample_match <- shiny::tags$p(shiny::icon("check"), "Sample names in 'sample_name' column are as same as the sample names in lipid abundance data.", style="font-size: 16px;")
        } else {
            sample_match <- shiny::tags$p(shiny::icon("times"), "Sample names in 'sample_name' column must be as same as the sample names in lipid abundance data.", style="font-size: 16px;color:red;")
            group_info_error <- group_info_error + 1
        }
        ## group_info sample names no duplicate
        if (isTRUE(.check_no_duplicates(group_info[1])) ) {
            group_info_dup <- shiny::tags$p(shiny::icon("check"), "All sample names are unique.", style="font-size: 16px;")
        } else {
            group_info_dup <- shiny::tags$p(shiny::icon("times"), "Sample names must be unique.", style="font-size: 16px;color:red;")
            group_info_error <- group_info_error + 1
        }
        ## group_info label names no duplicate
        if (isTRUE(.check_no_duplicates(group_info[2])) ) {
          group_info_label_dup <- shiny::tags$p(shiny::icon("check"), "All label names are unique.", style="font-size: 16px;")
        } else {
          group_info_label_dup <- shiny::tags$p(shiny::icon("times"), "Label names must be unique.", style="font-size: 16px;color:red;")
          group_info_error <- group_info_error + 1
        }
        ## less than 3 column
        if (ncol(group_info)==1) {
            group_info <- group_info %>% dplyr::mutate(NULL1=NA, NULL2=NA)
            group_info_colNum <- shiny::tags$p(shiny::icon("times"), "Incorrect column numbers. The column names must be arranged as 'sample_name', 'label_name', 'group', and 'pair' for two-group data, and as 'sample_name', 'label_name', and 'group' for multiple-group data.", style="font-size: 16px;color:red;")
            group_info_error <- group_info_error + 1
        } else if (ncol(group_info)==2 && "group" %in% colnames(group_info) ) {
            group_info <- group_info %>% dplyr::mutate(NULL1=NA, .before=group)
            group_info_colNum <- shiny::tags$p(shiny::icon("times"), "Incorrect column numbers. The column names must be arranged as 'sample_name', 'label_name', 'group', and 'pair' for two-group data, and as 'sample_name', 'label_name', and 'group' for multiple-group data.", style="font-size: 16px;color:red;")
            group_info_error <- group_info_error + 1
        } else if (ncol(group_info)==2) {
            group_info <- group_info %>% dplyr::mutate(NULL1=NA)
            group_info_colNum <- shiny::tags$p(shiny::icon("times"), "Incorrect column numbers. The column names must be arranged as 'sample_name', 'label_name', 'group', and 'pair' for two-group data, and as 'sample_name', 'label_name', and 'group' for multiple-group data.", style="font-size: 16px;color:red;")
            group_info_error <- group_info_error + 1
        } else {
            group_info_colNum <- NULL
        }
        ## no NA value in "sample_name", "label_name", "group"
        if (isTRUE(.check_no_NA(group_info[, 1:3])) ) {
            group_info_NA <- shiny::tags$p(shiny::icon("check"), "Columns of 'sample_name', 'label_name', and 'group' columns do not contain NA values.", style="font-size: 16px;")
        } else {
            group_info_NA <- shiny::tags$p(shiny::icon("times"), "NA values are not allowed in the 'sample_name', 'label_name', and 'group' columns.", style="font-size: 16px;color:red;")
            group_info_error <- group_info_error + 1
        }
        ## two group
        if (nGroup=="two") {
            ## group_info column names correct
            if (isTRUE(.check_colName(group_info, c("sample_name", "label_name", "group", "pair"), strict=TRUE)) ) {
                group_col <- shiny::tags$p(shiny::icon("check"), "The column names are arranged in order of sample_name, label_name, group, and pair.", style="font-size: 16px;")
            } else {
                group_col <- shiny::tags$p(shiny::icon("times"), "The column names must be arranged in order of sample_name, label_name, group, and pair.", style="font-size: 16px;color:red;")
                group_info_error <- group_info_error + 1
            }
            ## group number
            if (isTRUE(.check_groups(group_info, nGroup)) ) {
                group_num <- shiny::tags$p(shiny::icon("check"), "The column 'group' contain 2 groups.", style="font-size: 16px;")
            } else {
                group_num <- shiny::tags$p(shiny::icon("times"), "The column 'group' only can have 2 groups.", style="font-size: 16px;color:red;")
                group_info_error <- group_info_error + 1
            }
            ## pair
            if (isTRUE(.check_pair_group(group_info)) ) {
                group_pair <- shiny::tags$p(shiny::icon("check"), "Values in the 'pair' column are correct.", style="font-size: 16px;")
            } else {
                group_pair <- shiny::tags$p(shiny::icon("times"), "Incorrect values in the 'pair' column. For pair data, each pair should be sequentially numbered from 1 to N without any missing, blank, or skipped numbers; otherwise, the value is marked as NA.", style="font-size: 16px;color:red;")
                group_info_error <- group_info_error + 1
            }
            ## multiple
        } else if (nGroup=="multiple") {
            ## group_info column names correct
            if (isTRUE(.check_colName(group_info, c("sample_name", "label_name", "group"), strict=TRUE)) ) {
                group_col <- shiny::tags$p(shiny::icon("check"), "The column names are arranged in order of sample_name, label_name, and group.", style="font-size: 16px;")
            } else {
                group_col <- shiny::tags$p(shiny::icon("times"), "The column names must be arranged in order of sample_name, label_name, and group.", style="font-size: 16px;color:red;")
                group_info_error <- group_info_error + 1
            }
            ## group number
            if (isTRUE(.check_groups(group_info, nGroup)) ) {
                group_num <- shiny::tags$p(shiny::icon("check"), "The column 'group' contain more than 2 groups. Each group has more than 2 samples.", style="font-size: 16px;")
            } else {
                group_num <- shiny::tags$p(shiny::icon("times"), "The column 'group' must have more than 2 groups. Each group must have more than 2 samples.", style="font-size: 16px;color:red;")
                group_info_error <- group_info_error + 1
            }
            group_pair <- NULL
        }
        ## return message
        return_div <- shiny::tags$div(
            shiny::tags$p(shiny::strong("Data check results of Differential expression analysis"),style="font-size: 24px;"),
            shiny::tags$p(shiny::strong("Lipid abundance data"),style="font-size: 20px;"),
            shiny::tags$ul(
                exp_char, convert_value, miss_value, lipid_UTF8, lipid_UTF8_message, sample_UTF8, sample_UTF8_message, lipid_na, lipid_dup, rm_feature, id_convert, featureNum_aval, sample_num, rm_sample),
            shiny::tags$p(shiny::strong("Group information table"),style="font-size: 20px;"),
            shiny::tags$ul(
                group_info_colNum, group_col, sample_match, group_info_dup, group_info_label_dup, group_info_NA,
                group_num, group_pair, group_UTF8, group_UTF8_message ),
            style="font-size: 0px;"
        )
        ## return check summary
        if (exp_error==0 && exp_warning ==0) {
            exp_checks <- shiny::tags$p("No errors require correction.", style="font-size: 16px;")
            exp_warns <- NULL
        } else {
            exp_checks <- shiny::tags$p(paste0("Detect ", exp_error," error(s).") , style="font-size: 16px;color:red;")
            exp_warns <- shiny::tags$p(paste0("Detect ", exp_warning," warning(s).") , style="font-size: 16px;color:blue;")
        }
        if (group_info_error==0) {
            group_info_checks <- shiny::tags$p("No errors require correction.", style="font-size: 16px;")
        } else {
            group_info_checks <- shiny::tags$p(paste0("Detect ", group_info_error," error(s).") , style="font-size: 16px;color:red;")
        }
        check_sum_div <- shiny::tags$div(
            shiny::tags$p(shiny::strong("Data Check completed."),style="font-size: 24px;"),
            shiny::tags$p("A summary of the checking results for each input dataset is available below. Please correct any detected errors.
                          Warnings signify the steps taken in the data check process but do not impact the analysis. Scroll down for a detailed overview of all checks.
                          You will also find your uploaded data and the post-processing expression data.", style="font-size: 18px;"),
            shiny::tags$p(shiny::strong("Lipid abundance data"),style="font-size: 20px;"),
            shiny::tags$ul(exp_checks),
            shiny::tags$ul(exp_warns),
            shiny::tags$p(shiny::strong("Group information table"),style="font-size: 20px;"),
            shiny::tags$ul(group_info_checks),
            style="font-size: 0px;"
        )
    } else if (analysis_type=="ML") {
    ## ML ---------------------------------------------------------------------
        condition_error <- 0
        ## check samples number (ncol)
        if (isTRUE(.check_colNum(exp_data_check[-1], 60)) ) {
            sample_num <- shiny::tags$p(shiny::icon("check"), "Lipid abundance data contain at least 60 samples.", style="font-size: 16px;")
        } else {
            sample_num <- shiny::tags$p(shiny::icon("times"), "Lipid abundance data must contain at least 60 samples.", style="font-size: 16px;color:red;")
            exp_error <- exp_error + 1
        }
        ### check condition_table
        ## Check condition_table characters are UTF-8 encoded
        if(isTRUE(.utf8Check(condition_table)) & isTRUE(.utf8Check(colnames(condition_table)))){
          condition_UTF8 <- NULL
          condition_UTF8_message <- NULL
        }else{
          condition_table <- condition_table %>% as.data.frame() %>%
            apply(2, function(x){ifelse(stringi::stri_enc_isutf8(x), x, "Unknown")}) %>% 
            as.data.frame()
          uft8Locale <- which(!stringi::stri_enc_isutf8(colnames(condition_table)))
          colnames(condition_table)[uft8Locale] <- 'Unkown'
          condition_UTF8 <- shiny::tags$p(shiny::icon("times"), paste0("Condition table must be UTF-8 encoded text.") , style="font-size: 16px;color:red;")
          condition_UTF8_message <- tags$blockquote(shiny::tags$p(shiny::icon("caret-right"), paste0("Replace the non UTF-8 encoded text with 'Unknown' in the Condition table tab."), style="font-size: 16px;"),style='border: none;margin: 0 0 10.5px;padding: 0 21px;')
          condition_error <- condition_error + 1
        }
        ## check col num
        if (ncol(condition_table) < 2) {
            condition_table <- condition_table %>% dplyr::mutate(empty_column=NA)
        }
        ## The column must contain ‘sample_name’ (1st column), ‘group’ (2nd column)
        if (isTRUE(.check_colName(condition_table, c("sample_name", "group"), strict=TRUE)) ) {
            condition_col <- shiny::tags$p(shiny::icon("check"), "The column names are arranged in order of 'sample_name' and 'group'.", style="font-size: 16px;")
        } else {
            condition_col <- shiny::tags$p(shiny::icon("times"), "The column names must be arranged in order of 'sample_name' and 'group'.", style="font-size: 16px;color:red;")
            condition_error <- condition_error + 1
        }
        ## sample_name must same as the name of samples of exp_data
        if (isTRUE(.check_colName(exp_data_check[-1], condition_table[[1]], strict=FALSE)) ) {
            condition_sample_name <- shiny::tags$p(shiny::icon("check"), "Sample names 'sample_name' column are as same as the sample names in lipid abundance data.", style="font-size: 16px;")
        } else {
            condition_sample_name <- shiny::tags$p(shiny::icon("times"), "Sample names 'sample_name' column must be as same as the sample names in lipid abundance data.", style="font-size: 16px;color:red;")
            condition_error <- condition_error + 1
        }
        ## The columns sample_name must be characters; the column group must be numeric (only be 0 or 1).
        if (isTRUE(.check_all_character_value(condition_table[1])) ) {
            condition_name_col1 <- shiny::tags$p(shiny::icon("check"), "The first column contains a list of sample names.", style="font-size: 16px;")
        } else {
            condition_name_col1 <- shiny::tags$p(shiny::icon("times"), "The first column must contain a list of sample names.", style="font-size: 16px;color:red;")
            condition_error <- condition_error + 1
        }
        if (isTRUE(.check_numeric_01(condition_table[[2]])) ) {
            condition_name_col2 <- shiny::tags$p(shiny::icon("check"), "The column 'group' is numeric (only be 0 or 1; NAs are not allowed).", style="font-size: 16px;")
        } else {
            condition_name_col2 <- shiny::tags$p(shiny::icon("times"), "The column 'group' must be numeric (only be 0 or 1; NAs are not allowed).", style="font-size: 16px;color:red;")
            condition_error <- condition_error + 1
        }
        ## Each group must have more than 30 samples.
        condition_table_noNA <- condition_table[!is.na(condition_table[[2]]), ]
        if (isTRUE(all(.check_rowNum(condition_table_noNA[condition_table_noNA[[2]]==0, ], 30), .check_rowNum(condition_table_noNA[condition_table_noNA[[2]]==1, ], 30))) ) {
            condition_sample_num <- shiny::tags$p(shiny::icon("check"), "Each group has more than 30 samples.", style="font-size: 16px;")
        } else {
            condition_sample_num <- shiny::tags$p(shiny::icon("times"), "Each group must have more than 30 samples.", style="font-size: 16px;color:red;")
            condition_error <- condition_error + 1
        }
        ## return message
        return_div <- shiny::tags$div(
            shiny::tags$p(shiny::strong("Data check results of Machine learning analysis"),style="font-size: 24px;"),
            shiny::tags$p(shiny::strong("Lipid abundance data"),style="font-size: 20px;"),
            shiny::tags$ul(
                exp_char, convert_value, miss_value, lipid_UTF8, lipid_UTF8_message, sample_UTF8, sample_UTF8_message, lipid_na, lipid_dup, rm_feature, id_convert, featureNum_aval, sample_num, rm_sample),
            shiny::tags$p(shiny::strong("Condition table"),style="font-size: 20px;"),
            shiny::tags$ul(
                condition_col, condition_sample_name, condition_name_col1,
                condition_name_col2, condition_sample_num, condition_UTF8, condition_UTF8_message),
            style="font-size: 0px;"
        )
        ## return check summary
        if (exp_error==0 && exp_warning ==0) {
            exp_checks <- shiny::tags$p("No errors require correction.", style="font-size: 16px;")
            exp_warns <- NULL
        } else {
            exp_checks <- shiny::tags$p(paste0("Detect ", exp_error," error(s).") , style="font-size: 16px;color:red;")
            exp_warns <- shiny::tags$p(paste0("Detect ", exp_warning," warning(s).") , style="font-size: 16px;color:blue;")
        }
        if (condition_error==0) {
            condition_checks <- shiny::tags$p("No errors require correction.", style="font-size: 16px;")
        } else {
            condition_checks <- shiny::tags$p(paste0("Detect ", condition_error," error(s).") , style="font-size: 16px;color:red;")
        }
        check_sum_div <- shiny::tags$div(
            shiny::tags$p(shiny::strong("Data Check completed."),style="font-size: 24px;"),
            shiny::tags$p("A summary of the checking results for each input dataset is available below. Please correct any detected errors.
                          Warnings signify the steps taken in the data check process but do not impact the analysis. Scroll down for a detailed overview of all checks.
                          You will also find your uploaded data and the post-processing expression data.", style="font-size: 18px;"),
            shiny::tags$p(shiny::strong("Lipid abundance data"),style="font-size: 20px;"),
            shiny::tags$ul(exp_checks),
            shiny::tags$ul(exp_warns),
            shiny::tags$p(shiny::strong("Condition table"),style="font-size: 20px;"),
            shiny::tags$ul(condition_checks),
            style="font-size: 0px;"
        )
    } else if (analysis_type=="Correlation") {
    ## Correlation ------------------------------------------------------------
        condition_error <- 0
        ## check samples number (ncol)
        if (isTRUE(.check_colNum(exp_data_check[-1], 10)) ) {
            sample_num <- shiny::tags$p(shiny::icon("check"), "Lipid abundance data contain at least 10 samples.", style="font-size: 16px;")
        } else {
            sample_num <- shiny::tags$p(shiny::icon("times"), "Lipid abundance data must contain at least 10 samples.", style="font-size: 16px;color:red;")
            exp_error <- exp_error + 1
        }
        ## check condition_table
        ## Check condition_table characters are UTF-8 encoded
        if(isTRUE(.utf8Check(condition_table)) & isTRUE(.utf8Check(colnames(condition_table)))){
          condition_UTF8 <- NULL
          condition_UTF8_message <- NULL
        }else{
          condition_table <- condition_table %>% as.data.frame() %>%
            apply(2, function(x){ifelse(stringi::stri_enc_isutf8(x), x, "Unknown")}) %>% 
            as.data.frame()
          uft8Locale <- which(!stringi::stri_enc_isutf8(colnames(condition_table)))
          colnames(condition_table)[uft8Locale] <- 'Unkown'
          condition_UTF8 <- shiny::tags$p(shiny::icon("times"), paste0("Condition table must be UTF-8 encoded text.") , style="font-size: 16px;color:red;")
          condition_UTF8_message <- tags$blockquote(shiny::tags$p(shiny::icon("caret-right"), paste0("Replace the non UTF-8 encoded text with 'Unknown' in the Condition table tab."), style="font-size: 16px;"),style='border: none;margin: 0 0 10.5px;padding: 0 21px;')
          condition_error <- condition_error + 1
        }
        ## The first column must be sample_name
        if (isTRUE(.check_colName(condition_table[1], "sample_name", strict=TRUE)) ) {
            condition_col_name <- shiny::tags$p(shiny::icon("check"), "The first column name is 'sample_name'.", style="font-size: 16px;")
        } else {
            condition_col_name <- shiny::tags$p(shiny::icon("times"), "The first column name must be 'sample_name'.", style="font-size: 16px;color:red;")
            condition_error <- condition_error + 1
        }
        ## sample_name must same as the name of samples of exp_data
        if (isTRUE(.check_colName(exp_data[-1], condition_table[[1]], strict=FALSE)) ) {
            condition_sample_name <- shiny::tags$p(shiny::icon("check"), "Sample names in 'sample_name' column are as same as the sample names in lipid abundance data.", style="font-size: 16px;")
        } else {
            condition_sample_name <- shiny::tags$p(shiny::icon("times"), "Sample names in 'sample_name' column must be as same as the sample names in lipid abundance data.", style="font-size: 16px;color:red;")
            condition_error <- condition_error + 1
        }
        ## The columns sample_name must be characters; the other columns must be numeric.
        if (isTRUE(.check_all_character_value(condition_table[1])) ) {
            condition_char <- shiny::tags$p(shiny::icon("check"), "The column 'sample_name' contains character values.", style="font-size: 16px;")
        } else {
            condition_char <- shiny::tags$p(shiny::icon("times"), "The column 'sample_name' must contain character values.", style="font-size: 16px;color:red;")
            condition_error <- condition_error + 1
        }
        if (isTRUE(.check_all_numeric(condition_table[-1]) & .check_no_NA(condition_table[-1])) ) {
            condition_numeric <- shiny::tags$p(shiny::icon("check"), "Except for the first column, 'sample_name', all the columns are numeric and no missing values.", style="font-size: 16px;")
        } else {
            condition_numeric <- shiny::tags$p(shiny::icon("times"), "Except for the first column, 'sample_name', all the columns must be numeric and no missing values.", style="font-size: 16px;color:red;")
            condition_error <- condition_error + 1
        }
        ## At least 2 conditions
        if (isTRUE(.check_colNum(condition_table[-1], 2)) ) {
            condition_num <- shiny::tags$p(shiny::icon("check"), "The condition table contains at least 2 conditions.", style="font-size: 16px;")
        } else {
            condition_num <- shiny::tags$p(shiny::icon("times"), "The condition table must contain at least 2 conditions.", style="font-size: 16px;color:red;")
            condition_error <- condition_error + 1
        }
        ## check adjusted_table
        if (!is.null(adjusted_table)) {
            adjusted_error <- 0
            ## Check condition_table characters are UTF-8 encoded
            if(isTRUE(.utf8Check(adjusted_table)) & isTRUE(.utf8Check(colnames(adjusted_table)))){
              adjusted_UTF8 <- NULL
              adjusted_UTF8_message <- NULL
            }else{
              adjusted_table <- adjusted_table %>% as.data.frame() %>%
                apply(2, function(x){ifelse(stringi::stri_enc_isutf8(x), x, "Unknown")}) %>% 
                as.data.frame()
              uft8Locale <- which(!stringi::stri_enc_isutf8(colnames(adjusted_table)))
              colnames(adjusted_table)[uft8Locale] <- 'Unkown'
              adjusted_UTF8 <- shiny::tags$p(shiny::icon("times"), paste0("Condition table must be UTF-8 encoded text.") , style="font-size: 16px;color:red;")
              adjusted_UTF8_message <- tags$blockquote(shiny::tags$p(shiny::icon("caret-right"), paste0("Replace the non UTF-8 encoded text with 'Unknown' in the Adjusted table tab."), style="font-size: 16px;"),style='border: none;margin: 0 0 10.5px;padding: 0 21px;')
              adjusted_error <- adjusted_error + 1
            }
            ## The first column must be sample_name
            if (isTRUE(.check_colName(adjusted_table[1], "sample_name", strict=TRUE)) ) {
                adjusted_col_name <- shiny::tags$p(shiny::icon("check"), "The first column is 'sample_name'.", style="font-size: 16px;")
            } else {
                adjusted_col_name <- shiny::tags$p(shiny::icon("times"), "The first column must be 'sample_name'.", style="font-size: 16px;color:red;")
                adjusted_error <- adjusted_error + 1
            }
            ## The first column must be characters
            if (isTRUE(.check_all_character_value(adjusted_table[1])) ) {
                adjusted_col1_char <- shiny::tags$p(shiny::icon("check"), "The first column 'sample_name' contains character values.", style="font-size: 16px;")
            } else {
                adjusted_col1_char <- shiny::tags$p(shiny::icon("times"), "The first column 'sample_name' must contain character values.", style="font-size: 16px;color:red;")
                adjusted_error <- adjusted_error + 1
            }
            ## sample_name must same as the name of samples of exp_data
            if (isTRUE(.check_colName(exp_data[-1], adjusted_table[[1]], strict=FALSE)) ) {
                adjusted_sample_name <- shiny::tags$p(shiny::icon("check"), "Sample names in 'sample_name' column are as same as the sample names in lipid abundance data.", style="font-size: 16px;")
            } else {
                adjusted_sample_name <- shiny::tags$p(shiny::icon("times"), "Sample names in 'sample_name' column must be as same as the sample names in lipid abundance data.", style="font-size: 16px;color:red;")
                adjusted_error <- adjusted_error + 1
            }
            ## DIV
            if (adjusted_error==0) {
                adjusted_checks <- shiny::tags$p("No errors require correction.", style="font-size: 16px;")
            } else {
                adjusted_checks <- shiny::tags$p(paste0("Detect ", adjusted_error," error(s).") , style="font-size: 16px;color:red;")
            }
        } else {
            adjusted_checks <- shiny::tags$p("No data has been uploaded.", style="font-size: 16px;")
        }

        ## return message
        return_div <- shiny::tags$div(
            shiny::tags$p(shiny::strong("Data check results of Correlation analysis"),style="font-size: 24px;"),
            shiny::tags$p(shiny::strong("Lipid abundance data"),style="font-size: 20px;"),
            shiny::tags$ul(
                exp_char, convert_value, miss_value, lipid_UTF8, lipid_UTF8_message, sample_UTF8, sample_UTF8_message, lipid_na, lipid_dup, rm_feature, id_convert, featureNum_aval, sample_num, rm_sample),
            shiny::tags$p(shiny::strong("Condition table"),style="font-size: 20px;"),
            shiny::tags$ul(
                condition_col_name, condition_sample_name,
                condition_char, condition_numeric, condition_num, 
                condition_UTF8, condition_UTF8_message),
            shiny::tags$p(shiny::strong("Adjusted table"),style="font-size: 20px;"),
            if (!is.null(adjusted_table)) {
                shiny::tags$ul(
                    adjusted_col_name, adjusted_col1_char, adjusted_sample_name,
                    adjusted_UTF8, adjusted_UTF8_message)
            } else {
                shiny::tags$ul(
                    "No data has been uploaded.",style="font-size: 16px;")
            }
            , style="font-size: 0px;"
        )
        ## return check summary
        if (exp_error==0 && exp_warning ==0) {
            exp_checks <- shiny::tags$p("No errors require correction.", style="font-size: 16px;")
            exp_warns <- NULL
        } else {
            exp_checks <- shiny::tags$p(paste0("Detect ", exp_error," error(s).") , style="font-size: 16px;color:red;")
            exp_warns <- shiny::tags$p(paste0("Detect ", exp_warning," warning(s).") , style="font-size: 16px;color:blue;")
        }
        if (condition_error==0) {
            condition_checks <- shiny::tags$p("No errors require correction.", style="font-size: 16px;")
        } else {
            condition_checks <- shiny::tags$p(paste0("Detect ", condition_error," error(s).") , style="font-size: 16px;color:red;")
        }
        check_sum_div <- shiny::tags$div(
            shiny::tags$p(shiny::strong("Data Check completed."),style="font-size: 24px;"),
            shiny::tags$p("A summary of the checking results for each input dataset is available below. Please correct any detected errors.
                          Warnings signify the steps taken in the data check process but do not impact the analysis. Scroll down for a detailed overview of all checks.
                          You will also find your uploaded data and the post-processing expression data.", style="font-size: 18px;"),
            shiny::tags$p(shiny::strong("Lipid abundance data"),style="font-size: 20px;"),
            shiny::tags$ul(exp_checks),
            shiny::tags$ul(exp_warns),
            shiny::tags$p(shiny::strong("Condition table"),style="font-size: 20px;"),
            shiny::tags$ul(condition_checks),
            shiny::tags$p(shiny::strong("Adjusted table"),style="font-size: 20px;"),
            shiny::tags$ul(adjusted_checks),
            style="font-size: 0px;"
        )
    }

    ## output data frame
    if (lipid_unmatched==nrow(exp_data_check)) {
        exp_data_process <- NULL
    } else {
        exp_data_process <- as.data.frame(SummarizedExperiment::assay(parseable_se)) %>%
            tibble::rownames_to_column(var="feature")
    }
    # exp_data_process <- as.data.frame(SummarizedExperiment::assay(se)) %>%
    #     tibble::rownames_to_column(var="feature")

    if (!is.null(parseable_se)){
        lipid_char_process <- parseable_se %>% 
          SummarizedExperiment::rowData() %>% as.data.frame() 
    } else {
        lipid_char_process <- NULL
    }

    if (analysis_type=="DE") {
        if (all(c("NULL1", "NULL2") %in% colnames(group_info)) ){
            group_info <- group_info %>% dplyr::select(-c("NULL1", "NULL2"))
        } else if ("NULL1" %in% colnames(group_info)) {
            group_info <- group_info %>% dplyr::select(-"NULL1")
        }
        group_info_filt <- group_info[group_info[[1]] %in% colnames(exp_data) , ]
        if (isTRUE(nrow(group_info_filt)==0)) {
            group_info_process <- as.data.frame(group_info)
        } else {
            group_info_process <- as.data.frame(group_info_filt)
        }
    } else {
        group_info_process <- NULL
    }
    if (analysis_type %in% c("ML", "Correlation")) {
        condition_table_process <- condition_table[condition_table[[1]] %in% colnames(exp_data) , ]
    } else {
        condition_table_process <- NULL
    }
    if (analysis_type=="Correlation") {
        adjusted_table_process <- adjusted_table[adjusted_table[[1]] %in% colnames(exp_data) , ]
    } else {
        adjusted_table_process <- NULL
    }

    return(list(
        return_div=return_div, check_sum_div=check_sum_div,
        nonParseable_lipid=unmatched_names, exp_naming_error=exp_naming_error,
        exp_data=exp_data, group_info=group_info, 
        condition_table=condition_table, adjusted_table=adjusted_table,
        exp_data_process=exp_data_process, lipid_char_process=lipid_char_process,
        group_info_process=group_info_process, condition_table_process=condition_table_process,
        adjusted_table_process=adjusted_table_process))
}