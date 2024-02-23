#' Data segmentation based on Q16
#'
#' Generate data set with segmentation variables for "Communication and
#' Interaction".
#'
#' @param data_set data set (either raw or already added some segmentatin vars)
#' @param from_vals numeric vector giving which values to recode from
#' @param sum_score_val_grun numeric score value for summation score of
#'   "grunnleggende"; if summation score larger/larger than/equal
#'   `sum_score_val_grun`, then employee has grunnleggende skills
#' @param sum_score_val_vide numeric score value for summation score of
#'   "videregaende"; if summation score larger/larger than/equal
#'   `sum_score_val_vide`, then employee has videregaende skills
#' @param sum_score_val_avan numeric score value for summation score of
#'   "avansert"; if summation score larger/larger than/equal
#'   `sum_score_val_avan`, then employee has avansert skills
#' @param type_val_grun character string: either "larger-equal" or "equal" to
#'   indicate how to apply the sum_score threshold (allow larger/equal to get 1
#'   or only equality to that sum_score to get a 1 indicator, zero else)
#' @param type_val_vide character string: either "larger-equal" or "equal" to
#'   indicate how to apply the sum_score threshold (allow larger/equal to get 1
#'   or only equality to that sum_score to get a 1 indicator, zero else)
#' @param type_val_avan character string: either "larger-equal" or "equal" to
#'   indicate how to apply the sum_score threshold (allow larger/equal to get 1
#'   or only equality to that sum_score to get a 1 indicator, zero else)
#'
#' @return a data set with additional variables, most importantly
#'   "kat_kommunikasjon" which is a factor:
#'   \itemize{
#'   \item "Uerfaren": 0
#'   \item "Grunnleggende": 1
#'   \item "Videregående": 2
#'   \item "Avansert": 3
#'   }
#' @export
recode_q16 <- function(data_set,
                       from_vals = c(3, 4),
                       sum_score_val_grun = 4,
                       sum_score_val_vide = 2,
                       sum_score_val_avan = 2,
                       type_val_grun = "larger-equal",
                       type_val_vide = "larger-equal",
                       type_val_avan = "equal") {
  data_out <- data_set
  #### Coding of 'kat_kommunikasjon' from Q16
  #### Sets the value 5 to 1 on ALL indicator variables
  data_out <- data_out %>% recode_qXX_rVals(q_names = paste0("Q16r", 1:11),
                                            from = 5, to = 1)
  testvec <- names(data_out)
  # Create new dichotomous variables where values 3 and 4 of the indicator are
  # given a value of 1, and 0 otherwise (see argument 'from_vals')
  #### Start with grunnleggende niva -> Q16r1 - Q16r6
  ## 1. Generate new variable taking the sum of the 6 variables in grunnleggende
  ## 2. Generate new variable that says that if score on basic_com is 4 or
  ##    higher, then value = 1, 0 if not
  data_out <- data_out %>% generate_segmentation_variable(
    name_recode1 = "kom",
    from_vals = from_vals,
    seq_recode = 1:6,
    name_var_old = "Q16r",
    name_var_sum = "grunnleg_kom_sum",
    name_var_seg = "grunn_kom",
    ref_val = sum_score_val_grun,
    type = type_val_grun)
  #### Videregaende niva -> Q16r7 - Q16r9
  ## 1. Create an aggregate variable for high school. Sample one for high school
  ##    with less strict criteria: create one where you must have answered 3/4
  ##    on 2 out of 3 to be in the category
  ## 2. Generate a new variable that says that if the score on videre_kom is 2
  ##    or higher, then value = 2, 0 else
  data_out <- data_out %>% generate_segmentation_variable(
    name_recode1 = "kom",
    from_vals = from_vals,
    seq_recode = 7:9,
    name_var_old = "Q16r",
    name_var_sum = "videre_kom_sum",
    name_var_seg = "videre_kom",
    ref_val = sum_score_val_vide,
    type = type_val_vide)
  #### Avansert niva -> Q16r10 - Q16r11
  ## 1. Create an overall variable for advanced/avansert
  data_out <- data_out %>%
    generate_segmentation_variable(
      name_recode1 = "kom",
      from_vals = from_vals,
      seq_recode = 10:11,
      name_var_old = "Q16r",
      name_var_sum = "avan_kom_sum",
      name_var_seg = "avan_kom",
      ref_val = sum_score_val_avan,
      type = type_val_avan)
  # Total categorical variable for Q16
  data_out <- data_out %>%
    add_overall_kat(name_kat = "kat_kommunikasjon",
                    names_segmentation_vars = c("grunn_kom",
                                                "videre_kom",
                                                "avan_kom"))
  data_out <- data_out %>%
    dplyr::select(
      tidyselect::all_of(testvec),
      .data$kom1,
      .data$kom2,
      .data$kom3,
      .data$kom4,
      .data$kom5,
      .data$kom6,
      .data$kom7,
      .data$kom8,
      .data$kom9,
      .data$kom10,
      .data$kom11,
      .data$grunnleg_kom_sum,
      .data$grunn_kom,
      .data$videre_kom_sum,
      .data$videre_kom,
      .data$avan_kom,
      .data$kat_kommunikasjon)
  return(data_out)
}
#' Data segmentation based on Q17
#'
#' Generate data set with segmentation variables for "Information security and
#' privacy".
#'
#' @inheritParams recode_q16
#'
#' @return a data set with additional variables, most importantly
#'   "kat_informasjon1" which is a factor:
#'   \itemize{
#'   \item "Uerfaren": 0
#'   \item "Grunnleggende": 1
#'   \item "Videregående": 2
#'   \item "Avansert": 3
#'   }
#' @export
recode_q17 <- function(data_set,
                       from_vals = c(3, 4),
                       sum_score_val_grun = 3,
                       sum_score_val_vide = 3,
                       sum_score_val_avan = 3,
                       type_val_grun = "larger-equal",
                       type_val_vide = "equal",
                       type_val_avan = "equal") {
  data_out <- data_set
  # Coding of 'kat_informasjon1' from Q17
  # Sets the value 5 to 1 on ALL indicator variables
  data_out <- data_out %>% recode_qXX_rVals(q_names = paste0("Q17r", 1:10),
                                            from = 5, to = 1)
  testvec <- names(data_out)
  # Create new dichotomous variables where values 3 and 4 of the indicator are
  # given a value of 1, and 0 otherwise (see argument 'from_vals')
  #### Start with grunnleggende niva -> Q17r1 - Q17r4
  ## 1. Generate summation score variable for gunnleggende info taking the sum
  ##    of the first four info vars
  ## 2. Re-code summation score into 1 if larger than reference value (e.g. = 3)
  ##    zero else
  data_out <- data_out %>%
    generate_segmentation_variable(
      name_recode1 = "info",
      from_vals = from_vals,
      seq_recode = 1:4,
      name_var_old = "Q17r",
      name_var_sum = "grunnleg_info_sum",
      name_var_seg = "grunn_info1",
      ref_val = sum_score_val_grun,
      type = type_val_grun)

  #### Videregaende niva -> Q17r5 - Q17r7
  ## 1. Create an aggregate variable for secondary education where you have to
  ##    have answered 3/4 on 2 out of 3 to be in the category (same as in
  ##    kat_kommunikasjon)
  ## 2. Re-code into 0 if summation score for videre equals reference value
  ##    (e.g. 3), zero else
  data_out <- data_out %>%
    generate_segmentation_variable(
      name_recode1 = "info",
      from_vals = from_vals,
      seq_recode = 5:7,
      name_var_old = "Q17r",
      name_var_sum = "videre_info_sum",
      name_var_seg = "videre_info",
      ref_val = sum_score_val_vide,
      type = type_val_vide)
  #### Avansert niva -> Q17r8 - Q17r10
  ## 1. Re-code into 1 if all info vars 8-10 are present to get aggregate
  ##    variable for advanced skills
  data_out <- data_out %>%
    generate_segmentation_variable(
      name_recode1 = "info",
      from_vals = from_vals,
      seq_recode = 8:10,
      name_var_old = "Q17r",
      name_var_sum = "avan_info_sum",
      name_var_seg = "avan_info",
      ref_val = sum_score_val_avan,
      type = type_val_avan)
  # Overall categorical variable for Q17 - kat_informasjon1
  data_out <- data_out %>%
    add_overall_kat(name_kat = "kat_informasjon1",
                    names_segmentation_vars = c("grunn_info1",
                                                "videre_info",
                                                "avan_info"))
  data_out <- data_out %>%
    dplyr::select(tidyselect::all_of(testvec),
                  .data$info1,
                  .data$info2,
                  .data$info3,
                  .data$info4,
                  .data$info5,
                  .data$info6,
                  .data$info7,
                  .data$info8,
                  .data$info9,
                  .data$info10,
                  .data$grunnleg_info_sum,
                  .data$grunn_info1,
                  .data$videre_info_sum,
                  .data$videre_info,
                  .data$avan_info,
                  .data$kat_informasjon1)
  return(data_out)
}
#' Data segmentation based on Q14
#'
#' Generate data set with segmentation variables for "Proficiency in using
#' software".
#'
#' @inheritParams recode_q16
#'
#' @return a data set with additional variables, most importantly
#'   "kat_programmer1" which is a factor:
#'   \itemize{
#'   \item "Uerfaren": 0
#'   \item "Grunnleggende": 1
#'   \item "Videregående": 2
#'   \item "Avansert": 3
#'   }
#' @export
recode_q14 <- function(data_set,
                       from_vals = c(3, 4),
                       sum_score_val_grun = 2,
                       sum_score_val_vide = 3,
                       sum_score_val_avan = 2,
                       type_val_grun = "larger-equal",
                       type_val_vide = "equal",
                       type_val_avan = "equal") {
  data_out <- data_set
  # Coding of 'kat_programmer1' from Q14
  # Sets the value 5 to 1 on ALL indicator variables
  data_out <- data_out %>% recode_qXX_rVals(q_names = paste0("Q14r", 1:8),
                                            from = 5, to = 1)
  testvec <- names(data_out)
  # Create new dichotomous variables where values 3 and 4 of the indicator are
  # given a value of 1, and 0 otherwise (see argument 'from_vals')
  #### Start with grunnleggende niva -> Q14r1 - Q14r3
  ## 1. Generate summation score variable for gunnleggende prog taking the sum
  ##    of the first four prog vars
  ## 2. Re-code summation score into 1 if larger than reference value (e.g. = 2)
  ##    zero else
  data_out <- data_out %>% generate_segmentation_variable(
    name_recode1 = "prog",
    from_vals = from_vals,
    seq_recode = 1:3,
    name_var_old = "Q14r",
    name_var_sum = "grunnleg_prog_sum",
    name_var_seg = "grunn_prog1",
    ref_val = sum_score_val_grun,
    type = type_val_grun)

  #### Videregaende niva -> Q14r4 - Q14r6
  ## 1. Create an aggregate variable for secondary education where you have to
  ##    have answered 3/4 on 2 out of 3 to be in the category (same as in
  ##    kat_kommunikasjon)
  ## 2. Re-code into 0 if summation score for videre equals reference value
  ##    (e.g. 3), zero else
  data_out <- data_out %>% generate_segmentation_variable(
    name_recode1 = "prog",
    from_vals = from_vals,
    seq_recode = 4:6,
    name_var_old = "Q14r",
    name_var_sum = "videre_prog_sum",
    name_var_seg = "videre_prog",
    ref_val = sum_score_val_vide,
    type = type_val_vide)
  #### Avansert niva -> Q14r7 - Q14r8
  ## 1. Re-code into 1 if all prog vars 7-8 are present to get aggregate
  ##    variable for advanced skills
  data_out <- data_out %>% generate_segmentation_variable(
    name_recode1 = "prog",
    from_vals = from_vals,
    seq_recode = 7:8,
    name_var_old = "Q14r",
    name_var_sum = "avan_prog_sum",
    name_var_seg = "avan_prog",
    ref_val = sum_score_val_avan,
    type = type_val_avan)
  # Overall categorical variable for Q14 - kat_programmer1
  data_out <- data_out %>%
    add_overall_kat(name_kat = "kat_programmer1",
                    names_segmentation_vars = c("grunn_prog1",
                                                "videre_prog",
                                                "avan_prog"))
  data_out <- data_out %>% dplyr::select(tidyselect::all_of(testvec),
                                         .data$prog1,
                                         .data$prog2,
                                         .data$prog3,
                                         .data$prog4,
                                         .data$prog5,
                                         .data$prog6,
                                         .data$prog7,
                                         .data$prog8,
                                         .data$grunnleg_prog_sum,
                                         .data$grunn_prog1,
                                         .data$videre_prog_sum,
                                         .data$videre_prog,
                                         .data$avan_prog,
                                         .data$kat_programmer1)
  return(data_out)
}
#' Data segmentation based on Q19
#'
#' Generate data set with segmentation variables for "Proficiency in using
#' technology".
#'
#' @inheritParams recode_q16
#'
#' @return a data set with additional variables, most importantly
#'   "kat_utstyr1" which is a factor:
#'   \itemize{
#'   \item "Uerfaren": 0
#'   \item "Grunnleggende": 1
#'   \item "Videregående": 2
#'   \item "Avansert": 3
#'   }
#' @export
recode_q19 <- function(data_set,
                       from_vals = c(3, 4),
                       sum_score_val_grun = 2,
                       sum_score_val_vide = 5,
                       sum_score_val_avan = 3,
                       type_val_grun = "larger-equal",
                       type_val_vide = "equal",
                       type_val_avan = "equal") {
  data_out <- data_set
  # Coding of 'kat_utstyr1' from Q19
  # Sets the value 5 to 1 on ALL indicator variables
  data_out <- data_out %>% recode_qXX_rVals(q_names = paste0("Q19r", 1:11),
                                            from = 5, to = 1)
  testvec <- names(data_out)
  # Create new dichotomous variables where values 3 and 4 of the indicator are
  # given a value of 1, and 0 otherwise (see argument 'from_vals')
  #### Start with grunnleggende niva -> Q19r1 - Q19r3
  ## 1. Generate summation score variable for gunnleggende utstyr taking the sum
  ##    of the first four utstyr vars
  ## 2. Re-code summation score into 1 if larger than reference value (e.g. = 2)
  ##    zero else
  data_out <- data_out %>%
    generate_segmentation_variable(
      name_recode1 = "utstyr",
      from_vals = from_vals,
      seq_recode = 1:3,
      name_var_old = "Q19r",
      name_var_sum = "grunnleg_utstyr_sum",
      name_var_seg = "grunn_utstyr1",
      ref_val = sum_score_val_grun,
      type = type_val_grun)

  #### Videregaende niva -> Q19r4 - Q19r8
  ## 1. Create an aggregate variable for secondary education where you have to
  ##    have answered 3/4 on 2 out of 3 to be in the category (same as in
  ##    kat_kommunikasjon)
  ## 2. Re-code into 0 if summation score for videre equals reference value
  ##    (e.g. 5), zero else
  data_out <- data_out %>%
    generate_segmentation_variable(name_recode1 = "utstyr",
                                   from_vals = from_vals,
                                   seq_recode = 4:8,
                                   name_var_old = "Q19r",
                                   name_var_sum = "videre_utstyr_sum",
                                   name_var_seg = "videre_utstyr",
                                   ref_val = sum_score_val_vide,
                                   type = type_val_vide)
  #### Avansert niva -> Q19r9 - Q19r11
  ## 1. Re-code into 1 if all utstyr vars 9-11 are present to get aggregate
  ##    variable for advanced skills
  data_out <- data_out %>%
    generate_segmentation_variable(name_recode1 = "utstyr",
                                   from_vals = from_vals,
                                   seq_recode = 9:11,
                                   name_var_old = "Q19r",
                                   name_var_sum = "avan_utstyr_sum",
                                   name_var_seg = "avan_utstyr",
                                   ref_val = sum_score_val_avan,
                                   type = type_val_avan)
  # Overall categorical variable for Q19 - kat_utstyrr1
  data_out <- data_out %>%
    add_overall_kat(name_kat = "kat_utstyr1",
                    names_segmentation_vars = c("grunn_utstyr1",
                                                "videre_utstyr",
                                                "avan_utstyr"))
  data_out <- data_out %>% dplyr::select(tidyselect::all_of(testvec),
                                         .data$utstyr1,
                                         .data$utstyr2,
                                         .data$utstyr3,
                                         .data$utstyr4,
                                         .data$utstyr5,
                                         .data$utstyr6,
                                         .data$utstyr7,
                                         .data$utstyr8,
                                         .data$utstyr9,
                                         .data$utstyr10,
                                         .data$utstyr11,
                                         .data$grunnleg_utstyr_sum,
                                         .data$grunn_utstyr1,
                                         .data$videre_utstyr_sum,
                                         .data$videre_utstyr,
                                         .data$avan_utstyr,
                                         .data$kat_utstyr1)
  return(data_out)
}
#' Re-codes value of a variable (withlevels) into another value
#'
#' The variable types are of
#' \code{class(var_type) -> 'haven_labelled', 'vctrs_vctr', 'double'} and
#' \code{typeof(var_type) -> double} and can be e.g. \code{Q16r1-Q16r11,
#' Q17r1-Q17r10, Q14r1-Q14r8, Q19r1-Q19r11, Q36, Q37, Q38} etc. The level e.g.
#' 5 (which corresponds to "Vet ikke" i.e. "Do not know") is often re-coded to
#' 1 (which corresponds to "Ikke i det hele tatt" or similar, which corresponds
#' to some form of "no"/'not at all").
#'
#' @param data_set the data set containing the variable to re_code
#' @param q_names character giving the name of the variable to re-code
#' @param from the value to be changed
#' @param to the new value to be assigned (instead of the one passed via
#'    \code{from})
#' @param na_replacement value to replace \code{NA} values with; defaults to
#'   \code{NULL} but must be set to a numeric value is \code{NA}'s are present,
#'   otherwise there will be an error
#' @param SETTINGS_FACT a (named!) list with the following logical values:
#' \itemize{
#'   \item{\code{ADJUST_LABELS:}}{logical; if \code{TRUE} then
#'   \code{class(var_type) -> 'haven_labelled', 'vctrs_vctr', 'double'} labels
#'   are adjusted to the length of actual levels of the variable after
#'   re-coding took place}
#'   \item{\code{AS_FACTOR:}}{logical; if \code{TRUE}, then conversion to a
#'   factor is performed}
#'   \item{\code{ORDERED:}}{logical; if \code{TRUE} (and if \code{AS_FACTOR =
#'   TRUE}), then the factor will be ordered}
#'   \item{ADD_LABELS}{logical; if \code{TRUE} (and if \code{AS_FACTOR =
#'   TRUE}), then factor labels are set}
#' }
#'
#' @return the same data set but with the re-coded variable(s)
#' @export
recode_qXX_rVals <- function(data_set, q_names, from = 5, to = 1,
                             na_replacement = NULL,
                             SETTINGS_FACT = list(ADJUST_LABELS = FALSE,
                                                  AS_FACTOR = FALSE,
                                                  ORDERED = FALSE,
                                                  ADD_LABELS = FALSE)) {
  num_from <- length(from)
  num_to   <- length(to)
  stopifnot(num_from == num_to)
  data_out <- data_set
  for (i in q_names) {
    if (any(is.na(data_out[[i]]))) {
      if (!is.null(na_replacement)) {
        data_out[[i]][which(is.na(data_out[[i]]))] <- na_replacement
      } else {
        stop("Variable contains NA-values but replacement is set to 'NULL'.")
      }
    }
    for (j in 1:num_from) {
      data_out[[i]][data_out[[i]] == from[j]] <- to[j]
    }
    data_out[[i]] <- get_factor_adjusted(data_out[[i]], SETTINGS_FACT)
  }
  return(data_out)
}
get_factor_adjusted <- function(x, sttgs) {
  stopifnot(all(sapply(sttgs, is.logical)))
  if (sttgs$ADJUST_LABELS) {
    x <- get_adjusted_labels(x)
  }
  if (!sttgs$AS_FACTOR) return(x)
  if (sttgs$ADD_LABELS) {
    tmp_labels <- attr(x, which = "labels")
    if (!is.null(tmp_labels)) {
      tmp_levels <- unname(tmp_labels)
      tmp_labels <- names(tmp_labels)
      return(factor(x, ordered = sttgs$ORDERED,
                    levels = tmp_levels,
                    labels = tmp_labels))
    } else {
      return(factor(x, ordered = sttgs$ORDERED))
    }
  } else {
    return(factor(x, ordered = sttgs$ORDERED))
  }
}
get_adjusted_labels <- function(x) {
  lvl1    <- unname(attr(x, which = "labels"))
  if (!is.null(lvl1)) { # This is the case when x is a haven-labelled factor
    tmp_fac <- factor(x)
    lvl2    <- as.numeric(levels(tmp_fac))
    remove_lvl_ids <- setdiff(lvl1, lvl2)
    if (length(remove_lvl_ids) > 0) {
      attr(x, which = "labels") <- attr(x, which = "labels")[-remove_lvl_ids]
    }
  } else if (is.null(lvl1)) { # This is the case when x is a standard factor
    if (!is.null(attr(x, which = "labels"))) {
      lvl1 <- attr(x, which = "levels")
      lvl2 <- attr(droplevels(factor(x)), which = "levels")
      remove_lvl_ids <- setdiff(lvl1, lvl2)
      if (length(remove_lvl_ids) > 0) {
        attr(x, which = "labels") <- attr(x, which = "labels")[-remove_lvl_ids]
      }
    }
  } else if (is.null(lvl1)) {
    stop("Something wrong with input vars/labels/levels.")
  }
  return(x)
}
#' Re-codes a set of value of a have-factor variable (with levels)
#'
#' The variable types permitted are of \code{class(var_type) ->
#' 'haven_labelled', 'vctrs_vctr', 'double'} and \code{typeof(var_type) ->
#' double} and can be e.g. \code{Q16r1-Q16r11, Q17r1-Q17r10, Q14r1-Q14r8,
#' Q19r1-Q19r11, Q36, Q37, Q38} etc. The levels e.g. 5 (which corresponds to
#' "Vet ikke" i.e. "Do not know") and 1 (which corresponds to "Ikke i det hele
#' tatt" or similar, which means some form of "no"/'not at all") are often made
#' zero, whereas higher values like 2,3 and 4 are then mapped to 1. So this
#' functions creates a categorical or indicator variable
#'
#' @inheritParams recode_qXX_rVals
#' @param list_recodes a list of elements of \code{dynamic_dots}-type that will
#'   be passed down to [dplyr::recode_factor()] and gives the from/to value
#'   re-coding as required by this function
#' @param new_names character vector with same lengths as \code{q_names} and
#'   \code{list_recodes} giving the new variable names of the resulting
#'   indicators
#'
#' @return the same data set but with the re-coded variable(s)
#' @export
recode_qXX_cats <- function(data_set, q_names,
                            list_recodes,
                            new_names,
                            SETTINGS_FACT) {
  stopifnot(length(q_names) == length(list_recodes))
  stopifnot(length(q_names) == length(new_names))
  data_out <- data_set
  for (i in seq_along(q_names)) {
    tmp_factor <- get_factor_adjusted(data_out[[q_names[i]]], SETTINGS_FACT)
    tmp_factor <- tmp_factor %>%
      dplyr::recode_factor(!!!list_recodes[[i]],
                           .ordered = SETTINGS_FACT$ORDERED)
    data_out[[new_names[i]]] <- tmp_factor
  }
  return(data_out)
}
generate_segmentation_variable <- function(data_set,
                                           from_vals,
                                           seq_recode,
                                           name_recode1,
                                           name_var_old,
                                           name_var_sum,
                                           name_var_seg,
                                           ref_val,
                                           type) {
  new_var <- paste0(name_recode1, seq_recode)
  old_var <- paste0(name_var_old, seq_recode)
  seq_run <- seq_along(seq_recode)
  for (i in seq_run) {
    tmp_var <- new_var[i]
    data_set <- data_set %>%
      dplyr::mutate("{tmp_var}" := recode_skills_present(.data[[old_var[i]]],
                                                         from_vals = from_vals))
  }
  data_set %>%
    dplyr::mutate("{name_var_sum}" := get_sum_score(new_var)) %>%
    dplyr::mutate("{name_var_seg}" := sum_score_exceed(.data[[name_var_sum]],
                                                       ref_val = ref_val,
                                                       type = type))
}
recode_skills_present <- function(var, from_vals) {
  dplyr::if_else(var %in% from_vals, 1, 0)
}
get_sum_score <- function(var) {
  rowSums(dplyr::pick(tidyselect::all_of(var)))
}
sum_score_exceed <- function(var, ref_val, type) {
  if (type == "larger") {
    dplyr::if_else(var > ref_val, 1, 0)
  } else if (type == "equal") {
    dplyr::if_else(var == ref_val, 1, 0)
  } else if (type == "larger-equal") {
    dplyr::if_else(var >= ref_val, 1, 0)
  } else {
    stop("unknown arg value for 'type'")
  }
}
add_overall_kat <- function(data_set, name_kat,
                            names_segmentation_vars,
                            kat_levels = 0:3,
                            kat_labels = c("Uerfaren",
                                           "Grunnleggende",
                                           "Videreg\u00e5ende",
                                           "Avansert")) {
  data_out <- data_set %>%
    dplyr::mutate("{name_kat}" := dplyr::case_when(
      (.data[[names_segmentation_vars[1]]] == 0) ~ 0L,
      (.data[[names_segmentation_vars[1]]] == 1 & .data[[names_segmentation_vars[2]]] == 0 & .data[[names_segmentation_vars[3]]] == 0) ~ 1L,
      (.data[[names_segmentation_vars[1]]] == 1 & .data[[names_segmentation_vars[2]]] == 1 & .data[[names_segmentation_vars[3]]] == 0 |
         .data[[names_segmentation_vars[1]]] == 1 & .data[[names_segmentation_vars[2]]] == 0 & .data[[names_segmentation_vars[3]]] == 1) ~ 2L,
      (.data[[names_segmentation_vars[1]]] == 1 & .data[[names_segmentation_vars[2]]] == 1 & .data[[names_segmentation_vars[3]]] == 1) ~ 3L,
      TRUE ~ NA_integer_))
  # Transform to factor
  data_out[[name_kat]] <- factor(data_out[[name_kat]],
                                 levels = kat_levels,
                                 labels = kat_labels)
  return(data_out)
}
