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
                       sum_score_val_avan = 2) {
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
  data_out <- data_out %>% generate_segmentation_variable(name_recode1 = "kom",
                                                          from_vals = from_vals,
                                                          seq_recode = 1:6,
                                                          name_var_old = "Q16r",
                                                          name_var_sum = "grunnleg_kom_sum",
                                                          name_var_seg = "grunn_kom",
                                                          ref_val = sum_score_val_grun,
                                                          type = "larger-equal")
  #### Videregaende niva -> Q16r7 - Q16r9
  ## 1. Create an aggregate variable for high school. Sample one for high school
  ##    with less strict criteria: create one where you must have answered 3/4
  ##    on 2 out of 3 to be in the category
  ## 2. Generate a new variable that says that if the score on videre_kom is 2
  ##    or higher, then value = 2, 0 else
  data_out <- data_out %>% generate_segmentation_variable(name_recode1 = "kom",
                                                          from_vals = from_vals,
                                                          seq_recode = 7:9,
                                                          name_var_old = "Q16r",
                                                          name_var_sum = "videre_kom_sum",
                                                          name_var_seg = "videre_kom",
                                                          ref_val = sum_score_val_vide,
                                                          type = "larger-equal")
  #### Avansert niva -> Q16r10 - Q16r11
  ## 1. Create an overall variable for advanced/avansert
  data_out <- data_out %>% generate_segmentation_variable(name_recode1 = "kom",
                                                          from_vals = from_vals,
                                                          seq_recode = 10:11,
                                                          name_var_old = "Q16r",
                                                          name_var_sum = "avan_kom_sum",
                                                          name_var_seg = "avan_kom",
                                                          ref_val = sum_score_val_avan,
                                                          type = "equal")
  # Total categorical variable for Q16
  data_out <- data_out %>%
    dplyr::mutate(kat_kommunikasjon = dplyr::case_when(
      (grunn_kom == 0) ~ 0L,
      (grunn_kom == 1 & videre_kom == 0 & avan_kom == 0) ~ 1L,
      (grunn_kom == 1 & videre_kom == 1 & avan_kom == 0 |
         grunn_kom == 1 & videre_kom == 0 & avan_kom == 1) ~ 2L,
      (grunn_kom == 1 & videre_kom == 1 & avan_kom == 1) ~ 3L,
      TRUE ~ NA_integer_))
  # Transform to factor
  data_out$kat_kommunikasjon <- factor(data_out$kat_kommunikasjon,
                                       levels = c(0, 1, 2, 3),
                                       labels = c("Uerfaren",
                                                  "Grunnleggende",
                                                  "Videregående",
                                                  "Avansert"))
  data_out <- data_out %>% dplyr::select(tidyselect::all_of(testvec),
                                         kom1, kom2, kom3, kom4, kom5,
                                         kom6, kom7, kom8, kom9, kom10, kom11,
                                         grunnleg_kom_sum, grunn_kom,
                                         videre_kom_sum, videre_kom,
                                         avan_kom, kat_kommunikasjon)
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
                       sum_score_val_avan = 3) {
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
  data_out <- data_out %>% generate_segmentation_variable(name_recode1 = "info",
                                                          from_vals = from_vals,
                                                          seq_recode = 1:4,
                                                          name_var_old = "Q17r",
                                                          name_var_sum = "grunnleg_info_sum",
                                                          name_var_seg = "grunn_info1",
                                                          ref_val = sum_score_val_grun,
                                                          type = "larger-equal")

  #### Videregaende niva -> Q17r5 - Q17r7
  ## 1. Create an aggregate variable for secondary education where you have to
  ##    have answered 3/4 on 2 out of 3 to be in the category (same as in
  ##    kat_kommunikasjon)
  ## 2. Re-code into 0 if summation score for videre equals reference value
  ##    (e.g. 3), zero else
  data_out <- data_out %>% generate_segmentation_variable(name_recode1 = "info",
                                                          from_vals = from_vals,
                                                          seq_recode = 5:7,
                                                          name_var_old = "Q17r",
                                                          name_var_sum = "videre_info_sum",
                                                          name_var_seg = "videre_info",
                                                          ref_val = sum_score_val_vide,
                                                          type = "equal")
  #### Avansert niva -> Q17r8 - Q17r10
  ## 1. Re-code into 1 if all info vars 8-10 are present to get aggregate
  ##    variable for advanced skills
  data_out <- data_out %>% generate_segmentation_variable(name_recode1 = "info",
                                                          from_vals = from_vals,
                                                          seq_recode = 8:10,
                                                          name_var_old = "Q17r",
                                                          name_var_sum = "avan_info_sum",
                                                          name_var_seg = "avan_info",
                                                          ref_val = sum_score_val_avan,
                                                          type = "equal")
  # Overall categorical variable for Q17 - kat_informasjon1
  data_out <- data_out %>%
    dplyr::mutate(kat_informasjon1 = dplyr::case_when(
      (grunn_info1 == 0) ~ 0L,
      (grunn_info1 == 1 & videre_info == 0 & avan_info == 0) ~ 1L,
      (grunn_info1 == 1 & videre_info == 1 & avan_info == 0 |
         grunn_info1 == 1 & videre_info == 0 & avan_info == 1) ~ 2L,
      (grunn_info1 == 1 & videre_info == 1 & avan_info == 1) ~ 3L,
      TRUE ~ NA_integer_))
  # Transform to factor
  data_out$kat_informasjon1 <- factor(data_out$kat_informasjon1,
                                      levels = c(0, 1, 2, 3),
                                      labels = c("Uerfaren",
                                                 "Grunnleggende",
                                                 "Videregående",
                                                 "Avansert"))
  data_out <- data_out %>% dplyr::select(tidyselect::all_of(testvec),
                                         info1, info2, info3, info4, info5,
                                         info6, info7, info8, info9, info10,
                                         grunnleg_info_sum, grunn_info1,
                                         videre_info_sum, videre_info,
                                         avan_info, kat_informasjon1)
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
                       sum_score_val_avan = 2) {
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
  data_out <- data_out %>% generate_segmentation_variable(name_recode1 = "prog",
                                                          from_vals = from_vals,
                                                          seq_recode = 1:3,
                                                          name_var_old = "Q14r",
                                                          name_var_sum = "grunnleg_prog_sum",
                                                          name_var_seg = "grunn_prog1",
                                                          ref_val = sum_score_val_grun,
                                                          type = "larger-equal")

  #### Videregaende niva -> Q14r4 - Q14r6
  ## 1. Create an aggregate variable for secondary education where you have to
  ##    have answered 3/4 on 2 out of 3 to be in the category (same as in
  ##    kat_kommunikasjon)
  ## 2. Re-code into 0 if summation score for videre equals reference value
  ##    (e.g. 3), zero else
  data_out <- data_out %>% generate_segmentation_variable(name_recode1 = "prog",
                                                          from_vals = from_vals,
                                                          seq_recode = 4:6,
                                                          name_var_old = "Q14r",
                                                          name_var_sum = "videre_prog_sum",
                                                          name_var_seg = "videre_prog",
                                                          ref_val = sum_score_val_vide,
                                                          type = "equal")
  #### Avansert niva -> Q14r7 - Q14r8
  ## 1. Re-code into 1 if all prog vars 7-8 are present to get aggregate
  ##    variable for advanced skills
  data_out <- data_out %>% generate_segmentation_variable(name_recode1 = "prog",
                                                          from_vals = from_vals,
                                                          seq_recode = 7:8,
                                                          name_var_old = "Q14r",
                                                          name_var_sum = "avan_prog_sum",
                                                          name_var_seg = "avan_prog",
                                                          ref_val = sum_score_val_avan,
                                                          type = "equal")
  # Overall categorical variable for Q14 - kat_programmer1
  data_out <- data_out %>%
      dplyr::mutate(kat_programmer1 = dplyr::case_when(
        (grunn_prog1 == 0) ~ 0L,
        (grunn_prog1 == 1 & videre_prog == 0 & avan_prog == 0) ~ 1L,
        (grunn_prog1 == 1 & videre_prog == 1 & avan_prog == 0 |
           grunn_prog1 == 1 & videre_prog == 0 & avan_prog == 1) ~ 2L,
        (grunn_prog1 == 1 & videre_prog == 1 & avan_prog == 1) ~ 3L,
        TRUE ~ NA_integer_))
  # Transform to factor
  data_out$kat_programmer1 <- factor(data_out$kat_programmer1,
                                      levels = c(0, 1, 2, 3),
                                      labels = c("Uerfaren",
                                                 "Grunnleggende",
                                                 "Videregående",
                                                 "Avansert"))
  data_out <- data_out %>% dplyr::select(tidyselect::all_of(testvec),
                                         prog1, prog2, prog3, prog4, prog5,
                                         prog6, prog7, prog8,
                                         grunnleg_prog_sum, grunn_prog1,
                                         videre_prog_sum, videre_prog,
                                         avan_prog, kat_programmer1)
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
                       sum_score_val_avan = 3) {
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
  data_out <- data_out %>% generate_segmentation_variable(name_recode1 = "utstyr",
                                                          from_vals = from_vals,
                                                          seq_recode = 1:3,
                                                          name_var_old = "Q19r",
                                                          name_var_sum = "grunnleg_utstyr_sum",
                                                          name_var_seg = "grunn_utstyr1",
                                                          ref_val = sum_score_val_grun,
                                                          type = "larger-equal")

  #### Videregaende niva -> Q19r4 - Q19r8
  ## 1. Create an aggregate variable for secondary education where you have to
  ##    have answered 3/4 on 2 out of 3 to be in the category (same as in
  ##    kat_kommunikasjon)
  ## 2. Re-code into 0 if summation score for videre equals reference value
  ##    (e.g. 5), zero else
  data_out <- data_out %>% generate_segmentation_variable(name_recode1 = "utstyr",
                                                          from_vals = from_vals,
                                                          seq_recode = 4:8,
                                                          name_var_old = "Q19r",
                                                          name_var_sum = "videre_utstyr_sum",
                                                          name_var_seg = "videre_utstyr",
                                                          ref_val = sum_score_val_vide,
                                                          type = "equal")
  #### Avansert niva -> Q19r9 - Q19r11
  ## 1. Re-code into 1 if all utstyr vars 9-11 are present to get aggregate
  ##    variable for advanced skills
  data_out <- data_out %>% generate_segmentation_variable(name_recode1 = "utstyr",
                                                          from_vals = from_vals,
                                                          seq_recode = 9:11,
                                                          name_var_old = "Q19r",
                                                          name_var_sum = "avan_utstyr_sum",
                                                          name_var_seg = "avan_utstyr",
                                                          ref_val = sum_score_val_avan,
                                                          type = "equal")
  # Overall categorical variable for Q19 - kat_utstyrr1
  data_out <- data_out %>%
    dplyr::mutate(kat_utstyr1 = dplyr::case_when(
      (grunn_utstyr1 == 0) ~ 0L,
      (grunn_utstyr1 == 1 & videre_utstyr == 0 & avan_utstyr == 0) ~ 1L,
      (grunn_utstyr1 == 1 & videre_utstyr == 1 & avan_utstyr == 0 |
         grunn_utstyr1 == 1 & videre_utstyr == 0 & avan_utstyr == 1) ~ 2L,
      (grunn_utstyr1 == 1 & videre_utstyr == 1 & avan_utstyr == 1) ~ 3L,
      TRUE ~ NA_integer_))
  # Transform to factor
  data_out$kat_utstyr1 <- factor(data_out$kat_utstyr1,
                                     levels = c(0, 1, 2, 3),
                                     labels = c("Uerfaren",
                                                "Grunnleggende",
                                                "Videregående",
                                                "Avansert"))
  data_out <- data_out %>% dplyr::select(tidyselect::all_of(testvec),
                                         utstyr1, utstyr2, utstyr3, utstyr4,
                                         utstyr5, utstyr6, utstyr7, utstyr8,
                                         utstyr9, utstyr10, utstyr11,
                                         grunnleg_utstyr_sum, grunn_utstyr1,
                                         videre_utstyr_sum, videre_utstyr,
                                         avan_utstyr, kat_utstyr1)
  return(data_out)
}
recode_qXX_rVals <- function(data_set, q_names, from = 5, to = 1) {
  data_out <- data_set
  for (i in q_names) {
    data_set[[i]][data_set[[i]] == from] <- to
  }
  return(data_set)
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
