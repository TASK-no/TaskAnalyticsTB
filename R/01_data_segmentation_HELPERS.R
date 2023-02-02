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
#' @param verbose used to print additional information, now deprecated
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
                       from_vals =c(3, 4),
                       sum_score_val_grun = 4,
                       sum_score_val_vide = 2,
                       sum_score_val_avan = 2,
                       verbose = FALSE) {
  data_tmp <- data_set
  #### Coding of 'kat_kommunikasjon' from Q16
  #### Sets the value 5 to 1 on ALL indicator variables
  data_tmp$Q16r1[data_tmp$Q16r1 == 5] <- 1
  data_tmp$Q16r2[data_tmp$Q16r2 == 5] <- 1
  data_tmp$Q16r3[data_tmp$Q16r3 == 5] <- 1
  data_tmp$Q16r4[data_tmp$Q16r4 == 5] <- 1
  data_tmp$Q16r5[data_tmp$Q16r5 == 5] <- 1
  data_tmp$Q16r6[data_tmp$Q16r6 == 5] <- 1
  data_tmp$Q16r7[data_tmp$Q16r7 == 5] <- 1
  data_tmp$Q16r8[data_tmp$Q16r8 == 5] <- 1
  data_tmp$Q16r9[data_tmp$Q16r9 == 5] <- 1
  data_tmp$Q16r10[data_tmp$Q16r10 == 5] <- 1
  data_tmp$Q16r11[data_tmp$Q16r11 == 5] <- 1

  data_out <- data_tmp
  # Create new dichotomous variables where values 3 and 4 of the indicator are
  # given a value of 1, and 0 otherwise (see argument 'from_vals')
  #### Start with grunnleggende niva
  #### Q16r1 - Q16r6
  data_out <- data_out %>%
    dplyr::mutate(kom1 = dplyr::if_else(.data$Q16r1 %in% from_vals, 1, 0)) %>%
    dplyr::mutate(kom2 = dplyr::if_else(.data$Q16r2 %in% from_vals, 1, 0)) %>%
    dplyr::mutate(kom3 = dplyr::if_else(.data$Q16r3 %in% from_vals, 1, 0)) %>%
    dplyr::mutate(kom4 = dplyr::if_else(.data$Q16r4 %in% from_vals, 1, 0)) %>%
    dplyr::mutate(kom5 = dplyr::if_else(.data$Q16r5 %in% from_vals, 1, 0)) %>%
    dplyr::mutate(kom6 = dplyr::if_else(.data$Q16r6 %in% from_vals, 1, 0))
  #### Videregaende niva
  data_out <- data_out %>%
    dplyr::mutate(kom7 = dplyr::if_else(.data$Q16r7 %in% from_vals, 1, 0)) %>%
    dplyr::mutate(kom8 = dplyr::if_else(.data$Q16r8 %in% from_vals, 1, 0)) %>%
    dplyr::mutate(kom9 = dplyr::if_else(.data$Q16r9 %in% from_vals, 1, 0))
  #### Avansert niva
  data_out <- data_out %>%
    dplyr::mutate(kom10 = dplyr::if_else(.data$Q16r10 %in% from_vals, 1, 0)) %>%
    dplyr::mutate(kom11 = dplyr::if_else(.data$Q16r11 %in% from_vals, 1, 0))

  if (verbose) {
    # NOT NECESSARY ANYMORE
    # Check that it looks right: compare original var with dichotomous var
    # as.factor(data_out$kom1)
    # as.factor(data_out$kom2)
    # as.factor(data_out$kom3)
    # as.factor(data_out$kom4)
    # as.factor(data_out$kom5)
    # as.factor(data_out$kom6)
    # table(data_tmp$Q16r1, useNA = "always")
    # table(mydata$kom1, useNA = "always") # will be correct
    # table(mydata$kom10, useNA = "always")
    # # #    0    1 <NA>
    # # # 1334  591    0
  }
  # Generate new variable taking the sum of the 6 variables in grunnleggende
  data_out <- data_out %>%
    dplyr::mutate(grunnleg_kom_sum = rowSums(dplyr::pick(.data$kom1,
                                                         .data$kom2,
                                                         .data$kom3,
                                                         .data$kom4,
                                                         .data$kom5,
                                                         .data$kom6)))
  # Generate new variable that says that if score on basic_com is 4 or higher,
  # then value = 1, 0 if not
  data_out <- data_out %>%
    dplyr::mutate(grunn_kom = dplyr::if_else(.data$grunnleg_kom_sum >= sum_score_val_grun,
                                             1, 0))
  # Create an aggregate variable for high school
  # Sample one for high school with less strict criteria:
  # Create one where you must have answered 3/4 on 2 out of 3 to be in the category
  data_out <- data_out %>%
    dplyr::mutate(videre_kom_sum = rowSums(dplyr::pick(.data$kom7,
                                                       .data$kom8,
                                                       .data$kom9)))
  # Generate a new variable that says that if the score on videre_kom is 2 or
  # higher, then value = 1, 0 if n
  data_out <- data_out %>%
    dplyr::mutate(videre_kom = dplyr::if_else(.data$videre_kom_sum >= sum_score_val_vide,
                                              1, 0))
  # Create an overall variable for advanced/avansert
  data_out <- data_out %>%
    dplyr::mutate(avan_kom = dplyr::if_else(.data$kom10 == 1 & .data$kom11 == 1,
                                            1, 0))
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
                       from_vals =c(3, 4),
                       sum_score_val_grun = 3,
                       sum_score_val_vide = 3,
                       sum_score_val_avan = 2,
                       verbose = FALSE) {
  data_tmp <- data_set
  # Coding of 'kat_informasjon1' from Q17
  # Sets the value 5 to 1 on ALL indicator variables
  data_tmp$Q17r1[data_tmp$Q17r1 == 5] <- 1
  data_tmp$Q17r2[data_tmp$Q17r2 == 5] <- 1
  data_tmp$Q17r3[data_tmp$Q17r3 == 5] <- 1
  data_tmp$Q17r4[data_tmp$Q17r4 == 5] <- 1
  data_tmp$Q17r5[data_tmp$Q17r5 == 5] <- 1
  data_tmp$Q17r6[data_tmp$Q17r6 == 5] <- 1
  data_tmp$Q17r7[data_tmp$Q17r7 == 5] <- 1
  data_tmp$Q17r8[data_tmp$Q17r8 == 5] <- 1
  data_tmp$Q17r9[data_tmp$Q17r9 == 5] <- 1
  data_tmp$Q17r10[data_tmp$Q17r10 == 5] <- 1

  data_out <- data_tmp
  # Create new dichotomous variables where values 3 and 4 of the indicator are
  # given a value of 1, and 0 otherwise (see argument 'from_vals')
  #### Start with grunnleggende niva
  #### Q17r1 - Q17r4
  data_out <- data_out %>%
    dplyr::mutate(info1 = dplyr::if_else(.data$Q17r1 %in% from_vals, 1, 0)) %>%
    dplyr::mutate(info2 = dplyr::if_else(.data$Q17r2 %in% from_vals, 1, 0)) %>%
    dplyr::mutate(info3 = dplyr::if_else(.data$Q17r3 %in% from_vals, 1, 0)) %>%
    dplyr::mutate(info4 = dplyr::if_else(.data$Q17r4 %in% from_vals, 1, 0))
  #### Videregaende niva
  data_out <- data_out %>%
    dplyr::mutate(info5 = dplyr::if_else(.data$Q17r5 %in% from_vals, 1, 0)) %>%
    dplyr::mutate(info6 = dplyr::if_else(.data$Q17r6 %in% from_vals, 1, 0)) %>%
    dplyr::mutate(info7 = dplyr::if_else(.data$Q17r7 %in% from_vals, 1, 0))
  #### Avansert niva
  data_out <- data_out %>%
    dplyr::mutate(info8 = dplyr::if_else(.data$Q17r8 %in% from_vals, 1, 0)) %>%
    dplyr::mutate(info9 = dplyr::if_else(.data$Q17r9 %in% from_vals, 1, 0)) %>%
    dplyr::mutate(info10 = dplyr::if_else(.data$Q17r10 %in% from_vals, 1, 0))

  # Generate summation score variable for gunnleggende info taking the sum of
  # the first four info vars
  data_out <- data_out %>%
    dplyr::mutate(grunnleg_info_sum = rowSums(dplyr::pick(.data$info1,
                                                          .data$info2,
                                                          .data$info3,
                                                          .data$info4)))
  # Re-code summation score into 1 if larger than reference value (e.g. = 3)
  # zero else
  data_out <- data_out %>%
    dplyr::mutate(grunn_info1 = dplyr::if_else(.data$grunnleg_info_sum >= sum_score_val_grun,
                                               1, 0))
  # Create an aggregate variable for secondary education where you have to have
  # answered 3/4 on 2 out of 3 to be in the category (same as in
  # kat_kommunikasjon)
  data_out <- data_out %>%
    dplyr::mutate(videre_info_sum = rowSums(dplyr::pick(.data$info5,
                                                        .data$info6,
                                                        .data$info7)))
  # Re-code into 1 if summation score for videre equals reference value(e.g. 3),
  # zero else
  data_out <- data_out %>%
    dplyr::mutate(videre_info = dplyr::if_else(.data$videre_info_sum == sum_score_val_vide, 1, 0))
  # Re-code into 1 if all info vars 8-10 are present to get aggregate variable
  # for advanced skills
  data_out <- data_out %>%
    dplyr::mutate(avan_info = dplyr::if_else(.data$info8 == 1 & .data$info9 == 1 & .data$info10 == 1, 1, 0))
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
  return(data_out)
}
