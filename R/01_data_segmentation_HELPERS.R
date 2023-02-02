#' Data segmentation based on Q16
#'
#' Generate data set with segmentation variables for "Communication and
#' Interaction".
#'
#' @param data_set data set (either raw or already added some segmentatin vars)
#' @param from_vals numeric vector giving which values to recode from
#' @param cut_grun numeric cutoff value for summation score of "grunlegende"; if
#'   summation score larger than `cut_grun`, then employee has grunlegende
#'   skills
#' @param cut_vide numeric cutoff value for summation score of "videregaende";
#'   if summation score larger than `cut_vide`, then employee has videregaende
#'   skills
#' @param cut_avan numeric cutoff value for summation score of "avansert"; if
#'   summation score larger than `cut_avan`, then employee has avansert skills
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
                       cut_grun = 4,
                       cut_vide = 2,
                       cut_avan = 2,
                       verbose = FALSE) {
  data_tmp <- data_set
  #### Koding av kat_kommunikasjon
  #### Setter verdien 5 til 1 på ALLE indikatorvariabler
  # Q16
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
  # given a value of 1, and 0 otherwise


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
    dplyr::mutate(grunn_kom = dplyr::if_else(.data$grunnleg_kom_sum >= cut_grun,
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
    dplyr::mutate(videre_kom = dplyr::if_else(.data$videre_kom_sum >= cut_vide,
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
                                       levels = c(0,1,2,3),
                                       labels = c("Uerfaren",
                                                  "Grunnleggende",
                                                  "Videregående",
                                                  "Avansert"))
  return(data_out)
}
recode_q16 <- function(data_set,
                       from_vals =c(3, 4),
                       cut_grun = 4,
                       cut_vide = 2,
                       cut_avan = 2,
                       verbose = FALSE) {

}
