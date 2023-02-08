#' Perform a segmentation analysis on the raw data
#'
#' @param data_SVV raw data (as a \code{data.frame})
#'
#' @return data set after segmentation analysis
#' @export
segmentation_analysis <- function(data_SVV,
                                  ind1_vals = c(3, 4),
                                  settings_q16 = list(sum_score_val_grun = 4,
                                                      sum_score_val_vide = 2,
                                                      sum_score_val_avan = 2,
                                                      type_val_grun = "larger-equal",
                                                      type_val_vide = "larger-equal",
                                                      type_val_avan = "equal"),
                                  settings_q17 = list(sum_score_val_grun = 3,
                                                      sum_score_val_vide = 3,
                                                      sum_score_val_avan = 3,
                                                      type_val_grun = "larger-equal",
                                                      type_val_vide = "equal",
                                                      type_val_avan = "equal"),
                                  settings_q14 = list(sum_score_val_grun = 2,
                                                      sum_score_val_vide = 3,
                                                      sum_score_val_avan = 2,
                                                      type_val_grun = "larger-equal",
                                                      type_val_vide = "equal",
                                                      type_val_avan = "equal"),
                                  settings_q19 = list(sum_score_val_grun = 2,
                                                      sum_score_val_vide = 5,
                                                      sum_score_val_avan = 3,
                                                      type_val_grun = "larger-equal",
                                                      type_val_vide = "equal",
                                                      type_val_avan = "equal")) {
  data_out <- data_SVV
  # Segmentation variables for Q16 - Communication and interaction
  data_out <- recode_q16(data_out, ind1_vals,
                         settings_q16$sum_score_val_grun,
                         settings_q16$sum_score_val_vide,
                         settings_q16$sum_score_val_avan,
                         settings_q16$type_val_grun,
                         settings_q16$type_val_vide,
                         settings_q16$type_val_avan)
  # Segmentation variables for Q17 - Information security and privacy
  data_out <- recode_q17(data_out, ind1_vals,
                         settings_q17$sum_score_val_grun,
                         settings_q17$sum_score_val_vide,
                         settings_q17$sum_score_val_avan,
                         settings_q17$type_val_grun,
                         settings_q17$type_val_vide,
                         settings_q17$type_val_avan)
  # Segmentation variables for Q14 - Use of software
  data_out <- recode_q14(data_out, ind1_vals,
                         settings_q14$sum_score_val_grun,
                         settings_q14$sum_score_val_vide,
                         settings_q14$sum_score_val_avan,
                         settings_q14$type_val_grun,
                         settings_q14$type_val_vide,
                         settings_q14$type_val_avan)
  # SSegmentation variables for Q19 - Use of technology
  data_out <- recode_q19(data_out, ind1_vals,
                         settings_q19$sum_score_val_grun,
                         settings_q19$sum_score_val_vide,
                         settings_q19$sum_score_val_avan,
                         settings_q19$type_val_grun,
                         settings_q19$type_val_vide,
                         settings_q19$type_val_avan)
  return(data_segmentation = data_out)
}
