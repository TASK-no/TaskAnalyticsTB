#' Perform a segmentation analysis on the raw data
#'
#' This function applies a comprehensive segmentation analysis across multiple
#' questionnaire items (Q16, Q17, Q14, Q19), utilizing specific settings for
#' each to categorize responses into defined skill levels: "Uerfaren",
#' "Grunnleggende", "Videregående", and "Avansert". The analysis involves
#' recoding responses, aggregating scores, and classifying these scores into the
#' aforementioned categories based on predefined criteria in the settings.
#'
#' @param data_SVV raw data (as a \code{data.frame})
#' @param ind1_vals numeric vector giving which values to recode from
#' @param settings_q16 Settings for Q16 - Communication and interaction,
#'   encapsulated in a list with keys: `sum_score_val_grun`,
#'   `sum_score_val_vide`, `sum_score_val_avan` (numeric values setting
#'   thresholds for each skill level), and `type_val_grun`, `type_val_vide`,
#'   `type_val_avan` (strings specifying the comparison type, either
#'   "larger-equal" or "equal").
#' @param settings_q17 Similar to `settings_q16` but for Q17 - Information
#'   security and privacy.
#' @param settings_q14 Similar to `settings_q16` but for Q14 - Use of software.
#' @param settings_q19 Similar to `settings_q16` but for Q19 - Use of
#'   technology.
#' @return A \code{data.frame} enriched with segmentation variables that
#'   indicate the skill level classifications for each of the questionnaire
#'   items analyzed.
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
