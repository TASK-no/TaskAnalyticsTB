#' Perform a segmentation analysis on the raw data
#'
#' @param data_SVV raw data (as a \code{data.frame})
#'
#' @return data set after segmentation analysis
#' @export
segmentation_analysis <- function(data_SVV) {
  data_out <- data_SVV
  # Segmentation variables for Q16 - Communication and interaction
  data_out <- recode_q16(data_out)
  # Segmentation variables for Q17 - Information security and privacy
  data_out <- recode_q17(data_out)
  # Segmentation variables for Q14 - Use of software
  data_out <- recode_q14(data_out)
  # SSegmentation variables for Q19 - Use of technology
  data_out <- recode_q19(data_out)
  return(data_segmentation = data_out)
}
