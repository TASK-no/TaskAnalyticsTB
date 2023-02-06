#' Computes various data summaries from raw data
#'
#' Data summaries include
#'   \itemize{
#'     \item \code{df_final_data} the final data set
#'     \item \code{df_final_report}
#'     \item \code{df_final_divisions}
#'   }
#'
#' @param data_segm a \code{data.frame} or \code{tibble} of segmented data (this
#'   is typically the raw data set being run through [segmentation_analysis()]
#' @param year the year as a numeric variable
#'
#' @return a list of three elements: \itemize{
#'     \item \code{df_final_data} the final data set
#'     \item \code{df_final_report}
#'     \item \code{df_final_divisions}
#'   }
#'   see the details section, for more information on each data set.
#' @export
get_data_summary <- function(data_segm, year) {
  # num_div <- as.factor(unique(data_segm$SamDivision))
  # nam_div <- names(attr(tests_data2$SamDivision, "labels"))

  tests_data  <- data_segm %>% dplyr::select(dplyr::starts_with("kat"))
  tests_data2 <- data_segm %>% dplyr::select(.data$SamDivision,
                                             dplyr::starts_with("kat"))

  check_rowsums <- lapply((tests_data %>% lapply(table)), sum)

  # df_final_divisions <- vector("list", length(num_div))
  # df_final_divisions <- list()

  # tests_data2 <- tests_data2[order(tests_data2$SamDivision), ]
  df_final_data      <- generate_data_final(tests_data)
  df_final_divisions <- generate_data_division(tests_data2)
  df_final_report    <- generate_data_report(df_final_data, year)

  return(list(df_final_data = df_final_data,
              df_final_report = df_final_report,
              df_final_divisions = df_final_divisions))
}
generate_data_division <- function(df) {
  nam_div <- names(attr(df$SamDivision, "labels"))

  df_div_out <- df %>% dplyr::group_by(.data$SamDivision,
                                       .data$kat_kommunikasjon) %>%
    dplyr::summarise(kommunikasjon_perc = dplyr::n())
  df_div_out <- dplyr::full_join(df_div_out,
                                 df %>%
                                   dplyr::group_by(.data$SamDivision,
                                                   .data$kat_informasjon1) %>%
                                   dplyr::summarise(informasjon_perc = dplyr::n()),
                                 by = c("SamDivision", "kat_kommunikasjon" = "kat_informasjon1"))
  df_div_out <- dplyr::full_join(df_div_out,
                                 df %>% dplyr::group_by(.data$SamDivision,
                                                        .data$kat_programmer1) %>%
                                   dplyr::summarise(programmer_perc = dplyr::n()),
                                 by = c("SamDivision", "kat_kommunikasjon" = "kat_programmer1"))
  df_div_out <- dplyr::full_join(df_div_out,
                                 df %>% dplyr::group_by(.data$SamDivision,
                                                        .data$kat_utstyr1) %>%
                                   dplyr::summarise(utstyr_perc = dplyr::n()),
                                 by = c("SamDivision", "kat_kommunikasjon" = "kat_utstyr1"))
  df_div_out <- df_div_out %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total_perc = sum(.data$kommunikasjon_perc,
                                   .data$informasjon_perc,
                                   .data$programmer_perc,
                                   .data$utstyr_perc, na.rm = TRUE))

  df_div_out <- df_div_out[order(df_div_out$SamDivision,
                                 df_div_out$kat_kommunikasjon),]

  change_vec <- which(unname(sapply(table(df_div_out$SamDivision),
                                    function(x) {x<4})))
  if (length(change_vec) > 0) {
    for (i in 1:length(change_vec)) {
      missing_kat <- df_div_out[df_div_out$SamDivision == change_vec[i], ][["kat_kommunikasjon"]]
      missing_kat <- setdiff(c("Uerfaren", "Grunnleggende", paste0("Videreg",
                                                                   "\u00e5",
                                                                   "ende"),
                               "Avansert"),
                             missing_kat)

      impute_vec <- list(SamDivision = unique(df_div_out$SamDivision)[change_vec[i]],
                         kat_kommunikasjon = missing_kat,
                         kommunikasjon_perc = NA_integer_,
                         informasjon_perc = NA_integer_,
                         programmer_perc = NA_integer_,
                         utstyr_perc = NA_integer_,
                         total_perc = 0L)
      df_div_out <- rbind(df_div_out, impute_vec)
    }
    df_div_out <- df_div_out[order(df_div_out$SamDivision,
                                   df_div_out$kat_kommunikasjon),]
  }
  use_sum <- df_div_out %>% dplyr::group_by(.data$SamDivision) %>% dplyr::summarise(use_sum = sum(.data$kommunikasjon_perc, na.rm = TRUE)) %>% dplyr::pull(use_sum)
  df_div_out$kommunikasjon_perc <- round(df_div_out$kommunikasjon_perc/rep(use_sum, each = 4) * 100, digits = 2)
  use_sum <- df_div_out %>% dplyr::group_by(.data$SamDivision) %>% dplyr::summarise(use_sum = sum(.data$informasjon_perc, na.rm = TRUE)) %>% dplyr::pull(use_sum)
  df_div_out$informasjon_perc <- round(df_div_out$informasjon_perc/rep(use_sum, each = 4) * 100, digits = 2)
  use_sum <- df_div_out %>% dplyr::group_by(.data$SamDivision) %>% dplyr::summarise(use_sum = sum(.data$programmer_perc, na.rm = TRUE)) %>% dplyr::pull(use_sum)
  df_div_out$programmer_perc <- round(df_div_out$programmer_perc/rep(use_sum, each = 4) * 100, digits = 2)
  use_sum <- df_div_out %>% dplyr::group_by(.data$SamDivision) %>% dplyr::summarise(use_sum = sum(.data$utstyr_perc, na.rm = TRUE)) %>% dplyr::pull(use_sum)
  df_div_out$utstyr_perc <- round(df_div_out$utstyr_perc/rep(use_sum, each = 4) * 100, digits = 2)
  use_sum <- df_div_out %>% dplyr::group_by(.data$SamDivision) %>% dplyr::summarise(use_sum = sum(.data$total_perc, na.rm = TRUE)) %>% dplyr::pull(use_sum)
  df_div_out$total_perc <- round(df_div_out$total_perc/rep(use_sum, each = 4) * 100, digits = 2)

  df_div_out$SamDivision <- factor(df_div_out$SamDivision, labels = nam_div)

  return(df_div_out)
}
generate_data_final <- function(df) {
  num_obs <- nrow(df)
  num_obs <- num_obs/100

  df_final_out <- df %>% lapply(table)
  df_final_out <- as.data.frame(df_final_out)
  names(df_final_out) <- c("kommunikasjon", "kommunikasjon_freq",
                           "informasjon", "informasjon_freq",
                           "programmer", "programmer_freq",
                           "utstyr", "utstyr_freq")
  df_final_out$kommunikasjon_freq_perc <-round(df_final_out$kommunikasjon_freq/num_obs, digits = 2)
  df_final_out$informasjon_freq_perc <- round(df_final_out$informasjon_freq/num_obs, digits = 2)
  df_final_out$programmer_freq_perc <- round(df_final_out$programmer_freq/num_obs, digits = 2)
  df_final_out$utstyr_freq_perc <- round(df_final_out$utstyr_freq/num_obs, digits = 2)


  df_final_out <- tibble::as_tibble(df_final_out)
  df_final_out <- df_final_out[, -c(3,5,7)]
  names(df_final_out)[1] <- "kategorier"
  df_final_out$kategorier <- as.character(df_final_out$kategorier)

  df_final_out <- df_final_out %>%
    dplyr::arrange(dplyr::desc(.data$kategorier)) %>%
    dplyr::mutate(prop_k = .data$kommunikasjon_freq / sum(df_final_out$kommunikasjon_freq) *100) %>%
    dplyr::mutate(ypos_k = cumsum(.data$prop_k)- 0.5*.data$prop_k) %>%
    dplyr::mutate(prop_i = .data$informasjon_freq / sum(df_final_out$informasjon_freq) *100) %>%
    dplyr::mutate(ypos_i = cumsum(.data$prop_i)- 0.5*.data$prop_i) %>%
    dplyr::mutate(prop_p = .data$programmer_freq / sum(df_final_out$programmer_freq) *100) %>%
    dplyr::mutate(ypos_p = cumsum(.data$prop_p)- 0.5*.data$prop_p) %>%
    dplyr::mutate(prop_u = .data$utstyr_freq / sum(df_final_out$utstyr_freq) *100) %>%
    dplyr::mutate(ypos_u = cumsum(.data$prop_u)- 0.5*.data$prop_u) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total_freq = sum(.data$kommunikasjon_freq,
                                   .data$informasjon_freq,
                                   .data$programmer_freq,
                                   .data$utstyr_freq)) %>%
    dplyr::mutate(total_freq_perc =round(.data$total_freq/(4*num_obs), digits = 2))
  return(df_final_out)
}
generate_data_report <- function(df, year) {
  df_report_out <- df %>%
    dplyr::select(.data$kategorier, .data$kommunikasjon_freq,
                  .data$informasjon_freq, .data$programmer_freq,
                  .data$utstyr_freq, .data$total_freq,
                  .data$kommunikasjon_freq_perc, .data$informasjon_freq_perc,
                  .data$programmer_freq_perc, .data$utstyr_freq_perc,
                  .data$total_freq_perc)

  df_report_out <- df_report_out %>% dplyr::mutate_at(.vars = dplyr::vars(dplyr::contains("perc")),
                                                      .funs = function(x) {x/100})
  df_report_out$year <- year
  df_report_out <- df_report_out %>%
    dplyr::select("year", dplyr::everything())
}
