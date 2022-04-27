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
#'   is typically the raw data set being run through the function ....)
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
  num_obs <- nrow(data_segm)
  num_obs <- num_obs/100
  # num_div <- as.factor(unique(data_segm$SamDivision))
  # nam_div <- names(attr(tests_data2$SamDivision, "labels"))

  tests_data  <- data_segm %>% dplyr::select(dplyr::starts_with("kat"))
  tests_data2 <- data_segm %>% dplyr::select(.data$SamDivision,
                                             dplyr::starts_with("kat"))
  nam_div <- names(attr(tests_data2$SamDivision, "labels"))
  check_rowsums <- lapply((tests_data %>% lapply(table)), sum)

  # df_final_divisions <- vector("list", length(num_div))
  # df_final_divisions <- list()

  # tests_data2 <- tests_data2[order(tests_data2$SamDivision), ]
  df_final_divisions <- tests_data2 %>% dplyr::group_by(.data$SamDivision,
                                                        .data$kat_kommunikasjon) %>%
    dplyr::summarise(kommunikasjon_perc = dplyr::n())
  df_final_divisions <- dplyr::full_join(df_final_divisions,
                                         tests_data2 %>%
                                           dplyr::group_by(.data$SamDivision,
                                                           .data$kat_informasjon1) %>%
                                           dplyr::summarise(informasjon_perc = dplyr::n()),
                                         by = c("SamDivision", "kat_kommunikasjon" = "kat_informasjon1"))
  df_final_divisions <- dplyr::full_join(df_final_divisions,
                                         tests_data2 %>% dplyr::group_by(.data$SamDivision,
                                                                         .data$kat_programmer1) %>%
                                           dplyr::summarise(programmer_perc = dplyr::n()),
                                         by = c("SamDivision", "kat_kommunikasjon" = "kat_programmer1"))
  df_final_divisions <- dplyr::full_join(df_final_divisions,
                                         tests_data2 %>% dplyr::group_by(.data$SamDivision,
                                                                         .data$kat_utstyr1) %>%
                                           dplyr::summarise(utstyr_perc = dplyr::n()),
                                         by = c("SamDivision", "kat_kommunikasjon" = "kat_utstyr1"))
  df_final_divisions <- df_final_divisions %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total_perc = sum(.data$kommunikasjon_perc,
                                   .data$informasjon_perc,
                                   .data$programmer_perc,
                                   .data$utstyr_perc, na.rm = TRUE))

  df_final_divisions <- df_final_divisions[order(df_final_divisions$SamDivision,
                                                 df_final_divisions$kat_kommunikasjon),]

  change_vec <- which(unname(sapply(table(df_final_divisions$SamDivision),
                                    function(x) {x<4})))
  if (length(change_vec) > 0) {
    for (i in 1:length(change_vec)) {
      missing_kat <- df_final_divisions[df_final_divisions$SamDivision == change_vec[i], ][["kat_kommunikasjon"]]
      missing_kat <- setdiff(c("Uerfaren", "Grunnleggende", paste0("Videreg",
                                                                   "\u00e5",
                                                                   "ende"),
                               "Avansert"),
                             missing_kat)

      impute_vec <- list(SamDivision = unique(df_final_divisions$SamDivision)[change_vec[i]],
                         kat_kommunikasjon = missing_kat,
                         kommunikasjon_perc = NA_integer_,
                         informasjon_perc = NA_integer_,
                         programmer_perc = NA_integer_,
                         utstyr_perc = NA_integer_,
                         total_perc = 0L)
      df_final_divisions <- rbind(df_final_divisions, impute_vec)
    }
    df_final_divisions <- df_final_divisions[order(df_final_divisions$SamDivision,
                                                   df_final_divisions$kat_kommunikasjon),]
  }

  use_sum <- df_final_divisions %>% dplyr::group_by(.data$SamDivision) %>% dplyr::summarise(use_sum = sum(.data$kommunikasjon_perc, na.rm = TRUE)) %>% dplyr::pull(use_sum)
  df_final_divisions$kommunikasjon_perc <- round(df_final_divisions$kommunikasjon_perc/rep(use_sum, each = 4) * 100, digits = 2)
  use_sum <- df_final_divisions %>% dplyr::group_by(.data$SamDivision) %>% dplyr::summarise(use_sum = sum(.data$informasjon_perc, na.rm = TRUE)) %>% dplyr::pull(use_sum)
  df_final_divisions$informasjon_perc <- round(df_final_divisions$informasjon_perc/rep(use_sum, each = 4) * 100, digits = 2)
  use_sum <- df_final_divisions %>% dplyr::group_by(.data$SamDivision) %>% dplyr::summarise(use_sum = sum(.data$programmer_perc, na.rm = TRUE)) %>% dplyr::pull(use_sum)
  df_final_divisions$programmer_perc <- round(df_final_divisions$programmer_perc/rep(use_sum, each = 4) * 100, digits = 2)
  use_sum <- df_final_divisions %>% dplyr::group_by(.data$SamDivision) %>% dplyr::summarise(use_sum = sum(.data$utstyr_perc, na.rm = TRUE)) %>% dplyr::pull(use_sum)
  df_final_divisions$utstyr_perc <- round(df_final_divisions$utstyr_perc/rep(use_sum, each = 4) * 100, digits = 2)
  use_sum <- df_final_divisions %>% dplyr::group_by(.data$SamDivision) %>% dplyr::summarise(use_sum = sum(.data$total_perc, na.rm = TRUE)) %>% dplyr::pull(use_sum)
  df_final_divisions$total_perc <- round(df_final_divisions$total_perc/rep(use_sum, each = 4) * 100, digits = 2)

  df_final_divisions$SamDivision <- factor(df_final_divisions$SamDivision, labels = nam_div)

  final_data <- tests_data %>% lapply(table)
  df_final_data <- as.data.frame(final_data)
  names(df_final_data) <- c("kommunikasjon", "kommunikasjon_freq",
                            "informasjon", "informasjon_freq",
                            "programmer", "programmer_freq",
                            "utstyr", "utstyr_freq")
  df_final_data$kommunikasjon_freq_perc <-round(df_final_data$kommunikasjon_freq/num_obs, digits = 2)
  df_final_data$informasjon_freq_perc <- round(df_final_data$informasjon_freq/num_obs, digits = 2)
  df_final_data$programmer_freq_perc <- round(df_final_data$programmer_freq/num_obs, digits = 2)
  df_final_data$utstyr_freq_perc <- round(df_final_data$utstyr_freq/num_obs, digits = 2)


  df_final_data <- tibble::as_tibble(df_final_data)
  df_final_data <- df_final_data[, -c(3,5,7)]
  names(df_final_data)[1] <- "kategorier"
  df_final_data$kategorier <- as.character(df_final_data$kategorier)

  df_final_data <- df_final_data %>%
    dplyr::arrange(dplyr::desc(.data$kategorier)) %>%
    dplyr::mutate(prop_k = .data$kommunikasjon_freq / sum(df_final_data$kommunikasjon_freq) *100) %>%
    dplyr::mutate(ypos_k = cumsum(.data$prop_k)- 0.5*.data$prop_k) %>%
    dplyr::mutate(prop_i = .data$informasjon_freq / sum(df_final_data$informasjon_freq) *100) %>%
    dplyr::mutate(ypos_i = cumsum(.data$prop_i)- 0.5*.data$prop_i) %>%
    dplyr::mutate(prop_p = .data$programmer_freq / sum(df_final_data$programmer_freq) *100) %>%
    dplyr::mutate(ypos_p = cumsum(.data$prop_p)- 0.5*.data$prop_p) %>%
    dplyr::mutate(prop_u = .data$utstyr_freq / sum(df_final_data$utstyr_freq) *100) %>%
    dplyr::mutate(ypos_u = cumsum(.data$prop_u)- 0.5*.data$prop_u) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total_freq = sum(.data$kommunikasjon_freq,
                                   .data$informasjon_freq,
                                   .data$programmer_freq,
                                   .data$utstyr_freq)) %>%
    dplyr::mutate(total_freq_perc =round(.data$total_freq/(4*num_obs), digits = 2))

  df_final_report <- df_final_data %>%
    dplyr::select(.data$kategorier, .data$kommunikasjon_freq,
                  .data$informasjon_freq, .data$programmer_freq,
                  .data$utstyr_freq, .data$total_freq,
                  .data$kommunikasjon_freq_perc, .data$informasjon_freq_perc,
                  .data$programmer_freq_perc, .data$utstyr_freq_perc,
                  .data$total_freq_perc)

  df_final_report <- df_final_report %>% dplyr::mutate_at(.vars = dplyr::vars(dplyr::contains("perc")),
                                                          .funs = function(x) {x/100})
  df_final_report$year <- year
  df_final_report <- df_final_report %>%
    dplyr::select("year", dplyr::everything())

  return(list(df_final_data = df_final_data,
              df_final_report = df_final_report,
              df_final_divisions = df_final_divisions))
}
