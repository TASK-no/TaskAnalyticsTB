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
#' @param type the type of data set to return; if 'all' then a list of all three
#'    as described in "Value" are returned, otherwise for "report", "divisions",
#'    or "final", only the corresponding subtype is returned
#'
#' @return a list of three elements: \itemize{
#'     \item \code{df_final_data} the final data set
#'     \item \code{df_final_report}
#'     \item \code{df_final_divisions}
#'   }
#'   see the details section, for more information on each data set.
#' @export
get_data_summary <- function(data_segm, year, type = "all") {
  data_sub <- data_segm %>% dplyr::select(.data$SamDivision,
                                          dplyr::starts_with("kat"))
  if (type == "all") {
    df_final_data      <- generate_data_final(data_sub[-1])
    df_final_report    <- generate_data_report(df_final_data, year)
    df_final_divisions <- generate_data_division(data_sub, "divisions_area")
    return(list(df_final_data = df_final_data,
                df_final_report = df_final_report,
                df_final_divisions = df_final_divisions))
  } else if (type == "final") {
    generate_data_final(data_sub[-1])
  } else if (type == "divisions_area") {
    generate_data_division(data_sub, "divisions_area")
  } else if (type == "divisions_competence") {
    generate_data_division(data_sub, "divisions_competence")
  } else if (type == "report") {
    df_final_data <- generate_data_final(data_sub[-1])
    generate_data_report(df_final_data, year)
  }
  # else if (type == "report-divisions") {
  #   df_final_data <- generate_data_final(data_sub)
  #   generate_data_report(df_final_data, year)
  # }
}
generate_data_division <- function(df, type) {
  if (type == "divisions_area") {
    df_div_out <- df %>%
      generate_data_division_prelim() %>%
      generate_data_division_area()
    col_select <-  tidyselect::all_of(c("kommunikasjon", "informasjon",
                                        "programmer", "utstyr"))
  } else if (type == "divisions_competence") {
    df_div_out <- df %>%
      generate_data_division_prelim() %>%
      generate_data_division_competence()
    col_select <- tidyselect::all_of(c("Uerfaren",
                                       "Grunnleggende",
                                       "Videreg\u00e5ende",
                                       "Avansert"))
  }
  df_div_out <- df_div_out %>%
    dplyr::group_by(.data$SamDivision, .data$category) %>%
    tidyr::pivot_longer(cols = col_select,
                        names_to = "sub_category",
                        values_to = "total_num") %>%
    dplyr::mutate(perc = round(.data$total_num / .data$all_total * 100,
                               digits = 2)) %>%
    dplyr::select(.data$sub_category, .data$total_num, .data$perc) %>%
    dplyr::ungroup()
  if (type == "divisions_area") {
    df_div_out$category <- match_list_category(df_div_out$category,
                                               "competence")
    df_div_out$sub_category <- match_list_category(df_div_out$sub_category,
                                                   "area")
  } else if (type == "divisions_competence") {
    df_div_out$category <- match_list_category(df_div_out$category,
                                               "area")
    df_div_out$sub_category <- match_list_category(df_div_out$sub_category,
                                                   "competence")
  }
  return(df_div_out)
}
generate_data_division_prelim <- function(df) {
  df_div_out <- df
  df_div_out <- df_div_out %>%
    dplyr::group_by(.data$SamDivision,
                    .data$kat_kommunikasjon) %>%
    dplyr::summarise(kommunikasjon = dplyr::n())
  names(df_div_out)[2] <- "category"

  df_div_out <- dplyr::full_join(df_div_out,
                                 df %>%
                                   dplyr::group_by(.data$SamDivision,
                                                   .data$kat_informasjon1) %>%
                                   dplyr::summarise(informasjon = dplyr::n()),
                                 by = c("SamDivision",
                                        "category" = "kat_informasjon1"))
  df_div_out <- dplyr::full_join(df_div_out,
                                 df %>%
                                   dplyr::group_by(.data$SamDivision,
                                                   .data$kat_programmer1) %>%
                                   dplyr::summarise(programmer = dplyr::n()),
                                 by = c("SamDivision",
                                        "category" = "kat_programmer1"))
  df_div_out <- dplyr::full_join(df_div_out,
                                 df %>%
                                   dplyr::group_by(.data$SamDivision,
                                                   .data$kat_utstyr1) %>%
                                   dplyr::summarise(utstyr = dplyr::n()),
                                 by = c("SamDivision",
                                        "category" = "kat_utstyr1"))

  df_div_out <- df_div_out %>%
    add_missing_kat()

  if (inherits(df$SamDivision, "haven_labelled")) {
    nam_div <- names(attr(df$SamDivision, "labels"))
    df_div_out$SamDivision <- factor(df_div_out$SamDivision, labels = nam_div)
  }
  df_div_out$SamDivision <- match_list_sam_division(df_div_out$SamDivision)
  return(df_div_out)
}
generate_data_division_competence <- function(df) {
  df_div_out <- df
  names(df_div_out)[2] <- "sub_category"
  df_div_out <- df_div_out %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      all_comp = sum(
        .data$kommunikasjon,
        .data$informasjon,
        .data$programmer,
        .data$utstyr,
        na.rm = TRUE)
      )
  df_div_out <- df_div_out %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(c("kommunikasjon",
                                                    "informasjon",
                                                    "programmer",
                                                    "utstyr",
                                                    "all_comp")),
                        names_to = "category",
                        values_to = "all_total")
  df_div_out <- df_div_out %>%
    tidyr::pivot_wider(names_from = "sub_category",
                       values_from = "all_total")

  df_div_out <- df_div_out %>%
    dplyr::rowwise() %>%
    dplyr::mutate(all_total = sum(.data$Uerfaren,
                                  .data$Grunnleggende,
                                  .data[["Videreg\u00e5ende"]],
                                  .data$Avansert,
                                  na.rm = TRUE))
  return(df_div_out)
}
generate_data_division_area <- function(df) {
  df_div_out <- df %>%
    dplyr::rowwise() %>% dplyr::mutate(all_total = sum(.data$kommunikasjon,
                                                       .data$informasjon,
                                                       .data$programmer,
                                                       .data$utstyr,
                                                       na.rm = TRUE))
  return(df_div_out)
}
match_list_sam_division <- function(division) {
  div_new <- c("A - Trafikant og kj\u00f8ret\u00f8y" = "A - Trafikant",
               "B - Utbygging" = "B - Utbygging",
               "C - Drift og vedlikehold" = "C - Vedlikehold",
               "D - Transport og samfunn" = "D - Transport",
               "E - IT" = "E - IT",
               "F - Fellesfunksjoner" = "F - Fellesfunksjoner",
               "K - HR og HMS/Internrevisjonen/Kommunikasjon/\u00d8konomi" =
                 "K - HR/\u00d8konomi",
               "L - Myndighet og regelverk" = "L - Myndighet/regelverk")
  unname(div_new[division])
}
match_list_category <- function(cat, cat_type = "area") {
  if (cat_type == "area") {
    cat_new <- c("kommunikasjon" = "Kommunikasjon og samhandling",
                 "informasjon" = "Informasjonssikkerhet og personvern",
                 "programmer" = "Bruk av programvare",
                 "utstyr" = "Bruk av teknologi",
                 "all_comp" = "Alle kompetanser")
  } else if (cat_type == "competence") {
    cat_new <- c("Uerfaren" = "Uerfaren",
                 "Grunnleggende" = "Grunnleggende",
                 "Videreg\u00e5ende" = "Videreg\u00e5ende",
                 "Avansert" = "Avansert")
  } else {
    stop("Unknown value for 'cat_type': set to either 'area' or 'comptenece'.")
  }
  unname(cat_new[cat])
}
generate_total_perc_var <- function(df) {
  df_div_out <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total_perc = sum(.data$kommunikasjon_perc,
                                   .data$informasjon_perc,
                                   .data$programmer_perc,
                                   .data$utstyr_perc, na.rm = TRUE))

  df_div_out <- df_div_out[order(df_div_out$SamDivision,
                                 df_div_out$digital_competence),]
  return(dplyr::ungroup(df_div_out))
}
calculate_per_vars <- function(df, name_var) {
  use_sum <- df %>%
    dplyr::group_by(.data$SamDivision) %>%
    dplyr::summarise(use_sum = sum(.data[[name_var]], na.rm = TRUE)) %>%
    dplyr::pull(use_sum)
  tmp_perc <- round_perc(df[[name_var]], rep(use_sum, each = 4) , digits = 2)
  df[[name_var]] <- tmp_perc
  return(df)
}
round_perc <- function(var_to_round, total_sum, digits, add_perc_sign = FALSE) {
  if (add_perc_sign) {
    # paste0(round(var_to_round / total_sum * 100, digits = digits), "%")
    round(var_to_round / total_sum * 100, digits = digits)
  } else {
    round(var_to_round / total_sum * 100, digits = digits)
  }
}
add_missing_kat <- function(df) {
  change_vec <- get_vec_of_changes(df$SamDivision)
  sam_unique <- get_sam_div_unique(df$SamDivision)
  if (length(change_vec) > 0) {
    for (i in 1:length(change_vec)) {
      tmp_plh <- "kat_kommunikasjon"
      missing_kat <- df[df$SamDivision == change_vec[i], ][[tmp_plh]]
      missing_kat <- setdiff(c("Uerfaren",
                               "Grunnleggende",
                               "Videreg\u00e5ende",
                               "Avansert"),
                             missing_kat)

      for (kat in missing_kat) {
        df <- rbind(df, list(SamDivision = sam_unique[change_vec[i]],
                             kat_kommunikasjon = kat,
                             kommunikasjon_perc = NA_integer_,
                             informasjon_perc = NA_integer_,
                             programmer_perc = NA_integer_,
                             utstyr_perc = NA_integer_,
                             total_perc = 0L))
      }
    }
    df_out <- df[order(df$SamDivision, df$kat_kommunikasjon),]
  } else {
    df_out <- df
  }
  return(df_out)
}
get_vec_of_changes <- function(var) {
  if (inherits(var, "haven_labelled")) {
    out <- table(factor(as.character(var),
                        levels = unname(attr(var, which = "labels"))))
  } else {
    out <- table(var)
  }
  out <- which(unname(sapply(out, function(x) {x < 4})))
  return(out)
}
get_sam_div_unique <- function(sam_div) {
  if (inherits(sam_div, "haven_labelled")) {
    num_div <- length(attr(sam_div, which = "labels"))
    return(sort(c(unique(sam_div), setdiff(seq_len(num_div), unique(sam_div)))))
  } else {
    lvl_all <- levels(sam_div)
    return(factor(lvl_all, levels = lvl_all))
  }
}
generate_data_final <- function(df) {
  num_obs <- nrow(df)

  df_final_out <- df %>% lapply(table) %>% as.data.frame()
  names(df_final_out) <- c("kommunikasjon", "kommunikasjon_freq",
                           "informasjon", "informasjon_freq",
                           "programmer", "programmer_freq",
                           "utstyr", "utstyr_freq")
  df_final_out$kommunikasjon_freq_perc <- round_perc(
    df_final_out$kommunikasjon_freq,
    num_obs, digits = 2)
  df_final_out$informasjon_freq_perc <- round_perc(
    df_final_out$informasjon_freq,
    num_obs, digits = 2)
  df_final_out$programmer_freq_perc <- round_perc(
    df_final_out$programmer_freq,
    num_obs, digits = 2)
  df_final_out$utstyr_freq_perc <- round_perc(
    df_final_out$utstyr_freq,
    num_obs, digits = 2)

  df_final_out <- tibble::as_tibble(df_final_out)
  df_final_out <- df_final_out[, -c(3, 5, 7)]
  names(df_final_out)[1] <- "kategorier"
  df_final_out$kategorier <- as.character(df_final_out$kategorier)

  df_final_out <- df_final_out %>%
    dplyr::arrange(dplyr::desc(.data$kategorier)) %>%
    dplyr::mutate(prop_k = compute_prop_trns(.data$kommunikasjon_freq)) %>%
    dplyr::mutate(ypos_k = compute_ypos_trns(.data$prop_k)) %>%
    dplyr::mutate(prop_i = compute_prop_trns(.data$informasjon_freq)) %>%
    dplyr::mutate(ypos_i = compute_ypos_trns(.data$prop_i)) %>%
    dplyr::mutate(prop_p = compute_prop_trns(.data$programmer_freq)) %>%
    dplyr::mutate(ypos_p = compute_ypos_trns(.data$prop_p)) %>%
    dplyr::mutate(prop_u = compute_prop_trns(.data$utstyr_freq)) %>%
    dplyr::mutate(ypos_u = compute_ypos_trns(.data$prop_u)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total_freq = sum(.data$kommunikasjon_freq,
                                   .data$informasjon_freq,
                                   .data$programmer_freq,
                                   .data$utstyr_freq)) %>%
    dplyr::mutate(total_freq_perc = round_perc(.data$total_freq,
                                               (4 * num_obs),
                                               digits = 2))
  return(dplyr::ungroup(df_final_out))
}
generate_data_report <- function(df, year) {
  df_out <- df %>%
    dplyr::select(.data$kategorier, .data$kommunikasjon_freq,
                  .data$informasjon_freq, .data$programmer_freq,
                  .data$utstyr_freq, .data$total_freq,
                  .data$kommunikasjon_freq_perc, .data$informasjon_freq_perc,
                  .data$programmer_freq_perc, .data$utstyr_freq_perc,
                  .data$total_freq_perc)

  df_out <- df_out %>% dplyr::mutate_at(
    .vars = dplyr::vars(dplyr::contains("perc")),
    .funs = function(x) {x / 100})
  df_out$year <- year
  df_out <- df_out %>%
    dplyr::select("year", dplyr::everything())
  return(df_out)
}
compute_prop_trns <- function(x) {
  x / sum(x) * 100
}
compute_ypos_trns <- function(x) {
  cumsum(x) - 0.5 * x
}
