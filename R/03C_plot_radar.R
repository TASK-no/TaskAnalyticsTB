#' Generate radar-type plot
#'
#' @inheritParams plot_pie_figures
#' @inheritParams plot_overall
#'
#' @return a ggplot2-object of the radar plot type
#' @export
plot_radar <- function(df, year, col_scm = c(light_blue = "#189BC4",
                                             dark_blue = "#3F505A",
                                             orange = "#F59331",
                                             light_green = "#54B64E",
                                             green = "#58B02C",
                                             dark_green = "#356A1A",
                                             black1 = "#1F282D",
                                             black2 = "#263036")) {
  ggplot2::ggplot(df, ggplot2::aes(x = .data$kategorier,
                                   y = .data$value,
                                   fill = .data$kompetanseomrader,
                                   label = scales::percent(.data$value))) +
    ggplot2::geom_bar(position = "dodge",
                      stat = "identity") +
    ggplot2::geom_text(position = ggplot2::position_dodge(width = .9), # move to center of bars
                       hjust = -1, # nudge above top of bar
                       size = 3) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::scale_fill_manual(values = unname(col_scm[c("dark_green",
                                                         "black2",
                                                         "black1",
                                                         "green")]),
                               labels = c("Kommunikasjon og samhandling",
                                          "Informasjonssikkerhet og personvern",
                                          "Bruk av programmvare",
                                          "Bruk av teknologi")) +
    ggplot2::ylab("") +
    ggplot2::xlab("Digital Kompetanse") +
    ggplot2::ggtitle(paste0("\u00c5", "r ", year)) +
    ggplot2::coord_flip() +
    ggplot2::theme_void() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 10),
                   axis.title = ggplot2::element_text(size = 14),
                   # legend.justification = c(1, 0),
                   # legend.position = "bottom",
                   # legend.text = ggplot2::element_text(paste0("Kompetanseomr",
                   #                                            "\u00e5", "der")),
                   # legend.title = ggplot2::element_text(paste0("Kompetanseomr",
                   #                                             "\u00e5", "der")),
                   # legend.title = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5,
                                                      size = 20))
}
#' Generates summary data set for radar-type plot
#'
#' @param data_set a data set
#'
#' @return a \code{data.frame} for [plot_radar()]
#' @export
get_data_summary_radar <- function(data_set) {
  data_set$kategorier <- factor(data_set$kategorier,
                                levels = c("Avansert",
                                           paste0("Videreg",
                                                  "\u00e5",
                                                  "ende"),
                                           "Grunnleggende",
                                           "Uerfaren"), ordered = TRUE)
  data_out <- data_set[-12]
  data_out <- pivot_longer(data_out,
                           cols = tidyr::contains("perc"),
                           names_to = "kompetanseomrader")
  data_out$kompetanseomrader <- factor(data_out$kompetanseomrader,
                                       levels = c("utstyr_freq_perc",
                                                  "programmer_freq_perc",
                                                  "informasjon_freq_perc",
                                                  "kommunikasjon_freq_perc"),
                                       ordered = TRUE) %>%
    dplyr::recode_factor(kommunikasjon_freq_perc = "Kommunikasjon og samhandling",
                         informasjon_freq_perc = "Informasjonssikkerhet og personvern",
                         programmer_freq_perc = "Bruk av programmvare",
                         utstyr_freq_perc = "Bruk av teknologi",
                         .ordered = TRUE)
  data_out %>% dplyr::select(year, kategorier, kompetanseomrader, value)
}
