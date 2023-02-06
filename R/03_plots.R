#' Main function to produce the pie figure
#'
#' @param df a data.frame as returned as the first element from the output list
#'   from [get_data_summary()]
#' @param year the year as an integer (2021, 2022, or 2023)
#'
#' @return a "grid arranged" ggplot2 object
#' @export
plot_pie_figures <- function(df, year) {
  task_col_scheme <-  c(light_blue = "#189BC4", dark_blue = "#3F505A",
                        orange  = "#F59331", light_green = "#54B64E",
                        green = "#58B02C", dark_green = "#356A1A",
                        black1 = "#1F282D", black2 = "#263036")
  task_col_scheme_pie <- task_col_scheme[1:4]
  task_col_scheme_bar <- task_col_scheme[c("green",
                                           "dark_green",
                                           "black1", "black2")]
  df_final_data <- df
  id_perc <- which(grepl("perc", names(df_final_data)))
  df_final_data[, id_perc] <- lapply(df_final_data[, id_perc],
                                     function(x) {paste0(x, "%")})
  p1 <- generate_pie_plot(df = df_final_data,
                          var_y = "prop_k",
                          var_fill = "kategorier",
                          title = "Kommunikasjon og samhandling",
                          settings_text = list(y_pos = "ypos_k",
                                               lab = "kommunikasjon_freq_perc",
                                               col = "white",
                                               size = 4),
                          col_palette = "Set1")
  p2 <- generate_pie_plot(df = df_final_data,
                          var_y = "prop_i",
                          var_fill = "kategorier",
                          title = "Informasjonssikkerhet og personvern",
                          settings_text = list(y_pos = "ypos_i",
                                               lab = "informasjon_freq_perc",
                                               col = "white",
                                               size = 4),
                          col_palette = "Set1")
  p3 <- generate_pie_plot(df = df_final_data,
                          var_y = "prop_p",
                          var_fill = "kategorier",
                          title = "Bruk av programvare",
                          settings_text = list(y_pos = "ypos_p",
                                               lab = "programmer_freq_perc",
                                               col = "white",
                                               size = 4),
                          col_palette = "Set1")
  p4 <- generate_pie_plot(df = df_final_data,
                          var_y = "prop_u",
                          var_fill = "kategorier",
                          title = "Bruk av teknologi",
                          settings_text = list(y_pos = "ypos_u",
                                               lab = "utstyr_freq_perc",
                                               col = "white",
                                               size = 4),
                          col_palette = "Set1")
  gridExtra::grid.arrange(p1, p2, p3, p4,
                          nrow = 2,
                          # top = grid::textGrob(paste("År" , year),
                          # top = grid::textGrob(paste("Ar", year),
                          top = grid::textGrob(paste0("\u00c5", "r ", year),
                                               gp = grid::gpar(fontsize = 25,
                                                               font = 8)),
                          newpage = TRUE)
}
generate_pie_plot <- function(df, var_y, var_fill,
                              title,
                              settings_text,
                              col_palette) {
  ggplot2::ggplot(df, ggplot2::aes(x = "",
                                   y = .data[[var_y]],
                                   fill = .data[[var_fill]])) +
    ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::theme_void() + # remove background, grid, numeric labels
    ggplot2::ggtitle(title) +
    ggplot2::geom_text(ggplot2::aes(y = .data[[settings_text$y_pos]],
                                    label = .data[[settings_text$lab]]),
                       color = settings_text$col, size = settings_text$size) +
    ggplot2::scale_fill_brewer(palette = col_palette)
}
#' Binds (rbind()'s) data frames for different years
#'
#' @param ... data sets as produced by [get_data_summary()] through its second
#'   list output element; but can be, more generally, any number of data sets of
#'   the same kind (i.e. the same col-names/-types)
#'
#' @return \code{rbind}-ed data frames
#' @export
get_data_for_summary_plot <- function(...) {
  data_list <- list(...)
  Reduce(rbind, data_list)
}
#' Generates data set for [radar_plot()]
#'
#' @param data_set data set, joined with all years, as produced by
#'    [get_data_for_summary_plot()]
#'
#' @return a data frame, see function body for details
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
                                       ordered = TRUE)
  return(data_out)
}
#' Generate radar-type plot
#'
#' @inheritParams plot_pie_figures
#' @inheritParams plot_summary
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
                       hjust = -0.5, # nudge above top of bar
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
                   legend.justification = c(1, 0),
                   legend.position = c(1, 0),
                   legend.text = ggplot2::element_text(paste0("Kompetanseomr",
                                                              "\u00e5", "der")),
                   legend.title = ggplot2::element_text(paste0("Kompetanseomr",
                                                               "\u00e5", "der")),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5,
                                                      size = 20))
}
#' Produces a summary plot
#'
#' Horizontal bars showing, per year, the different levels of digital expertise
#' as percentages of the total.
#'
#' @param data_all data set, joined with all years, as produced by
#'    [get_data_for_summary_plot()]
#' @param col_scm the col-scheme; default is the task color scheme as hard coded
#'
#' @return a ggplot2-object, the summary plot
#' @export
plot_summary <- function(data_all, col_scm = c(light_blue = "#189BC4",
                                               dark_blue = "#3F505A",
                                               orange  = "#F59331",
                                               light_green = "#54B64E",
                                               green = "#58B02C",
                                               dark_green = "#356A1A",
                                               black1 = "#1F282D",
                                               black2 = "#263036")) {
  num_years <- seq_along(unique(data_all$year))
  ggplot2::ggplot(data_all,
                  ggplot2::aes(x = .data$kategorier,
                               y = .data$total_freq_perc,
                               fill = .data$year,
                               label = scales::percent(.data$total_freq_perc))) +
    ggplot2::geom_bar(position="dodge",
                      stat="identity") +
    ggplot2::geom_text(position = ggplot2::position_dodge(width = .9), # move to center of bars
                       hjust = -0.5, # nudge above top of bar
                       size = 3) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_manual(values = unname(col_scm[num_years])) +
    ggplot2::ylab("") +
    ggplot2::xlab("Digital Kompetanse") +
    ggplot2::theme_void() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 10),
                   axis.title = ggplot2::element_text(size = 14))+
    ggplot2::coord_flip()
}
