#' Main function to produce the pie figure
#'
#' @param df a data.frame as returned as the first element from the output list
#'   from [get_data_summary()]
#' @param ar the year as an integer (2021, 2022, or 2023)
#'
#' @return a "grid arranged" ggplot
#' @export
get_pie_figures <- function(df, ar) {
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
                          # top = grid::textGrob(paste("År" ,ar),
                          top = grid::textGrob(paste("Ar", ar),
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
    ggplot2::theme_void() +  # remove background, grid, numeric labels
    ggplot2::ggtitle(title) +
    ggplot2::geom_text(ggplot2::aes(y = .data[[settings_text$y_pos]],
                                    label = .data[[settings_text$lab]]),
                       color = settings_text$col, size = settings_text$size) +
    ggplot2::scale_fill_brewer(palette = col_palette)
}
# p1 <- ggplot2::ggplot(df_final_data, ggplot2::aes(x = "",
#                                                   y = .data$prop_k,
#                                                   fill = .data$kategorier)) +
#   ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
#   ggplot2::coord_polar("y", start = 0) +
#   ggplot2::theme_void() +  # remove background, grid, numeric labels
#   ggplot2::ggtitle("Kommunikasjon og samhandling") +
#   ggplot2::geom_text(ggplot2::aes(y = .data$ypos_k,
#                                   label = .data$kommunikasjon_freq_perc),
#                      color = "white", size = 4) +
#   ggplot2::scale_fill_brewer(palette = "Set1")
#
# p2 <- ggplot2::ggplot(df_final_data, ggplot2::aes(x = "",
#                                                   y = .data$prop_i,
#                                                   fill = .data$kategorier)) +
#   ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
#   ggplot2::coord_polar("y", start = 0) +
#   ggplot2::theme_void() +  # remove background, grid, numeric labels
#   ggplot2::geom_text(ggplot2::aes(y = .data$ypos_i,
#                                   label = .data$informasjon_freq_perc),
#                      color = "white", size = 4) +
#   ggplot2::scale_fill_brewer(palette = "Set1")+
#   ggplot2::ggtitle("Informasjonssikkerhet og personvern")
#
# p3 <- ggplot2::ggplot(df_final_data, ggplot2::aes(x = "",
#                                                   y = .data$prop_p,
#                                                   fill = .data$kategorier)) +
#   ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
#   ggplot2::coord_polar("y", start = 0) +
#   ggplot2::theme_void() +  # remove background, grid, numeric labels
#   ggplot2::geom_text(ggplot2::aes(y = .data$ypos_p,
#                                   label = .data$programmer_freq_perc),
#                      color = "white", size = 4) +
#   ggplot2::scale_fill_brewer(palette = "Set1")+
#   ggplot2::ggtitle("Bruk av programvare")
#
# p4 <- ggplot2::ggplot(df_final_data, ggplot2::aes(x="",
#                                                   y = .data$prop_u,
#                                                   fill = .data$kategorier)) +
#   ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
#   ggplot2::coord_polar("y", start = 0) +
#   ggplot2::theme_void() +  # remove background, grid, numeric labels
#   ggplot2::geom_text(ggplot2::aes(y = .data$ypos_u,
#                                   label = .data$utstyr_freq_perc),
#                      color = "white", size = 4) +
#   ggplot2::scale_fill_brewer(palette = "Set1")+
#   # ggplot2::scale_alpha_manual(values = c(0.25, 0.5, 0.75, 1)) +
#   # ggplot2::scale_fill_manual(values = rep("#54B64E" ,
#   #                                         times = 4)) +
#   ggplot2::ggtitle("Bruk av teknologi")
