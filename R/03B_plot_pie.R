#' Main function to produce the pie figure
#'
#' @param data_set a \code{data.frame} as returned as the first element from the
#'   output list from [get_data_summary()] (if set to type = "all") or the
#'   \code{data.frame} directly if type = 'final'
#' @param year the year as an integer (2021, 2022, or 2023)
#' @param return_type a character: either 'default' which generates a
#'   ggplot2-plot or 'shinyDB' which generates a plotly-type plot for the
#'   corresponding shiny Dashboard
#' @param col_scm the col-scheme; default is the task color scheme as hard coded
#'
#' @return a "grid arranged" ggplot2 object or a plotly subplot
#' @export
plot_pie_figures <- function(data_set, year, return_type = "default",
                             col_scm = c(light_blue = "#189BC4",
                                         dark_blue = "#3F505A",
                                         orange  = "#F59331",
                                         light_green = "#54B64E",
                                         green = "#58B02C",
                                         dark_green = "#356A1A",
                                         black1 = "#1F282D",
                                         black2 = "#263036")) {
  df_final_data <- data_set
  id_perc <- which(grepl("perc", names(df_final_data)))
  df_final_data[, id_perc] <- lapply(df_final_data[, id_perc],
                                     function(x) {paste0(x, "%")})
  title_taken <- paste0("\u00c5", "r ", year)
  if (return_type == "default") {
    title_taken <- grid::textGrob(title_taken,
                                  gp = grid::gpar(fontsize = 25,
                                                  font = 8))
    p_lst <- generate_pie_plot_def(data_set = df_final_data, col_scm = col_scm)
    p_jnd <- gridExtra::grid.arrange(p_lst[[1]], p_lst[[2]],
                                     p_lst[[3]], p_lst[[4]],
                                     nrow = 2, top = title_taken,
                                     newpage = TRUE)

  } else if (return_type == "shinyDB") {
    p_jnd <- generate_pie_plot_shy(data_set = df_final_data,
                                   title_text = title_taken,
                                   col_scm = col_scm)
  } else {
    stop("Unknown arg. to 'return_type': set to either 'default' or 'shinyDB'.")
  }
  return(p_jnd)
}
generate_pie_plot_def <- function(data_set,
                                  lo_vars = c("prop_k",
                                              "prop_i",
                                              "prop_p",
                                              "prop_u"),
                                  lo_pos = c("ypos_k",
                                             "ypos_i",
                                             "ypos_p",
                                             "ypos_u"),
                                  lo_labs = c("kommunikasjon_freq_perc",
                                              "informasjon_freq_perc",
                                              "programmer_freq_perc",
                                              "utstyr_freq_perc"),
                                  lo_subtitles = c("Kommunikasjon og samhandling",
                                                   "Informasjonssikkerhet og personvern",
                                                   "Bruk av programvare",
                                                   "Bruk av teknologi"),
                                  col_scm = "Set1") {
  stopifnot(length(lo_vars) == length(lo_pos))
  stopifnot(length(lo_pos) == length(lo_labs))
  stopifnot(length(lo_labs) == length(lo_subtitles))
  num_pies <- length(lo_vars)
  pie_list <- vector("list", num_pies)
  for (i in seq_len(num_pies)) {
    pie_list[[i]] <- add_pie_def(data_set = data_set,
                                 var_y = lo_vars[i],
                                 var_fill = "kategorier",
                                 title = lo_subtitles[i],
                                 settings_text = list(y_pos = lo_pos[i],
                                                      lab = lo_labs[i],
                                                      col = "white",
                                                      size = 4),
                                 col_palette = col_scm)
  }
  return(pie_list)
}
add_pie_def <- function(data_set, var_y, var_fill,
                        title, settings_text, col_palette) {
  ggplot2::ggplot(data_set, ggplot2::aes(x = "",
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
generate_pie_plot_shy <- function(data_set,
                                  lo_vars = c("kommunikasjon_freq",
                                              "informasjon_freq",
                                              "programmer_freq",
                                              "utstyr_freq"),
                                  lo_subtitles = c("Kommunikasjon og samhandling",
                                                   "Informasjonssikkerhet og personvern",
                                                   "Bruk av programvare",
                                                   "Bruk av teknologi"),
                                  title_text,
                                  col_scm) {
  stopifnot(length(lo_vars) == length(lo_subtitles))
  num_pies <- length(lo_vars)
  if (num_pies == 4) layout_pies <- list(first  = list(row = 0, column = 0),
                                         second = list(row = 0, column = 1),
                                         third  = list(row = 1, column = 0),
                                         fourth = list(row = 1, column = 1));
  col_scheme_pie <- col_scm[seq_len(num_pies)]
  fig <- plotly::plot_ly()
  for (i in seq_len(num_pies)) {
    fig <- local({i <- i;
    fig %>% add_pie_shy(data_set, lo_vars[i], lo_subtitles[i],
                        layout_pies[[i]], col_scheme_pie)
    })
  }
  fig <- fig %>% plotly::layout(title = title_text,
                                legend = list(orientation = "v",
                                              xanchor = "right"),
                                # showlegend = TRUE,
                                grid = list(rows=2, columns=2),
                                xaxis = list(showgrid = FALSE,
                                             zeroline = FALSE,
                                             showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE,
                                             zeroline = FALSE,
                                             showticklabels = FALSE))
  return(fig)
}
add_pie_shy <- function(fig, data_set, var_y, sub_title, pos, col_scm) {
  fig %>% plotly::add_pie(data = data_set,
                          labels = ~kategorier,
                          values = ~eval(parse(text = var_y)),
                          textposition = "outside",
                          textinfo = 'text',
                          insidetextfont = list(color = "#FFFFFF"),
                          hoverinfo = "text",
                          title = list(text = sub_title,
                                       font = list(size = 14)),
                          text = ~paste(round_perc(eval(parse(text = var_y)),
                                                   sum(eval(parse(text = var_y))),
                                                   digits = 2,
                                                   add_perc_sign = TRUE), "%"),
                          hovertext = ~paste(eval(parse(text = var_y)),
                                             " arbeidstaker"),
                          marker = list(colors = col_scm,
                                        line = list(color = "#FFFFFF",
                                                    width = 1)),
                          domain = pos)
}
