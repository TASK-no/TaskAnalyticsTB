#' Produces a summary plot
#'
#' Horizontal bars showing, per year, the different levels of digital expertise
#' as percentages of the total.
#'
#' @param data_all data set, joined with all years, as produced by
#'    [get_data_joined()]
#' @inheritParams plot_pie_figures
#'
#' @return a ggplot2- or plotly-object, the summary plot
#' @export
plot_overall <- function(data_all, return_type = "default",
                         col_scm = c(light_blue = "#189BC4",
                                     dark_blue = "#3F505A",
                                     orange  = "#F59331",
                                     light_green = "#54B64E",
                                     green = "#58B02C",
                                     dark_green = "#356A1A",
                                     black1 = "#1F282D",
                                     black2 = "#263036")) {
  browser()
  if (return_type == "default") {
    return(plot_overall_def(data_all, col_scm))
  } else if (return_type == "shinyDB") {
    return(plot_overall_shy(data_all, col_scm))
  } else {
    stop("Unknown arg. value to 'return_type'. Must be either 'default', or 'shinyDB'")
  }
}
plot_overall_shy <- function(data_set, col_scm) {
  col_scm_used <- unname(col_scm[seq_along(unique(data_set$year))])
  data_use <- data_set %>%
    dplyr::select(.data$year, .data$kategorier, .data$total_freq_perc) %>%
    dplyr::mutate(kategorier = factor(.data$kategorier,
                                      levels = c("Uerfaren",
                                                 "Grunnleggende",
                                                 paste0("Videreg", "\u00e5", "ende"),
                                                 "Avansert"),
                                      ordered = TRUE),
                  total_freq_perc = .data$total_freq_perc * 100)
  fig <-plotly::plot_ly(data = data_use)
  fig <- fig %>%
    plotly::add_trace(y = ~kategorier,
                      x = ~total_freq_perc,
                      color = ~year,
                      colors = col_scm_used,
                      type = "bar",
                      text = ~paste(total_freq_perc, "%"),
                      textposition = "outside",
                      outsidetextfont = list(color = "black")) %>%
    plotly::layout(legend = list(y = -0.05,
                                 orientation = "h",
                                 x = 0.5),
                   yaxis = list(title = list(text = "Digital Kompetanse",
                                             standoff = 7,
                                             font = list(size = 24)),
                                tickangle = 45),
                   xaxis = list(title = list(text = "")))
}
plot_overall_def <- function(data_set, col_scm) {
  col_scm_used <- unname(col_scm[seq_along(unique(data_set$year))])
  ggplot2::ggplot(data_set,
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
    ggplot2::scale_fill_manual(values = col_scm_used) +
    ggplot2::ylab("") +
    ggplot2::xlab("Digital Kompetanse") +
    ggplot2::theme_void() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 10),
                   axis.title = ggplot2::element_text(size = 14),
                   legend.title = ggplot2::element_blank())+
    ggplot2::coord_flip()
}
#' Binds (rbind()'s) data frames for different years
#'
#' @param ... data sets as produced by [get_data_summary()] through its second
#'   list output element; but can be, more generally, any number of data sets of
#'   the same kind (i.e. the same col-names/-types)
#'
#' @return \code{rbind}-ed data frames
#' @export
get_data_joined <- function(...) {
  data_list <- list(...)
  Reduce(rbind, data_list)
}
