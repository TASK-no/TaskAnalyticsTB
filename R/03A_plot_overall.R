#' Produces a summary plot
#'
#' Horizontal bars showing, per year, the different levels of digital expertise
#' as percentages of the total.
#'
#' @param data_all data set, joined with all years, as produced by
#'    [get_data_summary_overall()]
#' @param col_scm the col-scheme; default is the task color scheme as hard coded
#'
#' @return a ggplot2-object, the summary plot
#' @export
plot_overall <- function(data_all, col_scm = c(light_blue = "#189BC4",
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
#' Binds (rbind()'s) data frames for different years
#'
#' @param ... data sets as produced by [get_data_summary()] through its second
#'   list output element; but can be, more generally, any number of data sets of
#'   the same kind (i.e. the same col-names/-types)
#'
#' @return \code{rbind}-ed data frames
#' @export
get_data_summary_overall <- function(...) {
  data_list <- list(...)
  Reduce(rbind, data_list)
}
#' Generates data set for [radar_plot()]
#'
#' @param data_set data set, joined with all years, as produced by
#'    [get_data_summary_overall()]
#'
#' @return a data frame, see function body for details
#' @export
