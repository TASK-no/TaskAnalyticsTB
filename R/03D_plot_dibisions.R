#' Generate radar-type plot
#'
#' @inheritParams plot_pie_figures
#'
#' @return a ggplot2-object of the radar plot type
#' @export
plot_division <- function(data_set, year,
                          col_scm = c(light_blue = "#189BC4",
                                      dark_blue = "#3F505A",
                                      orange = "#F59331",
                                      light_green = "#54B64E",
                                      green = "#58B02C",
                                      dark_green = "#356A1A",
                                      black1 = "#1F282D",
                                      black2 = "#263036")) {
  return(plot_division_shy(data_set, year = year, col_scm))
}
plot_division_shy <- function(df, year, col_scm) {
  browser()
  col_scm_used <- unname(col_scm[1:4])

  fig <- plotly::plot_ly(data = df)
  fig <- fig %>%
    plotly::add_trace(y = ~SamDivision,
                      x = ~perc,
                      color = ~sub_category,
                      colors = col_scm_used,
                      type = "bar",
                      text = ~total_num,
                      hoverinfo = 'text') %>%
    plotly::layout(legend = list(y = -0.05,
                                 orientation = "h",
                                 x = 0),
                   title = list(text = paste0("\u00c5", "r ", year)),
                   yaxis = list(title = list(text = "Digital kompetanse per divisjon",
                                             standoff = 7,
                                             font = list(size = 24))),
                   xaxis = list(title = list(text = "")),
                   barmode = "stack")
}
