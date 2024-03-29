% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03C_plot_radar.R
\name{plot_radar}
\alias{plot_radar}
\title{Generate radar-type plot}
\usage{
plot_radar(
  data_set,
  year,
  return_type = "default",
  col_scm = c(light_blue = "#189BC4", dark_blue = "#3F505A", orange = "#F59331",
    light_green = "#54B64E", green = "#58B02C", dark_green = "#356A1A", black1 =
    "#1F282D", black2 = "#263036")
)
}
\arguments{
\item{data_set}{a \code{data.frame} as returned as the first element from the
output list from [get_data_summary()] (if set to type = "all") or the
\code{data.frame} directly if type = 'final'}

\item{year}{the year as an integer (2021, 2022, or 2023)}

\item{return_type}{a character: either 'default' which generates a
ggplot2-plot or 'shinyDB' which generates a plotly-type plot for the
corresponding shiny Dashboard}

\item{col_scm}{the col-scheme; default is the task color scheme as hard coded}
}
\value{
a ggplot2-object of the radar plot type
}
\description{
Generate radar-type plot
}
