% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotly_double_yaxis.R
\name{plotly_double_yaxis}
\alias{plotly_double_yaxis}
\title{Plotly double y axis}
\usage{
plotly_double_yaxis(
  data,
  type,
  mode,
  x,
  yaxis1,
  yaxis2,
  legend1,
  legend2,
  xaxis_title,
  yaxis1_title,
  yaxis2_title,
  main_title,
  source
)
}
\arguments{
\item{data}{data.frame}

\item{type}{type of plot (for instance scatter)}

\item{mode}{plotly mode (for instance lines)}

\item{x}{character string with column to plot on the x axis}

\item{yaxis1}{character string with column to plot on the y axis 1 (i.e. on the left hand side)}

\item{yaxis2}{character string with column to plot on the y axis 2 (i.e. on the right hand side)}

\item{legend1}{legend for yaxis1}

\item{legend2}{legend for yaxis2}

\item{xaxis_title}{title on the x axis}

\item{yaxis1_title}{title on y axis 1}

\item{yaxis2_title}{title on y axis 2}

\item{main_title}{title on top of the plot}

\item{source}{source passed on to plotly::plot_ly()}
}
\description{
Plotly double y axis
}
