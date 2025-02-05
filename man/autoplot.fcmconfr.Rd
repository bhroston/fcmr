% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-plot.fcmconfr.R
\name{autoplot.fcmconfr}
\alias{autoplot.fcmconfr}
\title{Autoplot fcmconfr}
\usage{
\method{autoplot}{fcmconfr}(
  object,
  interactive = FALSE,
  filter_limit = 0.001,
  xlim = NA,
  coord_flip = FALSE,
  text_font_size = NA,
  mc_avg_and_CIs_color = "blue",
  mc_inferences_color = "blue",
  mc_inferences_alpha = 0.3,
  mc_inferences_shape = 3,
  ind_inferences_color = "black",
  ind_inferences_alpha = 1,
  ind_inferences_shape = 16,
  agg_inferences_color = "red",
  agg_inferences_alpha = 1,
  agg_inferences_shape = 17,
  ind_ivfn_and_tfn_linewidth = 0.1,
  agg_ivfn_and_tfn_linewidth = 0.6,
  ...
)
}
\arguments{
\item{object}{A direct output of the \code{\link{fcmconfr}} function}

\item{filter_limit}{Only nodes with inferences above the filter_limit
across any analysis will be plotted. This removes nodes with mostly 0-valued
inferences indicating they were not impacted in the simulation.}

\item{xlim}{The x-axis plot limits. xlim = NA lets ggplot determine the
x-axis limits. xlim = c(lower_limit, upper_limit) for manual individual limits.
See ?ggplot2::xlim for additional info.}

\item{coord_flip}{Swap x- and y-axes (i.e. rotate plot). See
?ggplot2::coord_flip for additional info.}

\item{text_font_size}{The font size of axis labels. text_font_size = NA lets
ggplot determine the axis label font size.}

\item{mc_avg_and_CIs_color}{Color of the crossbar (lines) indicating the
avg inferences of empirical FCMs generated via Monte Carlo sampling and the
confidence intervals about those averages.}

\item{mc_inferences_color}{Color of the points representing inferences of
empirical FCMs generated via Monte Carlo sampling}

\item{mc_inferences_alpha}{Transparency of the points representing inferences
of empirical FCMs generated via Monte Carlo sampling. Range from 0 to 1
(0: Transparent to 1: Opaque).}

\item{mc_inferences_shape}{Point shapes of the points representing inferences
of empirical FCMs generated via Monte Carlo sampling. Accepts PCH point
values and character strings.}

\item{ind_inferences_color}{Color of the points representing inferences of
individual FCMs}

\item{ind_inferences_alpha}{Transparency of the points representing
inferences of individual FCMs. Range from 0 to 1 (0: Transparent to
1: Opaque).}

\item{ind_inferences_shape}{Point shapes of the points representing
inferences of individual FCMs. Accepts PCH point values and character
strings.}

\item{agg_inferences_color}{Color of the points representing inferences of
the aggregate FCM}

\item{agg_inferences_alpha}{Transparency of the points representing
inferences of the aggregate FCM. Range from 0 to 1 (0: Transparent to
1: Opaque).}

\item{agg_inferences_shape}{Point shapes of the points representing
inferences of the aggregate FCM. Accepts PCH point values and character
strings.}

\item{ind_ivfn_and_tfn_linewidth}{Linewidth of lines representing
inferences for analyses of individual IVFN- and TFN- FCMs.}

\item{agg_ivfn_and_tfn_linewidth}{Linewidth of lines representing inferences
for analyses of aggregate IVFN- and TFN- FCMs}

\item{...}{Additional inputs}
}
\value{
An autoplot plot of an fcmconfr object's results
}
\description{
Generates a generic plot visualizing \code{\link{fcmconfr}} results. Call the
function name directly (\code{\link{autoplot.fcmconfr}})) without parentheses
to see the exact code to generate the plots, then copy-and-paste and edit
as needed.
}
\examples{
NULL
}
\keyword{internal}
