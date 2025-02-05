% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.fcmconfr.R
\name{plot.fcmconfr}
\alias{plot.fcmconfr}
\title{Plot fcmconfr}
\usage{
\method{plot}{fcmconfr}(
  x,
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
\item{x}{A direct output of the \code{\link{fcmconfr}} function}

\item{interactive}{TRUE/FALSE Launch plot in a Shiny app to toggle on/off
results from different analyses}

\item{filter_limit}{Only nodes with inferences above the filter_limit
across any analysis will be plotted. This removes nodes with mostly 0-valued
inferences indicating they were not impacted in the simulation.}

\item{xlim}{The x-axis plot limits. xlim = NA lets ggplot determine the
x-axis limits. xlim = c(lower_limit, upper_limit) for manual input limits.
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
strings. Ignored for IVFN FCMs.}

\item{agg_inferences_color}{Color of the points representing inferences of
the aggregate FCM}

\item{agg_inferences_alpha}{Transparency of the points representing
inferences of the aggregate FCM. Range from 0 to 1 (0: Transparent to
1: Opaque).}

\item{agg_inferences_shape}{Point shapes of the points representing
inferences of the aggregate FCM. Accepts PCH point values and character
strings. Ignored for IVFN FCMs.}

\item{ind_ivfn_and_tfn_linewidth}{Linewidth of lines representing
inferences for analyses of individual IVFN- and TFN- FCMs.}

\item{agg_ivfn_and_tfn_linewidth}{Linewidth of lines representing inferences
for analyses of aggregate IVFN- and TFN- FCMs}

\item{...}{Additional inputs}
}
\value{
A plot of an fcmconfr object's results
}
\description{
This plots the output of fcmconfr() using ggplot. Set interactive = TRUE to
load plot in a Shiny app and toggle on/off results from different analyses.
}
\details{
Generates a generic plot visualizing \code{\link{fcmconfr}} results. Call the
function name directly (\code{\link{autoplot.fcmconfr}})) without parentheses
to see the exact code to generate the plots, then copy-and-paste and edit
as needed.
}
\examples{

# Example using TFN FCMs fcmconfr
tfn_example_fcmconfr <- fcmconfr(
  adj_matrices = sample_fcms$simple_fcms$tfn_fcms,
  # adj_matrices = group_tfn_fcms,
  # Aggregation and Monte Carlo Sampling
  agg_function = 'mean',
  num_mc_fcms = 100,
  # Simulation
  initial_state_vector = c(1, 1, 1, 1, 1, 1, 1),
  clamping_vector = c(1, 0, 0, 0, 0, 0, 0),
  activation = 'rescale',
  squashing = 'sigmoid',
  lambda = 1,
  point_of_inference = "final",
  max_iter = 1000,
  min_error = 1e-05,
  # Inference Estimation (bootstrap)
  ci_centering_function = mean,
  confidence_interval = 0.95,
  num_ci_bootstraps = 1000,
  # Runtime Options
  show_progress = TRUE,
  parallel = FALSE,
  # Additional Options
  run_agg_calcs = TRUE,
  run_mc_calcs = TRUE,
  run_ci_calcs = TRUE,
  include_zeroes_in_sampling = TRUE,
  include_sims_in_output = TRUE
)


# Plot Defaults
plot(tfn_example_fcmconfr,
     interactive = FALSE, # Set to TRUE to open shiny app
     # Plot Formatting Parameters
     filter_limit = 1e-4,
     xlim = c(-1, 1),
     coord_flip = FALSE,
     text_font_size = NA,
     # Plot Aesthetic Parameters
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
     agg_ivfn_and_tfn_linewidth = 0.6
)

# Different from Plot Defaults
plot(tfn_example_fcmconfr,
     interactive = FALSE, # Set to TRUE to open shiny app
     # Plot Formatting Parameters
     filter_limit = 1e-4,
     xlim = c(-0.6, 0.6),
     coord_flip = FALSE,
     # Plot Aesthetic Parameters
     mc_avg_and_CIs_color = "blue",
     mc_inferences_color = "blue",
     mc_inferences_shape = 3,
     ind_inferences_color = "black",
     ind_inferences_shape = 16,
     agg_inferences_color = "red",
     agg_inferences_shape = 17,
     ind_ivfn_and_tfn_linewidth = 0.1,
     agg_ivfn_and_tfn_linewidth = 0.6
)


# Plot Defaults w/ Shiny App
# plot(tfn_example_fcmconfr,
#      interactive = TRUE, # Set to TRUE to open shiny app
#      # Plot Formatting Parameters
#      filter_limit = 1e-4,
#      coord_flip = FALSE,
#      text_font_size = 12,
#      # Plot Aesthetic Parameters
#      mc_avg_and_CIs_color = "blue",
#      mc_inferences_color = "blue",
#      mc_inferences_shape = 3,
#      ind_inferences_color = "black",
#      ind_inferences_shape = 16,
#      agg_inferences_color = "red",
#      agg_inferences_shape = 17,
#      ind_ivfn_and_tfn_linewidth = 0.1,
#      agg_ivfn_and_tfn_linewidth = 0.6
# )
}
