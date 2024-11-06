
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fcmconfr <a href='https://github.com/bhroston/fcmconfr.git/'><img src="man/figures/logo.png" align="right" height="138"/></a>

<!-- badges: start -->

![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)
[![codecov](https://codecov.io/gh/bhroston/fcmconfr/graph/badge.svg?token=D83LF4TC8D)](https://codecov.io/gh/bhroston/fcmconfr)[![pkgcheck](https://github.com/bhroston/fcmconfr/actions/workflows/pkgcheck.yaml/badge.svg?branch=main)](https://github.com/bhroston/fcmconfr.git/actions?query=workflow%3Apkgcheck)
[![R-CMD-check](https://github.com/bhroston/fcmconfr/actions/workflows/check-standard.yaml/badge.svg?branch=main)](https://github.com/bhroston/fcmconfr/actions/workflows/R-CMD-check.yaml)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

<!-- badges: end -->

## Overview

fcmconfr is a tool to evaluate sets of Fuzzy Cognitive Maps (FCMs) to
generate inferences that leverage uncertainty in individual FCMs and
across the collective.

`fcmconfr()` is the package’s primary function which takes a list of FCM
adjacency matrices (or an individual adjacency matrix) and performs the
following analyses:

- Simulate Input FCMs

- Generate an Aggregate FCM and Simulate the result

- Generate Bulk Monte-Carlo FCMs and Simulate them

`fcmconfr_gui()` provides a graphical user interface (GUI) to help users
identify the appropriate inputs for their preferred analyses and
generally improve the overall user-experience in defining the many
inputs required by the primary `fcmconfr()` function.

**fcmconfr aims to consolidate existing software and theory into a
centralized package:** The landscape of software tools to analyse FCMs
is diverse, with approaches and outputs varying across them. This is
reflective of the decentralized nature of FCM theory as a whole which
features an array of analysis methods, each having their strengths and
weaknesses, without one in particular being consistently adopted
throughout the literature.

**fcmconfr offers novel tools to analyze FCMs with edge weights that
incorporate uncertainty:** One limitation of the conventional FCM
approach is that it restricts edge weights to discrete values. Many
extensions of the conventional approach have been introduced in the
literature that allow edge weights to be represented as intervals,
distributions, and a numerous other abstractions. fcmconfr offers novel
tools that streamline access to two such extensions: one represents edge
weights as Interval-Value Fuzzy Numbers (IVFNs, named here as IVFN-FCMs)
and the other represents edge weights as Triangular Fuzzy Numbers (TFNs,
named here as TFN-FCMs).

## Installation

You can install the development version of `fcmconfr()` from GitHub
with:

``` r
# install.packages("remotes")
remotes::install_github("bhroston/fcmconfr")
```

## Using fcmconfr (Example)

``` r
library(fcmconfr)

# This example uses the salinization_ses_fcms dataset included within the package
# 

# Use fcmconfr_gui() to select inputs or manually identify input parameters
# fcmconfr_gui()

# Use fcmconfr() to perform the analysis
fcmconfr(
  adj_matrices = salinization_ses_fcms,
  # Aggregation and Monte Carlo Sampling
  aggregation_function = 'mean',
  monte_carlo_sampling_draws = 1000,
  # Simulation
  initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  clamping_vector = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  activation = 'modified-kosko',
  squashing = 'sigmoid',
  lambda = 1,
  max_iter = 100,
  min_error = 1e-05,
  # Inference Estimation (bootstrap)
  inference_estimation_function = mean,
  inference_estimation_CI = 0.95,
  inference_estimation_bootstrap_reps = 1000,
  inference_estimation_bootstrap_draws_per_rep = 1000,
  # Runtime Options
  show_progress = TRUE,
  parallel = FALSE,
  n_cores = 1,
  # Additional Options
  perform_aggregate_analysis = TRUE,
  perform_monte_carlo_analysis = TRUE,
  perform_monte_carlo_inference_bootstrap_analysis = TRUE,
  include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
  include_monte_carlo_FCM_simulations_in_output = FALSE
)
```

## Citation

``` r
citation("fcmconfr")
#> To cite fcmconfr in publications use:
#> 
#>   Roston & Rippy, (2024). FCMConfR: FCM Uncertainty Analysis Tools.
#>   https://github.com/bhroston/fcmconfr.git.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @bibentry{,
#>     bibtype = {Manual}
#>     title = {FCMConfR: FCM Uncertainty Analysis Tools},
#>     author = {Ben Roston and Megan Rippy},
#>     year = {2024},
#>     url = {https://github.com/bhroston/fcmconfr},
#>     copyright = {GNU General Public License}
#>   }
```

## Further Reading

further reading here

## Contributing

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/bhroston/fcmconfr/issues).

- Please include a
  [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
  (a minimal, reproducible example) to clearly communicate about your
  code.

------------------------------------------------------------------------

However, most remain untested, and their mathematical complexity leaves
many largely inaccessible (citation).

Package to FCM packages across the R
([fcm](https://cran.r-project.org/web/packages/fcm/index.html "CRAN: fcm"))
and Python ([PyFCM](https://github.com/payamaminpour/PyFCM.git))
ecosystems as well

(Dikopoulou and Papageorgiou 2017; Aminpour 2018)

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-aminpourPyFCM2018" class="csl-entry">

Aminpour, Payam. 2018. “PyFCM.”

</div>

<div id="ref-dikopoulouFcmInferenceFuzzy2017" class="csl-entry">

Dikopoulou, Zoumpoulia, and Elpiniki Papageorgiou. 2017. “Fcm: Inference
of Fuzzy Cognitive Maps (FCMs).”

</div>

</div>
