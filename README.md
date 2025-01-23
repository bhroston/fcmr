
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fcmconfr

\# fcmconfr \<a
href=’[https://github.com/bhroston/fcmconfr.git/’](https://github.com/bhroston/fcmconfr.git/')\<img
src=“man/figures/logo.png” align=“right” height=“138”/\>\</a\>

<!-- badges: start -->

![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](%5Bhttps://www.repostatus.org/badges/latest/wip.svg)\](<https://www.repostatus.org/badges/latest/wip.svg>))
[![codecov](%5Bhttps://codecov.io/gh/bhroston/fcmconfr/graph/badge.svg?token=D83LF4TC8D)\](https://codecov.io/gh/bhroston/fcmconfr)\[![pkgcheck](https://github.com/bhroston/fcmconfr/actions/workflows/pkgcheck.yaml/badge.svg?branch=main)\](https://github.com/bhroston/fcmconfr.git/actions?query=workflow%3Apkgcheck)](https://codecov.io/gh/bhroston/fcmconfr/graph/badge.svg?token=D83LF4TC8D)\](<https://codecov.io/gh/bhroston/fcmconfr>)[![pkgcheck](https://github.com/bhroston/fcmconfr/actions/workflows/pkgcheck.yaml/badge.svg?branch=main)](https://github.com/bhroston/fcmconfr.git/actions?query=workflow%3Apkgcheck))
[![R-CMD-check](%5Bhttps://github.com/bhroston/fcmconfr/actions/workflows/check-standard.yaml/badge.svg?branch=main)\](https://github.com/bhroston/fcmconfr/actions/workflows/R-CMD-check.yaml)](https://github.com/bhroston/fcmconfr/actions/workflows/check-standard.yaml/badge.svg?branch=main)\](<https://github.com/bhroston/fcmconfr/actions/workflows/R-CMD-check.yaml>))
[![License: GPL
v3](%5Bhttps://img.shields.io/badge/License-GPLv3-blue.svg)\](https://www.gnu.org/licenses/gpl-3.0)](https://img.shields.io/badge/License-GPLv3-blue.svg)\](<https://www.gnu.org/licenses/gpl-3.0>))

<!-- badges: end -->

The goal of fcmconfr is to is to provide a suite of functions that
streamline the analysis of Fuzzy Cognitive Maps (FCMs). Fuzzy Cognitive
Maps (FCMs), as introduced in Kosko (1986), are network graphs whose
nodes and edges represent causal connections in a system. A key
characteristic of FCMs are their capacity to represent causal
connections through the language of fuzzy logic, where the strength of
causality between nodes may be represented along a spectrum of values as
opposed to a simple, binary depiction of the existence of connection
between nodes of ordinary Cognitive Map.

FCMs serve as an accessible medium for stakeholders and researchers
alike to explore perspectives in systems thinking. However, despite
their accessibility as tools for data collection, FCM analysis is
mathematically intensive and requires a combination of academic and
programming expertise to both navigate the FCM literature and implement
the array of algorithms proposed in the theory.

fcmconfr lets users analyze FCMs without the previously described
barriers-to-entry. Rather than theoretical and programming expertise,
users with moderate experience with the R programming language can use
fcmconfr to go straight from data collection and creating FCMs to
analyzing results.

## Installation

You can install the development version of fcmconfr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("bhroston/fcmconfr")

# OR

# install.packages("remotes")
remotes::install_github("bhroston/fcmconfr")
```

## Example

### Loading Data

FCMs, represented as adjacency matrices, are the fundamental inputs for
fcmconfr. Whether stored in .xlsx or .csv files, users will need to load
adjacency matrices into R to use fcmconfr. This is an example of loading
an adjacency matrix or set of adjacency matrices from an excel file.
Follow a similar process for loading from .csv files. Note that multiple
FCMs are loaded as a list of multiple FCMs but stored in an individual
object.

### Using fcmconfr

The fcmconfr() function itself takes many inputs, so it is recommended
for users to call fcmconfr_gui() to help write the call to fcmconfr with
their intended parameters. This example uses the sample_fcms object
provided in fcmconfr.

fcmconfr_gui() opens a shiny app that asks the user to select an
adjacency matrix or list of adjacency matrices from the Global
Environment. Note the sidebar panel arrow to the right of the app. Open
this sidebar to see definitions and descriptions for each parameter.

<figure>
<img src="images/fcmconfr_gui_screenshot-01.png"
alt="fcmconfr_gui() opens a shiny app that guides users through selecting inputs for the fcmconfr() function" />
<figcaption aria-hidden="true">fcmconfr_gui() opens a shiny app that
guides users through selecting inputs for the fcmconfr()
function</figcaption>
</figure>

Once parameters are selected, scroll down in the app and click the
“Submit” button. This outputs an example call to fcmconfr() in the
RStudio console that uses the parameters selected by the user in the
gui. For example, fcmconfr_gui() may return:

![](images/fcmconfr_gui_output.png)

Users can copy-and-paste the generated call to the main fcmconfr()
function into a separate file or directly into the console.

When running fcmconfr(), the console will output messages to update the
user on the function’s progress. Depending on the complexity of the
inputs and the capacity of the local machine, fcmconfr() can take
anywhere from a few seconds to many minutes to run. Note: the progress
text will be different depending on inputs to fcmconfr().

<img src="images/fcmconfr_run_text.png" width="428" />

### fcmconfr Outputs

As fcmconfr() performs a collection of analyses on potentially many
FCMs, its output is typically a large object to navigate. Use
get_inferences(fcmconfr_output_obj) to get a list of organized
dataframes for the results of each analysis performed by fcmconfr().

Use plot() to plot the analysis’ results. Adding interactive = TRUE to
the call to plot() loads the figure in a shiny app where users can
select sepecific analysis results to display and/or hide. Additional
parameters for plot() are given below:
