
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
