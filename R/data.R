
#' salinization_conventional_fcms
#'
#' An example fcm dataset collected by collecting stakeholder perceptions of
#' rising salinization in a socio-ecological system. To reduce file size and
#' protect stakeholder anonymity, the provided adjacency matrices contain a
#' subset of nodes and edges across the data used in the cited paper.
#'
#' Given in Rippy et al, (In Review)
#'
#' @format ## `salinization_conventional_fcms`
#' A list of conventional fcm adjacency matrices
#'
"salinization_conventional_fcms"

#' salinization_ivfn_fcms
#'
#' An example fcm dataset collected by collecting stakeholder perceptions of
#' rising salinization in a socio-ecological system. To reduce file size and
#' protect stakeholder anonymity, the provided adjacency matrices contain a
#' subset of nodes and edges across the data used in the cited paper.
#'
#' A buffer has been added to each edge weight so that they may be represented
#' as interval-value fuzzy numbers (IVFNs) instead of discrete numerical
#' values. The edge weight of the original, conventional fcms is contained
#' within the domain of all IVFN edge weights to allow for comparison.
#'
#' Given in Rippy et al, (In Review)
#'
#' @format ## `salinization_ivfn_fcms`
#' A list of conventional fcm adjacency matrices
#'
"salinization_ivfn_fcms"

#' salinization_tfn_fcms
#'
#' An example fcm dataset collected by collecting stakeholder perceptions of
#' rising salinization in a socio-ecological system. To reduce file size and
#' protect stakeholder anonymity, the provided adjacency matrices contain a
#' subset of nodes and edges across the data used in the cited paper.
#'
#' A buffer has been added to each edge weight so that they may be represented
#' as triangular-value fuzzy numbers (TFNs) instead of discrete numerical
#' values. The edge weight of the original, conventional fcms is contained
#' as the mode value of all TFN edge weights to allow for comparison.
#'
#' Given in Rippy et al, (In Review)
#'
#' @format ## `salinization_tfn_fcms`
#' A list of conventional fcm adjacency matrices
#'
"salinization_tfn_fcms"

#' OLD! salinization_conventional_fcms
#'
#' An example fcm dataset collected by collecting stakeholder perceptions of
#' rising salinization in a socio-ecological system. To reduce file size and
#' protect stakeholder anonymity, the provided adjacency matrices contain a
#' subset of nodes and edges across the data used in the cited paper.
#'
#' Given in Rippy et al, (In Review)
#'
#' @format ## `salinization_conventional_fcms`
#' A list of conventional fcm adjacency matrices
#'
"old_salinization_conventional_fcms"

#' OLD! salinization_ivfn_fcms
#'
#' An example fcm dataset collected by collecting stakeholder perceptions of
#' rising salinization in a socio-ecological system. To reduce file size and
#' protect stakeholder anonymity, the provided adjacency matrices contain a
#' subset of nodes and edges across the data used in the cited paper.
#'
#' A buffer has been added to each edge weight so that they may be represented
#' as interval-value fuzzy numbers (IVFNs) instead of discrete numerical
#' values. The edge weight of the original, conventional fcms is contained
#' within the domain of all IVFN edge weights to allow for comparison.
#'
#' Given in Rippy et al, (In Review)
#'
#' @format ## `salinization_ivfn_fcms`
#' A list of conventional fcm adjacency matrices
#'
"old_salinization_ivfn_fcms"

#' OLD! salinization_tfn_fcms
#'
#' An example fcm dataset collected by collecting stakeholder perceptions of
#' rising salinization in a socio-ecological system. To reduce file size and
#' protect stakeholder anonymity, the provided adjacency matrices contain a
#' subset of nodes and edges across the data used in the cited paper.
#'
#' A buffer has been added to each edge weight so that they may be represented
#' as triangular-value fuzzy numbers (TFNs) instead of discrete numerical
#' values. The edge weight of the original, conventional fcms is contained
#' as the mode value of all TFN edge weights to allow for comparison.
#'
#' Given in Rippy et al, (In Review)
#'
#' @format ## `salinization_tfn_fcms`
#' A list of conventional fcm adjacency matrices
#'
"old_salinization_tfn_fcms"

#' Vat FGCM Examples
#'
#' An example fgcm representing a multi-tank vat system. Depicted in
#' Salmeron & Papageorgiou, 2014 - https://doi.org/10.1007/s10489-013-0511-z
#'
#' @format ## `vat_fgcm_examples`
#' A list of fgcm adjacency matrices and initial state vectors
#' \describe{
#'   \item{fcm_as_fgcm}{An fgcm where every value is a white number to allow
#'   users to compare results with ordinary fcm}
#'   \item{fgcm}{An fgcm with grey values incorporated in its adjacency matrix}
#'   ...
#' }
#' @source <https://doi.org/10.1007/s10489-013-0511-z>
"vat_fgcm_examples"
