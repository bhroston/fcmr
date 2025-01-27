
#' dopar operator
#'
#' See https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf for details.
#'
#' @name %dopar%
#' @keywords internal
#' @export
#' @importFrom foreach %dopar%
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

# here::here() is used in a vignette and must be officially called in the
# main R directory for R CMD Check to recognize the dependence
# Feel free to ignore
invisible(here::here())

