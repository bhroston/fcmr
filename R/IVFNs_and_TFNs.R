
################################################################################
# IVFNs_and_TFNs.R
#
# These functions manage and interact with IVFN and TFN objects
#
#   - defuzz_ivfn_or_tfn
#
#   Interval-Valued Fuzzy Numbers (IVFNs)
#   - make_adj_matrix_w_ivfns
#   - ivfn
#   - print.ivfn
#   - c.ivfn
#
#   Triangular Fuzzy Numbers (TFNs)
#   - make_adj_matrix_w_tfns
#   - tfn
#   - print.tfn
#   - c.tfn
#   - rtriangular_dist
#   - plot.rtriangular_dist
#
################################################################################

#' Defuzz (IVFN or TFN)
#'
#' @description
#' Convert a fuzzy number to a crisp value. For IVFNs, return the average of the
#' upper and lower bounds. For TFNs, return the average of the lower bound, the
#' mode, and the upper bound.
#'
#' @param fuzzy_number A fuzzy number object. Either an ivfn or tfn
#'
#' @returns A crisp value representative of the input IVFN or TFN
#'
#' @export
#' @examples
#' defuzz_ivfn_or_tfn(ivfn(-1, 1))
#' defuzz_ivfn_or_tfn(tfn(-1, 0, 1))
defuzz_ivfn_or_tfn <- function(fuzzy_number) {
  fuzzy_class <- methods::is(fuzzy_number)[1]
  if (fuzzy_class == "numeric" | fuzzy_class == "integer") {
    crisp_value <- fuzzy_number
  } else if (fuzzy_class == "ivfn") {
    crisp_value <- (fuzzy_number$lower + fuzzy_number$upper)/2
  } else if (fuzzy_class == "tfn") {
    crisp_value <- (fuzzy_number$lower + fuzzy_number$mode + fuzzy_number$upper)/3
  } else {
    stop("Cannot defuzz input fuzzy_number. Must be either an ivfn or tfn. (Accepts numerics but does nothing with them.)")
  }
  crisp_value
}


# INTERVAL-VALUED FUZZY NUMBERS ----
#' Create Adj. Matrix w/ Edges Represented as IVFNs
#'
#' @family interval-valued-fuzzy-numbers
#'
#' @description
#' This constructs an adjacency matrix with edges represented by interval-value
#' fuzzy numbers (IVFNs) from an adjacency matrix of lower bounds and an
#' adjacency matrix of upper bounds
#'
#' @details
#' The input adjacency matrices must square n x n matrices with the same dimensions.
#' The input can be either matrix, data.table, tibble, or data.table type objects,
#' but the output will always be a data.frame. This is for output readability.
#' data.table and tibble objects work logically, but their outputs require
#' additional steps to parse from the user's perspective.
#'
#' If the input matrices have named columns, those names will be carried over
#' in the grey adjacency matrix. Otherwise, generic node IDs will be used
#' (C1, C2, ... Cn).
#'
#' @param lower An n x n adjacency matrix that represents the lower limits of
#'              edges in an FCM
#' @param upper An n x n adjacency matrix that represents the upper limits of
#'              edges in an FCM
#'
#' @returns An adj. matrix (of class 'ivfn') with edges represented as IVFNs
#'
#' @export
#' @example  man/examples/ex-make_adj_matrix_w_ivfns.R
make_adj_matrix_w_ivfns <- function(lower = matrix(), upper = matrix()) {
  # browser()

  if (!identical(dim(lower), dim(upper))) {
    stop(cli::format_error(c(
      "x" = "Error: {.var lower} and {.var upper} must have the same dimensions (i.e. be the same size) and must be square (n x n) matrices",
      "+++++> Input {.var lower} had dimensions: {dim(lower)}",
      "+++++> Input {.var upper} had dimensions: {dim(upper)}"
    )))
  }

  if (nrow(lower) != ncol(lower) | nrow(upper) != ncol(upper)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var lower} and {.var upper} must have the same dimensions (i.e. be the same size) and must be square (n x n) matrices",
      "+++++> Input {.var lower} had dimensions: {dim(lower)}",
      "+++++> Input {.var upper} had dimensions: {dim(upper)}"
    )))
  } else {
    size <- nrow(lower)
  }

  if (identical(colnames(lower), colnames(upper)) & !identical(colnames(lower), NULL)) {
    IDs <- colnames(lower)
  } else {
    IDs <- paste0("C", 1:nrow(lower))
    colnames(lower) <- IDs
    colnames(upper) <- IDs
  }

  if ((!all(lower <= upper))) {
    offense_locs <- unique(rbind(which(!lower <= mode, arr.ind = TRUE), which(!mode <= upper, arr.ind = TRUE)))
    offenses_df <- data.frame(
      row = offense_locs[, 1],
      col = offense_locs[, 2],
      lower = apply(offense_locs, 1, function(locs) lower[locs[1], locs[2]]),
      upper = apply(offense_locs, 1, function(locs) upper[locs[1], locs[2]])
    )
    rownames(offenses_df) <- NULL
    print(offenses_df)
    stop(cli::format_error(c(
      "x" = "Error: Failed to create adj. matrix from input",
      "+++++>  All lower values must be less than or equal to upper values.",
      "+++++>  Check offenses printed above."
    )))
  }

  adj_matrix_w_ivfns <- as.data.frame(matrix(data = list(0), nrow = size, ncol = size))
  colnames(adj_matrix_w_ivfns) <- IDs
  rownames(adj_matrix_w_ivfns) <- IDs

  for (i in 1:length(IDs)) {
    for (j in 1:length(IDs)) {
      adj_matrix_w_ivfns[[j]][[i]] <- ivfn(
        # [[j]][[i]] instead of [[i]][[j]]
        # because this notation is
        # [[col]][[row]] for data.frames
        lower = lower[i, j],
        upper = upper[i, j]
      )
    }
  }

  class(adj_matrix_w_ivfns) <- c("adj_matrix_w_ivfns", methods::is(adj_matrix_w_ivfns))

  adj_matrix_w_ivfns
}



#' Interval-Valued Fuzzy Number (IVFN) - S3 Class
#'
#' @family interval-valued-fuzzy-numbers
#'
#' @description
#' This constructs an interval-valued fuzzy number (ivfn) that represents a
#' continuous, uniform distribution of values within a given range
#'
#' @details
#' The IVFN class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of IVFNs
#'
#' For IVFNs, the lower bound must be less than or equal to the upper bound.
#' If the lower bound and upper bound are equal, the IVFN represents a "crisp"
#' numeric value.
#'
#' @param lower An n x n adjacency matrix that represents the lower limits of
#'              edges in an FCM
#' @param upper An n x n adjacency matrix that represents the upper limits of
#'              edges in an FCM
#'
#' @returns An interval-valued fuzzy number (IVFN)
#'
#' @export
#' @example  man/examples/ex-ivfn.R
ivfn <- function(lower = double(), upper = double()) {
  lower <- unlist(lower)
  upper <- unlist(upper)

  if (identical(lower, double())) {
    lower <- -Inf
  }

  if (identical(upper, double())) {
    upper <- Inf
  }

  if ((!is.numeric(lower)) | (!is.numeric(upper))) {
    stop(cli::format_error(c(
      "x" = "Error: {.var lower} and {.var upper} must be single, numeric values",
      "+++++> Input {.var lower} was: {lower}",
      "+++++> Input {.var upper} was: {upper}"
    )))
  }

  if (lower > upper) {
    stop(cli::format_error(c(
      "x" = "Error: {.var lower} must be less than or equal to {.var upper}",
      "+++++> Input {.var lower} was: {lower}",
      "+++++> Input {.var upper} was: {upper}"
    )))
  }

  structure(
    .Data = data.frame(lower = lower, upper = upper),
    class = "ivfn"
  )
}


#' IVFN Subtraction (Interval Calculus)
#'
#' @family interval-valued-fuzzy-numbers
#'
#' @description
#' This subtracts one Interval-Value Fuzzy Number (IVFN) from another via
#' interval calculus. An IVFN represented by the interval X:
#'
#' \deqn{X\  =( x_{1}, x_{3}) \ =\ [ x_{1} ;x_{2}] =\{x\in \mathbb{R} \ |\ x_{1} \ \leq x\ \leq x_{2}\}}{ascii}
#'
#' where \eqn{x_1}{ascii} and \eqn{x_2}{ascii} represent the lower and upper
#' bounds, respectively.
#'
#' The IVFN X may have another IVFN Y subtracted from it via:
#'
#' \deqn{X\ -\ Y\ =\ [ x_{1} \ -\ y_{2} ;\ x_{2} \ -\ y_{1}]}{ascii}
#'
#' @details
#' It is not required for one IVFN to be "greater than" the other.
#'
#' This difference may also be estimated by translating the IVFN's into
#' their corresponding distributions (e.g. ivfn(-1, 1) = runif(n, -1, 1)),
#' subtracting one distribution from the other, and estimating the minimum
#' and maximum values of the difference distribution.
#'
#' @references \insertRef{mooreIntervalAnalysisFuzzy2003}{fcmconfr}
#' @references \insertRef{dimuroIntervalFuzzyNumbers2011}{fcmconfr}
#' @references \insertRef{mooreIntervalAnalysis1966}{fcmconfr}
#'
#' @param ivfn_1 An interval-value fuzzy number (ivfn) object
#' @param ivfn_2 An interval-value fuzzy number (ivfn) object
#'
#' @returns An IVFN object representing the subtraction of ivfn_2 from ivfn_1
#'
#' @importFrom Rdpack reprompt
#'
#' @export
#' @examples
#' subtract_ivfn(ivfn(0.5, 0.8), ivfn(0.2, 0.5))
#' subtract_ivfn(ivfn(-0.5, 0.3), ivfn(0.4, 0.6))
#' subtract_ivfn(ivfn(-1, 1), ivfn(-0.5, 0.5))
subtract_ivfn <- function(ivfn_1 = ivfn(), ivfn_2 = ivfn()) {
  # browser()

  if ((!identical(methods::is(ivfn_1), "ivfn")) | (!identical(methods::is(ivfn_2), "ivfn"))) {
    stop("Input Error: Both inputs must be valid ivfn objects")
  }

  new_lower <- ivfn_1$lower - ivfn_2$upper
  new_upper <- ivfn_1$upper - ivfn_2$lower
  ivfn(new_lower, new_upper)
}



#' Print an Interval-Valued Fuzzy Number (IVFN) - S3 Class
#'
#' @family interval-valued-fuzzy-numbers
#'
#' @description
#' This prints an ivfn object
#'
#' @param x a ivfn object
#' @param ... additional inputs
#'
#' @returns A console printout of an IVFN object
#'
#' @export
#' @examples
#' print(ivfn(-1, 1))
print.ivfn <- function(x, ...) {
  cat(class(x), ": [", x$lower, ", ", x$upper, "]", sep = "")
}


#' c.ivfn
#'
#' @family interval-valued-fuzzy-numbers
#'
#' @description
#' This forces the output of c() to the equivalent of list() only for inputs of
#' type ivfn
#'
#' @details
#' For ivfn objects, c() combines all of the lower and upper data into
#' a single ivfn object, but list() returns the expected output of a
#' list of distinct ivfn objects.
#'
#' @param ... a set of ivfn objects
#'
#' @returns A list of ivfn objects
#'
#' @export
#' @examples
#' c(ivfn(0, 1), ivfn(0.2, 0.5))
c.ivfn <- function(...) {
  list(...)
}


# ----
# TRIANGULAR FUZZY NUMBERS (TFNs) ----

#' Create Adj. Matrix w/ Edges Represented as TFNs
#'
#' @family triangular-fuzzy-numbers
#'
#' @description
#' This constructs an adjacency matrix with edges represented by triangular
#' fuzzy numbers (TFNs) from an adjacency matrix of lower bounds, an adjacency
#' matrix of modes, and an adjacency matrix of upper bounds
#'
#' @details
#' The input adjacency matrices must square n x n matrices with the same dimensions.
#' The input can be either matrix, data.table, tibble, or data.table type objects,
#' but the output will always be a data.frame. This is for output readability.
#' data.table and tibble objects work logically, but their outputs require
#' additional steps to parse from the user's perspective.
#'
#' If the input matrices have named columns, those names will be carried over
#' in the triangular adjacency matrix. Otherwise, generic node IDs will be used
#' (C1, C2, ... Cn).
#'
#' @param lower An n x n adjacency matrix that represents the lower limits of
#'              edges in an FCM
#' @param mode An n x n adjacency matrix that represents the modes of edges in an FCM
#' @param upper An n x n adjacency matrix that represents the upper limits of
#'              edges in an FCM
#'
#' @returns An adj. matrix (of class 'tfn') with edges represented as TFNs
#'
#' @export
#' @example  man/examples/ex-make_adj_matrix_w_tfns.R
make_adj_matrix_w_tfns <- function(lower = matrix(),
                                   mode = matrix(),
                                   upper = matrix()) {
  if (!(identical(dim(lower), dim(mode)) & identical(dim(mode), dim(upper)))) {
    stop(cli::format_error(c(
      "x" = "Error: {.var lower}, {.var mode}, and {.var upper} must have the same dimensions (i.e. be the same size) and must be square (n x n) matrices",
      "+++++> Input {.var lower} had dimensions: {dim(lower)}",
      "+++++> Input {.var lower} had dimensions: {dim(mode)}",
      "+++++> Input {.var upper} had dimensions: {dim(upper)}"
    )))
  }

  if (nrow(lower) != ncol(lower) | nrow(mode) != ncol(mode) | nrow(upper) != ncol(upper)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var lower}, {.var mode}, and {.var upper} must have the same dimensions (i.e. be the same size) and must be square (n x n) matrices",
      "+++++> Input {.var lower} had dimensions: {dim(lower)}",
      "+++++> Input {.var lower} had dimensions: {dim(mode)}",
      "+++++> Input {.var upper} had dimensions: {dim(upper)}"
    )))
  } else {
    size <- nrow(lower)
  }

  all_input_matrices_have_same_colnames <- length(unique(list(colnames(lower), colnames(mode), colnames(upper)))) == 1
  if (all_input_matrices_have_same_colnames & !identical(colnames(lower), NULL)) {
    IDs <- colnames(lower)
  } else {
    IDs <- paste0("C", 1:nrow(lower))
  }

  if ((!all(lower <= mode) | !all(mode <= upper))) {
    offense_locs <- unique(rbind(which(!lower <= mode, arr.ind = TRUE), which(!mode <= upper, arr.ind = TRUE)))
    offenses_df <- data.frame(
      row = offense_locs[, 1],
      col = offense_locs[, 2],
      lower = apply(offense_locs, 1, function(locs) lower[locs[1], locs[2]]),
      mode = apply(offense_locs, 1, function(locs) mode[locs[1], locs[2]]),
      upper = apply(offense_locs, 1,function(locs) upper[locs[1], locs[2]])
    )
    rownames(offenses_df) <- NULL
    print(offenses_df)
    stop(cli::format_error(c(
      "x" = "Error: Failed to create adj. matrix from input",
      "+++++>  All lower values must be less than or equal to mode values which in turn, \n  must be less than or equal to upper values.",
      "+++++>  Check offenses printed above."
    )))
  }

  adj_matrix_w_tfns <- as.data.frame(matrix(data = list(0), nrow = size, ncol = size))
  colnames(adj_matrix_w_tfns) <- IDs
  rownames(adj_matrix_w_tfns) <- IDs

  for (i in 1:length(IDs)) {
    for (j in 1:length(IDs)) {
      adj_matrix_w_tfns[[j]][[i]] <- tfn(
        # [[j]][[i]] instead of [[i]][[j]]
        # because this notation is
        # [[col]][[row]] for data.frames
        lower = lower[i, j],
        mode = mode[i, j],
        upper = upper[i, j]
      )
    }
  }

  class(adj_matrix_w_tfns) <- c("adj_matrix_w_tfns", methods::is(adj_matrix_w_tfns))

  adj_matrix_w_tfns
}



#' Triangular Fuzzy Number (TFN) - S3 Class
#'
#' @family triangular-fuzzy-numbers
#'
#' @description
#' This constructs an triangular fuzzy number (ivfn) that represents a
#' continuous, triangular distribution of values within a given range
#'
#' @details
#' The TFN class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of TFNs
#'
#' For TFNs, the lower bound must be less than or equal to the mode which must
#' be less than or equal to the upper bound.
#' If the lower bound, mode, and upper bound are equal, the TFN represents a
#' "crisp" numeric value.
#'
#' @param lower lower limit of a Triangular Number set (the lower value must be
#' less than or equal to the upper value)
#' @param mode the most likely value of a Triangular Number set
#' @param upper upper limit of a Triangular Number set (the upper value must be
#' greater or equal to the lower value)
#'
#' @returns A triangular fuzzy number (TFN)
#'
#' @export
#' @example  man/examples/ex-tfn.R
tfn <- function(lower = double(), mode = double(), upper = double()) {
  lower <- unlist(lower)
  mode <- unlist(mode)
  upper <- unlist(upper)

  if (identical(lower, double())) {
    lower <- -Inf
  }

  if (identical(mode, double())) {
    mode <- 0
  }

  if (identical(upper, double())) {
    upper <- Inf
  }

  if ((!is.numeric(lower)) | (!is.numeric(mode)) | (!is.numeric(upper))) {
    stop(cli::format_error(c(
      "x" = "Error: {.var lower} and {.var upper} must be single, numeric values",
      "+++++> Input {.var lower} was: {lower}",
      "+++++> Input {.var mode} was: {mode}",
      "+++++> Input {.var upper} was: {upper}"
    )))
  }

  if (lower > upper | lower > mode) {
    stop(cli::format_error(c(
      "x" = "Error: {.var lower} must be less than or equal to {.var mode}, which in turn must be less than or equal to {.var upper}",
      "+++++> Input {.var lower} was: {lower}",
      "+++++> Input {.var mode} was: {mode}",
      "+++++> Input {.var upper} was: {upper}"
    )))
  }

  if (mode > upper) {
    stop(cli::format_error(c(
      "x" = "Error: {.var lower} must be less than or equal to {.var mode}, which in turn must be less than or equal to {.var upper}",
      "+++++> Input {.var lower} was: {lower}",
      "+++++> Input {.var mode} was: {mode}",
      "+++++> Input {.var upper} was: {upper}"
    )))
  }

  structure(
    .Data = data.frame(lower = lower, mode = mode, upper = upper),
    class = "tfn"
  )
}



#' TFN Subtraction (Interval Calculus)
#'
#' @family triangular-fuzzy-numbers
#'
#' @description
#' This subtracts one Triangular Fuzzy Number (TFN) from another via
#' interval calculus. A TFN represented by the fuzzy set (triangular distribution):
#'
#' \deqn{X\  =( x_{1} ,x_{2} ,x_{3}) \ =\ \left\{\begin{matrix}
#' 0 & for\  & x< x_{1}\\
#' \frac{2( x\ -\ x_{1})}{( x_{3} -x_{1})( x_{2} -x_{1})} & for\  & x_{1} \leq x< x_{3} \ \\
#' \frac{2}{x_{3} -x_{1}} & for & x\ =\ x_{2}\\
#' \frac{2( x_{3} -x)}{( x_{3} -x_{1})( x_{3} -x_{2})} & for\  & x_{2} < x\leq x_{3}\\
#' 0 & for & x >x_{3}
#' \end{matrix}\right.}{ascii}
#'
#' where \eqn{x_1}{ascii} and \eqn{x_3}{ascii} are the lower and upper bounds, respectively,
#' and \eqn{x_2}{ascii} is the mode.
#'
#' The TFN X may have another TFN Y subtracted from it via:
#'
#' \deqn{X\ -\ Y\ =\ ( x_{1} -y_{3} ,\ x_{2} -y_{2} ,\ x_{3} -y_{1})}{ascii}
#'
#' @details
#' It is not required for one IVFN to be "greater than" the other.
#'
#' This difference may also be estimated by translating the TFN's into
#' their corresponding distributions
#' (e.g. tfn(-1, 0, 1) = EnvStats::rtri(n, min = -1, max = 1, mode = 0)),
#' subtracting one distribution from the other, and estimating the minimum,
#' mode, and maximum values of the difference distribution.
#'
#' @references \insertRef{chakravertyFuzzyNumbers2019}{fcmconfr}
#' @references \insertRef{hanssAppliedFuzzyArithmetic2005}{fcmconfr}
#' @references \insertRef{trillasFuzzyArithmetic2015}{fcmconfr}
#'
#' @param tfn_1 A triangular fuzzy number (tfn) object
#' @param tfn_2 A triangular fuzzy number (tfn) object
#'
#' @returns An TFN object representing the subtraction of tfn_2 from tfn_1
#'
#' @importFrom Rdpack reprompt
#'
#' @export
#' @examples
#' subtract_tfn(tfn(0.5, 0.6, 0.8), tfn(0.2, 0.3, 0.5))
#' subtract_tfn(tfn(-0.5, -0.2, 0.3), tfn(0.4, 0.5, 0.6))
#' subtract_tfn(tfn(-1, 0, 1), tfn(-0.5, 0, 0.5))
subtract_tfn <- function(tfn_1 = tfn(), tfn_2 = tfn()) {
  if ((!identical(methods::is(tfn_1), "tfn")) | (!identical(methods::is(tfn_2), "tfn"))) {
    stop("Input Error: Both inputs must be valid tfn objects")
  }

  new_lower <- tfn_1$lower - tfn_2$upper
  new_mode <- tfn_1$mode - tfn_2$mode
  new_upper <- tfn_1$upper - tfn_2$lower
  tfn(new_lower, new_mode, new_upper)
}



#' Print a Triangular Fuzzy Number (TFN)
#'
#' @family triangular-fuzzy-numbers
#'
#' @description
#' This improves the readability of the output
#'
#' @description
#' This prints a tfn object
#'
#' @param x a tfn object
#' @param ... additional inputs
#'
#' @returns A console printout of a TFN object
#'
#' @export
#' @examples
#' tfn(-1, 0, 1)
print.tfn <- function(x, ...) {
  cat(class(x), ": [", x$lower, ", ", x$mode, ", ", x$upper, "]", sep = "")
}


#' c.tfn
#'
#' @family triangular-fuzzy-numbers@family triangular-fuzzy-numbers
#'
#' @description
#' This forces the output of c() to the equivalent of list() only for inputs of
#' type tfn
#'
#' @details
#' For tfn objects, c() combines all of the lower and upper data into
#' a single tfn object, but list() returns the expected output of a
#' list of distinct tfn objects.
#'
#' @param ... a set of tfn objects
#'
#' @returns a list of tfn objects
#'
#' @export
#' @examples
#' c(tfn(0, 1), tfn(0.2, 0.5))
c.tfn <- function(...) {
  list(...)
}



#' The Triangular Distribution
#'
#' @description
#' This function generates a triangular distribution on the interval from lower
#' to upper bounds with a a given mode,
#'
#' @param n number of samples to draw from the triangular distribution
#' @param lower lower limit or minimum of the sample space
#' @param upper upper limit or maximum of the sample space
#' @param mode peak of the sample space
#'
#' @details
#' Additional details...
#'
#' @returns a set c() of values representing a triangular distribution
#'
#' @export
#' @example  man/examples/ex-rtriangular_dist.R
rtriangular_dist <- function(n = integer(), lower = double(), mode = double(), upper = double()) {
  # Confirm n is an integer
  if (!is.numeric(n)) {
    stop("Input Error: n must be an integer")
  }
  if (is.numeric(n) & (n %% 2 != 0)) {
    stop("Input Error: n must be an integer")
  }

  # Confirm lower <= mode <= upper
  tfn(lower, mode, upper)

  if (identical(mode, double())) {
    mode <- (lower + upper)/2
  }

  # browser()

  if (lower == upper) {
    midpoint_domain <- 0
  } else {
    midpoint_domain <- (mode - lower)/(upper - lower)
  }

  inv_cdf <- vector(mode = "numeric", length = n)
  for (i in 1:n) {
    x <- i/n
    if (x <= midpoint_domain) {
      inv_cdf[i] <- lower + sqrt((mode - lower)*(upper - lower)*x)
    } else if (x > midpoint_domain) {
      inv_cdf[i] <- upper - sqrt((upper - lower)*(upper - mode)*(1 - x))
    }
  }
  values_distribution <- inv_cdf

  structure(
    .Data = values_distribution,
    .label = paste0("rtriangular_dist(", n, ", ", lower, ", ", mode, ", ", upper, ")"),
    class = "rtriangular_dist"
  )
}


#' plot.rtriangular_dist
#'
#' @description
#' Plot rtriangular_dist distribution similar to how runif is plotted with the base plot function
#'
#' @param x an rtriangular_dist object
#' @param ... additional inputs (leave empty)
#'
#' @returns A plot of the triangular distribution generated by rtriangular_dist
#' (in the vain of plot(runif))
#'
#' @export
#' @examples
#' plot(rtriangular_dist(1000, -1, 0, 1))
plot.rtriangular_dist <- function(x, ...) {
  index <- sample(1:length(x), length(x), replace = FALSE)
  plot(x = index, y = x, xlab = "Index", ylab = attr(x, ".label"))
}
