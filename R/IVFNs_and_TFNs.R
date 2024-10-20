

# INTERVAL-VALUED FUZZY NUMBERS ----


#' Create Adj. Matrix w/ Edges Represented as IVFNs
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
#' @example  man/examples/examples-IVFNs_and_TFNs/examples-IVFNs_and_TFNs-make_adj_matrix_w_ivfns.R
make_adj_matrix_w_ivfns <- function(lower = matrix(), upper = matrix()) {
  if (!identical(dim(lower), dim(upper))) {
    stop("Failed Validation: Input adjacency matrices must be the same size")
  }

  if (nrow(lower) != ncol(lower) | nrow(upper) != ncol(upper)) {
    stop("Failed Validation: Input adjacency matrices must be square matrices (n x n)")
  } else {
    size <- nrow(lower)
  }

  if (identical(colnames(lower), colnames(upper)) & !identical(colnames(lower), NULL)) {
    IDs <- colnames(lower)
  } else {
    IDs <- paste0("C", 1:nrow(lower))
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
#' @example  man/examples/examples-IVFNs_and_TFNs/examples-IVFNs_and_TFNs-ivfn.R
ivfn <- function(lower = double(), upper = double()) {
  if (identical(lower, double())) {
    lower <- -Inf
  }

  if (identical(upper, double())) {
    upper <- Inf
  }

  if ((!is.numeric(lower)) | (!is.numeric(upper))) {
    stop("lower and upper must be single, numeric values", call. = FALSE)
  }

  if (lower > upper) {
    stop("The lower input must be less than or equal to the upper input", call. = FALSE)
  }

  structure(
    .Data = data.frame(lower = lower, upper = upper),
    class = "ivfn"
  )
}



#' Print an Interval-Valued Fuzzy Number (IVFN) - S3 Class
#'
#' @description
#' This prints an ivfn object
#'
#' @param x a ivfn object
#' @param ... additional inputs
#'
#' @returns NULL
#'
#' @export
print.ivfn <- function(x, ...) {
  cat(class(x), ": [", x$lower, ", ", x$upper, "]", sep = "")
}


#' c.ivfn
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
#' @example  man/examples/examples-IVFNs_and_TFNs/examples-IVFNs_and_TFNs-make_adj_matrix_w_tfns.R
make_adj_matrix_w_tfns <- function(lower = matrix(),
                                   mode = matrix(),
                                   upper = matrix()) {
  if (!identical(dim(lower), dim(mode), dim(upper))) {
    stop("Failed Validation: Input adjacency matrices must be the same size")
  }

  if (nrow(lower) != ncol(lower) | nrow(mode) != ncol(mode) | nrow(upper) != ncol(upper)) {
    stop("Failed Validation: Input adjacency matrices must be square matrices (n x n)")
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
      lower = apply(offense_locs, 1,function(locs) lower[locs[1], locs[2]]),
      mode = apply(offense_locs, 1,function(locs) mode[locs[1], locs[2]]),
      upper = apply(offense_locs, 1,function(locs) upper[locs[1], locs[2]])
    )
    rownames(offenses_df) <- NULL
    writeLines("\n\nERROR: Failed to create triangular adj. matrix from input.\nCheck:")
    print(offenses_df)
    stop(
      "All lower values must be less than or equal to mode values which in turn, \n  must be less than or equal to upper values."
    )
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
#' @example  man/examples/examples-IVFNs_and_TFNs/examples-IVFNs_and_TFNs-tfn.R
tfn <- function(lower = double(), mode = double(), upper = double()) {
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
    stop("lower, mode, and upper must be single, numeric values", call. = FALSE)
  }

  if (lower > upper | lower > mode) {
    stop("The lower input must be less than or equal to both the mode and upper inputs", call. = FALSE)
  }

  if (mode > upper) {
    stop("The mode input must be less than or equal to the upper input", call. = FALSE)
  }

  structure(
    .Data = data.frame(lower = lower, mode = mode, upper = upper),
    class = "tfn"
  )
}


#' Print a Triangular Fuzzy Number (TFN)
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
#' @returns NULL
#'
#' @export
print.tfn <- function(x, ...) {
  cat(class(x), ": [", x$lower, ", ", x$mode, ", ", x$upper, "]", sep = "")
}


#' c.tfn
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
#' @usage rtri(n, lower = 0, mode = 0.5, upper = 1)
#'
#' @param n number of samples to draw from the triangular distribution
#' @param lower lower limit or minimum of the sample space
#' @param upper upper limit or maximum of the sample space
#' @param mode peak of the sample space
#'
#' @details
#' Additional details...
#'
#'
#' @returns a set c() of values representing a triangular distribution
#'
#' @export
rtri <- function(n = integer(), lower = double(), mode = double(), upper = double()) {
  if (lower > upper) {
    stop("lower input must be less than upper input")
  }

  if (identical(mode, double())) {
    mode <- (lower + upper)/2
  }

  if (lower >= 0) {
    domain = 1
  } else if (lower < 0) {
    domain = 2
  }

  # Something's wrong with the rtri function, does NOT work with negative values
  browser()
  tri_height <- 2*(upper - lower)
  upward_slope <- tri_height/(mode - lower)
  upward_y_intercept <- tri_height - (mode - lower)/2
  # y = m*x + b
  # y = (m/2)x^2 + ((mode - lower)/2)*x
  # y = (m/2)x^2 + bx
  # x = (m/2)y^2 + by
  # (2/m)x = y^2 + (2b/m)y
  # (2/m)x = y(y + (2b/m))

  # x = sqrt((-y + 1)*(base)*(widths))
  # (x^2/(base*width) = -y + 1
  # y = 1 - (x^2/(base*width))

  inv_cdf <- vector(mode = "numeric", length = n)
  for (i in 1:n) {
    x <- i/n
    if (x <= mode) {
      # inv_cdf[i] <- sq
      inv_cdf[i] <- mode + sqrt((-x + 0.25)*(upper - lower)*(mode - lower))
    } else if (x > mode) {
      inv_cdf[i] <- sqrt((-x + 0.25)*(upper - lower)*(upper - mode))
    } else {
      stop("Unknown input")
    }
  }
  values_distribution <- inv_cdf

  structure(
    .Data = values_distribution,
    .label = paste0("rtri(", n, ", ", lower, ", ", mode, ", ", upper, ")"),
    class = "rtri"
  )
}


#' plot.rtri
#'
#' @description
#' Plot rtri distribution similar to how runif is plotted with the base plot function
#'
#' @param x an rtri object
#' @param ... additional inputs (leave empty)
#'
#' @export
plot.rtri <- function(x, ...) {
  index <- sample(1:length(x), length(x), replace = FALSE)
  plot(x = index, y = x, xlab = "Index", ylab = attr(x, ".label"))
}
