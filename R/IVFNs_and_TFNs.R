

# INTERVAL-VALUED FUZZY NUMBERS ----


#' make_adj_matrix_w_ivfns
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
#' @export
#' @examples
#' make_adj_matrix_w_ivfns(
#'  lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
#'  upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
#' )
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


#' ivfn S3 class
#'
#' @description
#' This class is an organization scheme for Grey Numbers (See
#' ____, XXXX for example).
#'
#' @details
#' Interval Value Fuzzy Numbers (IVFNs) represent intervals within a possibility space (typically
#' [0, 1] or [-1, 1]) that contains the true value of an object.
#'
#' The IVFN class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of IVFNs
#'
#' @param lower lower limit of an IVFN (the lower value must be less than or equal to the upper value)
#' @param upper upper limit of an IVFN (the upper value must be greater or equal to the lower value)
#'
#' @export
#' @examples
#' ivfn(lower = 0, upper = 1)
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


#' print.ivfn
#'
#' @description
#' This improves the readability of the output
#'
#' @details
#' Grey Numbers represent intervals within a possibility space (typically
#' [0, 1] or [-1, 1]) that contains the true value of an object.
#'
#' The ivfn class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of grey numbers
#'
#' ivfns are constructed using vctrs::new_vctr instead of the typical call to
#' structure() because ...
#'
#' Use vignette("fgcm-class") for more information.
#'
#' @param x a ivfn object
#' @param ... additional inputs
#'
#' @export
#' @examples
#' ivfn(lower = 0, upper = 1)
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

#' make_adj_matrix_w_tfns
#'
#' @description
#' This constructs a triangular adjacency matrix from an adjacency matrix of the lower
#' limits of edges in an FCM, an adjacency matrix of the averages or expected values
#' of edges, and an adjacency matrix of the upper limits of edges in an FCM.
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
#' @export
#' @examples
#' make_adj_matrix_w_tfns(
#'  lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
#'  mode = matrix(data = c(0, 0.3, 0, 0.6), nrow = 2, ncol = 2),
#'  upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
#' )
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



#' tfn S3 class
#'
#' @description
#' This class is an organization scheme for Triangular Numbers (See
#' ____, XXXX for example).
#'
#' @details
#' Triangular Numbers represent intervals within a possibility space (typically
#' [0, 1] or [-1, 1]) that contains the true value of an object.
#'
#' The tfn class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of triangular numbers
#'
#' Use vignette("fcm_w_fcm_w_tfn-class") for more information.
#'
#' @param lower lower limit of a Triangular Number set (the lower value must be less than or equal to the upper value)
#' @param mode the most likely value of a Triangular Number set
#' @param upper upper limit of a Triangular Number set (the upper value must be greater or equal to the lower value)
#'
#' @export
#' @examples
#' tfn(lower = 0, mode = 0.5, upper = 1)
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


#' print.tfn
#'
#' @description
#' This improves the readability of the output
#'
#' @details
#' triangular Numbers represent intervals within a possibility space (typically
#' [0, 1] or [-1, 1]) that contains the true value of an object.
#'
#' The tfn class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of triangular numbers
#'
#' tfns are constructed using vctrs::new_vctr instead of the typical call to
#' structure() because ...
#'
#' Use vignette("fcm_w_fcm_w_tfn-class") for more information.
#'
#' @param x a tfn object
#' @param ... additional inputs
#'
#' @export
#' @examples
#' tfn(lower = 0, upper = 1)
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



#' rtri
#'
#' @description
#' This pulls n samples from a triangular distribution described by shape parameters
#' defined by a lower limit, upper limit, and mode
#'
#' @details
#'
#' Use vignette("fcmcmr-class") for more information.
#'
#' @param lower lower limit or minimum of the sample space
#' @param upper upper limit or maximum of the sample space
#' @param mode peak of the sample space
#' @param n number of samples to draw from the triangular distribution
#'
#' @export
rtri <- function(n = integer(), lower = double(), mode = double(), upper = double()) {
  if (lower > upper) {
    stop("lower input must be less than upper input")
  }

  if (identical(mode, double())) {
    mode <- (lower + upper)/2
  }
  inv_cdf <- vector(mode = "numeric", length = n)
  for (i in 1:n) {
    x <- i/n
    if (x <= mode) {
      inv_cdf[i] <- sqrt(x*(upper - lower)*(mode - lower)) + lower
    } else if (x > mode) {
      inv_cdf[i] <- upper - sqrt((-x + 1)*(upper - lower)*(upper - mode))
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
