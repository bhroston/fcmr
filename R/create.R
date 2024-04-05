
#' fcmr (fuzzy cognitive map) S3 class
#'
#' @description
#' This class is an organization scheme for ordinary fuzzy cognitive maps (See
#' Kosko, XXXX for example). It stores the nodes of an FCM and its
#' corresponding adjacency matrix and edgelist.
#'
#' @details
#' fcmr stores fcm data in forms useful to data manipulation, particular
#' regarding pairing with popular network analysis libraries like igraph
#' or visNetwork.
#'
#' fcmr are constructed using vctrs::new_vctr instead of the typical call to
#' structure() because ...
#'
#' Use vignette("fcmr-class") for more information.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#'
#' @export
#' @examples
#' fcmr(adj_matrix = matrix(data = c(0, 1, 1, 0), nrow = 2, ncol = 2))
fcmr <- function(adj_matrix = matrix()) {
  # Validate input
  rows <- nrow(adj_matrix)
  cols <- ncol(adj_matrix)
  rowIDs <- rownames(adj_matrix)
  colIDs <- colnames(adj_matrix)
  n_datatypes <- length(typeof(adj_matrix))
  if (rows != cols) {
    stop("Failed Validation: Input adjacency matrix must be a square (n x n) matrix")
  }
  if (n_datatypes != 1) {
    stop("Failed Validation: Input adjacency matrix must only contain numeric objects, and all
         objects must be of the same")
  }

  if (identical(rowIDs, colIDs) & identical(colIDs, NULL)) {
    IDs <- paste0("C", as.character(1:rows))
  } else if (!identical(rowIDs, colIDs) & identical(rowIDs, NULL)) {
    IDs <- colIDs
  } else if (identical(rowIDs, colIDs)) {
    IDs <- colIDs
  } else {
    stop("Failed Validation: Unable to compare rownames and colnames of adjacency matrix")
  }

  vctrs::new_vctr(
    .data = list(
      "edgelist" = get_edgelist_from_adj_matrix(adj_matrix, IDs),
      "adj_matrix" = adj_matrix,
      "concepts" = IDs
    ),
    class = "fcmr"
  )
}



#' fgcmr (fuzzy grey cognitive map) S3 class
#'
#' @description
#' This class is an organization scheme for fuzzy grey cognitive maps (See
#' ____, XXXX for example). It stores the nodes of an FGCM and its
#' corresponding adjacency matrix and edgelist.
#'
#' @details
#' fgcmr stores fgcm data in forms useful to data manipulation, particular
#' regarding pairing with popular network analysis libraries like igraph
#' or visNetwork.
#'
#' fgcmr allow for the depiction of uncertainty in fcm as edge weights are
#' represented as Grey Numbers (___, XXXX). Grey Numbers represent a range of
#' possible values that an edge may be within.
#'
#' fgcmr are constructed using vctrs::new_vctr instead of the typical call to
#' structure() because ...
#'
#' Use vignette("fgcmr-class") for more information.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#'
#' @export
#' @examples
#' NULL
fgcmr <- function(adj_matrix = matrix()) {
  # Validate input
  rowIDs <- rownames(adj_matrix)
  colIDs <- colnames(adj_matrix)
  if (identical(rowIDs, colIDs) & identical(colIDs, NULL)) {
    IDs <- paste0("C", as.character(1:nrow(adj_matrix)))
  } else if (!identical(rowIDs, colIDs) & identical(rowIDs, NULL)) {
    IDs <- colIDs
  } else if (identical(rowIDs, colIDs)) {
    IDs <- colIDs
  } else {
    stop("Failed Validation: Unable to compare rownames and colnames of adjacency matrix")
  }

  vctrs::new_vctr(
    .data = list(
      "edgelist" = get_edgelist_from_adj_matrix(adj_matrix, IDs),
      "adj_matrix" = adj_matrix,
      "concepts" = IDs
    ),
    class = "fgcmr"
  )
}

#'
#'
#' grey_number S3 class
#'
#' @description
#' This class is an organization scheme for Grey Numbers (See
#' ____, XXXX for example).
#'
#' @details
#' Grey Numbers represent intervals within a possibility space (typically
#' [0, 1] or [-1, 1]) that contains the true value of an object.
#'
#' The grey_number class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of grey numbers
#'
#' grey_numbers are constructed using vctrs::new_vctr instead of the typical call to
#' structure() because ...
#'
#' Use vignette("fgcmr-class") for more information.
#'
#' @param lower lower limit of a Grey Number set (the lower value must be less than or equal to the upper value)
#' @param upper upper limit of a Grey Number set (the upper value must be greater or equal to the lower value)
#'
#' @export
#' @examples
#' grey_number(lower = 0, upper = 1)
grey_number <- function(lower = double(), upper = double()) {
  if (identical(lower, double())) {
    lower <- -Inf
  }

  if (identical(upper, double())) {
    upper <- Inf
  }

  if (lower > upper) {
    stop("The lower value must be less than or equal to the upper value", call. = FALSE)
  }

  if ((!is.numeric(lower)) | (!is.numeric(upper))) {
    stop("lower and upper must be single, numeric values", call. = FALSE)
  }

  if ((length(lower) > 1) | (length(upper) > 1)) {
    stop("lower and upper must be single, numeric values", call. = FALSE)
  }

  vctrs::new_vctr(
    .data = list(lower = lower, upper = upper),
    class = "grey_number"
  )

  #structure(
  #  .Data = data.frame(lower = lower, upper = upper),
  #  class = "grey_number"
  #)
}
