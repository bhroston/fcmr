% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-general.R
\name{standardize_adj_matrices}
\alias{standardize_adj_matrices}
\title{Standardize a List of Adjacency Matrices}
\usage{
standardize_adj_matrices(adj_matrices = list(matrix()))
}
\arguments{
\item{adj_matrices}{A list of adj. matrix objects}
}
\value{
A list of adj. matrices constructed from the input adj. matrices,
that contain the same concepts (and dimensions). Rows/Columns of added
concepts are all 0's.
}
\description{
Given a list of adj. matrices of different sizes, transforms the inputs into
adj. matrices of the same size with 0 values added where edges are undefined
or not included in maps.

Solves the problem where Map 1 has nodes A and B but Map 2 has nodes B and C
and need to analyse them both together.
}
\examples{
adj_matrix_1 <- data.frame(
  "A" = c(0, 0),
  "B" = c(1, 0)
)
adj_matrix_2 <- data.frame(
  "A" = c(0, 0, 0),
  "B" = c(0.25, 0, 1),
  "C" = c(0, 0.7, 0)
)
adj_matrix_3 <- data.frame(
  "B" = c(0, 0),
  "D" = c(0.75, 0)
)
adj_matrix_4 <- data.frame(
  "A" = c(0, 0, 0.3, 0),
  "B" = c(0.5, 0, 0, 0.6),
  "E" = c(0, 0, 0, 0),
  "F" = c(1, 0, 1, 0)
)
adj_matrices_w_different_concepts <- list(
  adj_matrix_1, adj_matrix_2, adj_matrix_3, adj_matrix_4
)
standardize_adj_matrices(adj_matrices_w_different_concepts)

}
\seealso{
Other utility: 
\code{\link{check_if_local_machine_has_access_to_parallel_processing_functionalities}()},
\code{\link{check_if_local_machine_has_access_to_show_progress_functionalities}()},
\code{\link{fcm_view}()},
\code{\link{get_adj_matrices_input_type}()},
\code{\link{get_edgelist_from_adj_matrix}()},
\code{\link{get_inferences}()},
\code{\link{get_node_IDs_from_input}()}
}
\concept{utility}
