% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-general.R
\name{get_node_IDs_from_input}
\alias{get_node_IDs_from_input}
\title{Get Node IDs (Concepts) from Adj. Matrix}
\usage{
get_node_IDs_from_input(adj_matrix = matrix())
}
\arguments{
\item{adj_matrix}{An n x n adjacency matrix that represents an FCM}
}
\value{
A vector of concept names
}
\description{
Get the column names of an adjacency matrix as the names of concepts. If no
column names are given, use generic column names (C1, C2, ..., Cn)

Intended for developer use only to improve package readability.
}
\examples{
adj_matrix <- data.frame(
  "A" = c(0, 0, 0.3, 0),
  "B" = c(0.5, 0, 0, 0.6),
  "C" = c(0, 0, 0, 0),
  "D" = c(1, 0, 1, 0)
)
get_node_IDs_from_input(adj_matrix)
}
\seealso{
Other utility: 
\code{\link{check_if_local_machine_has_access_to_parallel_processing_functionalities}()},
\code{\link{check_if_local_machine_has_access_to_show_progress_functionalities}()},
\code{\link{fcm_view}()},
\code{\link{get_adj_matrices_input_type}()},
\code{\link{get_edgelist_from_adj_matrix}()},
\code{\link{get_inferences}()},
\code{\link{standardize_adj_matrices}()}
}
\concept{utility}
