% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-general.R
\name{estimate_fcm_lambda}
\alias{estimate_fcm_lambda}
\title{Estimate lambda}
\usage{
estimate_fcm_lambda(
  fcm_adj_matrix = matrix(),
  squashing = c("sigmoid", "tanh")
)
}
\arguments{
\item{fcm_adj_matrix}{An n x n adjacency matrix that represents an FCM}

\item{squashing}{A squashing function to apply. Must be one of the following: 'tanh', or 'sigmoid'.}
}
\value{
The maximum lambda that ensures simulation convergence for the
input FCM.
}
\description{
This function calculates the largest possible lambda for the sigmoid and
tanh squashing functions that guarantees convergence of an FCM simulation.
Lambda is estimated such that 'squashed' simulation values are contained
within the near-linear region of the sigmoid or tanh functions.

Note: The lambda value estimated will vary for different FCMs.
}
\details{
This algorithm was first explored by Kottas et al. 2010 (https://doi.org/10.1007/978-3-642-03220-2_5),
Harmati et al. 2018 (https://doi.org/10.1109/FUZZ-IEEE.2018.8491447), and
expanded upon by Koutsellis et al. 2022 (https://doi.org/10.1007/s12351-022-00717-x)

The function applies an algorithm that can be used to optimize lambda which
comparing the lambda calculated based on the Frobenius-norm
(\eqn{\lambda^{'}}) and S-norm  (\eqn{\lambda^{*}}) of an
adjacency matrix and selects the minimum.

\deqn{
\lambda _{s} < \min\left( \lambda _{s}^{'} ,\lambda _{s}^{*}\right)
}
\deqn{
\lambda _{h} < \min\left( \lambda _{h}^{'} ,\lambda _{h}^{*}\right)
}

where \eqn{\lambda _{s}} is the lambda calculation when using the sigmoid
squashing function and \eqn{\lambda _{h}} is the lambda calculation when
using tanh.

The equations for \eqn{\lambda ^{'}} were developed by
Harmati et al. 2018 (https://doi.org/10.1109/FUZZ-IEEE.2018.8491447) and are
given below.

\deqn{
\lambda _{s} < \lambda _{s}^{'} =\frac{4}{\| \mathbf{W} \| _{F}}
}

\deqn{
\lambda _{h} < \lambda _{h}^{'} =\frac{1}{\| \mathbf{W} \| _{F}}
}

where \eqn{\| \mathbf{W} \| _{F} \ =\ \sqrt{\sum\limits _{i=1}^{n}\sum\limits _{j=1}^{n}\left( w_{ij}^{2}\right)}}
is the Frobenius norm of the adj. matrix (or \code{norm(x, type = "F")}).

The equations for \eqn{\lambda ^{*}} were developed by
Koutsellis et al. 2022 (https://doi.org/10.1007/s12351-022-00717-x). Unlike
for \eqn{\lambda ^{'}}, the calculations for \eqn{\lambda ^{*}} follow
different steps based on whether calculating \eqn{\lambda} for the sigmoid
or tanh squashing function.

For sigmoid:
\deqn{
|x_{i}^{k} \| _{\max} =\max\left(\left| 0.211\cdot \sum _{i=1}^{p} w_{ij}^{+} +0.789\cdot \sum _{i=1}^{p} w_{ij}^{-}\right| ,\left| 0.211\cdot \sum _{i=1}^{p} w_{ij}^{-} +0.789\cdot \sum _{i=1}^{p} w_{ij}^{+}\right| \right)
}
\deqn{
\| \mathbf{W} \| _{s} =\underset{i}{\max}\left( |x_{i}^{k} \| _{\max}\right)
}
\deqn{
\lambda _{s} < \lambda _{s}^{*} =\frac{1.317}{\| \mathbf{W} \| _{s}}
}

For tanh:
\deqn{
\lambda _{h} < \lambda _{h}^{*} =\frac{1}{\| \mathbf{W} \| _{\infty }}
}
\deqn{
\| \mathbf{W} \| _{\infty } =\underset{i}{\max}\sum _{j=1}^{n}| w_{ij}|
}

Finally, the maximum lambda the ensures convergence of the simulation for
the input adjacency matrix is the minimum of \eqn{\lambda ^{'}} and
\eqn{\lambda ^{*}}.

Note: This is only algorithm included at present, but the code for
\code{estimate_fcm_lambda} is organized to streamline the addition of
new algorithms in the future.
}
\examples{

estimate_fcm_lambda(sample_fcms$simple_fcms$conventional_fcms[[1]], squashing = "sigmoid")

estimate_fcm_lambda(sample_fcms$simple_fcms$conventional_fcms[[1]], squashing = "tanh")

estimate_fcm_lambda(sample_fcms$simple_fcms$tfn_fcms[[1]], squashing = "sigmoid")
}
