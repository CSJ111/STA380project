\name{plot_permutation_distribution}
\alias{plot_permutation_distribution}
\title{Plot Permutation Distribution}
\usage{
plot_permutation_distribution(t_perm, t_obs, breaks = 30)
}
\arguments{
  \item{t_perm}{Numeric vector of permutation statistics.}
  \item{t_obs}{Observed test statistic.}
  \item{breaks}{Histogram break count.}
}
\value{Invisibly returns \code{NULL}.}
\description{Draws a histogram of permutation statistics and marks the observed value.}
\examples{
set.seed(1)
plot_permutation_distribution(rnorm(100), t_obs = 0.5)
}
