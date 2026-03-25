#' Plot permutation distribution and observed statistic
#'
#' @description Draws a histogram of the permutation statistics with a vertical
#' line at the observed value. This is primarily used in the vignette.
#'
#' @param t_perm Numeric vector of permutation statistics.
#' @param t_obs Numeric observed test statistic.
#' @param breaks Number of histogram breaks. Default is 30.
#' @return Invisibly returns `NULL`.
#' @examples
#' set.seed(1)
#' vals <- rnorm(1000)
#' plot_permutation_distribution(vals, t_obs = 0.8)
#' @importFrom graphics hist abline
#' @export
plot_permutation_distribution <- function(t_perm, t_obs, breaks = 30) {
  hist(
    t_perm,
    breaks = breaks,
    main = "Permutation Distribution",
    xlab = "Permuted statistic"
  )
  abline(v = t_obs, lwd = 2)
  invisible(NULL)
}
