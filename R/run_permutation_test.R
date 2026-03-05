#' Run a two-sample permutation test
#'
#' @description Performs a two-sided permutation test comparing two independent
#' groups on a continuous outcome. Group labels are permuted B times; the
#' p-value is the proportion of permuted statistics at least as extreme as
#' the observed statistic.
#'
#' @param x a numeric vector of outcome values for group 1.
#'   For this project, \code{df$age_gap[df$AF == TRUE]}.
#' @param y a numeric vector of outcome values for group 2.
#'   For this project, \code{df$age_gap[df$AF == FALSE]}.
#' @param stat a character string choosing the test statistic:
#' \describe{
#'   \item{\code{"mean"}}{mean difference: \eqn{\bar x - \bar y} (default)}
#'   \item{\code{"median"}}{median difference: \eqn{\mathrm{med}(x) - \mathrm{med}(y)}}
#'   \item{\code{"ks"}}{Kolmogorov-Smirnov statistic: \eqn{\sup_t|F_x(t)-F_y(t)|}}
#' }
#' @param B a positive integer: number of permutations. Default 10000.
#' @param seed an integer random seed for reproducibility. Default 42.
#'   A seed guarantees the same permutation sequence and therefore the same
#'   p-value each time.
#' @return a named list with elements:
#' \describe{
#'   \item{t_obs}{the observed test statistic}
#'   \item{t_perm}{numeric vector of length B: the permuted statistics}
#'   \item{p_value}{the two-sided permutation p-value}
#'   \item{n_af}{number of observations in group 1 (AF)}
#'   \item{n_nonaf}{number of observations in group 2 (non-AF)}
#'   \item{stat}{the name of the test statistic used}
#'   \item{B}{number of permutations performed}
#' }
#' @examples
#' set.seed(99)
#' x <- rnorm(300, mean = 2)   # simulated AF group: age_gap shifted up
#' y <- rnorm(500, mean = 0)   # simulated non-AF group: age_gap centered at 0
#' result <- run_permutation_test(x, y, stat = "mean", B = 1000, seed = 1)
#' result$t_obs
#' result$p_value
#' result2 <- run_permutation_test(x, y, stat = "median", B = 1000, seed = 1)
#' result3 <- run_permutation_test(x, y, stat = "ks", B = 1000, seed = 1)
#' @importFrom stats median ks.test
#' @export
run_permutation_test <- function(x, y,
                                 stat = "mean",
                                 B    = 10000,
                                 seed = 42) {

  stat <- match.arg(stat, c("mean", "median", "ks"))

  calc_stat <- function(a, b) {
    if (stat == "mean")   return(mean(a)   - mean(b))
    if (stat == "median") return(median(a) - median(b))
    if (stat == "ks")     return(ks.test(a, b)$statistic)
  }

  t_obs    <- calc_stat(x, y)
  n1       <- length(x)
  n2       <- length(y)
  combined <- c(x, y)

  # Start permutation
  set.seed(seed)
  t_perm <- numeric(B)

  for (b in seq_len(B)) {
    shuffled    <- sample(combined)
    x_perm      <- shuffled[1:n1]
    y_perm      <- shuffled[(n1 + 1):(n1 + n2)]
    t_perm[b]   <- calc_stat(x_perm, y_perm)
  }

  p_value <- (1 + sum(abs(t_perm) >= abs(t_obs))) / (B + 1)

  return(list(
    t_obs   = t_obs,
    t_perm  = t_perm,
    p_value = p_value,
    n_af    = n1,
    n_nonaf = n2,
    stat    = stat,
    B       = B
  ))
}

# AI Usage: The @examples section was generated with the assistance of
# generative AI (Claude) to replace \dontrun{} with self-contained,
# runnable examples.
