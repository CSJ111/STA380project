#' Load and preprocess the CODE-15% ECG dataset
#' @description Reads the exams.csv metadata file from the CODE-15% ECG dataset
#' and returns a data with relevant columns only("patient_id", "exam_id","age", "nn_predicted_age", "AF").
#' @param path a character string specifying the file path to exams.csv.
#' @return a data frame containing columns: patient_id, exam_id, age,
#' nn_predicted_age, AF.
#' @examples
#' \dontrun{
#' df <- load_ecg_data("data/exams.csv")
#' }
#' @importFrom
#' @export
load_ecg_data <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  cols_needed <- c("patient_id", "exam_id", "age",
                   "nn_predicted_age", "AF")
  df <- df[, cols_needed]
    return(df)
}


#' Filter to only one exam recorded per patient
#' @description Addresses the repeated measures / non-independence problem by
#' keeping only one exam recorded per patient. Because the dataset may contain multiple
#' exams for the same patient(the patient_id), using all rows would violate the independence
#' assumption required by the permutation test.
#' @param df a data frame returned by \code{load_ecg_data}
#' @param method a string specifying which exam to keep per patient.
#' \code{"latest"} (default) keeps the exam with the numerically largest
#' exam_id; \code{"random"} keeps a randomly selected exam.
#' @param seed an integer random seed used only when \code{method = "random"}.
#' Default is 6.
#' @return a data frame with exactly one row per unique patient_id.
#' @examples
#' \dontrun{
#' df      <- load_ecg_data("data/exams.csv")
#' df_one  <- filter_to_unique_patient(df, method = "latest")
#' # number of rows should equal number of unique patients
#' nrow(df_one) == length(unique(df$patient_id))
#' }
#' @export
filter_to_unique_patient <- function(df, method = "latest", seed = 6) {

  if (method == "latest") {
    df  <- df[order(df$patient_id, df$exam_id, decreasing = c(FALSE, TRUE)), ]
    df  <- df[!duplicated(df$patient_id), ]
  } else if (method == "random") {
    set.seed(seed)
    patient_list <- split(df, df$patient_id)
    df <- do.call(rbind, lapply(patient_list, function(one_patient) {
      one_patient[sample(nrow(one_patient), size = 1), ]
    }))

  } 
  rownames(df) <- NULL
  return(df)
}


#' Compute the ECG-derived Age Gap
#' @description Adds a new column \code{age_gap} to the data frame, defined as
#' \deqn{g = \texttt{nn\_predicted\_age} - \texttt{age}.}
#' @param df a data frame containing numeric columns \code{nn_predicted_age}
#' and \code{age}.
#' @return the same data frame with one extra column \code{age_gap}.
#' @examples
#' \dontrun{
#' df <- load_ecg_data("data/exams.csv")
#' df <- filter_one_per_patient(df)
#' df <- compute_age_gap(df)
#' summary(df$age_gap)
#' }
#' @export
compute_age_gap <- function(df) {
  df$age_gap <- df$nn_predicted_age - df$age
  return(df)
}


#' Run a two-sample permutation test
#' @description Performs a two-sided permutation test comparing two independent
#' groups on a continuous outcome. Group labels are permuted B times; the
#' p-value is the proportion of permuted statistics at least as extreme as
#' the observed statistic.
#' @param x a numeric vector of outcome values for group 1.
#'   For this project, code{df$age_gap[df$AF == TRUE]}.
#' @param y a numeric vector of outcome values for group 2.
#'   For this project, \code{df$age_gap[df$AF == FALSE]}.
#' @param stat a character string choosing the test statistic:
#' \describe{
#'   \item{\code{"mean"}}{mean difference: \eqn{\bar x - \bar y} (default)}
#'   \item{\code{"median"}}{median difference: \eqn{\mathrm{med}(x) - \mathrm{med}(y)}}
#'   \item{\code{"ks"}}{Kolmogorov-Smirnov statistic: \eqn{\sup_t|F_x(t)-F_y(t)|}}
#' }
#' @param B a positive integer: number of permutations. Default 10000.
#' @param seed an integer random seed for reproducibility. Default 6.
#'   a seed guarantees the same permutation sequence and therefore the same p-value each time.
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
#' x <- rnorm(300, mean =  2)   # simulated AF group:     age_gap shifted up
#' y <- rnorm(500, mean =  0)   # simulated non-AF group: age_gap centered at 0
#' result <- run_permutation_test(x, y, stat = "mean", B = 1000, seed = 1)
#' result$t_obs    # should be ~2
#' result$p_value  # should be very small -> reject H0
#' result2 <- run_permutation_test(x, y, stat = "median", B = 1000, seed = 1)
#' result3 <- run_permutation_test(x, y, stat = "ks",     B = 1000, seed = 1)
#' @importFrom stats median ks.test
#' @export
run_permutation_test <- function(x, y,
                                 stat = "mean",
                                 B    = 10000,
                                 seed = 42) {

  if (!stat %in% c("mean", "median", "ks")) {
    stop("choose one of stats: 'mean', 'median', 'ks'")
  }
  calc_stat <- function(a, b) {
    if (stat == "mean")   return(mean(a)   - mean(b))
    if (stat == "median") return(median(a) - median(b))
    if (stat == "ks")     return(ks.test(a, b)$statistic)
  }

  t_obs    <- calc_stat(x, y)
  n1       <- length(x)
  n2       <- length(y)
  combined <- c(x, y)          

#start permutation
  set.seed(seed)
  t_perm <- numeric(B)

  for (b in seq_len(B)) {
    shuffled    <- sample(combined) # randomly permutate all values
    x_perm      <- shuffled[1:n1]  # first n1 become "group 1"
    y_perm      <- shuffled[(n1+1):(n1+n2)]  # remaining become "group 2"
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
