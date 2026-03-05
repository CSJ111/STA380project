#' Compute the ECG-derived Age Gap
#'
#' @description Adds a new column \code{age_gap} to the data frame, defined as
#' \deqn{g = \texttt{nn\_predicted\_age} - \texttt{age}.}
#'
#' @param df a data frame containing numeric columns \code{nn_predicted_age}
#' and \code{age}.
#' @return the same data frame with one extra column \code{age_gap}.
#' @examples
#' df <- data.frame(
#'   age = c(50, 60, 70),
#'   nn_predicted_age = c(55, 58, 75)
#' )
#' df <- compute_age_gap(df)
#' df$age_gap
#' @export
compute_age_gap <- function(df) {
  df$age_gap <- df$nn_predicted_age - df$age
  return(df)
}
