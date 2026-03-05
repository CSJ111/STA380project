#' Load and preprocess the CODE-15% ECG dataset
#'
#' @description Reads the exams.csv metadata file from the CODE-15% ECG dataset
#' and returns a data frame with relevant columns only
#' ("patient_id", "exam_id", "age", "nn_predicted_age", "AF").
#'
#' @param path a character string specifying the file path to exams.csv.
#' @return a data frame containing columns: patient_id, exam_id, age,
#' nn_predicted_age, AF.
#' @examples
#' # Create a small example CSV to demonstrate
#' tmp <- tempfile(fileext = ".csv")
#' write.csv(data.frame(
#'   patient_id = 1:5,
#'   exam_id = 101:105,
#'   age = c(50, 60, 70, 55, 65),
#'   nn_predicted_age = c(52, 63, 68, 58, 70),
#'   AF = c(TRUE, FALSE, TRUE, FALSE, TRUE),
#'   extra_col = 1:5
#' ), tmp, row.names = FALSE)
#' df <- load_ecg_data(tmp)
#' head(df)
#' @importFrom utils read.csv
#' @export
load_ecg_data <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  cols_needed <- c("patient_id", "exam_id", "age",
                   "nn_predicted_age", "AF")
  df <- df[, cols_needed]
  return(df)
}
