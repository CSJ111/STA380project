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

  } else {
    stop("method must be 'latest' or 'random'")
  }

  rownames(df) <- NULL
  return(df)
}
