#' Filter to only one exam recorded per patient
#'
#' @description Addresses the repeated measures / non-independence problem by
#' keeping only one exam recorded per patient. Because the dataset may contain
#' multiple exams for the same patient (the patient_id), using all rows would
#' violate the independence assumption required by the permutation test.
#'
#' @param df a data frame returned by \code{load_ecg_data}.
#' @param method a string specifying which exam to keep per patient.
#' \code{"latest"} (default) keeps the exam with the numerically largest
#' exam_id; \code{"random"} keeps a randomly selected exam.
#' @param seed an integer random seed used only when \code{method = "random"}.
#' Default is 6.
#' @return a data frame with exactly one row per unique patient_id.
#' @examples
#' df <- data.frame(
#'   patient_id = c(1, 1, 2, 2, 3),
#'   exam_id = c(101, 102, 201, 202, 301),
#'   age = c(50, 51, 60, 61, 70),
#'   nn_predicted_age = c(52, 53, 62, 63, 72),
#'   AF = c(TRUE, TRUE, FALSE, FALSE, TRUE)
#' )
#' df_one <- filter_to_unique_patient(df, method = "latest")
#' nrow(df_one) == length(unique(df$patient_id))
#' @export
filter_to_unique_patient <- function(df, method = "latest", seed = 6) {
  if (method == "latest") {
    df  <- df[order(df$patient_id, -df$exam_id), ] 
    ## Anna: are you sure this was fixed?
    ## I had to modify this to fix your code...
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

# AI Usage: The @examples section was generated with the assistance of
# generative AI (Claude) to replace \dontrun{} with self-contained,
# runnable examples.
