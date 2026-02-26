library(testthat)

# ============================================================
# Load Project Functions
# ------------------------------------------------------------
# Source the R file containing all user-defined functions.
# This allows the testing environment to access:
#   - filter_to_unique_patient()
#   - compute_age_gap()
#   - run_permutation_test()
# ============================================================

source("functions.R")


# ============================================================
# Generate Synthetic Test Dataset
# ------------------------------------------------------------
# A small artificial dataset is constructed to:
#   - Ensure fast execution
#   - Avoid dependence on external files
#   - Provide controlled structure for reproducible testing
# ============================================================

set.seed(1)

n_patients <- 200
exams_per_patient <- sample(1:3, n_patients, replace = TRUE)
N <- sum(exams_per_patient)

test_df <- data.frame(
  patient_id = rep(seq_len(n_patients), times = exams_per_patient),
  exam_id = sample(100000:5000000, N, replace = FALSE),
  age = sample(18:95, N, replace = TRUE),
  nn_predicted_age = round(rnorm(N, mean = 55, sd = 12), 5),
  AF = sample(c(TRUE, FALSE), N, replace = TRUE, prob = c(0.15, 0.85))
)


# ============================================================
# Test: compute_age_gap()
# ------------------------------------------------------------
# Verify that:
#   - The function adds a new column named "age_gap"
#   - The computed values equal nn_predicted_age - age
# ============================================================

test_that("compute_age_gap correctly creates age_gap variable", {
  
  df2 <- compute_age_gap(test_df)
  
  expect_true("age_gap" %in% names(df2))
  
  expect_equal(
    df2$age_gap,
    df2$nn_predicted_age - df2$age
  )
})


# ============================================================
# Test: filter_to_unique_patient() — latest method
# ------------------------------------------------------------
# Verify that:
#   - Exactly one row per patient_id is returned
# ============================================================

test_that("filter_to_unique_patient returns one row per patient (latest)", {
  
  df_latest <- filter_to_unique_patient(
    test_df,
    method = "latest"
  )
  
  expect_equal(
    nrow(df_latest),
    length(unique(test_df$patient_id))
  )
})


# ============================================================
# Test: filter_to_unique_patient() — random method reproducibility
# ------------------------------------------------------------
# Verify that:
#   - Using the same random seed produces identical output
# ============================================================

test_that("filter_to_unique_patient random method is reproducible", {
  
  df_rand1 <- filter_to_unique_patient(
    test_df,
    method = "random",
    seed   = 99
  )
  
  df_rand2 <- filter_to_unique_patient(
    test_df,
    method = "random",
    seed   = 99
  )
  
  expect_equal(df_rand1, df_rand2)
})


# ============================================================
# Test: run_permutation_test() structure and validity
# ------------------------------------------------------------
# Verify that:
#   - Output is a list
#   - Required elements are present
#   - t_perm has correct length
#   - p_value lies within [0, 1]
# ============================================================

test_that("run_permutation_test returns correct structure and valid values", {
  
  df2 <- compute_age_gap(test_df)
  
  x <- df2$age_gap[df2$AF == TRUE]
  y <- df2$age_gap[df2$AF == FALSE]
  
  result <- run_permutation_test(
    x, y,
    stat = "mean",
    B    = 200,
    seed = 1
  )
  
  expect_true(is.list(result))
  
  expect_true(all(c(
    "t_obs", "t_perm", "p_value",
    "n_af", "n_nonaf",
    "stat", "B"
  ) %in% names(result)))
  
  expect_equal(length(result$t_perm), 200)
  
  expect_true(result$p_value >= 0)
  expect_true(result$p_value <= 1)
  
  expect_equal(result$n_af,    length(x))
  expect_equal(result$n_nonaf, length(y))
  expect_equal(result$stat,    "mean")
  expect_equal(result$B,       200)
})


# ============================================================
# Test: run_permutation_test() error handling
# ------------------------------------------------------------
# Verify that:
#   - Invalid statistic input produces an error
# ============================================================

test_that("run_permutation_test rejects invalid stat argument", {
  
  df2 <- compute_age_gap(test_df)
  
  x <- df2$age_gap[df2$AF == TRUE]
  y <- df2$age_gap[df2$AF == FALSE]
  
  expect_error(
    run_permutation_test(x, y, stat = "invalid")#,
    #"choose one of stats"
  )
  
  # below should be valid, match.arg helps you.
  expect_no_error(
    run_permutation_test(x, y, stat = "media")
  )
})
