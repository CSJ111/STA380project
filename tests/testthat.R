library(testthat)
library(STA380project)

# ============================================================
# Files:
#   - load_ecg_data.R
#   - filter_to_unique_patient.R
#   - compute_age_gap.R
#   - run_permutation_test.R
# Use AI to generate test cases
# Functions name changed, some minor changes with AI
# ============================================================

# ------------------------------------------------------------
# load_ecg_data() tests
# ------------------------------------------------------------

test_that("load_ecg_data returns only required columns and preserves rows", {
  tmp <- tempfile(fileext = ".csv")

  raw <- data.frame(
    patient_id = c(1, 1, 2),
    exam_id = c(101, 102, 201),
    age = c(50, 51, 60),
    nn_predicted_age = c(52, 53, 62),
    AF = c(TRUE, TRUE, FALSE),
    extra_col = c("drop", "drop", "drop")
  )

  write.csv(raw, tmp, row.names = FALSE)

  df <- load_ecg_data(tmp)

  expect_equal(names(df), c("patient_id", "exam_id", "age", "nn_predicted_age", "AF"))
  expect_equal(nrow(df), nrow(raw))
})

# ------------------------------------------------------------
# filter_to_unique_patient() tests
# ------------------------------------------------------------

test_that("filter_to_unique_patient(method='latest') keeps one row per patient", {
  df <- data.frame(
    patient_id = c(1, 1, 2, 2, 3),
    exam_id = c(101, 102, 201, 202, 301),
    age = c(50, 51, 60, 61, 70),
    nn_predicted_age = c(52, 53, 62, 63, 72),
    AF = c(TRUE, TRUE, FALSE, FALSE, TRUE)
  )

  out <- filter_to_unique_patient(df, method = "latest")

  # One row per patient
  expect_equal(nrow(out), length(unique(df$patient_id)))
  expect_equal(length(unique(out$patient_id)), length(unique(df$patient_id)))

  # "latest" keeps the row with the largest exam_id per patient
  expect_equal(out$exam_id[out$patient_id == 1], 102)
  expect_equal(out$exam_id[out$patient_id == 2], 202)
  expect_equal(out$exam_id[out$patient_id == 3], 301)
})

test_that("filter_to_unique_patient(method='random') returns one row per patient and is reproducible given seed", {
  df <- data.frame(
    patient_id = rep(1:50, each = 3),
    exam_id = as.integer(rep(1:50, each = 3) * 100 + rep(1:3, times = 50)),
    age = rep(sample(20:80, 50, replace = TRUE), each = 3),
    nn_predicted_age = rep(sample(20:80, 50, replace = TRUE), each = 3),
    AF = rep(sample(c(TRUE, FALSE), 50, replace = TRUE), each = 3)
  )

  out1 <- filter_to_unique_patient(df, method = "random", seed = 7)
  out2 <- filter_to_unique_patient(df, method = "random", seed = 7)

  expect_equal(nrow(out1), length(unique(df$patient_id)))
  expect_equal(nrow(out2), length(unique(df$patient_id)))

  # Same seed => identical selection
  expect_identical(out1, out2)
})

# ------------------------------------------------------------
# compute_age_gap() tests
# ------------------------------------------------------------

test_that("compute_age_gap creates age_gap column with correct values", {
  df <- data.frame(
    age = c(50, 60, 70),
    nn_predicted_age = c(55, 58, 75),
    patient_id = 1:3,
    exam_id = 101:103,
    AF = c(TRUE, FALSE, TRUE)
  )

  out <- compute_age_gap(df)

  expect_true("age_gap" %in% names(out))
  expect_equal(out$age_gap, c(5, -2, 5))
})

# ------------------------------------------------------------
# run_permutation_test() tests
# ------------------------------------------------------------

test_that("run_permutation_test returns expected structure and sizes", {
  set.seed(1)
  x <- rnorm(50, mean = 1)
  y <- rnorm(60, mean = 0)

  res <- run_permutation_test(x, y, stat = "mean", B = 500, seed = 123)

  expect_true(is.list(res))
  expect_true(all(c("t_obs","t_perm","p_value","n_af","n_nonaf","stat","B") %in% names(res)))

  expect_equal(length(res$t_perm), 500)
  expect_equal(res$n_af, length(x))
  expect_equal(res$n_nonaf, length(y))
  expect_equal(res$stat, "mean")
  expect_equal(res$B, 500)

  expect_true(is.numeric(res$t_obs))
  expect_true(is.numeric(res$t_perm))
  expect_true(is.numeric(res$p_value))
  expect_true(res$p_value >= 0 && res$p_value <= 1)
})

test_that("run_permutation_test(stat) validates choices via match.arg()", {
  x <- c(1, 2, 3)
  y <- c(0, 0, 0)

  expect_error(run_permutation_test(x, y, stat = "means", B = 10, seed = 1))
  expect_error(run_permutation_test(x, y, stat = "KS",    B = 10, seed = 1))

  expect_no_error(run_permutation_test(x, y, stat = "mean",   B = 10, seed = 1))
  expect_no_error(run_permutation_test(x, y, stat = "median", B = 10, seed = 1))
  expect_no_error(run_permutation_test(x, y, stat = "ks",     B = 10, seed = 1))
})

test_that("run_permutation_test is reproducible given seed argument", {
  set.seed(9)
  x <- rnorm(40, mean = 0.5)
  y <- rnorm(40, mean = 0)

  r1 <- run_permutation_test(x, y, stat = "mean", B = 300, seed = 777)
  r2 <- run_permutation_test(x, y, stat = "mean", B = 300, seed = 777)

  expect_identical(r1$t_perm, r2$t_perm)
  expect_identical(r1$p_value, r2$p_value)
  expect_identical(r1$t_obs, r2$t_obs)
})
