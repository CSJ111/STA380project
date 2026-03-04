library(testthat)

# ============================================================
# Test file: acceptance-rejection generators (Laplace + Logistic)
# Note: this file has not change the function name!!!
# ============================================================

# ------------------------------
# Laplace distribution tests
# Target: Laplace(location = 0, scale = b)
# Theoretical: E[X] = 0, Var[X] = 2 b^2
# ------------------------------

test_that("Laplace generator returns a numeric vector of the correct length", {
  n <- 1000
  x <- MISSING_laplace_ar(n, b = 1)
  
  expect_true(is.numeric(x))
  expect_equal(length(x), n)
  expect_false(anyNA(x))
})

test_that("Laplace generator has approximately correct mean (location 0)", {
  set.seed(1)
  n <- 20000
  b <- 1
  x <- MISSING_laplace_ar(n, b = b)
  
  # Mean should be close to 0 for a symmetric Laplace
  expect_lt(abs(mean(x)), 0.10)
})

test_that("Laplace generator has approximately correct variance", {
  set.seed(2)
  n <- 30000
  b <- 1
  x <- MISSING_laplace_ar(n, b = b)
  
  theoretical_var <- 2 * b^2
  expect_lt(abs(var(x) - theoretical_var), 0.30)
})

test_that("Laplace generator scales correctly with b (variance scales with b^2)", {
  set.seed(3)
  n <- 30000
  b1 <- 1
  b2 <- 2
  
  x1 <- MISSING_laplace_ar(n, b = b1)
  x2 <- MISSING_laplace_ar(n, b = b2)
  
  v1 <- var(x1)
  v2 <- var(x2)
  
  # Var should scale by (b2/b1)^2 = 4
  expect_lt(abs((v2 / v1) - (b2 / b1)^2), 0.25)
})


# ------------------------------
# Logistic distribution tests
# Target: Logistic(location = 0, scale = 1) by default
# Theoretical: E[X] = 0, Var[X] = pi^2 / 3  (for scale = 1)
# ------------------------------

test_that("Logistic generator returns a numeric vector of the correct length", {
  n <- 1000
  x <- MISSING_logistic_ar(n)
  
  expect_true(is.numeric(x))
  expect_equal(length(x), n)
  expect_false(anyNA(x))
})

test_that("Logistic generator has approximately correct mean (location 0)", {
  set.seed(4)
  n <- 20000
  x <- MISSING_logistic_ar(n)
  
  expect_lt(abs(mean(x)), 0.10)
})

test_that("Logistic generator has approximately correct variance (scale 1)", {
  set.seed(5)
  n <- 30000
  x <- MISSING_logistic_ar(n)
  
  theoretical_var <- (pi^2) / 3
  expect_lt(abs(var(x) - theoretical_var), 0.50)
})


# ============================================================
# Documentation / examples checks (per instructor suggestion)
# ------------------------------------------------------------
# Roxygen @examples should be runnable so that example() works.
# These tests encourage keeping @examples executable (avoid \\dontrun{}).
# ============================================================

test_that("Roxygen examples for Laplace function run without error", {
  expect_error(example("MISSING_laplace_ar", package = "MISSINGPACKAGENAME"), NA)
})

test_that("Roxygen examples for Logistic function run without error", {
  expect_error(example("MISSING_logistic_ar", package = "MISSINGPACKAGENAME"), NA)
})


# ============================================================
# Minimal RNG reproducibility
# ------------------------------------------------------------
# Same seed -> same output. Not a core requirement, but harmless sanity check.
# ============================================================

test_that("Laplace generator is reproducible under a fixed seed", {
  set.seed(1)
  x1 <- MISSING_laplace_ar(2000, b = 1)
  
  set.seed(1)
  x2 <- MISSING_laplace_ar(2000, b = 1)
  
  expect_identical(x1, x2)
})