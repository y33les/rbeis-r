# =============================================================================
# -------------------------- TESTING TEMPLATE ---------------------------------
# =============================================================================
# - Test type validation on all input parameters
# - Test constraints on input parameters:
# --     impute variable and auxiliary variables are in the tibble
# --     auxiliary variables have no missing values
# --     weights and distance functions are within range
# --     threshold & custom_map are specified for, and only for, appropriate
# --                                                       distance function
# TEST DATA:
# A simple dummy data tibble is created for tests on input parameters
# =============================================================================
#
# TODO:
# Test type validation on custom_map
# Test validity of specified range of possible values
# Test custom_map specified for, and only for, appropriate distance function
# =============================================================================

# Set up dummy_data tibble for testing
dummy_impute_var <- c(30, 20, 30, 40, NA)
dummy_aux_var1 <- c(21, 19, 18, 67, 20)
dummy_aux_var2 <- c(2, 3, 1, 1, 3)
dummy_aux_var_missing <- c(4, 3, 6, NA, 6)
dummy_aux_var_categorical <- c('dog', 'cat', 'parrot', 'giraffe', 'hedgehog')
dummy_data <- tibble::tibble(dummy_impute_var,
                             dummy_aux_var1,
                             dummy_aux_var2,
                             dummy_aux_var_missing,
                             dummy_aux_var_categorical)

# Set up default parameter values
aux_vars <- c("dummy_aux_var1", "dummy_aux_var2")
dfs <- c(1,1)
custom_maps <- c(NA, NA)
thresholds <- c(NA, NA)
weights <- c(2,3)
dummy_aux_vars <- list(aux_vars, dfs, custom_maps, thresholds, weights)


# -----------------------------------------------------------------------------
# TESTS: TYPE VALIDATION ON INPUT PARAMETERS
# -----------------------------------------------------------------------------

test_that("Test that error raised if input data NOT a tibble", {
  expect_error(impute(data = "not_a_tibble",
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Specified value for data must be a tibble.", fixed = TRUE)
})

test_that("Test that error raised if impute variable NOT a string", {
  expect_error(impute(data = dummy_data,
                      imp_var = 7,
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Specified value for imp_var must be a string.", fixed = TRUE)
})

test_that("Test that error raised if possible Values is not vector or is empty", {
  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = matrix(),
                      aux_vars = dummy_aux_vars,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Specified value for possible_vals must be a vector.",
               fixed = TRUE)
  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = vector(),
                      aux_vars = dummy_aux_vars,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Specified possible_vals vector must be non-zero length.",
               fixed = TRUE)
})

test_that("Test that error raised if aux_vars NOT a list", {
  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = "not_a_list",
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Specified value for aux_vars must be a list.", fixed = TRUE)
})

test_that("Test that error raised if auxiliary variables NOT strings", {

  aux_vars_temp <- c(1,2)
  dummy_aux_vars_temp <- list(aux_vars_temp, dfs, custom_maps, thresholds, weights)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_temp,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Specified values for auxiliary variables must be strings.", fixed = TRUE)
})

test_that("Test that error raised if distance function values NOT numeric", {

  dfs_temp <- c("one","two")
  dummy_aux_vars_temp <- list(aux_vars, dfs_temp, custom_maps, thresholds, weights)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_temp,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Specified values for distance functions must be numeric.", fixed = TRUE)
})

# TODO: Test type validation on custom_map to go here
#
#
#

test_that("Test that error raised if threshold values NOT numeric if specified", {

  thresholds_temp <- c("two", NA)
  dummy_aux_vars_temp <- list(aux_vars, dfs, custom_maps, thresholds_temp, weights)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_temp,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Specified values for thresholds must be numeric if specified.", fixed = TRUE)
})

test_that("Test that error raised if weight values NOT numeric.", {

  weights_temp <- c("2","3")
  dummy_aux_vars_temp <- list(aux_vars, dfs, custom_maps, thresholds, weights_temp)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_temp,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Specified values for weights must be numeric.", fixed = TRUE)
})


test_that("Test that error raised if ratio NOT numeric", {
  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars,
                      ratio = "not-Numeric",
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Specified value for ratio must be numeric.", fixed = TRUE)
})

test_that("Test that error raised if in_place NOT Boolean", {
  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars,
                      ratio = 1,
                      in_place = "not_boolean",
                      keep_intermediates = FALSE),
               "Specified value for in_place must be Boolean.", fixed = TRUE)
})

test_that("Test that error raised if keep_intermediates NOT Boolean", {
  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = "not_boolean"),
               "Specified value for keep_intermediates must be Boolean.",
               fixed = TRUE)
})


# -----------------------------------------------------------------------------
# TESTS: CHECK VALUES OF INPUT VARIABLES
# -----------------------------------------------------------------------------

test_that("Test that error raised if impute variable NOT in data tibble", {
  expect_error(impute(data = dummy_data,
                      imp_var = "not_in_tibble",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Specified impute variable must be in the data tibble.",
               fixed = TRUE)
})

# TODO: Tests on validity of range of possible values would go here
#
#
#

test_that("Test that error raised if auxiliary variable NOT in data tibble", {

  aux_vars_temp <- c("not_in_tibble", "dummy_aux_var2")
  dummy_aux_vars_temp <- list(aux_vars_temp, dfs, custom_maps, thresholds, weights)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_temp,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Specified auxiliary variables must be in the data tibble.",
               fixed = TRUE)
})

test_that("Test that error raised if any auxiliary variables have missing values", {

  aux_vars_missing <- c("dummy_aux_var1", "dummy_aux_var_missing")
  dummy_aux_vars_missing <- list(aux_vars_missing, dfs, custom_maps, thresholds, weights)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_missing,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "One or more records are missing the specified auxiliary variables.",
               fixed = TRUE)
})

test_that("Test that error raised if distance functions not between 1 and 6", {

  dfs_temp <- c(-3,-8)
  dummy_aux_vars_temp <- list(aux_vars, dfs_temp, custom_maps, thresholds, weights)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_temp,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Specified values for distance functions must be between 1 and 6.", fixed = TRUE)
})

test_that("Test that error raised if distance functions other than 1 or 4 specified for categorical auxiliary variable", {

  aux_vars_categorical <- c("dummy_aux_var1", "dummy_aux_var_categorical")

  dfs_temp <- c(1,2)
  thresholds_temp <- c(NA, 1)
  dummy_aux_vars_categorical <- list(aux_vars_categorical, dfs_temp, custom_maps, thresholds_temp, weights)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_categorical,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Only distance functions 1 or 4 can be specified for categorical auxiliary variable.",
               fixed = TRUE)

  dfs_temp <- c(1,3)
  thresholds_temp <- c(NA, 1)
  dummy_aux_vars_categorical <- list(aux_vars_categorical, dfs_temp, custom_maps, thresholds_temp, weights)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_categorical,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Only distance functions 1 or 4 can be specified for categorical auxiliary variable.",
               fixed = TRUE)

  dfs_temp <- c(1,5)
  thresholds_temp <- c(NA, 1)
  dummy_aux_vars_categorical <- list(aux_vars_categorical, dfs_temp, custom_maps, thresholds_temp, weights)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_categorical,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Only distance functions 1 or 4 can be specified for categorical auxiliary variable.",
               fixed = TRUE)

  dfs_temp <- c(2,1)
  thresholds_temp <- c(1, NA)
  dummy_aux_vars_categorical <- list(aux_vars_categorical, dfs_temp, custom_maps, thresholds_temp, weights)
  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_categorical,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Only distance functions 1 or 4 can be specified for categorical auxiliary variable.",
               fixed = TRUE)
  })

test_that("Test that error raised if threshold specified for distance function 1", {

  dfs_temp <- c(1,1)
  thresholds_temp <- c(1,NA)
  dummy_aux_vars_temp <- list(aux_vars, dfs_temp, custom_maps, thresholds_temp, weights)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_temp,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Threshold should NOT be specified for distance function 1.",
               fixed = TRUE)
})

test_that("Test that error raised if threshold is specified for distance function 4", {

  dfs_temp <- c(4,4)
  thresholds_temp <- c(1,NA)
  dummy_aux_vars_temp <- list(aux_vars, dfs_temp, custom_maps, thresholds_temp, weights)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_temp,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Threshold should NOT be specified for distance function 4.",
               fixed = TRUE)
})

test_that("Test that error raised if threshold NOT specified for distance function 2", {

  dfs_temp <- c(2,2)
  thresholds_temp <- c(NA,NA)
  aux_vars <- c("dummy_aux_var1", "dummy_aux_var2")
  dummy_aux_vars_temp <- list(aux_vars, dfs_temp, custom_maps, thresholds_temp, weights)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_temp,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Threshold should be specified for distance function 2.",
               fixed = TRUE)
})

test_that("Test that error raised if threshold NOT specified for distance function 3", {

  dfs_temp <- c(3,3)
  thresholds_temp <- c(NA,NA)
  aux_vars <- c("dummy_aux_var1", "dummy_aux_var2")
  dummy_aux_vars_temp <- list(aux_vars, dfs_temp, custom_maps, thresholds_temp, weights)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_temp,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Threshold should be specified for distance function 3.",
               fixed = TRUE)
})

test_that("Test that error raised if threshold NOT specified for distance function 5", {

  dfs_temp <- c(5,5)
  thresholds_temp <- c(NA,NA)
  aux_vars <- c("dummy_aux_var1", "dummy_aux_var2")
  dummy_aux_vars_temp <- list(aux_vars, dfs_temp, custom_maps, thresholds_temp, weights)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_temp,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Threshold should be specified for distance function 5.",
               fixed = TRUE)
})

test_that("Test that error raised if threshold NOT specified for distance function 6", {

  dfs_temp <- c(6,6)
  thresholds_temp <- c(NA,NA)
  aux_vars <- c("dummy_aux_var1", "dummy_aux_var2")
  dummy_aux_vars_temp <- list(aux_vars, dfs_temp, custom_maps, thresholds_temp, weights)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_temp,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Threshold should be specified for distance function 6.",
               fixed = TRUE)
})

# TODO: custom_map specified for, and only for, appropriate distance function
#
#
#

test_that("Test that error raised if weights not all positive", {

  dfs <- c(1,1)
  weights_temp <- c(2,-3)
  dummy_aux_vars_temp <- list(aux_vars, dfs, custom_maps, thresholds, weights_temp)

  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars_temp,
                      ratio = 1,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Specified values for weights must be positive.", fixed = TRUE)
})

test_that("Test that error raised if value for ratio is less than 1", {
  expect_error(impute(data = dummy_data,
                      imp_var = "dummy_impute_var",
                      possible_vals = c(1:101),
                      aux_vars = dummy_aux_vars,
                      ratio = 0.5,
                      in_place = FALSE,
                      keep_intermediates = FALSE),
               "Specified value for ratio must be greater than 1.", fixed = TRUE)
})
