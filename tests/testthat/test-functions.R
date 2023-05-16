# =============================================================================
# -------------------------- TESTING TEMPLATE ---------------------------------
# =============================================================================
# Test each function does what it is expected to do
# =============================================================================


# Import test data from R folder
test_data_filepath <- "../../R/testdata.csv"
test_data <- readr::read_csv(test_data_filepath, na = "", show_col_types = FALSE)


# -----------------------------------------------------------------------------
# TESTS: Function: add_impute_col
# This function adds a boolean column '__RBEISImpute' indicating whether
# record will be imputed.
# -----------------------------------------------------------------------------

test_that("Correct Boolean values are assigned for __RBEISImpute column.", {

  test_data <- add_impute_col(test_data, whitney_count)
  check_vector <- is.na(test_data[["whitney_count"]])

  expect_true(identical(test_data[["__RBEISImpute"]], check_vector))

})

