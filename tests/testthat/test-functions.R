# =============================================================================
# -------------------------- TESTING TEMPLATE ---------------------------------
# =============================================================================
# Test each function does what it is expected to do
# =============================================================================

# Test data file path
test_data_filepath <- "../../R/testdata.csv"


# -----------------------------------------------------------------------------
# TESTS: Function: add_impute_col
# This function adds a boolean column '__RBEISImpute' indicating whether
# record will be imputed.
# -----------------------------------------------------------------------------

test_that("Test that correct Boolean values are assigned for __RBEISImpute column.", {

  test_data <- readr::read_csv(test_data_filepath, na =  "", show_col_types = FALSE)
  test_data <- add_impute_col(test_data, whitney_count)

  check_vector <- is.na(test_data[["whitney_count"]])
  expect_true(identical(test_data[["__RBEISImpute"]], check_vector))
})


# -----------------------------------------------------------------------------
# TESTS: Function: assign_igroups
# This function adds a column '__RBEISIGroup' representing
# the IGroup each recipient record is assigned to.
# -----------------------------------------------------------------------------

test_that("Test that Igroup column values are assigned correctly.", {

  test_data <- readr::read_csv(test_data_filepath, na =  "", show_col_types = FALSE)
  test_data <- add_impute_col(test_data, whitney_count)

  aux_var_names <- c("moma_count", "artist_nationality_other")
  test_data <- assign_igroups(test_data, aux_var_names)

  # Select donor records and check Igroup is set to ""
  donors <- test_data %>%
    dplyr::filter( `__RBEISImpute` == FALSE)
  check <- ifelse( all( (donors[["__RBEISIGroup"]]) == ""), TRUE, FALSE)
  expect_true(check)

  # Select recipient records (add column containing combination of the
  # auxiliary variables for later) and check Igroup is set to a value
  recipients <- test_data %>%
    dplyr::filter( `__RBEISImpute` == TRUE) %>%
    mutate(combine_aux_vars = paste0(as.character(moma_count),
                                    as.character(artist_nationality_other)))
  check <- ifelse( all( recipients[["__RBEISIGroup"]] > 0), TRUE, FALSE)
  expect_true(check)

  # Select IGroups with only one recipient and check all have different
  # combination of auxiliary variables
  single_recipient_IGroups <- recipients %>%
    dplyr::count(`__RBEISIGroup`) %>%
    dplyr::filter(n == 1)

  single_recipient_data <- recipients %>%
    dplyr::filter( `__RBEISIGroup` %in% single_recipient_IGroups[["__RBEISIGroup"]])

  expect_true( nrow(single_recipient_data) ==
                 dplyr::n_distinct(single_recipient_data[["combine_aux_vars"]]))

  # Select IGroups containing multiple recipients and check that records in
  # same IGroup have same values for auxiliary variables
  multiple_recipient_IGroups <- recipients %>%
    dplyr::count(`__RBEISIGroup`) %>%
    dplyr::filter(n > 1)

  multiple_recipient_data <- recipients %>%
    dplyr::filter( `__RBEISIGroup` %in% multiple_recipient_IGroups[["__RBEISIGroup"]])

  for (each_iGroup in multiple_recipient_IGroups[["__RBEISIGroup"]]) {
    igroup_data <- multiple_recipient_data %>%
      filter(`__RBEISIGroup` == each_iGroup)
    expect_true(length(unique(igroup_data[["combine_aux_vars"]])) ==1)
  }

})


# -----------------------------------------------------------------------------
# TESTS: Function: get_igroup_aux_var
# This function returns a vector of each IGroup's value for a given
# auxiliary variable.
# -----------------------------------------------------------------------------

test_that("Test that vector of auxiliary variable values for each iGroup is correct.", {

  test_data <- readr::read_csv(test_data_filepath, na =  "", show_col_types = FALSE)
  test_data <- add_impute_col(test_data, whitney_count)

  aux_var_names <- c("moma_count", "artist_nationality_other")
  test_data <- assign_igroups(test_data, aux_var_names)

  # Test get_igroup_aux_var function with moma_count as auxiliary variable
  aux_var1_list  <- get_igroup_aux_var(test_data, "moma_count")
  moma_count_vector <- aux_var1_list[["moma_count"]]
  igroup_vector <- aux_var1_list[["__RBEISIGroup"]]

  # For all data in iGroup, check moma_count matches what is returned by
  # get_igroup_aux_var function
  for( each_iGroup in c(1:nrow(aux_var1_list)) ) {
    igroup_data <- test_data %>%
      filter(`__RBEISIGroup` == igroup_vector[each_iGroup])
    check <- ifelse( all(igroup_data[["moma_count"]] ==
                           moma_count_vector[each_iGroup] ), TRUE, FALSE)
    expect_true(check)
  }

  # Test get_igroup_aux_var with artist_nationality_other as auxiliary variable
  aux_var2_list  <- get_igroup_aux_var(test_data, "artist_nationality_other")
  artist_nationality_vector <- aux_var2_list[["artist_nationality_other"]]
  igroup_vector <- aux_var2_list[["__RBEISIGroup"]]

  for( each_iGroup in c(1:nrow(aux_var2_list)) ) {
    igroup_data <- test_data %>%
      filter(`__RBEISIGroup` == igroup_vector[each_iGroup])
    check <- ifelse( all(igroup_data[["artist_nationality_other"]] ==
                           artist_nationality_vector[each_iGroup] ), TRUE, FALSE)
    expect_true(check)
  }
})