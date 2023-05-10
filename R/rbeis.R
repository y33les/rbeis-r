# TODO: Vignette
# TODO: Specify 'missing' value/s (currently assumes NA)

library(tibble)
library(dplyr)
library(magrittr)
library(purrr)

d <- readr::read_csv("R/testdata.csv")

check_missing_auxvars <- function(data, aux_vars) {
  if (data %>%
    select(aux_vars) %>%
    mutate(`__RBEISTempCat` = pmap(select(., everything()), ~ c(...))) %>%
    select(`__RBEISTempCat`) %>%
    map((function(l) reduce(map(l, is.na), or))) %>%
    as_vector() %>%
    reduce(or)) {
    stop("One or more records are missing the specified auxiliary variables.")
  }
}

add_impute_col <- function(data, imp_var) {
  # TODO: Throw error or fix it if imp_var is given as a string (should be a tibble colname, no quotes)
  data %>%
    rowwise() %>%
    mutate(`__RBEISImpute` = is.na({{imp_var}})) %>%
    ungroup()
}

assign_igroups <- function(data, aux_var_names) {
  data %>%
    mutate(`__RBEISIGroup_temp` = pmap(select(data, {{aux_var_names}}), ~ paste0(as.character(c(...)), collapse = ""))) %>%
    rowwise() %>%
    mutate(`__RBEISIGroup` = if (`__RBEISImpute`) "" else as.character(`__RBEISIGroup_temp`)) %>%
    select(-`__RBEISIGroup_temp`) %>%
    ungroup()
}

get_igroup_aux_var <- function(data, aux_var_name) {
  # TODO
}

#' @export
impute <- function(data, # Tibble
                   imp_var, # String
                   possible_vals, # Vector
                   aux_vars, # List
                   ratio = 1,
                   in_place = FALSE, # Note difference from Pandas version as in-place would be weird by default in R
                   keep_intermediates = FALSE) {
  print(data)
  print(imp_var)
  print(possible_vals)
  print(aux_vars)
  print(ratio)
  print(in_place)
  print(keep_intermediates)
}
