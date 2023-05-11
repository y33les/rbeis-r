# TODO: Vignette
# TODO: Specify 'missing' value/s (currently assumes NA)

library(tibble)
library(dplyr)
library(magrittr)
library(purrr)
library(tidyr)

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
    mutate(`__RBEISImpute` = is.na({{ imp_var }})) %>%
    ungroup()
}

assign_igroups <- function(data, aux_var_names) {
  # aux_var_names seems OK if either strings or symbols; TODO: check inside impute
  data %>%
    mutate(`__RBEISIGroup_temp` = pmap(select(data, {{ aux_var_names }}), ~ paste0(as.character(c(...)), collapse = ""))) %>%
    rowwise() %>%
    mutate(`__RBEISIGroup` = if (`__RBEISImpute`) "" else as.character(`__RBEISIGroup_temp`)) %>%
    select(-`__RBEISIGroup_temp`) %>%
    ungroup()
}

get_igroup_aux_var <- function(data, aux_var_name) {
  var_tbl <- data %>%
    filter(!`__RBEISImpute`) %>%
    select({{ aux_var_name }}, `__RBEISIGroup`) %>%
    unique() %>%
    as.list()
  var_list <- var_tbl$moma_count
  names(var_list) <- var_tbl$`__RBEISIGroup`
  return(var_list)
}

calc_distances <- function(data, aux_vars) {
  # aux_vars must be strings, not symbols; TODO: check if this is what we want / consistent with the rest of the functions
  # data %>% select(all_of({{aux_vars}}))
  ## map get_igroup_aux_var across aux_vars for each imputable record
  ## calculate distances for each imputable record
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
