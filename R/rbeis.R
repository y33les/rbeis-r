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

get_igroups_single_aux_var_vals <- function(data, aux_var_name) {
  var_tbl <- data %>%
    filter(!`__RBEISImpute`) %>%
    select(!!as.name(aux_var_name), `__RBEISIGroup`) %>%
    unique() %>%
    as.list() %>%
    as.character # FIXME trying to make values all strings to join later on 2023-09-13
  var_list <- var_tbl[[aux_var_name]]
  names(var_list) <- var_tbl$`__RBEISIGroup`
  var_list %>%
    t %>%
    as_tibble %>%
    pivot_longer(cols=everything(),names_to="__RBEISIGroup",values_to=aux_var_name)
}

get_igroups_all_aux_var_vals <- function(data, aux_var_names) {
  out <- map(aux_var_names, function(v) {get_igroups_single_aux_var_vals(data,v)})
  out %>%
    reduce(function(x,y){x %>% full_join(y,by="__RBEISIGroup")}) %>%
    pivot_longer(!matches("__RBEISIGroup"),names_to="__RBEISIGroupVariable",values_to="__RBEISIGroupValue")
}

lookup_igroup_value <- function(ig_vals,var,igroup) {
  ig_vals %>% filter(`__RBEISIGroup`==igroup,`__RBEISIGroupVariable`==var)
  # FIXME finish writing this function 2023-09-13
}

calc_distances <- function(data, aux_vars) {
  # aux_vars must be strings, not symbols; TODO: check if this is what we want / consistent with the rest of the functions
  igroup_vals <- get_igroups_all_aux_var_vals(data,aux_vars)
  # TODO: so it now returns a tibble of igroup values; what next?
  # FIXME "subscript out of bounds" 2023-09-13
}

d %>% add_impute_col(book) %>% assign_igroups(c(artist_race_nwi,moma_count)) %>% calc_distances(c("artist_race_nwi","moma_count"))

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
