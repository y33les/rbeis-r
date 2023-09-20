# TODO: Vignette
# TODO: Specify 'missing' value/s (currently assumes NA)

library(tibble)
library(dplyr)
library(magrittr)
library(purrr)
library(tidyr)

# d <- readr::read_csv("R/testdata.csv") # With missing data
d <- readr::read_csv("R/testdata_full.csv") # Full dataset

check_missing_auxvars <- function(data, aux_vars) {
  if (data %>%
      select(aux_vars) %>%
      mutate(`__RBEISTempCat` = pmap(select(., everything()), ~ c(...))) %>%
      select(`__RBEISTempCat`) %>%
      map((function(l)
        reduce(map(l, is.na), or))) %>%
      as_vector() %>%
      reduce(or)) {
    stop("One or more records are missing the specified auxiliary variables.")
  }
}

add_impute_col <- function(data, imp_var) {
  # TODO: Throw error or fix it if imp_var is given as a string (should be a tibble colname, no quotes)
  data %>%
    rowwise() %>%
    mutate(`__RBEISImpute` = is.na({
      {
        imp_var
      }
    })) %>%
    ungroup()
}

assign_igroups <- function(data, aux_var_names) {
  # aux_var_names seems OK if either strings or symbols; TODO: check inside impute
  data %>%
    mutate(`__RBEISIGroup_temp` = pmap(select(data, {
      {
        aux_var_names
      }
    }), ~ paste0(as.character(c(
      ...
    )), collapse = ""))) %>%
    rowwise() %>%
    mutate(`__RBEISIGroup` = if (`__RBEISImpute`)
      ""
      else
        as.character(`__RBEISIGroup_temp`)) %>%
    select(-`__RBEISIGroup_temp`) %>%
    ungroup()
}

get_igroups_single_aux_var_vals <- function(data, aux_var_name) {
  var_tbl <- data %>%
    filter(!`__RBEISImpute`) %>%
    select(!!as.name(aux_var_name), `__RBEISIGroup`) %>%
    unique() %>%
    as.list() %>%
    map(as.character)
  var_list <- var_tbl[[aux_var_name]]
  names(var_list) <- var_tbl$`__RBEISIGroup`
  var_list %>%
    t %>%
    as_tibble %>%
    pivot_longer(cols = everything(),
                 names_to = "__RBEISIGroup",
                 values_to = aux_var_name)
}

get_igroups_all_aux_var_vals <- function(data, aux_var_names) {
  out <-
    map(aux_var_names, function(v) {
      get_igroups_single_aux_var_vals(data, v)
    })
  out %>%
    reduce(function(x, y) {
      x %>% full_join(y, by = "__RBEISIGroup")
    }) %>%
    pivot_longer(!matches("__RBEISIGroup"),
                 names_to = "__RBEISIGroupVariable",
                 values_to = "__RBEISIGroupValue")
}

lookup_igroup_value <- function(ig_vals, var, igroup) {
  (ig_vals %>% filter(`__RBEISIGroup` == igroup, `__RBEISIGroupVariable` ==
                        var))$`__RBEISIGroupValue`
}

calc_single_distance <- function(df, var, value, igroup, ig_vals) {
  # FIXME: Is using NaNs really a proper solution here?  NAs would make more sense but cause problems down the line as map just cuts them out.  Will NaNs cause other problems?  None of the DFs should return NaN though.
  if (igroup == "")
    NaN
  else
    df(value, lookup_igroup_value(ig_vals, var, igroup))
  # FIXME: weights!!
}

calc_distances <- function(data, df, aux_vars) {
  # aux_vars must be strings, not symbols; TODO: check if this is what we want / consistent with the rest of the functions
  igroup_vals <- get_igroups_all_aux_var_vals(data, aux_vars)
  dists <- map(aux_vars, function(v) {
    vals <- data %>% select(v) %>% as_vector
    igroups <- data %>% select(`__RBEISIGroup`) %>% as_vector
    map(1:length(vals), function(i) {
      calc_single_distance(df, v, vals[i], igroups[i], igroup_vals)
    }) %>% as_tibble_col(column_name = paste0("__RBEISDistance__", v))
  }) %>% reduce(tibble) %>% mutate_all(as_vector)
  dists <-
    dists %>% rowwise %>% mutate(`__RBEISDistanceSum` = rowSums(across(where(is.numeric)))) %>% ungroup # %>% select(`__RBEISDistanceSum`) # TODO: uncomment to remove intermediate distance columns
  data %>% add_column(dists)
  # TODO 2023-09-14: we now have separate columns for each variable's distances - the rest of this function will be to calculate the total distance and deselect these temporary `__RBEISDistance__*` columns
}

calc_donors <- function(data, ratio = 1) {
  # data %>% group_by(`__RBEISIGroup`) %>% mutate(`__RBEISDonorThreshold`=ratio*min(`__RBEISDistanceSum`)) %>% select(artist_name,`__RBEISImpute`,`__RBEISIGroup`,`__RBEISDistanceSum`,`__RBEISDonorThreshold`)
  data %>% group_by(`__RBEISIGroup`) %>% mutate(`__RBEISDonorThreshold` =
                                                  if (0 == min(`__RBEISDistanceSum`))
                                                    9
                                                else
                                                  ratio * min(`__RBEISDistanceSum`)) %>% select(
                                                    artist_name,
                                                    `__RBEISImpute`,
                                                    `__RBEISIGroup`,
                                                    `__RBEISDistanceSum`,
                                                    `__RBEISDonorThreshold`
                                                  ) %>% ungroup
}

df1 <- compose(as.numeric, `!=`)

d %>% add_impute_col(book) %>% assign_igroups(c(artist_race_nwi)) %>% calc_distances(df1, c("moma_count", "whitney_count")) %>% calc_donors

#' @export
impute <- function(data,
                   # Tibble
                   imp_var,
                   # String
                   possible_vals,
                   # Vector
                   aux_vars,
                   # List
                   ratio = 1,
                   in_place = FALSE,
                   # Note difference from Pandas version as in-place would be weird by default in R
                   keep_intermediates = FALSE) {
  print(data)
  print(imp_var)
  print(possible_vals)
  print(aux_vars)
  print(ratio)
  print(in_place)
  print(keep_intermediates)
}
