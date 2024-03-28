reticulate::use_condaenv("rbeis",conda="C:/Users/yeelep/Anaconda3/Scripts/conda.exe")
library(reticulate)
rbeis <- import("rbeis.pandas")

library(tibble)

#### EE's dummy data

dummy <- tibble(
  dummy_impute_var = c(30,20,30,40,NA),
  dummy_aux_var1 = c(21,19,18,67,20),
  dummy_aux_var2 = c(2,3,1,1,3),
  dummy_aux_var_missing = c(4,3,6,NA,6),
  dummy_aux_var_categorical = c("dog","cat","parrot","giraffe","hedgehog")
)

rbeis$impute(
  dummy,
  "dummy_impute_var",
  1:101,
  dict(dummy_aux_var1 = rbeis$RBEISDistanceFunction(1,weight=2),
       dummy_aux_var2 = rbeis$RBEISDistanceFunction(1,weight=3)),
  in_place = FALSE,
  keep_intermediates = TRUE
)

#### newtest

td <- readr::read_csv("C:/Users/yeelep/dev/python/rbeis/newtest_data.csv") %>% tidyr::replace_na(list(book=NaN))

tr <- rbeis$impute(
  td,
  "book",
  c("Gardner","Janson"),
  dict(artist_race_nwi = rbeis$RBEISDistanceFunction(1),
       artist_gender = rbeis$RBEISDistanceFunction(1)),
  in_place = FALSE,
  keep_intermediates = TRUE
) %>% as_tibble

tr %>% dplyr::select(book,book_imputed)
