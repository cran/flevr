## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library("flevr")

## ----load-biomarker-data------------------------------------------------------
# load the dataset
data("biomarkers")
library("dplyr")
# set up vector "y" of outcomes and matrix "x" of features
cc <- complete.cases(biomarkers)
y <- biomarkers$mucinous
y_cc <- y[cc]
x_cc <- biomarkers %>%
  na.omit() %>%
  select(starts_with("lab"), starts_with("cea"))
x_names <- names(x_cc)

## ----fit-spvim----------------------------------------------------------------
set.seed(1234)

# set up a library for SuperLearner; this is too simple a library for use in most applications
learners <- "SL.glm"
univariate_learners <- "SL.glm"
V <- 2

# estimate the SPVIMs
library("SuperLearner")
library("vimp")
est <- suppressWarnings(
  sp_vim(Y = y_cc, X = x_cc, V = V, type = "auc",
              SL.library = learners, gamma = .1, alpha = 0.05, delta = 0,
              cvControl = list(V = V), env = environment())
)
est

## ----intrinsic-selection------------------------------------------------------
intrinsic_set <- intrinsic_selection(
  spvim_ests = est, sample_size = nrow(x_cc), alpha = 0.2, feature_names = x_names,
  control = list( quantity = "gFWER", base_method = "Holm", k = 1)
)
intrinsic_set

## ----intrinsic-selection-fdr--------------------------------------------------
intrinsic_set_fdr <- intrinsic_selection(
  spvim_ests = est, sample_size = nrow(x_cc), alpha = 0.2, feature_names = x_names,
  control = list( quantity = "FDR", base_method = "Holm", k = 1)
)
intrinsic_set_fdr

## ----impute-setup-------------------------------------------------------------
n_imp <- 2

## ----impute, eval = FALSE-----------------------------------------------------
#  library("mice")
#  set.seed(20231121)
#  mi_biomarkers <- mice::mice(data = biomarkers, m = n_imp, printFlag = FALSE)
#  imputed_biomarkers <- mice::complete(mi_biomarkers, action = "long") %>%
#    rename(imp = .imp, id = .id)

## ----est-spvim-imp, eval = FALSE----------------------------------------------
#  set.seed(20231121)
#  est_lst <- lapply(as.list(1:n_imp), function(l) {
#    this_x <- imputed_biomarkers %>%
#      filter(imp == l) %>%
#      select(starts_with("lab"), starts_with("cea"))
#    this_y <- biomarkers$mucinous
#    suppressWarnings(
#      sp_vim(Y = this_y, X = this_x, V = V, type = "auc",
#      SL.library = learners, gamma = 0.1, alpha = 0.05, delta = 0,
#      cvControl = list(V = V), env = environment())
#    )
#  })

## ----intrinsic-select-mi, eval = FALSE----------------------------------------
#  intrinsic_set <- intrinsic_selection(
#    spvim_ests = est_lst, sample_size = nrow(biomarkers),
#    feature_names = x_names, alpha = 0.05,
#    control = list(quantity = "gFWER", base_method = "Holm", k = 5)
#  )
#  intrinsic_set

