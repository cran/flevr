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
y_cc <- biomarkers$mucinous[cc]
x_cc <- biomarkers %>%
  na.omit() %>%
  select(starts_with("lab"), starts_with("cea"))
x_df <- as.data.frame(x_cc)
x_names <- names(x_df)

## ----fit-sl-------------------------------------------------------------------
set.seed(1234)
# fit a Super Learner ensemble; note its simplicity, for speed
library("SuperLearner")
learners <- c("SL.glm", "SL.ranger.imp", "SL.glmnet")
V <- 2
fit <- SuperLearner::SuperLearner(Y = y_cc, X = x_df,
                                  SL.library = learners,
                                  cvControl = list(V = V),
                                  family = "binomial")
# check the SL weights
fit$coef
# extract importance based on the whole Super Learner
sl_importance_all <- extract_importance_SL(
  fit = fit, feature_names = x_names, import_type = "all"
)
sl_importance_all

## ----sl-best-alg--------------------------------------------------------------
sl_importance_best <- extract_importance_SL(
  fit = fit, feature_names = x_names, import_type = "best"
)
sl_importance_best

## ----extrinsic-selection------------------------------------------------------
extrinsic_selected <- extrinsic_selection(
  fit = fit, feature_names = x_names, threshold = 5, import_type = "all"
)
extrinsic_selected

## ----impute-setup-------------------------------------------------------------
n_imp <- 2

## ----impute, eval = FALSE-----------------------------------------------------
#  library("mice")
#  set.seed(20231121)
#  mi_biomarkers <- mice::mice(data = biomarkers, m = n_imp, printFlag = FALSE)
#  imputed_biomarkers <- mice::complete(mi_biomarkers, action = "long") %>%
#    rename(imp = .imp, id = .id)

## ----extrinsic-selection-with-missing-data, eval = FALSE----------------------
#  set.seed(20231121)
#  # set up a list to collect selected sets
#  all_selected_vars <- vector("list", length = 5)
#  # for each imputed dataset, do extrinsic selection
#  for (i in 1:n_imp) {
#    # fit a Super Learner
#    these_data <- imputed_biomarkers %>%
#      filter(imp == i)
#    this_y <- these_data$mucinous
#    this_x <- these_data %>%
#      select(starts_with("lab"), starts_with("cea"))
#    this_x_df <- as.data.frame(this_x)
#    fit <- SuperLearner::SuperLearner(Y = this_y, X = this_x_df,
#                                    SL.library = learners,
#                                    cvControl = list(V = V),
#                                    family = "binomial")
#    # do extrinsic selection
#    all_selected_vars[[i]] <- extrinsic_selection(
#      fit = fit, feature_names = x_names, threshold = 5, import_type = "all"
#    )$selected
#  }
#  # perform extrinsic variable selection
#  selected_vars <- pool_selected_sets(sets = all_selected_vars, threshold = 1 / n_imp)
#  x_names[selected_vars]

