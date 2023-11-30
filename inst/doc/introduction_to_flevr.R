## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(flevr)

## ----install, eval = FALSE----------------------------------------------------
#  # install devtools if you haven't already
#  # install.packages("devtools", repos = "https://cloud.r-project.org")
#  devtools::install_github(repo = "bdwilliamson/flevr")

## ----gen-data-----------------------------------------------------------------
# generate the data -- note that this is a simple setting, for speed
set.seed(4747)
p <- 2
n <- 500
# generate features
x <- replicate(p, stats::rnorm(n, 0, 1))
x_df <- as.data.frame(x)
x_names <- names(x_df)
# generate outcomes
y <- 1 + 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)

## ----sl-fit-and-imp-----------------------------------------------------------
set.seed(1234)
# fit a Super Learner ensemble; note its simplicity, for speed
library("SuperLearner")
learners <- c("SL.glm", "SL.mean")
V <- 2
fit <- SuperLearner::SuperLearner(Y = y, X = x_df,
                                  SL.library = learners,
                                  cvControl = list(V = V))
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
  fit = fit, feature_names = x_names, threshold = 1.5, import_type = "all"
)
extrinsic_selected

## ----fit-spvim----------------------------------------------------------------
set.seed(1234)

# set up a library for SuperLearner
learners <- "SL.glm"
univariate_learners <- "SL.glm"
V <- 2

# estimate the SPVIMs
library("vimp")
est <- suppressWarnings(
  sp_vim(Y = y, X = x, V = V, type = "r_squared",
              SL.library = learners, gamma = .1, alpha = 0.05, delta = 0,
              cvControl = list(V = V), env = environment())
)
est

## ----intrinsic-selection------------------------------------------------------
intrinsic_set <- intrinsic_selection(
  spvim_ests = est, sample_size = n, alpha = 0.2, feature_names = x_names,
  control = list( quantity = "gFWER", base_method = "Holm", k = 1)
)
intrinsic_set

