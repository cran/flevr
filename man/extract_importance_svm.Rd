% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_importance_svm.R
\name{extract_importance_svm}
\alias{extract_importance_svm}
\title{Extract the learner-specific importance from an svm object}
\usage{
extract_importance_svm(
  fit = NULL,
  feature_names = "",
  coef = 0,
  x = NULL,
  y = NULL
)
}
\arguments{
\item{fit}{the \code{svm} object.}

\item{feature_names}{the feature names}

\item{coef}{the Super Learner coefficient associated with the learner.}

\item{x}{the features}

\item{y}{the outcome}
}
\value{
a tibble, with columns \code{algorithm} (the fitted algorithm),
\code{feature} (the feature), \code{importance} (the algorithm-specific
extrinsic importance of the feature), \code{rank} (the feature importance
rank, with 1 indicating the most important feature), and \code{weight}
(the algorithm's weight in the Super Learner)
}
\description{
Extract the individual-algorithm extrinsic importance from a glm object,
along with the importance rank.
}
\examples{
data("biomarkers")
# subset to complete cases for illustration
cc <- complete.cases(biomarkers)
dat_cc <- biomarkers[cc, ]
# use only the mucinous outcome, not the high-malignancy outcome
y <- dat_cc$mucinous
x <- as.data.frame(dat_cc[, !(names(dat_cc) \%in\% c("mucinous", "high_malignancy"))])
x_mat <- as.matrix(x)
feature_nms <- names(x)
# get the fit 
set.seed(20231129)
fit <- kernlab::ksvm(x_mat, y)
# extract importance
importance <- extract_importance_svm(fit = fit, feature_names = feature_nms, x = x, y = y)
importance

}
