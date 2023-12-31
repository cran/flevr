#' Extract the learner-specific importance from a glm object
#'
#' Extract the individual-algorithm extrinsic importance from a glm object,
#' along with the importance rank.
#'
#' @param fit the \code{glm} object.
#' @param feature_names the feature names
#' @param coef the Super Learner coefficient associated with the learner.
#'
#' @return a tibble, with columns \code{algorithm} (the fitted algorithm),
#'   \code{feature} (the feature), \code{importance} (the algorithm-specific
#'   extrinsic importance of the feature), \code{rank} (the feature importance
#'   rank, with 1 indicating the most important feature), and \code{weight}
#'   (the algorithm's weight in the Super Learner)
#' 
#' @examples
#' data("biomarkers")
#' # subset to complete cases for illustration
#' cc <- complete.cases(biomarkers)
#' dat_cc <- biomarkers[cc, ]
#' # use only the mucinous outcome, not the high-malignancy outcome
#' y <- dat_cc$mucinous
#' x <- dat_cc[, !(names(dat_cc) %in% c("mucinous", "high_malignancy"))]
#' feature_nms <- names(x)
#' # get the fit
#' fit <- stats::glm(y ~ ., family = "binomial", data = data.frame(y = y, x))
#' # extract importance
#' importance <- extract_importance_glm(fit = fit, feature_names = feature_nms)
#' importance
#' 
#' @export
extract_importance_glm <- function(fit = NULL, feature_names = "", coef = 0) {
  if (!inherits(fit, "glm")) {
    stop("This is not a glm object. Please use a different importance extraction function.")
  } else {
    p <- length(feature_names)
    coeffs <- summary(fit)$coefficients[-1, ]
    summ2 <- as.data.frame(coeffs[rank(-abs(coeffs[, 3]), ties.method = "last"), ])
    summ2$rank <- rank(-abs(summ2[, 3]), ties.method = "last")
    summ2$feature <- rownames(summ2)
    if (nrow(summ2) < p) {
      current_length <- nrow(summ2)
      current_nms <- row.names(summ2)
      avg_remaining_rank <- mean((current_length + 1):p)
      remaining_features <- feature_names[!(feature_names %in% current_nms)]
      na_mat <- matrix(NA, nrow = p - nrow(summ2), ncol = ncol(summ2))
      na_df <- as.data.frame(na_mat)
      names(na_df) <- names(summ2)
      na_df$feature <- remaining_features
      na_df$rank <- avg_remaining_rank
      summ2 <- dplyr::bind_rows(summ2, na_df)
    }
    imp_dt  <- tibble::tibble(algorithm = "glm", feature = summ2$feature,
                              importance = abs(summ2[, grepl("value",
                                                             names(summ2))]),
                              rank = summ2$rank,
                              weight = coef)
    imp_dt[order(imp_dt$rank), ]
  }
}
