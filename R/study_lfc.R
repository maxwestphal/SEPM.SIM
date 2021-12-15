#' Conduct an evaluation study (classification accuracy)
#'
#' This is a \code{batchtools} friendly implementation of the evaluation of the artificial
#' prediction models simulated via
#' \code{\link{sample_lfc_acc}} regarding classification accuracy with functions from the
#' \code{SEPM} package.
#'
#' @param alternative character, either \code{"two.sided"} (default), \code{"greater"} or \code{"less"}
#' @param alpha numeric, significance level (default: \code{0.05})
#' @param estimate character, estimation method (default: \code{"default"}, i.e. ML estimation)
#' @param infer character, inference method (default: "maxT", currently only choice)
#' @param transform character, application of transform and delta method (default: \code{"none"},
#' other suitable choices are \code{"logit"} and \code{"asin.sqrt"})
#' @param data ignored (required for batchtools compatibility)
#' @param job ignored (required for batchtools compatibility)
#' @param instance artificial performance data, result of \code{\link{sample_lfc_acc}(...)} call
#'
#' @return A single row \code{data.frame} with important characteristics of the evaluation study.
#'
#' @examples
#' study_lfc_acc(instance=sample_lfc_acc())
#'
#' @export
study_lfc_acc <- function(instance=sample_lfc_acc(),
                          alternative = "greater",
                          alpha = 0.025,
                          estimate = "beta.approx",
                          infer = "maxT",
                          transform = "none",
                          data = NULL,
                          job = NULL){

  SEPM::define_hypothesis("accuracy", threshold = instance$args$acc,
                          alternative = alternative, alpha=alpha) %>%
    SEPM::compare(comparison = instance$comp) %>%
    SEPM::estimate(method=estimate) %>%
    SEPM::infer(method = infer, transform = transform) %>%
    compress(type="acc", cols=3:9)
}


#' Conduct an evaluation study (sensitivity and specificity)
#'
#' This is a \code{batchtools} friendly implementation of the evaluation of the artifical
#' prediction models simulated via
#' \code{\link{sample_lfc_cpe}} regarding sensitivity and specificity with functions from the
#' \code{SEPM} package.
#'
#' @param alternative character, either \code{"two.sided"} (default), \code{"greater"} or \code{"less"}
#' @param alpha numeric, significance level (default: \code{0.05})
#' @param estimate character, estimation method (default: \code{"default"}, i.e. ML estimation)
#' @param infer character, inference method (default: "maxT", currently only choice)
#' @param transform character, application of transform and delta method (default: \code{"none"},
#' other suitable choices are \code{"logit"} and \code{"asin.sqrt"})
#' @param data ignored (required for batchtools compatibility)
#' @param job ignored (required for batchtools compatibility)
#' @param instance artificial performance data, result of \code{\link{sample_lfc_cpe}(...)} call
#'
#' @examples
#' study_lfc_cpe(instance=sample_lfc_cpe())
#'
#' @return A single row \code{data.frame} with important characteristics of the evaluation study.
#'
#' @export
study_lfc_cpe <- function(instance = sample_lfc_cpe(),
                          alternative = "greater",
                          alpha = 0.025,
                          estimate = "beta.approx",
                          infer = "maxT",
                          transform = "none",
                          data = NULL,
                          job = NULL)
{

  a <- instance$args

  SEPM::define_hypothesis("sensspec", threshold = c(a$se, a$sp),
                          alternative = alternative, alpha=alpha) %>%
    SEPM::compare(comparison = instance$comp) %>%
    SEPM::estimate(method=estimate) %>%
    SEPM::infer(method = infer, transform = transform) %>%
    compress(type="cpe", cols=3:9)
}







