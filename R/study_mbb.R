#' Analyze synthetic data from multivariate beta-binomial model.
#'
#' @param instance result of \code{sample_mbb_acc()}
#' @param prob numeric, coverage probability
#' @param method character, inference method ("sample", "copula" or "approx")
#' @param prior character, specifies the prior assumptions ("vague", "correct" or "liberal")
#' @param contrast character, specifies the contrast to be applied ("raw" or "rdm")
#' @param data ignored (required for batchtools compatibility)
#' @param job ignored (required for batchtools compatibility)
#'
#' @importFrom multcomp contrMat
#' @importFrom mvtnorm qmvnorm
#' @importFrom methods as
#' @importFrom stats median rbeta rbinom
#'
#' @export
study_mbb_acc <- function(instance = sample_mbb_acc(),
                          prob = 0.95,
                          method = c("sample", "copula", "approx"),
                          prior = c("vague", "correct", "liberal"),
                          contrast = c("raw", "rdm"),
                          data = NULL,
                          job = NULL){

  if(isTRUE(attr(instance, "batch"))){
    return(
      do.call(rbind, lapply(instance, function(x){
        study_mbb_acc(instance = x, prob = prob, method = method, prior = prior, contrast = contrast)
      }))
    )
  }

  method <- match.arg(method)
  prior <- match.arg(prior)
  contrast <- match.arg(contrast)

  m <- ncol(instance$data)
  theta <- instance$pars$theta

  ## Construct prior:
  dist <- prior_setup(instance$args$gd, prior)

  ## Data update:
  udist <- SIMPle::update_dist(dist, data=list(instance$data))

  ## Inference:
  if(contrast == "raw"){
    k <- m
    cr <- SIMPle::infer(udist, prob=prob, method = method)[[1]]
  }

  ## Inference versions not implemented in SIMPle:
  if(contrast == "rdm"){

    K <- as.matrix(multcomp::contrMat(1:m, type = "Dunnett", base=sample(m, 1)))
    k <- nrow(K)
    theta <- as.numeric(theta %*% t(K))

    if(method == "sample"){
      sample <-  SIMPle::convert_sample(SIMPle::draw_sample(udist, n=10000),
                                        margin = 1, fun = function(x){as.numeric(K %*% x)})
      cr <- SIMPle::infer(sample, prob=prob, method = method)[[1]]
    }

    if(method == "copula"){
      sample <-  SIMPle::convert_sample(SIMPle::draw_sample(udist, n=10000),
                                        margin = 1, fun = function(x){as.numeric(K %*% x)})
      R <- stats::cov2cor(K %*% SIMPle::features(udist, 1)$cov %*% t(K))
      pr <- 1-(1-stats::pnorm(mvtnorm::qmvnorm(1-(1-prob)/2, tail="lower.tail", corr=R)$quantile))*2
      cr <- SIMPle:::get_cr(pr, sample, dist=NULL)[[1]]
    }

    if(method == "approx"){
      mu <- K %*% SIMPle::features(udist, 1)$mean
      C <- K %*% SIMPle::features(udist, 1)$cov %*% t(K)
      R <- stats::cov2cor(C)
      se <- sqrt(diag(C))
      cv <- mvtnorm::qmvnorm(1-(1-prob)/2, tail="lower.tail", corr=R)$quantile
      cr <- data.frame(lower = as.numeric(mu - cv*se),
                       upper = as.numeric(mu + cv*se))
    }

  }

  ## Prepare result:
  lower <- upper <- NULL
  res <- dplyr::mutate(cr,
                       diff = upper - lower,
                       adm_upper = upper <= 1,
                       adm_lower = lower >= 0,
                       adm_diff = diff >= 0,
                       theta = theta,
                       covered = theta >= lower & theta <=upper)

  out <- data.frame(gen_prior = as.character(instance$args$gd),
                    n = as.numeric(instance$args$n),
                    prob = prob,
                    method = method,
                    prior = prior,
                    contrast = contrast,
                    m = m,
                    k = k,
                    adm_upper = all(res$adm_upper),
                    adm_lower = all(res$adm_lower),
                    adm_diff = all(res$adm_diff),
                    min_len = min(res$diff),
                    max_len = max(res$diff),
                    volume = prod(res$diff),
                    max_theta = max(res$theta),
                    med_theta = stats::median(res$theta),
                    min_theta = min(res$theta),
                    all_covered = all(res$covered),
                    sum_covered = sum(res$covered),
                    max_lower = max(res$lower),
                    max_lower_theta = max(res$theta[which.max(res$lower)]),
                    min_upper = min(res$upper),
                    min_upper_theta = min(res$theta[which.min(res$upper)]))
  return(out)
}












