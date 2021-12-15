#' Generate artificial performance data (classification accuracy)
#'
#' Generate binary data, simulating the assessment of classification accuracy.
#' Result may be passed to \code{SEPM::evaluate()} as argument \code{comparison}.
#'
#' @param n integer, sample size of evaluation study
#' @param S integer, number of classification models that are assessed
#' @param acc numeric, accuracy (between 0 and 1) of all models (under the global null), default
#' value is 0.8.
#' @param delta shift parameter
#' @param corr character string (default: "type=equi_rho=0"), specifies correlation structure,
#' see details
#' @param data ignored (required for batchtools compatability)
#' @param job ignored (required for batchtools compatability)
#'
#' @details Specification of correlation structure: always starts with "type=..." specifying the
#' correlation matrix type. Currently, only "type=equi" (equicorrelation) and
#' "type=ak" (autocorrelation) are supported.
#' Equicorrelation may be specified as "type=equi_rho=x" (e.g. "type=equi_rho=0.5") for correlation
#' matrix with entries R[i,j]=x^(|i-j|>0)
#' Autocorrelation may be specified as "type=ak_rho=x" (e.g. "type=ak_rho=0.5") for correlation
#' matrix with entries R[i,j]=x^|i-j|.
#'
#' @return a list with two entries: \code{args} (list of function arguments) and
#' \code{comp} (matrix of simulated correct (1) and false (0) predictions)
#'
#' @examples
#' dat <- sample_lfc_acc()
#' str(dat, 2)
#' dat$comp
#'
#' @export
sample_lfc_acc <- function(n = 100,
                           S = 5,
                           acc = 0.8,
                           delta = 0,
                           corr = "type=equi_rho=0",
                           data = NULL,
                           job=NULL)
{
  a <- as.list(environment())

  a$R <- corr2R(corr, list(S=S))

  if(delta==0){
    a$accvec <- rep(acc, S)
  }else{
    a$accvec <- seq(acc-delta, acc, length.out=S)
  }

  comp <- draw_sample_lfc_acc(n, a$accvec, a$R)

  return(list(args=a, comp=comp))
}

#' Generate artificial performance data (sensitivity and specificity)
#'
#' Generate binary data, simulating the simultaneous assessment of sensitivity and specificity.
#' Result may be passed to \code{SEPM::evaluate()} as argument \code{comparison}.
#'
#' @param n integer, sample size of evaluation study
#' @param prev numeric, prevalence of positive class
#' @param S integer, number of classification models that are assessed
#' @param se numeric, sensitivity (between 0 and 1) of all models (under the global null), default
#' value is 0.8.
#' @param sp numeric, specificity (between 0 and 1) of all models (under the global null), default
#' value is 0.8.
#' @param B integer, number of models with sensitivity of L instead of se (default: \code{round(S/2)}).
#' The exact indices are random. The complementary S-B specificities will have value L instead of sp.
#' @param L numeric, gives alternative value (default: L=1, corresponding to LFC)
#' @param eps numeric, default 0 corresponds to LFC, small positive values \code{eps}
#' induce less hostile paramter configurations
#' @param corr.se character string (default: "type=equi_rho=0", i.e. independence),
#' specifies correlation structure, see details
#' @param corr.sp character string (default: "type=equi_rho=0", i.e. independence),
#' specifies correlation structure, see details
#' @param data ignored (required for batchtools compatability)
#' @param job ignored (required for batchtools compatability)

#'
#' @return a list with two entries: \code{args} (list of function arguments) and
#' \code{comp} (list with matrices of simulated correct (1) and false (0) predictions in diseased
#' and healthy, respectively)
#'
#' @details Specification of correlation structure: always starts with "type=..." specifying the
#' correlation matrix type. Currently, only "type=equi" (equicorrelation) and
#' "type=ak" (autocorrelation) are supported.
#' Equicorrelation may be specified as "type=equi_rho=x" (e.g. "type=equi_rho=0.5") for correlation
#' matrix with entries R[i,j]=x^(|i-j|>0)
#' Autocorrelation may be specified as "type=ak_rho=x" (e.g. "type=ak_rho=0.5") for correlation
#' matrix with entries R[i,j]=x^|i-j|.
#'
#' @examples
#' dat <- sample_lfc_cpe()
#' str(dat, 2)
#' dat$comp
#'
#' @export
sample_lfc_cpe <- function(n = 100,
                           prev = 0.5,
                           S = 5,
                           se = 0.8,
                           sp = 0.8,
                           B = round(S/2),
                           L = 1,
                           eps = 0,
                           corr.se = "type=equi_rho=0",
                           corr.sp = "type=equi_rho=0",
                           data = NULL,
                           job=NULL)
{
  a <- as.list(environment())

  a$b <- switch((B >= 0) + 1,
                NULL,
                sample(rep(c(TRUE, FALSE), times=c(B,S-B))))

  a$Rse <- corr2R(corr.se, list(S=S, d=!a$b))
  a$Rsp <- corr2R(corr.sp, list(S=S, d= a$b))

  se.vec <- se - (0:(S-1))*eps
  sp.vec <- rev(sp - (0:(S-1))*eps)

  comp <- draw_sample_lfc_cpe(n=n, prev=prev, se=se.vec, sp=sp.vec,
                   b=a$b, L=L, Rse=a$Rse, Rsp=a$Rsp)

  return(list(args=a, comp=comp))
}
