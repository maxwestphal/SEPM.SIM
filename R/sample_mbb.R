#' Sample data from multivariate beta-binomial model
#'
#' @param gd character, generative distribution, needs to be one of \code{names(DIST)}
#' @param params alternatively to \code{gd}, params can also be specified directly,
#' e.g. via \code{params = SIMPle::define_dist()[[1]]$params}
#' @param n integer, total sample size
#' @param data ignored (required for batchtools compatibility)
#' @param job ignored (required for batchtools compatibility)
#' @param nrep integer, if != 1, then output is list with nrep repetitions
#' @param ... further arguments (ignored)
#'
#' @export
sample_mbb_acc <- function(gd = "nu40_rho50_blocks2",
                           params = NULL,
                           n = 200,
                           data = NULL,
                           job = NULL,
                           nrep = 1,
                           ...
){
  if(nrep != 1){
    stopifnot(nrep %% 1 == 0)
    out <- lapply(1:nrep, function(i){
      do.call(sample_mbb_acc, list(gd=gd, params=params, n=n, data=data, job=job))})
    attr(out, "batch") <- TRUE
    return(out)
  }

  args <- as.list(environment())

  ## Construct/load mbeta:
  if(is.null(params)){
    params <- DISTS[[gd]][[1]]$params
  }

  ## Parameter sampling:
  pars <- do.call(sample_pars, params)

  ## Data sampling:
  data <- do.call(sample_data_mbin, c(list(n=n), pars))

  ## Output:
  return(list(args=args, pars=pars, data=data))
}




