#' Define a multivariate beta distribution
#'
#' @param nu numeric, concentration parameter
#' @param mu1 numeric, mean of first variable
#' @param delta numeric, distance between mean parameters
#' @param rho numeric, correlation parameter
#' @param blocks integer, specify block correlation structure
#' @param size integer, block size
#' @param ... further arguments (ignored)
#'
#' @return A \code{SIMPle.dist} object with derived parameters,
#' compare \code{SIMPle::define_dist()}
#'
#' @details For details regarding the block correlation matrix, se \code{\link{block_corrmat}}
construct_mbeta <- function(nu=10,
                            mu1=0.85,
                            delta=0.05,
                            rho=0.5,
                            blocks=1,
                            size=5,
                            ...){
  m <- size*blocks
  mu <- round(rep(mu1 -delta*(0:(blocks-1)), each=size), 3)
  R <- block_corrmat(rho, blocks, size)
  SIMPle::define_dist(nu=nu, mean=mu, corr=R)
}

#' Sample parameters from multivariate binomial distribution
#'
#' @param nu numeric, concentration parameter
#' @param moments matrix, moment matrix
#' @param gamma numeric, concentration vector
#' @param ... further arguments (ignored)
#'
#' @importFrom gdata upperTriangle lowerTriangle
#' @importFrom MCMCpack rdirichlet
#' @importFrom bindata rmvbin
#' @importFrom bindata check.commonprob
sample_pars <- function(nu=NULL, moments=NULL, gamma=NULL, ...){
  out <-list()
  if(!is.null(gamma)){
    w <- log(length(gamma), 2)
    stopifnot(w %% 1 == 0)
    out$p <- as.numeric(MCMCpack::rdirichlet(1, gamma))
    out$cp <- Hmat(w) %*% diag(out$p) %*% t(Hmat(w))
  }else{
    out$p <- NULL
    adm <- FALSE
    while(!adm){
      cp <- apply(moments, 1:2, function(x) stats::rbeta(1, x, nu-x))
      gdata::upperTriangle(cp) <- gdata::lowerTriangle(cp, byrow=TRUE)
      cp <- SIMPle::define_dist(vars=ncol(cp), moments=cp, nu=1)[[1]]$params$moments
      adm <- bindata::check.commonprob(cp)[1]
    }
    out$cp <- cp
  }
  stopifnot(length(out) > 0)
  out$theta <- diag(out$cp)
  return(out)
}

sample_data_mbin <- function(n=200, cp=NULL, p=NULL, ...){
  m <- nrow(cp)
  if(!is.null(p)){
    return(t(Hmat(m)[ , base::sample(1:2^m, n, replace=T, p)]))
  }else{
    return(bindata::rmvbin(n,
                           margprob=diag(cp),
                           commonprob=cp))
  }
}

Hrow <- function(j, m, class="integer"){
  x <- methods::as(0:1, class)
  rep(rep(x, each=2^(m-j)), length.out=2^m)
}

Hmat <- function(m, class="integer"){
  t(sapply(1:m, function(j) Hrow(j,m)))
}

#' Create a block correlation matrix
#'
#' @param rho numeric, correlation strength
#' @param blocks integer, number of blocks
#' @param size integer, block size
#'
#' @details Implementation copied from
#' https://stats.stackexchange.com/questions/171342/more-efficient-way-to-create-block-toeplitz-matrix-in-r
#'
#' @export
block_corrmat <- function(rho=0.8, blocks = 3, size=2){
  U <- lapply(1:blocks, function(b) matrix(rho^b, size, size))
  k <- min(unlist(lapply(U, dim)))
  n <- length(U)
  #
  # Create the "strip".
  #
  strip <- array(NA, dim=c(k,k,2*n-1))
  for (i in 1:n) strip[,,i] <- U[[n+1-i]]
  if (n > 1) for (i in 2:n) strip[,,n+i-1] <- t(U[[i]])
  #
  # Assemble into "block-Toeplitz" form.
  #
  X <- array(NA, dim=c(k,k,n,n))
  #
  # Blast the strip across X.
  #
  for (i in 1:n) X[,,,i] <- strip[,,(n+1-i):(2*n-i)]
  X <- matrix(aperm(X, c(1,3,2,4)), n*k)
  diag(X) <- 1
  return(X)
}

prior_setup <- function(gd, prior){
  pd <- switch(prior,
               correct = gd,
               vague = paste0("vague", substr(gd, nchar(gd)-7, nchar(gd))),
               liberal = paste0("liberal", substr(gd, nchar(gd)-7, nchar(gd))))
  return(DISTS[[pd]])
}
