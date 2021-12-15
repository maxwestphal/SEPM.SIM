#' @importFrom bindata rmvbin
draw_sample_lfc_acc <- function(n=100, acc = c(0.8, 0.8), R=diag(length(acc)))
{
  if(length(acc)==1)return(bindata::rmvbin(n=n, margprob=acc))
  return(rmvbin(n=n, margprob = acc, bincorr = R))
}

draw_sample_lfc_cpe <- function(n=200,
                            prev=0.5,
                            se=rep(0.8, 5),
                            sp=se,
                            b=NULL,
                            L=1,
                            Rse = diag(length(se)),
                            Rsp = diag(length(sp)))
{
  if(!all(diff(c(length(se), length(sp), dim(Rse), dim(Rse)))==0)){stop("Wrong dimensions!")}
  if(!is.logical(b) | length(b) != length(se)){stop("Something is wrong with b!")}

  n1 <- round(n*prev); n0 <- n - n1

  comp1 <- matrix(-1, ncol=length(se), nrow=n1)
  comp0 <- matrix(-1, ncol=length(sp), nrow=n0)

  # worst alternative under side condition Acc <= L
  sp.alt <- pmin(1, (L - prev*se)/(1-prev))
  se.alt <- pmin(1, (L - (1-prev)*sp)/(prev))

  if(sum( b) > 0){
    comp0[,  b] <- draw_sample_lfc_acc(n0, sp[ b], Rsp[ b,  b])
    comp1[,  b] <- sapply(se.alt[ b], function(p) stats::rbinom(n1, 1, p))
  }
  if(sum(!b) > 0){
    comp1[, !b] <- draw_sample_lfc_acc(n1, se[!b], Rse[!b, !b])
    comp0[, !b] <- sapply(sp.alt[!b], function(p) stats::rbinom(n1, 1, p))
  }

  if(! all(rbind(comp0, comp1) %in% 0:1)){stop("Something went wrong!!!")}
  return(list(sens = comp1, spec = comp0))
}

corr2R <- function(s, args=list()){
  l <- string2list(s, sep1 = "_", sep2 = "=")
  a <- c(l[-1], args)
  R <- do.call(paste0("create_R_", l$type), list(a=a))
  return(R)
}

create_R_equi <- function(a){
  a <- lapply(a, as.numeric)
  R <- matrix(a$rho, a$S, a$S)
  if(!is.null(a$d)){
    R <- diag(a$d, length(a$d)) %*% R %*% diag(a$d, length(a$d))
  }
  diag(R) <- 1
  return(R)
}

create_R_ak <- function(a){
  a <- lapply(a, as.numeric)
  M <- matrix(a$rho, a$S, a$S)
  R <- M^(abs(col(M) - row(M)))
  if(!is.null(a$d)){
    R <- diag(a$d, length(a$d)) %*% R %*% diag(a$d, length(a$d))
  }
  diag(R) <- 1
  return(R)
}

#' @importFrom dplyr mutate
compress <- function(x, type="acc", cols=3:9){

  if(type=="acc"){
    mstar <- which.min(x$inference[[1]]$pvalue)
    out <- cbind(x$inference[[1]][mstar, cols],
                 calpha = x$inference$info$c.alpha,
                 mod = x$inference$info$dependence)
  }

  if(type=="cpe"){
    mstar <- which.min(apply(cbind(x$inference[[1]]$pvalue,
                                   x$inference[[2]]$pvalue), 1, max))
    se.reject <- sp.reject <- NULL
    out <- list2df(lapply(x$inference[1:2], '[', mstar, cols)) %>%
      dplyr::mutate(reject = se.reject & sp.reject,
                    calpha = x$inference$info$c.alpha,
                    mod = x$inference$info$dependence)

  }

  return(out)
}

list2df <- function(l, n=c("se", "sp"), combine=cbind){
  do.call(combine,
          lapply(1:length(n),
                 function(i){names(l[[i]]) <- paste(n[i], names(l[[i]]), sep="."); l[[i]]})
  )
}






