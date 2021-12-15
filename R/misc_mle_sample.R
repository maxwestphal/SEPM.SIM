# Generic -------------------------------------------------------------------------------------
get_prediction <- function(model, newdata){
  fac2num(stats::predict(model, newdata = newdata, type="raw"))
}

#' @importFrom Matrix Matrix
get_predictions <- function(models, newdata){
  Matrix::Matrix(sapply(models, get_prediction, newdata=newdata), sparse=T)
}

predict_theta <- function(pred, labels, stratify=FALSE){
  stopifnot(max(nrow(labels), length(labels)) == nrow(pred))

  C <- as.matrix(pred == labels)
  result <- data.frame(theta = colMeans(C))

  u <- unique(labels)
  if(stratify & length(u) <= 2){
    for(l in u){
      result[,paste0("theta", l)] <- colMeans(C[labels==l, ])
    }
  }

  return(result)
}

combine_samples <- function(d1, d2){
  list(Y=c(d1$Y, d2$Y), X=rbind(d1$X, d2$X))
}

# Training process ----------------------------------------------------------------------------
training <- function(method="glmnet",
                     M=10,
                     tuning = "grid",
                     samples){
  stopifnot(length(method) == length(M))
  hp <- models.train <- models.learn <- list()
  for(k in 1:length(method)){
    hp[[method[k]]] <- draw_hyperparams(method=method[k], M=M[k], tuning=tuning,
                                        x=samples$train$X, y=samples$train$Y)
    rownames(hp[[method[k]]]) <- paste0(method[k], 1:M[k])
    models.train[[method[k]]] <- fit_models(method=method[k], hp=hp[[method[k]]],
                                            sample=samples$train)
    models.learn[[method[k]]] <- fit_models(method=method[k], hp=hp[[method[k]]],
                                            sample=samples$learn)
  }
  model.list <- list(train.models=unlist(models.train, recursive=FALSE),
                     learn.models=unlist(models.learn, recursive=FALSE),
                     hp=hp)
  return(model.list)
}

fit_models <- function(method,
                       hp,
                       sample,
                       metric="Accuracy"){
  lapply(1:nrow(hp), function(i) fit_model(method, hp=hp[i,], sample=sample, metric))
}

fit_model <- function(method,
                      hp,
                      sample,
                      metric="Accuracy"){

  ctrl <- caret::trainControl(method = "none",
                              search = "grid",
                              preProc = c("center", "scale"),
                              verboseIter = F,
                              returnResamp = "all")

  model <- caret::train(x = sample$X,
                        y = factor(sample$Y),
                        method = method,
                        tuneGrid = hp,
                        trControl = ctrl,
                        metric = metric)
  return(model)
}

sample2df <- function(sample){
  data.frame(Y=factor(sample$Y), sample$X)
}

fac2num <- function(x){
  as.numeric(as.character(x))
}

# Hyperparameter generation -------------------------------------------------------------------
draw_hyperparams <- function(method="glmnet",
                             M=10,
                             tuning="random",
                             x=NULL,
                             y=NULL,
                             ...){
  do.call(what = paste("draw_hyperparams", method, sep="_"),
          args = list(len=M, tuning=tuning, x=x, y=y))
}

sample_hyperparams <- function(M=5,
                               ...){
  args <- list(...)
  do.call(cbind.data.frame, lapply(args, sample, size=M, replace=TRUE))
}

## Ref: https://github.com/topepo/caret/blob/master/models/files/glmnet.R
#' @importFrom glmnet glmnet
draw_hyperparams_glmnet <- function(len=10,
                                    tuning="random",
                                    x=NULL,
                                    y=NULL){
  if(tuning == "grid"){
    init <- glmnet::glmnet(Matrix::as.matrix(x), y,
                           family = "binomial",
                           nlambda = ceiling(sqrt(len))+2,
                           alpha = .5)
    lambda <- unique(init$lambda)
    lambda <- lambda[-c(1, length(lambda))]
    lambda <- lambda[1:min(length(lambda), ceiling(sqrt(len)))]
  }
  switch(tuning,
         grid   = expand.grid(alpha = seq(1, 0.1, length.out = ceiling(sqrt(len))),
                              lambda = lambda)[1:len, ],
         random = data.frame(alpha = stats::runif(len, min = 0, 1),
                             lambda = 2^stats::runif(len, min = -10, 3)))
}

## Ref: https://github.com/topepo/caret/blob/master/models/files/ranger.R
draw_hyperparams_ranger <- function(len=10,
                                    tuning="random",
                                    x=NULL,
                                    y=NULL){
  srules <- c("gini", "extratrees")
  switch(tuning,
         grid = expand.grid(splitrule = srules,
                            min.node.size = 1,
                            mtry  = round(seq(ncol(args$x), 2, length.out=len/2)))[1:len, ],
         random = data.frame(
           min.node.size= sample(1:(min(20,nrow(x))), size = len, replace = TRUE),
           mtry = sample(1:ncol(x), size = len, replace = TRUE),
           splitrule = sample(srules, size = len, replace = TRUE)))
}

## Ref: https://github.com/topepo/caret/blob/master/models/files/xgbTree.R
draw_hyperparams_xgbTree <- function(len=10,
                                     tuning="random",
                                     x=NULL,
                                     y=NULL){
  switch(tuning,
         grid = expand.grid(max_depth = seq(1, len),
                            nrounds = floor((1:len) * 50),
                            eta = c(.3, .4),
                            gamma = 0,
                            colsample_bytree = c(.6, .8),
                            min_child_weight = c(1),
                            subsample = seq(.5, 1, length = len)),
         random = data.frame(nrounds = sample(1:1000, size = len, replace = TRUE),
                             max_depth = sample(1:10, replace = TRUE, size = len),
                             eta = stats::runif(len, min = .001, max = .6),
                             gamma = stats::runif(len, min = 0, max = 10),
                             colsample_bytree = stats::runif(len, min = .3, max = .7),
                             min_child_weight = sample(0:20, size = len, replace = TRUE),
                             subsample = stats::runif(len, min = .25, max = 1)))
}

## Ref: https://github.com/topepo/caret/blob/master/models/files/rpartCost.R
## (Cost is apparently the cost of a false positive, hence this should be < 1 if prev < 0.5)
draw_hyperparams_rpartCost <- function(len=10,
                                       tuning="random",
                                       x=NULL,
                                       y=NULL){
  switch(tuning,
         grid = expand.grid(cp=10^seq(-5, -1, 1),
                            Cost = 2^seq(-4,4, length.out=len/5)),
         random = data.frame(cp = 10^stats::runif(len, min = -8, max = -1),
                             Cost = 2^stats::runif(len, -1/mean(y), 1/(1-mean(y)))))
}

## Ref: https://github.com/topepo/caret/blob/master/models/files/svmLinearWeights2.R
## (weight = runif(len, min = 1, max = 25) --> seems way off for low prevalence of Y=1)
draw_hyperparams_svmLinearWeights2 <- function(len=10,
                                               tuning="random",
                                               x=NULL,
                                               y=NULL){
  switch(tuning,
         grid = expand.grid(cost = 2 ^((1:len) - 3),
                            Loss = c("L1", "L2"),
                            weight = 1:len),
         random = data.frame(cost = 2^stats::runif(len, min = -10, max = 10),
                             Loss = sample(c("L1", "L2"), size = len, replace = TRUE),
                             weight = 2^stats::runif(len, -1/(1-mean(y)), 1/(mean(y)))))
}


# Feature-label data generation ---------------------------------------------------------------
#' Draw feature-label data sample
#'
#' @importFrom mvtnorm rmvnorm
#' @importFrom graphics hist
#' @param n integer, sample size
#' @param score function specifying score
#' @param pars list of score parameters
#' @param seed integer to set seed explicitly (or NULL)
#' @param tune logical (default: FALSE) if TRUE, return true parameter values
draw_sample <- function(n = 100,
                        score = score_linearsparse,
                        pars = list(P=50, Prel=5, rho=0, red=0, b=0.5, s=1, mu=4),
                        seed = NULL,
                        tune = FALSE){
  set.seed(seed)
  ## create correlation matrix and feature data:
  R <- construct_corrmat(P=pars$P, Prel=pars$Prel, rho=pars$rho, red=pars$red)
  x <- mvtnorm::rmvnorm(n=n, mean=rep(0, pars$P), sigma=R)
  colnames(x) <- paste0("X", 1:pars$P)

  ## derive score and label data:
  nu <- do.call(score, list(x=x, pars=pars))*pars$s + pars$m
  y <- rbinom(n, 1, sigmoid(nu))

  ## use tune=TRUE to assess scenario properties:
  if(tune){
    graphics::hist(sigmoid(nu), xlim=c(0,1), freq=FALSE)
    y2 <- as.numeric(sigmoid(nu)>0.5)
    result <- data.frame(prev=mean(y), acc=mean(y==y2),
                         sens=sum(y==1 & y2==1)/sum(y==1),
                         spec=sum(y==0 & y2==0)/sum(y==0))
    return(result)
  }
  return(list(Y=y, X=x))
}

normalize <- function(x){
  (x - mean(x))/stats::sd(x)
}

logit <- function(x){
  log(x/(1-x))
}

sigmoid <- function(x){
  1/(1+exp(-x))
}

construct_corrmat <- function(P=15,
                              Prel=5,
                              rho = 0.5,
                              red=1){
  Pred <- Prel * red
  stopifnot(Prel + Pred <= P)
  R <- diag(rep(1,P))
  prel <- 1:Prel
  IJ <- as.matrix(do.call(rbind, lapply(prel, function(x){p = seq(x, x+Pred, Prel); expand.grid(i=p, j=p)})))
  R[IJ[IJ[,1] != IJ[,2], ]] <- rho
  R
}

# score ---------------------------------------------------------------------------------------
## FUNCTION: score_friedman1 (scenarios "C" and "D")
score_friedman1 <- function(x, pars=list()){
  10 * sin(pi * x[, 1] * x[, 2]) + 20 * (x[, 3] - 0.5)^2 + 10 * x[, 4] + 5 * x[, 5]
}

## FUNCTION: score_friedman2
score_friedman2 <- function(x, pars=list()){
  sqrt(x[, 1]^2 + (x[, 2] * x[, 3] - 1/(x[, 2] * x[, 4]))^2)
}

## FUNCTION: score_friedman3
score_friedman3 <- function(x, pars=list()){
  atan((x[, 2] * x[, 3] - 1/(x[, 2] * x[, 4]))/x[, 1])
}

## FUNCTION: score_friedman12
score_friedman12 <- function(x, pars=list(w1=1, w2=1)){
  normalize(pars$w1 * score_friedman1(x[, 1:5]) + pars$w2 * score_friedman2(x[, 6:9]))
}

## FUNCTION: score_friedman13 (scenarios "E" and "F")
score_friedman13 <- function(x, pars=list(w1=1, w3=1)){
  pars$w1 * score_friedman1(x[, 1:5]) + pars$w3 * score_friedman3(x[, 6:9])
}

## FUNCTION: score_friedman23
score_friedman23 <- function(x, pars=list(w2=1, w3=1)){
  pars$w2 * score_friedman2(x[, 1:4]) + pars$w3 * score_friedman3(x[, 5:8])
}

## FUNCTION: score_linear
score_linear <- function(x, beta, intercept=0){
  intercept + x%*%beta
}

## FUNCTION: score_linearsparse (EOMPM scenario "A")
score_linearsparse <- function(x, pars=list(P=50, Prel=5, mu=4)){
  beta <- c(rep(pars$mu, pars$Prel), rep(0, pars$P-pars$Prel))
  score_linear(x, beta, ifelse(is.null(pars$intercept), 0, pars$intercept))
}

## FUNCTION: score_lineardense (EOMPM scenario "B")
score_lineardense <- function(x, pars=list(P=50, Prel=50, mu=6)){
  beta <- pars$mu*c((-1)^(1:pars$Prel-1)*1/(1:pars$Prel), rep(0, pars$P-pars$Prel))
  score_linear(x, beta, ifelse(is.null(pars$intercept), 0, pars$intercept))
}

# score tuning/testing ------------------------------------------------------------------------
### Tune problems to yield realistic optimal performance ~ 85%-95%
### (=performance of true data-generating model)
#
# set.seed(1337)
# N <- 100000
### EOMPM A/B:
# draw_sample(n=N, score = score_linearsparse,
#             pars=list(P=50, Prel=5, rho=0, red=0, m=0, s=1, mu=2), seed=1, tune=T)
# draw_sample(n=N, score = score_lineardense,
#             pars=list(P=50, Prel=5, rho=0, red=0, m=0, s=1, mu=3), seed=1, tune=T)
#
### Friedman1:
# draw_sample(n=N, score = score_friedman1,
#             pars=list(P=50, Prel=5, rho=0, red=0, m=2.5, s=-0.5), seed=1, tune=T)
# draw_sample(n=N, score = score_friedman1,
#             pars=list(P=50, Prel=5, rho=0, red=0, m=-2.5, s=-0.55), seed=1, tune=T)
#
### Friedman2: not suitable
#
### Friedman3:
# draw_sample(n=N, score = score_friedman3,
#             pars=list(P=50, Prel=4, rho=0, red=0, m=-15, s=-12), seed=1, tune=T)
# draw_sample(n=N, score = score_friedman3,
#             pars=list(P=50, Prel=4, rho=0, red=0, m=-50.75, s=-34), seed=1, tune=T)
#
#
### Friedman13:
# draw_sample(n=N, score = score_friedman13,
#             pars=list(P=50, Prel=9, rho=0, red=0, m=2.5, s=-0.5, w1=1, w3=-1), seed=1, tune=T)
# draw_sample(n=N, score = score_friedman13,
#             pars=list(P=50, Prel=9, rho=0, red=0, m=-2.5, s=-1, w1=0.5, w3=-0.75), seed=1, tune=T)


