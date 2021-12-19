#' Save MLE simulations instances to disk
#'
#' @param instance object generated via \code{sample_mle}
#' @param id integer, id to append to \code{filename}. If set to NA (default), id is retrieved via
#' id = instance$job$id
#' @param filename character, base file name
#' @param folder character, folder to file
#' @param sep character, path separator
#' @param write logical, should instance be saved a (if FALSE, instance is only returned)
#' @param data NULL (required for batchtools compatibility)
#' @param job NULL (required for batchtools compatibility)
#'
#' @export
save_instance <- function(instance,
                          id = NA,
                          filename = "mle_instance_",
                          folder = "E:\\MLE_SIM\\DATA",
                          sep = "\\",
                          write = TRUE,
                          data = NULL,
                          job = NULL){
  if(write){
    if(is.na(id)){
      id <- instance$job$id
    }
    stopifnot(is.integer(id))
    file <- paste0(filename, id, ".rds")
    saveRDS(instance, file=paste(folder, file, sep=sep))
  }
  return(instance)
}

#' Load MLE simulations instances from disk
#'
#' @param id integer, id to append to \code{filename}
#' @param filename character, file name
#' @param folder character, file folder
#' @param sep character, path separator
#' @param data NULL (required for batchtools compatibility)
#' @param job NULL (required for batchtools compatibility)
#'
#' @export
load_instance <- function(id = 1,
                          filename = "mle_instance_",
                          folder = "E:\\MLE_SIM\\DATA",
                          sep = "\\",
                          data = NULL,
                          job = NULL){
  readRDS(normalizePath(paste0(folder, sep, filename,id, ".rds"), winslash=sep))
}

#' Save MLE simulation results in tabular form
#'
#' @param reg batchtools registry
#' @param JP.data data.table, jobPars of MLE data generation registry
#' @param target character, target directory
#' @param return logical, should the object also be returned?
#'
#' @export
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr select rename
#' @importFrom utils object.size
#' @importFrom batchtools findDone unwrap reduceResultsDataTable getJobPars
save_results_mle <- function(reg,
                             JP.data,
                             target,
                             return = FALSE){

  job.id <- methods <- M <- filename <- subfolder <- folder <- sep <- algorithm <- n.eval <- problem <- NULL

  R <- batchtools::unwrap(batchtools::reduceResultsDataTable(
    batchtools::findDone(reg=reg), function(x){x$result}, reg=reg))
  JP <- batchtools::unwrap(batchtools::getJobPars(batchtools::findDone(reg=reg), reg=reg))

  stopifnot(all(R$job.id == JP$job.id) & nrow(R) == nrow(JP))

  if(is.character(JP.data)){
    JP.data <- readr::read_csv(JP.data)
  }
  JP.data <- JP.data %>%
    dplyr::rename(load.id = job.id) %>%
    dplyr::select(-methods, -M, -write, -filename, -subfolder, -folder, -sep,
                  -problem, -algorithm, -n.eval)

  excl <- 1:6
  DD <- dplyr::right_join(JP.data, cbind(JP %>% dplyr::select(-excl), R))

  readr::write_csv(DD, paste0(target))

  message(paste0("The resulting table has ", nrow(DD), " rows and ", ncol(DD),
                 " columns. Its size is ", round(utils::object.size(DD)*10^-6, 1), " MB."))
  if(return){
    return(DD)
  }else{
    return(NULL)
  }
}

#' Process MLE simulation results to investigate test decision characteristics
#'
#' @param analysis character, either "acc" or "cpe"
#' @param results data.frame with simulation results produced by \code{save_results}
#' @param vals numeric, shift parameters to be investigated
#' @param delta numeric, difference between se0 and sp0 (default: 0, ignored for analysis="acc")
#' @importFrom data.table CJ
#' @importFrom dplyr mutate
#' @importFrom dplyr inner_join
#' @export
postproc_mle <- function(results,
                         analysis = c("acc", "cpe"),
                         vals = seq(-0.05,0.1,0.01),
                         delta = 0){
  do.call(paste0("postproc_mle_", match.arg(analysis)),
          list(results=results, vals = vals, delta = delta))
}

postproc_mle_acc <- function(results, vals = seq(-0.05,0.1,0.01), ...){
  opt.theta <- shift <- final.mod.lower <- theta0 <- reject <- final.mod.learn.theta <- NULL
  D <- data.table::CJ(job.id=unique(results$job.id), shift=vals)
  R <- dplyr::inner_join(results, D) %>%
    dplyr::mutate(theta0 = opt.theta - shift) %>%
    dplyr::mutate(reject = final.mod.lower > theta0) %>%
    dplyr::mutate(tp = reject & final.mod.learn.theta > theta0) %>%
    dplyr::mutate(fp = reject & final.mod.learn.theta <= theta0)
  return(R)
}

postproc_mle_cpe <- function(results, vals = seq(-0.05,0.1,0.01), delta=0){
  opt.se.cpe <- opt.sp.cpe <- shift <- sp0 <- final.sp.lower <- final.se.lower <- se0 <- NULL
  reject.se <- reject.sp <- final.learn.sp <- final.learn.se <- tp.se <- tp.sp <- reject <- NULL
  D <- data.table::CJ(job.id=unique(results$job.id), shift=vals)
  R <- dplyr::inner_join(results, D) %>%
    dplyr::mutate(sp0 = pmin(opt.se.cpe - delta, opt.sp.cpe) - shift) %>%
    dplyr::mutate(se0 = sp0 + delta) %>%
    dplyr::mutate(reject.sp = final.sp.lower > sp0,
                  reject.se = final.se.lower > se0) %>%
    dplyr::mutate(reject = reject.se & reject.sp) %>%
    dplyr::mutate(tp.sp = reject.sp & final.learn.sp > sp0,
                  tp.se = reject.se & final.learn.se > se0) %>%
    dplyr::mutate(tp = tp.se & tp.sp) %>%
    dplyr::mutate(fp.sp = reject.sp & final.learn.sp <= sp0,
                  fp.se = reject.se & final.learn.se <= se0,
                  fp = reject & (final.learn.sp <= sp0 | final.learn.se <= se0))
  return(R)
}

#' @importFrom dplyr filter arrange
#' @importFrom stats t.test
report_results <- function(data,
                           ep = "final.theta",
                           sr = c("oracle", "default", "within1SE", "optimalEFP"),
                           vs = c(4, 3),
                           ne = c(400, 800),
                           nl = c(400, 800),
                           alpha = 0.01,
                           de = 3,
                           dd = 3,
                           dc = 3){

  n.learn <- n.eval <- load.id <- select.rule <- NULL

  df <- data %>% dplyr::filter(n.eval %in% ne, n.learn %in% nl)

  e <- list()
  for(r in sr){
    e[[r]] <- df %>%
      dplyr::filter(select.rule==r) %>%
      dplyr::arrange(n.learn, n.eval, load.id) %>%
      `[[`(ep)
  }

  stopifnot(all(diff(sapply(e, length))==0))

  d <- e[[vs[1]]] - e[[vs[2]]]
  tr <- stats::t.test(d, conf.level = 1-alpha)

  result <- formatC(round(sapply(e, mean), de), digits=de, format="f")
  result <- c(result,
              diff = paste0(formatC(round(mean(d),  dd), digits=dd, format="f"), " (",
                            formatC(round(tr$conf.int[1], dc), digits=dc, format="f"), ", ",
                            formatC(round(tr$conf.int[2], dc), digits=dc, format="f"), ")"))

  return(result)
}

#' Summarize simulation study in results table
#'
#' @param data data.frame, containing simulation results
#' @param vars character, list of variables to report
#' @param rules character vector, selection rules to investigate
#' @param comp numeric of length 2, which two selection rules should be compared (delta = rules[comp[1]] - rules[comp[2]])
#' @param NE list, each element is a numeric vector hinting which evaluation sample sizes (n.eval) are considered
#' @param NL list, each element is a numeric vector hinting which learning sample sizes (n.learn) are considered
#' @param alpha numeric, significance level of t-test in comparison
#' @param digits numeric, to which degree should results be rounded
#'
#' @return
#' data.frame with aggregated results
#' @export
summarize_results <- function(data,
                              vars = "final.theta",
                              rules = c("oracle", "default", "within1SE", "optimalEFP"),
                              comp = c(4, 3),
                              NL = list(c(400, 800)),
                              NE = list(400, 800),
                              alpha = 0.01,
                              digits = 3){

  nl <- ne <- NULL

  results <- list()
  j <- 0

  for(ep in vars){
    for(ie in 1:length(NE)){
      for(il in 1:length(NL)){
        j = j+1
        results[[j]] <-
          c(variable = ep,
            nl = paste(NL[[il]], collapse = ", "),
            ne = paste(NE[[ie]], collapse = ", "),
            report_results(data = data, ep = ep,
                           sr = rules, vs = comp,
                           ne = NE[[ie]], nl = NL[[il]],
                           de = digits, dd = digits, dc = digits)
          )
      }
    }
  }

  ## compile output
  out <- do.call(rbind, results) %>%
    as.data.frame() %>%
    rename(n.learn = nl, n.eval = ne)
  names(out)[ncol(out)] <- paste0(rules[comp[1]], " - ", rules[comp[2]])
  rownames(out) <- NULL

  return(out)
}





