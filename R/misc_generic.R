#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

string2list <- function(s, sep1="#", sep2="=", convert=function(x)x){
  l <- strsplit(s, split=sep1)[[1]]
  ll <- strsplit(l, split=sep2)
  a <- lapply(ll, function(x)x[2])
  names(a) <- lapply(ll, function(x)x[1])
  a <- lapply(a, convert)
  return(a)
}
