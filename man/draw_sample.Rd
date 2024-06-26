% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_mle_sample.R
\name{draw_sample}
\alias{draw_sample}
\title{Draw feature-label data sample}
\usage{
draw_sample(
  n = 100,
  score = score_linearsparse,
  pars = list(P = 50, Prel = 5, rho = 0, red = 0, b = 0.5, s = 1, mu = 4),
  seed = NULL,
  tune = FALSE
)
}
\arguments{
\item{n}{integer, sample size}

\item{score}{function specifying score}

\item{pars}{list of score parameters}

\item{seed}{integer to set seed explicitly (or NULL)}

\item{tune}{logical (default: FALSE) if TRUE, return true parameter values}
}
\description{
Draw feature-label data sample
}
