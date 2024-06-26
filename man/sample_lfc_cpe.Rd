% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_lfc.R
\name{sample_lfc_cpe}
\alias{sample_lfc_cpe}
\title{Generate artificial performance data (sensitivity and specificity)}
\usage{
sample_lfc_cpe(
  n = 100,
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
  job = NULL
)
}
\arguments{
\item{n}{integer, sample size of evaluation study}

\item{prev}{numeric, prevalence of positive class}

\item{S}{integer, number of classification models that are assessed}

\item{se}{numeric, sensitivity (between 0 and 1) of all models (under the global null), default
value is 0.8.}

\item{sp}{numeric, specificity (between 0 and 1) of all models (under the global null), default
value is 0.8.}

\item{B}{integer, number of models with sensitivity of L instead of se (default: \code{round(S/2)}).
The exact indices are random. The complementary S-B specificities will have value L instead of sp.}

\item{L}{numeric, gives alternative value (default: L=1, corresponding to LFC)}

\item{eps}{numeric, default 0 corresponds to LFC, small positive values \code{eps}
induce less hostile paramter configurations}

\item{corr.se}{character string (default: "type=equi_rho=0", i.e. independence),
specifies correlation structure, see details}

\item{corr.sp}{character string (default: "type=equi_rho=0", i.e. independence),
specifies correlation structure, see details}

\item{data}{ignored (required for batchtools compatability)}

\item{job}{ignored (required for batchtools compatability)}
}
\value{
a list with two entries: \code{args} (list of function arguments) and
\code{comp} (list with matrices of simulated correct (1) and false (0) predictions in diseased
and healthy, respectively)
}
\description{
Generate binary data, simulating the simultaneous assessment of sensitivity and specificity.
Result may be passed to \code{SEPM::evaluate()} as argument \code{comparison}.
}
\details{
Specification of correlation structure: always starts with "type=..." specifying the
correlation matrix type. Currently, only "type=equi" (equicorrelation) and
"type=ak" (autocorrelation) are supported.
Equicorrelation may be specified as "type=equi_rho=x" (e.g. "type=equi_rho=0.5") for correlation
matrix with entries R[i,j]=x^(|i-j|>0)
Autocorrelation may be specified as "type=ak_rho=x" (e.g. "type=ak_rho=0.5") for correlation
matrix with entries R[i,j]=x^|i-j|.
}
\examples{
dat <- sample_lfc_cpe()
str(dat, 2)
dat$comp

}
