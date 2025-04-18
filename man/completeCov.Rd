% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/completeCov.R
\name{completeCov}
\alias{completeCov}
\title{Expand a set of covariate values into a data.set with reference
value}
\usage{
completeCov(covlist, data, col.id = "ID", sigdigs = 2)
}
\arguments{
\item{covlist}{A covariate specififed in a list. See
?expandCovLists.}

\item{data}{See ?expandCovLists.}

\item{col.id}{The subject ID column name. Necessary because
quantiles sould be quantiles of distribution of covariate on
subjects, not on observations (each subject contributes once).}

\item{sigdigs}{Used for rounding of covariate values if using
quantiles or if using a function to find reference.}
}
\description{
Expand a set of covariate values into a data.set with reference
value
}
\examples{
    NMsim:::completeCov(covlist=list(covvar="WEIGHTB",values=c(30,60,90),ref=50),sigdigs=3)
}
\keyword{internal}
