% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datadictto.R
\name{dd2script}
\alias{dd2script}
\title{Data Dictionary to R script}
\usage{
dd2script(l, file = "", factorHandle = c("duplicate", "unchanged", "changed"))
}
\arguments{
\item{l}{data dictionary list}

\item{file}{character string naming the file to print to; defaults to standard output}

\item{factorHandle}{character value of either \sQuote{duplicated},
\sQuote{unchanged}, \sQuote{changed}. Determines how to handle factor variables.}
}
\value{
character vector of R commands, returned invisibly
}
\description{
Convert data dictionary to an R script
}
\examples{
dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
# \sQuote{duplicate} will create new factor variable with name \sQuote{variablename.factor}
dd2script(dd)
# \sQuote{unchanged} will leave variables as character strings
dd2script(dd, factorHandle = 'unchanged')
# \sQuote{changed} will replace character string with factor variable
dd2script(dd, factorHandle = 'changed')

}
