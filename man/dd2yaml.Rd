% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datadictto.R
\name{dd2yaml}
\alias{dd2yaml}
\title{Data Dictionary to YAML file}
\usage{
dd2yaml(l, file = stdout(), ...)
}
\arguments{
\item{l}{data dictionary list}

\item{file}{character string naming the file to print to; defaults to standard output}

\item{\dots}{Additional arguments, passed to \code{\link[yaml]{write_yaml}}.}
}
\value{
creates CSV file
}
\description{
Convert data dictionary to a YAML file
}
\examples{
dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
dd2yaml(dd)

}
