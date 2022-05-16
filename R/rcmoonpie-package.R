#' RedCap Moonpie
#'
#' add details
#'
#' This package is experimental. Please submit bugs to
#' \url{https://github.com/couthcommander/rcmoonpie}.
#'
#' @docType package
#'
#' @author Cole Beck \email{cole.beck@@vumc.org}
#'
#' Maintainer: Cole Beck \email{cole.beck@@vumc.org}
#'
#' @importFrom yaml yaml.load_file write_yaml
#' @importFrom utils read.csv write.csv
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' # exclude a variable
#' dd <- excludeVar(dd, 'data', 'redcap_event_name')
#' # convert to data.frame
#' dd2df(dd)
#' # reproduce R script
#' dd2script(dd, factorHandle = 'unchanged')
"_PACKAGE"
