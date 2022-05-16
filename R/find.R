#' Find Dataset Within Data Dictionary List
#'
#' Find index location for a named data set within
#' the data dictionary.
#'
#' @param l data dictionary list
#' @param dataset name of data set
#'
#' @return integer
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' findDataset(dd, 'data')
#'
#' @export

findDataset <- function(l, dataset) {
  # "dataset" isn't required if only 1 is available
  if(missing(dataset) && length(l[['dataset']]) == 1L) return(1)
  ans <- which(vapply(l[['dataset']], `[[`, character(1), "name") == dataset)
  if(length(ans) == 0) ans <- NA
  ans
}

#' Find Variable Within Data Dictionary List
#'
#' Find index location for a named variable within
#' the data dictionary.
#'
#' @param l data dictionary list
#' @param dataset name of data set
#' @param variable name of variable to remove
#'
#' @return list with data set location and variable location
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' findVariable(dd, 'data', 'redcap_event_name')
#'
#' @export

findVariable <- function(l, dataset, variable) {
  data_loc <- findDataset(l, dataset)
  ans <- integer(0)
  if(!is.na(data_loc)) {
    myvars <- l[['dataset']][[data_loc]][['variables']]
    ans <- which(vapply(myvars, `[[`, character(1), "name") == variable)
  }
  if(length(ans) == 0) ans <- NA
  list(data_loc = data_loc, field_loc = ans)
}

#' Find Variable By Pattern
#'
#' Find variable names that match a pattern.
#'
#' @param l data dictionary list
#' @param pattern character value passed to \code{grep} function
#'
#' @return character vector of variable names
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' findVariableByPattern(dd, 'redcap')
#' # regular expression for strings that start with \dQuote{s}
#' findVariableByPattern(dd, '^s')
#'
#' @export

findVariableByPattern <- function(l, pattern) {
  vl <- variableList(l)
  grep(pattern, vl[,'variable'], value = TRUE)
}
