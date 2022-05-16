#' Exclude Variable
#'
#' Remove a variable from the data set.
#'
#' @param l data dictionary list
#' @param dataset name of data set
#' @param variable name of variable to remove
#' @param true The value to assign to \sQuote{exclude}, assumed to be TRUE.
#'
#' @return updated data dictionary list
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' dd <- excludeVar(dd, 'data', 'redcap_event_name')
#' dd$dataset[[1]]$variables[[2]]$exclude
#'
#' @export

excludeVar <- function(l, dataset, variable, true = TRUE) {
# excluding a variable adds NULL assignment
# irrecoverable once written to script
  loc <- findVariable(l, dataset, variable)
  if(is.na(loc$field_loc)) {
    vars <- findVariableByPattern(l, variable)
    for(i in seq_along(vars)) {
      l <- excludeVar(l, dataset, vars[i], true)
    }
  }
  if(!is.na(loc$field_loc)) {
    l[['dataset']][[loc$data_loc]][['variables']][[loc$field_loc]][['exclude']] <- true
  }
  l
}

#' Include Variable
#'
#' Include a variable in the data set, by allowing a previously removed variable.
#'
#' @param l data dictionary list
#' @param dataset name of data set
#' @param variable name of variable to include
#'
#' @return updated data dictionary list
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' dd <- excludeVar(dd, 'data', 'redcap_event_name')
#' dd <- unexcludeVar(dd, 'data', 'redcap_event_name')
#' dd$dataset[[1]]$variables[[2]]$exclude
#'
#' @export
unexcludeVar <- function(l, dataset, variable) {
  excludeVar(l, dataset, variable, true = FALSE)
}

#' Exclude Factor Level
#'
#' Remove a level for a factor variable in the data set
#'
#' @param l data dictionary list
#' @param dataset name of data set
#' @param variable name of factor variable
#' @param level name of factor level to remove
#' @param true The value to assign to \sQuote{exclude}, assumed to be TRUE.
#'
#' @return updated data dictionary list
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' dd <- excludeLevel(dd, 'data', 'sex', 666)
#' dd$dataset[[1]]$variables[[5]]$factor[[3]]$exclude
#'
#' @export

excludeLevel <- function(l, dataset, variable, level, true = TRUE) {
# excluding a level results in NA
# irrecoverable once written to script
  set <- FALSE
  loc <- findVariable(l, dataset, variable)
  if(!is.null(l[['dataset']][[loc$data_loc]][['variables']][[loc$field_loc]][['factor']])) {
    lev_opt <- sapply(l[['dataset']][[loc$data_loc]][['variables']][[loc$field_loc]][['factor']], `[[`, "level")
    lev_loc <- which(lev_opt == level)
    if(length(lev_loc)) {
      l[['dataset']][[loc$data_loc]][['variables']][[loc$field_loc]][['factor']][[lev_loc]][['exclude']] <- true
      set <- TRUE
    }
  }
  if(!set) warning(sprintf('level [%s].[%s].[%s] not found', dataset, variable, level))
  l
}

#' Include Factor Level
#'
#' Include a level for a factor variable in the data set, by allowing a previously removed level.
#'
#' @param l data dictionary list
#' @param dataset name of data set
#' @param variable name of factor variable
#' @param level name of factor level to include
#'
#' @return updated data dictionary list
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' dd <- excludeLevel(dd, 'data', 'sex', 666)
#' dd <- unexcludeLevel(dd, 'data', 'sex', 666)
#' dd$dataset[[1]]$variables[[5]]$factor[[3]]$exclude
#'
#' @export

unexcludeLevel <- function(l, dataset, variable, level) {
  excludeLevel(l, dataset, variable, level, true = FALSE)
}
