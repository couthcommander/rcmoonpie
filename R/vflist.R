#' Variable Information
#'
#' Data.frame with variable meta-data
#'
#' @param l data dictionary list
#'
#' @return data.frame
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' variableList(dd)
#'
#' @export

variableList <- function(l) {
  ll <- lapply(l[['dataset']], function(i) {
    df <- data.frame(
      name = i[['name']],
      variable = vapply(i[['variables']], `[[`, character(1), "name"),
      label = vapply(i[['variables']], `[[`, character(1), "label")
    )
    df[,'exclude'] <- FALSE
    excl_opt <- lapply(i[['variables']], `[[`, "exclude")
    excl_ix <- which(lengths(excl_opt) == 1)
    if(length(excl_ix) > 0L) {
      # "excl_opt" all have length 1, so safe to unlist
      excl_ix <- excl_ix[unlist(excl_opt[excl_ix])]
#       df <- df[setdiff(seq(nrow(df)), excl_ix),]
      df[excl_ix, 'exclude'] <- TRUE
    }
    df
  })
  do.call(rbind, ll)
}

#' Factor Variable Information
#'
#' Data.frame with meta-data for factor variables
#'
#' @param l data dictionary list
#'
#' @return data.frame
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' factorList(dd)
#'
#' @export

factorList <- function(l) {
  sf_factors <- function(x) {
    if('factor' %in% names(x)) {
      l1 <- lapply(x[['factor']], `[[`, "level")
      l2 <- lapply(x[['factor']], `[[`, "label")
      l3 <- logical(length(l1))
      excl_opt <- lapply(x[['factor']], `[[`, "exclude")
      excl_ix <- which(lengths(excl_opt) == 1)
      if(length(excl_ix) > 0L) {
        excl_ix <- excl_ix[excl_opt[[excl_ix]]]
        l3[excl_ix] <- TRUE
      }
    } else {
      l1 <- l2 <- NA
      # if no factors, "fake" exclude
      l3 <- TRUE
    }
    data.frame(variable = x[['name']], level = unlist(l1), label = unlist(l2), exclude = l3)
  }
  ll <- lapply(l[['dataset']], function(i) {
    df <- cbind(name = i[['name']], do.call(rbind, lapply(i[['variables']], sf_factors)))
#     df[!df[,'exclude'], c('name','variable','level','label')]
    df
  })
  do.call(rbind, ll)
}
