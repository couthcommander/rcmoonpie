#' Data Dictionary to data.frame
#'
#' Convert data dictionary to a data.frame
#'
#' @param l data dictionary list
#'
#' @return data.frame
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' dd2df(dd)
#'
#' @export

dd2df <- function(l) {
  x <- variableList(l)
  y <- factorList(l)
  m <- merge(x, y, by = c('name','variable'), all.x = TRUE, suffixes = c('','.factor'))
  # restore variable order
  rowOrder1 <- unique(do.call(paste, c(x[,c('name','variable')], sep = '.')))
  rowOrder2 <- do.call(paste, c(m[,c('name','variable')], sep = '.'))
  m <- m[order(match(rowOrder2, rowOrder1)),]
  rownames(m) <- NULL
  m
}

#' Data Dictionary to R script
#'
#' Convert data dictionary to an R script
#'
#' @param l data dictionary list
#' @param file character string naming the file to print to; defaults to standard output
#' @param factorHandle character value of either \sQuote{duplicated},
#' \sQuote{unchanged}, \sQuote{changed}. Determines how to handle factor variables.
#'
#' @return character vector of R commands, returned invisibly
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' # \sQuote{duplicate} will create new factor variable with name \sQuote{variablename.factor}
#' dd2script(dd)
#' # \sQuote{unchanged} will leave variables as character strings
#' dd2script(dd, factorHandle = 'unchanged')
#' # \sQuote{changed} will replace character string with factor variable
#' dd2script(dd, factorHandle = 'changed')
#'
#' @export

dd2script <- function(l, file = "", factorHandle = c('duplicate','unchanged','changed')) {
# changing factorHandle is irrecoverable once written to script
  factorHandle <- match.arg(factorHandle)
  # use "hack" to build from data.frame
  if(identical(names(l), c('hack','x','y')) && l$hack == TRUE) {
    x <- l$x
    y <- l$y
  } else {
    x <- variableList(l)
    y <- factorList(l)
  }
  # exclude variable/factor with "exclude: yes"
  x2rm <- x[x[,'exclude'],]
  x <- x[!x[,'exclude'],]
  y <- y[!y[,'exclude'],]
  # also, exclude factor if part of `x2rm`
  rmkey1 <- do.call(paste, c(x2rm[,c('name','variable')], sep = '|'))
  rmkey2 <- do.call(paste, c(y[,c('name','variable')], sep = '|'))
  y <- y[!(rmkey2 %in% rmkey1),]
  d_col <- paste0(x[,'name'], '$', x[,'variable'])
  # variable labels
  out <- paste(sprintf('label(%s) = "%s"', d_col, x[,'label']), collapse = '\n')

  # variables to delete
  if(nrow(x2rm)) {
    rm_col <- paste0(x2rm[,'name'], '$', x2rm[,'variable'])
    out <- c(out, paste(sprintf('%s = NULL', rm_col), collapse = '\n'))
  }

  # factor labels
  if(factorHandle != 'unchanged') {
    yl <- split(y, y[,c('name','variable')])
    # restore original order
    rowOrder <- unique(do.call(paste, c(y[,c('name','variable')], sep = '.')))
    yl <- yl[match(rowOrder, names(yl))]
    lev_l <- lapply(yl, `[[`, 'level')
    lab_l <- lapply(yl, `[[`, 'label')
    levs <- vapply(lev_l, function(i) sprintf('c("%s")', paste(i, collapse = '","')), character(1))
    labs <- vapply(lab_l, function(i) sprintf('c("%s")', paste(i, collapse = '","')), character(1))
    # split returns "name.variable"
    id1 <- do.call(paste, c(y[,c('name','variable')], sep = '.'))
    # convert to "name$variable"
    id2 <- do.call(paste, c(y[,c('name','variable')], sep = '$'))
    o_col <- id2[match(names(yl), id1)]
    if(factorHandle == 'duplicate') {
      n_col <- paste0(o_col, '.factor')
    } else {
      n_col <- o_col
    }
    toFactor <- paste(sprintf('%s = factor(%s, levels = %s)', n_col, o_col, levs), collapse = '\n')
    setFactorLabs <- paste(sprintf('levels(%s) = %s', n_col, labs), collapse = '\n')
    out <- c(out, toFactor, setFactorLabs)
  }
  cat(paste(out, collapse = '\n\n'), file = file)
  cat('\n', file = file, append = TRUE)
  invisible(out)
}

#' Data Dictionary to CSV file
#'
#' Convert data dictionary to a CSV file
#'
#' @param l data dictionary list
#' @param file character string naming the file to print to; defaults to standard output
#' @param \dots Additional arguments, passed to \code{\link[utils]{write.csv}}.
#'
#' @return creates CSV file
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' dd2csv(dd)
#'
#' @export

dd2csv <- function(l, file = '', ...) {
  utils::write.csv(dd2df(l), file, row.names = FALSE, ...)
}

#' Data Dictionary to YAML file
#'
#' Convert data dictionary to a YAML file
#'
#' @param l data dictionary list
#' @param file character string naming the file to print to; defaults to standard output
#' @param \dots Additional arguments, passed to \code{\link[yaml]{write_yaml}}.
#'
#' @return creates CSV file
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' dd2yaml(dd)
#'
#' @export

dd2yaml <- function(l, file = stdout(), ...) {
  yaml::write_yaml(l, file)
}
