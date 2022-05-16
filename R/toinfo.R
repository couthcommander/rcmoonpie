#' data.frame to Data Dictionary
#'
#' Convert data.frame to data dictionary
#'
#' @param df data.frame with variable meta-data
#'
#' @return list
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' df <- dd2df(dd)
#' dd_alt <- df2info(df)
#' all.equal(dd, dd_alt)
#'
#' @export

df2info <- function(df) {
#   x_vars <- c('name','variable','label')
#   y_vars <- c('name','variable','level','label.factor')
#   if('exclude' %in% names(df)) x_vars <- c(x_vars, 'exclude')
#   if('exclude.factor' %in% names(df)) y_vars <- c(y_vars, 'exclude.factor')
#   x <- unique(df[, x_vars])
#   y <- df[!is.na(df[,'level']), y_vars]
#   names(y)[4] <- 'label'
#   names(y)[grep('exclude.factor', names(y))] <- 'exclude'
#   ll <- list(hack = TRUE, x = x, y = y)
#   txt_info <- capture.output(dd2script(ll, file = '', factorHandle))
#   script2info(txt = txt_info)
  has_exclude <- 'exclude' %in% names(df)
  has_factor_exc <- 'exclude.factor' %in% names(df)
  x_vars <- c('name','variable','label')
  y_vars <- c('name','variable','level','label.factor')
  if(has_exclude) x_vars <- c(x_vars, 'exclude')
  if(has_factor_exc) y_vars <- c(y_vars, 'exclude.factor')
  x <- unique(df[, x_vars])
  step3 <- lapply(split(x[, x_vars], x[,'name']), function(i) {
    if(has_exclude) do_var_exclude <- i[,'exclude'] == TRUE
    ll2 <- lapply(seq(nrow(i)), function(j) {
      ll <- list(name = i[j,'variable'], label = i[j,'label'])
      if(has_exclude && do_var_exclude[j]) ll$exclude <- TRUE
      ll
    })
    list(name = i[1,'name'], variables = ll2)
  })
  names(step3) <- NULL
  step4 <- list(dataset = step3)

  y <- df[!is.na(df[,'level']), y_vars]
  names(y)[4] <- 'label'
  names(y)[grep('exclude.factor', names(y))] <- 'exclude'
  ys <- split(y, y[,c('name','variable')])
  for(i in seq_along(ys)) {
    loc <- findVariable(step4, ys[[i]][1,'name'], ys[[i]][1,'variable'])
    if(has_factor_exc) do_var_exclude <- ys[[i]][,'exclude'] == TRUE
    if(!is.na(loc$field_loc)) {
      f_dat <- lapply(seq(nrow(ys[[i]])), function(j) {
        ll <- list(label = ys[[i]][j,'label'], level = ys[[i]][j,'level'])
        if(has_factor_exc && do_var_exclude[j]) ll$exclude <- TRUE
        ll
      })
      tmp <- step4[['dataset']][[loc$data_loc]][['variables']][[loc$field_loc]]
      tmp[['factor']] <- f_dat
      # not necessary, but preserve expected element order
      if('exclude' %in% names(tmp)) {
        tmp <- tmp[c('name','label','factor','exclude')]
      }
      step4[['dataset']][[loc$data_loc]][['variables']][[loc$field_loc]] <- tmp
    }
  }
  step4
}

#' CSV file to Data Dictionary
#'
#' Convert CSV file to a data dictionary
#'
#' @param file character string naming the file to read with \code{\link[utils]{read.csv}}
#' @param \dots Additional arguments, passed to \code{\link[utils]{read.csv}}.
#'
#' @return list
#'
#' @examples
#' dd <- csv2info(system.file("examples", "ex_script.csv", package = "rcmoonpie"))
#' dd2df(dd)
#'
#' @export

csv2info <- function(file, ...) {
  df <- read.csv(file, ...)
  df2info(df)
}

#' R script to Data Dictionary
#'
#' Convert R script to a data dictionary
#'
#' @param file character string naming the file to read with \code{\link{scan}}
#' @param txt character string used as an alternative to reading
#'
#' @return list
#'
#' @examples
#' dd <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
#' dd2df(dd)
#'
#' @export

script2info <- function(file, txt = NULL) {
  if(missing(file) && !is.null(txt)) {
    ex <- txt
  } else {
    ex <- scan(file, '', sep = '\n', quiet = TRUE)
  }
  var_labels <- grep('^label\\(', ex)
  var_factors <- grep('factor\\(', ex)
  factor_levels <- grep('^levels\\(', ex)
  v1_a <- sub('^label\\((.*)[$](.*))[ ]*[=][ ]*"(.*)"$', '\\1', ex[var_labels])
  v1_b <- sub('^label\\((.*)[$](.*))[ ]*[=][ ]*"(.*)"$', '\\2', ex[var_labels])
  v1_c <- sub('^label\\((.*)[$](.*))[ ]*[=][ ]*"(.*)"$', '\\3', ex[var_labels])

  v2_a <- sub('^(.*)[$](.*)[.]factor[ ]*[=][ ]*factor\\((.*)[$](.*)[,][ ]*levels[ ]*=[ ]*c\\((.*)))$', '\\1', ex[var_factors])
  v2_b <- sub('^(.*)[$](.*)[.]factor[ ]*[=][ ]*factor\\((.*)[$](.*)[,][ ]*levels[ ]*=[ ]*c\\((.*)))$', '\\2', ex[var_factors])
  v2_c <- sub('^(.*)[$](.*)[.]factor[ ]*[=][ ]*factor\\((.*)[$](.*)[,][ ]*levels[ ]*=[ ]*c\\((.*)))$', '\\3', ex[var_factors])
  v2_d <- sub('^(.*)[$](.*)[.]factor[ ]*[=][ ]*factor\\((.*)[$](.*)[,][ ]*levels[ ]*=[ ]*c\\((.*)))$', '\\4', ex[var_factors])
  v2_e <- sub('^(.*)[$](.*)[.]factor[ ]*[=][ ]*factor\\((.*)[$](.*)[,][ ]*levels[ ]*=[ ]*c\\((.*)))$', '\\5', ex[var_factors])
  v2_f <- strsplit(gsub('^"|"$', '', gsub('",[ ]*"', '~:~', v2_e)), '~:~')

  v3_a <- sub('^levels\\((.*)[$](.*)[.]factor)[ ]*=[ ]*c\\((.*))$', '\\1', ex[factor_levels])
  v3_b <- sub('^levels\\((.*)[$](.*)[.]factor)[ ]*=[ ]*c\\((.*))$', '\\2', ex[factor_levels])
  v3_c <- sub('^levels\\((.*)[$](.*)[.]factor)[ ]*=[ ]*c\\((.*))$', '\\3', ex[factor_levels])
  v3_d <- strsplit(gsub('^"|"$', '', gsub('",[ ]*"', '~:~', v3_c)), '~:~')

  my_list1 <- function(n, l) list(name = n, label = l)
  my_list2 <- function(l1, l2) list(label = l1, level = l2)
  step1 <- mapply(my_list1, v1_b, v1_c, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  step2 <- split(step1, v1_a)
  step3 <- lapply(seq_along(step2), function(i) {
    list(name = names(step2)[i], variables = step2[[i]])
  })
  step4 <- list(dataset = step3)

  for(i in seq_along(v3_b)) {
    loc <- findVariable(step4, v3_a[i], v3_b[i])
    if(!is.na(loc$field_loc)) {
      f_dat <- mapply(my_list2, v3_d[[i]], v2_f[[i]], SIMPLIFY = FALSE, USE.NAMES = FALSE)
      step4[['dataset']][[loc$data_loc]][['variables']][[loc$field_loc]][['factor']] <- f_dat
    }
  }
  step4
}

#' YAML file to Data Dictionary
#'
#' Convert YAML file to a data dictionary
#'
#' @param file character string naming the file to read with \code{\link[yaml]{yaml.load_file}}
#'
#' @return list
#'
#' @examples
#' dd <- yaml2info(system.file("examples", "ex_script.yaml", package = "rcmoonpie"))
#' dd2df(dd)
#'
#' @export

yaml2info <- function(file) {
  yaml::yaml.load_file(file)
}
