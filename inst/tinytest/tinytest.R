library(rcmoonpie)

test1 <- script2info(system.file("examples", "ex_script.R", package = "rcmoonpie"))
expect_equal(length(test1), 1)
expect_equal(names(test1$dataset[[1]]), c('name','variables'))
expect_equal(length(test1$dataset[[1]]$variables), 8)
expect_equal(names(test1$dataset[[1]]$variables[[2]]), c('name','label','factor'))
expect_equal(length(test1$dataset[[1]]$variables[[4]]$factor), 2)
expect_equal(names(test1$dataset[[1]]$variables[[4]]$factor[[1]]), c('label','level'))
test1 <- excludeVar(test1, 'data', 'redcap_event_name')
expect_equal(names(test1$dataset[[1]]$variables[[2]]), c('name','label','factor','exclude'))
test1 <- unexcludeVar(test1, 'data', 'redcap_event_name')
expect_equal(test1$dataset[[1]]$variables[[2]]$exclude, FALSE)
test1 <- excludeVar(test1, 'data', 'redcap_event_name')
expect_equal(test1$dataset[[1]]$variables[[2]]$exclude, TRUE)
test1 <- excludeLevel(test1, 'data', 'sex', 666)
expect_equal(names(test1$dataset[[1]]$variables[[5]]$factor[[3]]), c('label','level','exclude'))
expect_equal(test1$dataset[[1]]$variables[[5]]$factor[[3]]$exclude, TRUE)
td <- tempdir()
dd2yaml(test1, file.path(td, 'test1.yaml'))
dd2csv(test1, file.path(td, 'test1.csv'))
test2 <- yaml2info(file.path(td, 'test1.yaml'))
test3 <- csv2info(file.path(td, 'test1.csv'))
test4 <- df2info(dd2df(test1))
# df > info should equal info
expect_equal(test1, test4)
# df > info > df should equal df
expect_equal(dd2df(test1), dd2df(test4))
# script and yaml input should equal
expect_equal(test1, test2)
# script and CSV input should equal
expect_equal(test1, test3)
# check factor name
expect_equal(sub(' .*', '', capture.output(dd2script(test1, ''))[[11]]), 'data$randomization.factor')
# check factor name
expect_equal(sub(' .*', '', capture.output(dd2script(test2, '', 'changed'))[[11]]), 'data$randomization')
# exclude factor modifications
expect_equal(length(capture.output(dd2script(test3, '', 'unchanged'))), 9)
