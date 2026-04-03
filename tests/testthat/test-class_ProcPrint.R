test_that("proc contents no options", {
  sas_code <- "proc print data=iris; run;"
  test <- ProcPrint(sas_code)
  expect_equal(test@proc_data@data, "iris")
  expect_equal(transpile(test), "iris")
})


test_that("proc contents options whatever", {
  sas_code <- "proc print data=iris noobs; run;"
  test <- ProcPrint(sas_code)
  expect_equal(test@proc_data@data, "iris")
  expect_equal(test@proc_options, "noobs")
  expect_equal(transpile(test), "iris")
})
