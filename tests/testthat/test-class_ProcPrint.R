test_that("proc contents no options", {
  code_sas <- "proc print data=iris; run;"
  test <- ProcPrint(code_sas)
  expect_equal(test@proc_data, "iris")
  expect_equal(transpile(test), "iris")
})


test_that("proc contents options whatever", {
  code_sas <- "proc print data=iris noobs; run;"
  test <- ProcPrint(code_sas)
  expect_equal(test@proc_data, "iris")
  expect_equal(test@proc_options, "noobs")
  expect_equal(transpile(test), "iris")
})
