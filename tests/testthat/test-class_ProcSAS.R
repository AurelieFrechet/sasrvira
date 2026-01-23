test_that("proc contents no options", {
  code_sas <- "proc contents data=iris; run;"
  test <- ProcSAS(code_sas)
  expect_error(transpile2(test), "Not implemented")
})


