test_that("proc contents no options", {
  code_sas <- "proc contents data=iris; run;"
  test <- ProcContents(code_sas)
  expect_equal(transpile2(test), "str(iris)")
})


test_that("proc contents options whatever", {
  code_sas <- "proc contents data=iris noprint; run;"
  test <- ProcContents(code_sas)
  expect_equal(transpile2(test), "str(iris)")
})
