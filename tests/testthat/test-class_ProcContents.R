test_that("proc contents no options", {
  sas_code <- "proc contents data=iris; run;"
  test <- ProcContents(sas_code)
  expect_equal(transpile(test), "str(iris)")
})


test_that("proc contents options whatever", {
  sas_code <- "proc contents data=iris noprint; run;"
  test <- ProcContents(sas_code)
  expect_equal(transpile(test), "str(iris)")
})
