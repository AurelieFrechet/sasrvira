test_that("proc contents no options", {
  sas_code <- "proc contents data=iris; run;"
  test <- ProcContents(sas_code)
  expect_equal(transpile(test), "iris %>%\n\tstr()")
})


test_that("proc contents options whatever", {
  sas_code <- "proc contents data=iris(where = (Species = 'visicolor')) noprint; run;"
  test <- ProcContents(sas_code)
  expect_equal(transpile(test), "iris %>%\n\tfilter(Species == 'visicolor') %>%\n\tstr()")
})
