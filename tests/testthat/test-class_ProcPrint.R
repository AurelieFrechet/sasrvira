test_that("proc contents no options", {
  code_sas <- "proc print data=iris; run;"
  test <- proc_print(code_sas)
  expect_equal(test@pp_data, "iris")
  expect_equal(transpile3(test), "iris")
})


test_that("proc contents options whatever", {
  code_sas <- "proc print data=iris noobs; run;"
  test <- proc_print(code_sas)
  expect_equal(test@pp_data, "iris")
  expect_equal(test@pp_options, "noobs")
  expect_equal(transpile3(test), "iris")
})
