test_that("proc means : multiple by and class and no indics", {
  code_sas = "proc means data = diamonds n means stddev; var carat; by color; class cut; run;"
  test <- proc_means(code_sas)

  ## Correct properties
  expect_equal(test@data, "diamons")

  expect_equal(test@stats, "default")
  expect_equal(test@var, "carat")
  expect_equal(test@by, "color")
  expect_equal(test@class, "cut")
  expect_equal(test@weight, NULL)

  ## Transpile method
  expect_equal(
    transpile(test),
    "diamonds %>%\n\tgroup_by(color, cut) %>%\n\tselect(carat) %>%\n\tsummary()"
  )
})
