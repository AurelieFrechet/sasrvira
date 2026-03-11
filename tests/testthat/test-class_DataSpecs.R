test_that("Where clause =", {
  data_statement <- "sales(where=(product='whizmo'))"
  test <- DataSpecs(data_statement)
  expect_equal(test@data, "sales")
  expect_equal(test@where, "product='whizmo'")
  expect_equal(test@keep, character(0))
  expect_equal(test@drop, character(0))
  expect_equal(transpile_data_specs(test), "sales %>%\nfilter(product == 'whizmo')")
})


test_that("keep clause", {
  data_statement <- "sashelp.class (keep=name weight height)"
  test <- DataSpecs(data_statement)
  expect_equal(test@data, "sashelp.class")
  expect_equal(test@where, character(0))
  expect_equal(test@keep, c("name", "weight", "height"))
  expect_equal(test@drop, character(0))

  expect_equal(transpile_data_specs(test), "sashelp.class %>%\nselect(name, weight, height)")
})

test_that("drop clause", {
  data_statement <- "sashelp.class (drop=name weight height)"
  test <- DataSpecs(data_statement)
  expect_equal(test@data, "sashelp.class")
  expect_equal(test@where, character(0))
  expect_equal(test@keep, character(0))
  expect_equal(test@drop, c("name", "weight", "height"))

  expect_equal(transpile_data_specs(test), "sashelp.class %>%\nselect(-name, -weight, -height)")
})

