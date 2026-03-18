test_that("Where clause =", {
  data_statement <- "sales(where=(product='whizmo'))"
  test <- DataSpecs(data_statement)
  expect_equal(test@data, "sales")
  expect_equal(test@where, "product='whizmo'")
  expect_equal(test@keep, character(0))
  expect_equal(test@drop, character(0))
  expect_equal(transpile_data_specs(test), "sales %>%\n\tfilter(product == 'whizmo')")
})


test_that("keep clause", {
  data_statement <- "sashelp.class (keep=name weight height)"
  test <- DataSpecs(data_statement)
  expect_equal(test@data, "sashelp.class")
  expect_equal(test@where, character(0))
  expect_equal(test@keep, c("name", "weight", "height"))
  expect_equal(test@drop, character(0))

  expect_equal(transpile_data_specs(test), "sashelp.class %>%\n\tselect(name, weight, height)")
})

test_that("drop clause", {
  data_statement <- "sashelp.class (drop=name weight height)"
  test <- DataSpecs(data_statement)
  expect_equal(test@data, "sashelp.class")
  expect_equal(test@where, character(0))
  expect_equal(test@keep, character(0))
  expect_equal(test@drop, c("name", "weight", "height"))

  expect_equal(transpile_data_specs(test), "sashelp.class %>%\n\tselect(-name, -weight, -height)")
})

test_that("multi clauses", {
  data_statement <- "sashelp.class (drop=name  keep=weight height where = (height ge 14))"
  test <- DataSpecs(data_statement)
  expect_equal(test@data, "sashelp.class")
  expect_equal(test@where, "height ge 14")
  expect_equal(test@keep, c("weight", "height"))
  expect_equal(test@drop, c("name"))

  expect_equal(transpile_data_specs(test), "sashelp.class %>%\n\tfilter(height >= 14) %>%\n\tselect(weight, height, -name)")
})

test_that("unexcepted clause", {
  data_statement <- "sashelp.class(label='something' drop=name weight height)"
  test <- DataSpecs(data_statement)
  expect_equal(test@data, "sashelp.class")
  expect_equal(test@where, character(0))
  expect_equal(test@keep, character(0))
  expect_equal(test@drop, c("name", "weight", "height"))

  expect_equal(transpile_data_specs(test), "sashelp.class %>%\n\tselect(-name, -weight, -height)")
})

test_that("no clause", {
  data_statement <- "sashelp.class"
  test <- DataSpecs(data_statement)
  expect_equal(test@data, "sashelp.class")

  expect_equal(transpile_data_specs(test), "sashelp.class")
})

# Transpile only ----------------------------------------------------------



test_that("order output", {
  data_statement <- "mydata(
    keep=id age salary
    rename=(salary=income)
    where=(salary>50000)
)"
  test <- DataSpecs(data_statement)

  expect_equal(transpile_data_specs(test), "mydata %>%\n\tfilter(salary > 50000) %>%\n\tselect(id, age, salary) %>%\n\trename(income = salary)")
})

test_that("firstobs and obs", {
  data_statement <- "mydata(firstobs=10 obs=20)"
  test <- DataSpecs(data_statement)

  expect_equal(transpile_data_specs(test), "mydata %>%\n\tslice(10:20)")
})


test_that("where with function", {
  data_statement <- "mydata(where=(upcase(sex)=\"F\"))"
  test <- DataSpecs(data_statement)

  expect_equal(transpile_data_specs(test), "mydata %>%\n\tfilter(toupper(sex) == \"F\")")
})

test_that("where with missing", {
  data_statement <- "mydata(where=(age ne .))"
  test <- DataSpecs(data_statement)

  expect_equal(transpile_data_specs(test), "mydata %>%\n\tfilter(!is.na(age))")
})

test_that("rename multiple", {
  data_statement <- "mydata(rename=(age=age_years salary=income))"
  test <- DataSpecs(data_statement)

  expect_equal(transpile_data_specs(test), "mydata %>%\n\trename(age_years = age, income = salary)")
})
