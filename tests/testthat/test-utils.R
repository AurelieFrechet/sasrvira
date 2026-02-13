
# data_equal_to -----------------------------------------------------------

test_that("data equal to sans librairie", {
code_sas = "proc contents data=table_1;run;"
expect_equal(data_equal_to(code_sas),
             "table_1")

})

test_that("data equal to avec librairie", {
  code_sas = "proc contents data=lib4.table_1;run;"
  expect_equal(
    data_equal_to(code_sas),
    "file.path(lib4, \"table_1\")"
  )

})



