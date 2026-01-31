
test_that("Default", {
  code_sas <- "proc sort data = iris;
  by Sepal.Lenght ;
  run;"
  test <- ProcSort(code_sas)
  transpile(test)
})


test_that("Descending", {
  code_sas <- "proc sort data = iris;
  by  descending Sepal.Lenght Petal.Lenght;
  run;"
  test <- ProcSort(code_sas)
  expect_equal(transpile(test),
               "iris %>%\n\tarrange(desc(Sepal.Lenght), Petal.Lenght)")
})


test_that("With out table", {
  # https://thesasreference.wordpress.com/category/les-procedures/proc-sort-proc/
  code_sas <-
  "proc sort data=sashelp.class out=class;
    by sex descending age descending name;
   run;"
  test <- ProcSort(code_sas)
  expect_equal(transpile(test),
               "class <- sashelp.class %>%\n\tarrange(sex, desc(age), desc(name))")
})

## Old tests -----
test_that("Sort simple", {
  code_sas = "proc sort data=agregtva;
  by AA_CODET;
  run;"

  test <- ProcSort(code_sas)
  expect_equal(transpile(test),
               "agregtva %>%\n\tarrange(AA_CODET)")
})


test_that("Sort descendant et out : exemple support sas", {
  code_sas = "proc sort data=account out =sorted;
  by town descending debt accountnumber;
  run;"

  test <- ProcSort(code_sas)
  expect_equal(transpile(test),
               "sorted <- account %>%\n\tarrange(town, desc(debt), accountnumber)")
})
