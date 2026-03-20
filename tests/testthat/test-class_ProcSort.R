
test_that("Default", {
  sas_code <- "proc sort data = iris;
  by Sepal.Lenght ;
  run;"
  test <- ProcSort(sas_code)
  expect_equal(transpile(test), "iris %>%\n\tarrange(Sepal.Lenght)")
})


test_that("Descending", {
  sas_code <- "proc sort data = iris;
  by  descending Sepal.Lenght Petal.Lenght;
  run;"
  test <- ProcSort(sas_code)
  expect_equal(transpile(test),
               "iris %>%\n\tarrange(desc(Sepal.Lenght), Petal.Lenght)")
})


test_that("With out table", {
  # https://thesasreference.wordpress.com/category/les-procedures/proc-sort-proc/
  sas_code <-
  "proc sort data=sashelp.class out=class;
    by sex descending age descending name;
   run;"
  test <- ProcSort(sas_code)
  expect_equal(transpile(test),
               "class <- sashelp.class %>%\n\tarrange(sex, desc(age), desc(name))")
})

## With data specs ----
test_that("proc sort data specs", {
  sas_code <- "proc sort data = iris(where=(Species='viridis'));
  by  descending Sepal.Lenght Petal.Lenght;
  run;"
  test <- ProcSort(sas_code)
  expect_equal(transpile(test),
               "iris %>%\n\tfilter(Species == 'viridis') %>%\n\tarrange(desc(Sepal.Lenght), Petal.Lenght)")

})

## Old tests -----
test_that("Sort simple", {
  sas_code = "proc sort data=agregtva;
  by AA_CODET;
  run;"

  test <- ProcSort(sas_code)
  expect_equal(transpile(test),
               "agregtva %>%\n\tarrange(AA_CODET)")
})


test_that("Sort descendant et out : exemple support sas", {
  sas_code = "proc sort data=account out =sorted;
  by town descending debt accountnumber;
  run;"

  test <- ProcSort(sas_code)
  expect_equal(transpile(test),
               "sorted <- account %>%\n\tarrange(town, desc(debt), accountnumber)")
})



# With data specs ---------------------------------------------------------

test_that("Sort desc, out and data specs", {
  sas_code = "proc sort data=account(where debt>3000) out =sorted;
  by town descending debt accountnumber;
  run;"

  test <- ProcSort(sas_code)
  expect_equal(transpile(test),
               "sorted <- account %>%\n\tfilter(debt > 3000) %>%\n\tarrange(town, desc(debt), accountnumber)")
})
