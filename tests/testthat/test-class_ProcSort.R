
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
  transpile(test)
})


test_that("With out table", {
  # https://thesasreference.wordpress.com/category/les-procedures/proc-sort-proc/
  code_sas <-
  "proc sort data=sashelp.class out=class;
    by sex descending age descending name;
   run;"
  test <- ProcSort(code_sas)
  transpile(test)
})

## Old tests
test_that("Sort simple", {
  code_sas = "proc sort data=agregtva;
  by AA_CODET;
  run;"

  test <- ProcSort(code_sas)
  # expect_equal(sasr_sort(code_sas),
  #              "agregtva %>% arrange(AA_CODET)")
})


test_that("Sort descendant et out : exemple support sas", {
  code_sas = "proc sort data=account out =sorted;
  by town descending debt accountnumber;
  run;"

  test <- ProcSort(code_sas)
  # expect_equal(sasr_sort(code_sas),
  #              "sorted <- account %>% arrange(town, desc(debt), accountnumber)")
})
