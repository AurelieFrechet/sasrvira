test_that("test sur iris", {
  code_sas <- "proc contents data=iris; run;
  proc sql;
  select * from iris where Species=\"setosa\";
  quit;"
  expect_equal(transastor(code_sas), "str(iris)\n  iris %>%\n\tfilter(Species == \"setosa\")")

})

test_that("test boulot", {
  code_sas = "PROC SQL;
  select rB010, rB030, RB080, RB0808F
  from table
  where (RB080 not between 1890 and 2018 and RB080 not = .)
  order by rb010, rB030;
  QUIT;"
  transastor(code_sas)
})


test_that("vide", {
  code_sas <- ""
  expect_equal(transastor(code_sas), "")

})

test_that("code non reconnu", {
  code_sas <- "code non reconnu"
  expect_equal(transastor(code_sas), "code non reconnu")

})

test_that("proc means", {
  code_sas = "PROC MEANS DATA=TrialSorted;      BY Sex;      VAR Age; RUN;"
  expect_equal(
    transastor(code_sas),
    "TrialSorted %>%\n\tgroup_by(Sex) %>%\n\tselect(Age) %>%\n\tsummary()"
  )
})



# TODO
# test_that("sasr_sql : code de William", {
#   code_sas <- "Proc sql noprint;
#   select date as date format yyddmm8., field1, length(field2), catx('|',field3,field4) as concat
#   from my_lib.my_table
#   order by date;
# quit;"
#   transastor(code_sas)
#   "my_lib.my_table %>%\n\tmutate(date = date, length(field2), concat = paste(field3, field4, sep = \"|\")) %>%\n\tselect(date, field1, length(field2), concat))"
# })
