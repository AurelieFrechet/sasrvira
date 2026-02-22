test_that("ProcFreq constructor parses PROC FREQ statements correctly", {

  sas_code <- "
  proc freq data=mydata;
    by site;
    tables sex*group;
    exact fisher;
    test chisq;
    weight wgt;
    output out=freq_out;
  run;
  "

  proc <- ProcFreq(sas_code)

  # PROC FREQ specific fields
  expect_equal(proc@pf_by,     "site")
  expect_equal(proc@pf_tables, list(expand.grid("sex", "group")))
  expect_equal(proc@pf_exact,  "fisher")
  expect_equal(proc@pf_test,   "chisq")
  expect_equal(proc@pf_weight, "wgt")
  expect_equal(proc@pf_output, "freq_out")

  test <- transpile(proc)
  expect_equal(test, "freq_out <- table(mydata$sex, mydata$group)")
})



test_that("ProcFreq handles missing optional statements", {

  sas_code <- "
  proc freq data=mydata;
    tables x;
  run;
  "

  proc <- ProcFreq(sas_code)

  expect_equal(proc@pf_tables, list(expand.grid("x")))

  expect_equal(proc@pf_by,     character(0))
  expect_equal(proc@pf_exact,  character(0))
  expect_equal(proc@pf_test,   character(0))
  expect_equal(proc@pf_weight, character(0))
  expect_equal(proc@pf_output, character(0))

  test <- transpile(proc)
  expect_equal(test, "table(mydata$x)")
})

test_that("Multiple tables", {
  sas_code <- "proc freq data=Color;
   tables Eyes Hair Eyes*Hair;
run;
"
  proc <- ProcFreq(sas_code)

  test <- transpile(proc)
  expect_equal(test, "table(Color$Eyes)\ntable(Color$Hair)\ntable(Color$Eyes, Color$Hair)")
})


test_that("Multiple tables + output", {
  sas_code <- "proc freq data=Color;
   tables Eyes Hair Eyes*Hair;
   output out= eyes hair eyes_hair;
run;
"
  proc <- ProcFreq(sas_code)

  test <- transpile(proc)
  expect_equal(test, "eyes <- table(Color$Eyes)\nhair <- table(Color$Hair)\neyes_hair <- table(Color$Eyes, Color$Hair)")
})


# tables_pairs ------------------------------------------------------------

test_that("pairs_tables deal with white spaces A*B",{
  expect_equal(pairs_tables("A*B"), list(expand.grid("A", "B")))
  expect_equal(pairs_tables("A* B"), list(expand.grid("A", "B")))
  expect_equal(pairs_tables("A *  B"), list(expand.grid("A", "B")))
})


test_that("pairs_tables A * (B D)",{
  expect_equal(pairs_tables("A * (B D)"), list(expand.grid("A", c("B", "D"))))
  expect_equal(pairs_tables("A*(B C D)"), list(expand.grid("A", c("B", "C", "D"))))
})

test_that("pairs_tables A B  C",{
  expect_equal(pairs_tables("A B  C"), list(expand.grid("A"), expand.grid("B"), expand.grid("C")))
  expect_equal(pairs_tables("A*B C* D"), list(expand.grid("A", "B"), expand.grid("C", "D")))
})

# test_that("pairs_tables (A --C)",{
#   expect_equal(pairs_tables("(A --C)"))
#   expect_equal(pairs_tables("(A - - D)*C"))
# })

# tables_pairs (SAS documentations)------------------------------------------------------------

test_that("pairs_tables A*(B C)", {
  expect_equal(pairs_tables("A*(B C)"), list(expand.grid("A", c("B", "C"))))
})

test_that("pairs_tables (A B)*(C D)",{
  expect_equal(pairs_tables("(A B)*(C D)"), list(expand.grid(c("A", "B"), c("C", "D"))))
})

test_that("pairs_tables (A B C)*D",{
  expect_equal(pairs_tables("(A B C)*D"), list(expand.grid(c("A", "B", "C"), c("D"))))
})

# TODO: Aside for now
# test_that("pairs_tables A - - C)",{
#   expect_equal(pairs_tables("A - - C"), "A B C")
# })
#
# test_that("pairs_tables (A - - C)*D",{
#   expect_equal(pairs_tables("(A - - C)*D"), "A*D B*D C*D")
# })


# test_that("tables [...] / options", {
#   sas_code <- "proc freq data=Color;
#    tables Eyes Hair Eyes*Hair / out=FreqCount outexpect sparse;
#    weight Count;
# run;
# "
#   test <- ProcFreq(sas_code)
# })
