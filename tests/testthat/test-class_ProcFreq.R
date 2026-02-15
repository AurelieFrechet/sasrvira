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

  # ---- class & inheritance ----
  expect_s7_class(proc, ProcFreq)
  expect_s7_class(proc, ProcSAS)

  # ---- inherited ProcSAS fields ----
  expect_equal(proc@proc_data, "mydata")
  expect_true(is.character(proc@source))
  expect_true(nchar(proc@source) > 0)

  # ---- PROC FREQ specific fields ----
  expect_equal(proc@pf_by,     "site")
  expect_equal(proc@pf_tables, "sex*group")
  expect_equal(proc@pf_exact,  "fisher")
  expect_equal(proc@pf_test,   "chisq")
  expect_equal(proc@pf_weight, "wgt")
  expect_equal(proc@pf_output, "freq_out")
})



test_that("ProcFreq handles missing optional statements", {

  sas_code <- "
  proc freq data=mydata;
    tables x;
  run;
  "

  proc <- ProcFreq(sas_code)

  expect_equal(proc@pf_tables, "x")

  expect_equal(proc@pf_by,     character(0))
  expect_equal(proc@pf_exact,  character(0))
  expect_equal(proc@pf_test,   character(0))
  expect_equal(proc@pf_weight, character(0))
  expect_equal(proc@pf_output, character(0))
})



# tables_pairs ------------------------------------------------------------

test_that("pairs_table A*B",{
  expect_equal(pairs_table("A*B"), "A*B")
  expect_equal(pairs_table("A* B"), "A*B")
  expect_equal(pairs_table("A *  C", ))
})


test_that("pairs_table A * (B D)",{
  expect_equal(pairs_table("A * (B D)", ))
  expect_equal(pairs_table("A*(B C D)", ))
})

test_that("pairs_table A B  C",{
  expect_equal(pairs_table("A B  C", ))
  expect_equal(pairs_table("A*B C* D", ))
})

test_that("pairs_table (A --C)",{
  expect_equal(pairs_table("(A --C)", ))
  expect_equal(pairs_table("(A - - D)*C", ))
})

# tables_pairs (SAS documentations)------------------------------------------------------------

test_that("pairs_table A*(B C)",{
  expect_equal(pairs_table("A*(B C)"), "A*B A*C")
})

test_that("pairs_table (A B)*(C D)",{
  expect_equal(pairs_table("(A B)*(C D)"), "A*C B*C A*D B*D")
})

test_that("pairs_table (A B C)*D",{
  expect_equal(pairs_table("(A B C)*D"), "A*D B*D C*D")
})

# TODO: Aside for now
# test_that("pairs_table A - - C)",{
#   expect_equal(pairs_table("A - - C"), "A B C")
# })
#
# test_that("pairs_table (A - - C)*D",{
#   expect_equal(pairs_table("(A - - C)*D"), "A*D B*D C*D")
# })


# test_that("tables [...] / options", {
#   sas_code <- "proc freq data=Color;
#    tables Eyes Hair Eyes*Hair / out=FreqCount outexpect sparse;
#    weight Count;
# run;
# "
#   test <- ProcFreq(sas_code)
# })
