test_that("inside a froq means", {
  sas_query <- "VAR var1 var2 var3 var4 var5 var6 ;
CLASS var2 ;
BY var7 ;
OUTPUT OUT=nomtab_output;"

  expect_equal(test <- split_sas_args(sas_query),
  list(var    = "var1 var2 var3 var4 var5 var6",
       class  = "var2",
       by     = "var7",
       output = "OUT=nomtab_output"))
})
