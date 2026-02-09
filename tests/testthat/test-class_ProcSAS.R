test_that("proc contents no options", {
  sas_code <- "proc contents data=iris; run;"
  test <- ProcSAS(sas_code)
  expect_error(transpile(test), "Not implemented")
})


test_that("proc contents options", {
  sas_code <- "proc sort data=iris out = iris_sorted; run;"
  test <- ProcSAS(sas_code)
  expect_equal(test@proc_data, "iris")
  expect_equal(test@proc_options, "out=iris_sorted")
  expect_error(transpile(test), "Not implemented")
})


# TODO
# test_that("proc contents options with output conditions", {
#   sas_code <- 'PROC SORT DATA=Assurance
#     OUT=Assurance_F (LABEL="Nb années travaillées
#       par femme" DROP=SEXE) noduplicates;
#     BY NbAnnees ID ;
#     WHERE sexe=\'F\';
#     LABEL NbAnnees=\"Nombre d\'années travaillées\"
#     ID=\"n° d\'employé\";
#     RUN ;'
#   test <- ProcSAS(sas_code)
# })
