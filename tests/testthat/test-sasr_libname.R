test_that("Conversion libname simple", {
  sas_code = "libname librairie \"C:\\Users\\chemin\\dossier\""
  expect_equal(sasr_libname(sas_code),
  "librairie <- \"C:/Users/chemin/dossier\"")
})

#
# test_that("libname avec option", {
#
# })
