# transform conditions ----------------------------------------------------

test_that("Equations", {
  expr <-
    "var1 > 1 and var2 = \"frechet\" and var <> \"vide\""
  expect_equal(transform_conditions(expr),
               "var1 > 1 & var2 == \"frechet\" & var != \"vide\"")
})

test_that("conditions", {
  expect_equal(transform_conditions("utilisateur_id IS NOT NULL"), "!is.na(utilisateur_id)")
})

test_that("transform_conditions handles NULL checks", {
  x <- "x is null"
  expect_equal(
    transform_conditions(x),
    "is.na(x)"
  )
})

test_that("transform_conditions handles comparison operators", {
  x <- "a ne b"
  expect_equal(
    transform_conditions(x),
    "a != b"
  )
})

test_that("transform_conditions handles IN clauses", {
  x <- "x in (1,2,3)"
  expect_equal(
    transform_conditions(x),
    "x %in% c(1,2,3)"
  )
})

# transform functions -----------------------------------------------------

test_that("transform_casewhen builds valid case_when syntax", {
  x <- "case when x > 0 then 1 else 0 end"
  res <- transform_case_when(x)

  expect_true(grepl("case_when", res))
  expect_true(grepl("TRUE ~ 0", res))
})

test_that("transform_functions replaces SQL functions", {
  x <- "avg(x) + count(*)"
  res <- transform_functions(x)

  expect_true(grepl("mean", res))
  expect_true(grepl("n\\(\\)", res))
})


# transform list ----------------------------------------------------------

test_that("liste character", {
  expr <- "{l1 l2 l3 l4 l5 }"
  expect_equal(
    transform_list(expr),
    "c(\"l1\", \"l2\", \"l3\", \"l4\", \"l5\")"
  )

})

test_that("liste numerique", {
  expr <- "{1 2 3 4 5 }"
  expect_equal(
    transform_list(expr),
    "c(1, 2, 3, 4, 5)"
  )

})



# transform path ----------------------------------------------------------

test_that("test de chemin SAS", {
  expr <- "M:\\Chemin\\Dossier\\Sous-dossier\\fichier.xls"
  expect_equal(transform_path(expr),
               "M:/Chemin/Dossier/Sous-dossier/fichier.xls")
})
