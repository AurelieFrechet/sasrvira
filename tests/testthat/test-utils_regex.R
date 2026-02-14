test_that("regex_remove removes matched patterns", {
  x <- "abc123def"
  expect_equal(
    regex_remove(x, "[0-9]+"),
    "abcdef"
  )
})

test_that("regex_replace replaces matched patterns", {
  x <- "abc123def"
  expect_equal(
    regex_replace(x, "[0-9]+", "_"),
    "abc_def"
  )
})

test_that("regex_match_groups extracts capture groups", {
  x <- "x=10;y=20"
  res <- regex_match_groups(x, "(x|y)=(\\d+)")

  expect_type(res, "list")
  expect_length(res, 2)
  expect_equal(res[[1]], c("x", "y"))
  expect_equal(res[[2]], c("10", "20"))
})

test_that("regex_match_groups returns NULL when no match", {
  x <- "no numbers here"
  expect_null(
    regex_match_groups(x, "(\\d+)")
  )
})

test_that("regex_count_matches counts occurrences correctly", {
  x <- "a1 b2 c3"
  expect_equal(
    regex_count_matches(x, "(\\d)"),
    3
  )
})

test_that("regex_locate_matches returns correct positions", {
  x <- "abc123def456"
  res <- regex_locate_matches(x, "\\d+")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$start, c(4, 10))
  expect_equal(res$end, c(6, 12))
})

test_that("regex_locate_matches returns NULL when no match", {
  x <- "abcdef"
  expect_null(
    regex_locate_matches(x, "\\d+")
  )
})
