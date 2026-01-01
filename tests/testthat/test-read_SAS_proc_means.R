#  source : https://www.listendata.com/2015/01/sas-detailed-explanation-of-proc-means.html

test_that("Simple Example", {
 text = c("data = test",
          "var q1")
 list(data = "test",
      var = "q1")
})

test_that("Listing var", {
  text = c("data = test n nmiss",
           "var q1 - q5")

  list(data = "test",
       stats = c("n", "nmiss"),
       var= c("q1", "q2", "q3", "q4", "q5"))
})

test_that("Change Sorting Order", {
  text = c("data = test",
           "class age / descending",
           "var q1")
  list(data = "test",
       by = c("age"),
       by_options = c("descending"),
       var = "q1")
})

test_that("types with class", {
  c("data=grade maxdec=3",
    "var Score",
    "class Status Year",
    "types () status*year")
  list(data = "test",
       by = c("Status", "Year"),
       var = "Score")
})




