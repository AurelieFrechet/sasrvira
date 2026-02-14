#' Extract DATA= target from SAS code
#'
#' @description
#' Detects the value assigned to a SAS \code{DATA=} statement and returns it.
#' If the dataset is qualified with a SAS library (e.g. \code{lib.dataset}),
#' it is converted into an R-compatible \code{file.path()} expression.
#'
#' @param sas_code Character string containing SAS code.
#'
#' @return
#' A character string representing the dataset name. If a SAS library is
#' specified, the result is a \code{file.path()} call combining the library
#' and dataset name.
#'
#' @keywords internal
data_equal_to <- function(sas_code){
  data_equal <- regex_match_groups(x = sas_code,
            pattern = "data\\s?=\\s?([0-9a-zA-Z._]+)")[[1]]

  if (grepl(x = data_equal, pattern = "\\.")){
    data_equal <- data_equal |>
      strsplit(split = "\\.", perl = T) |>
      unlist()

    data_equal <- paste0("file.path(", data_equal[1],", \"", data_equal[2], "\")")
  }

  return(data_equal)
}














