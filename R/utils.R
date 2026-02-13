#' data_equal_to
#' @description detecte la valeur da data et la renvoie, contenur dans une
#' fonction file.path si elle est associée à une librairie SAS
#' @param sas_code character: code sas
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














