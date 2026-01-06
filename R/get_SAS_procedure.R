#' get SAS procedures
#' @import stringr
#' @import magrittr
#' @description read SAS code, identify procedures and extract their contents
#' @param text SAS code
#'
#' @return list of identified procedures as
#' -proc : name of the procedure in listed values in procs
#' -contenu : what's inside the procedure
#'
#' @examples
#' get_SAS_procedure(text = "PROC CONTENTS DATA=sample;RUN;")
get_SAS_procedure <- function(text) {
  # Detect procedures
  matching <- str_match(string  = text,
            pattern = "proc (\\w+)\\s?;?\\s*(.*?)\\s*(run|quit);")

  proc <- matching[, 2]
  contenu <- matching[, 3] %>%
    strsplit(";") %>%
    unlist() %>%
    str_trim()

  if (is.na(proc)) {
    message("text not identified")
    return(NULL)
  }

  return(list(proc = proc,
              contenu = contenu))

}
