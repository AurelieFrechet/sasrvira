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
  matching <- match_multiple_string(x = text,
               pattern = "proc (\\w+)\\s?;?\\s*(.*?)\\s*(run|quit);")

  if (is.null(matching)) {
    message("text not identified")
    return(NULL)
  }

  proc <- matching[[1]]
  contenu <- matching[[2]] |>
    strsplit(";") |>
    unlist() |>
    trimws()

  return(list(proc = proc,
              contenu = contenu))

}
