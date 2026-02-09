#' Translate SAS code to R
#'
#' @description
#' Functions used to translate (transpile) SAS code into R code.
#' The main entry point is \code{transastor()}, which parses SAS code,
#' dispatches each SAS procedure to the appropriate translator,
#' and replaces the original code with its R equivalent.
#'
#' @param sas_code Character string containing SAS code.
#' @param id Character string identifying the SAS procedure.
#' @param code Character string containing the SAS procedure body.
#'
#' @return
#' A character string containing translated R code.
#'
#' @importFrom stringi stri_sub_all
#'
#' @examples
#' # Example SAS code (simplified)
#' sas_code <- "
#' proc means data=mydata;
#' run;
#' "
#'
#' transastor(sas_code)
#'
#' @export
transastor <- function(sas_code) {
  # TODO : Suppression des options non transcriptibles en R
  sas_code <- sas_code |>
    remove_string(pattern = "noprint", ignore.case = T)
  # sas_code <- readLines(input, encoding = "UTF-8", warn=FALSE) |>
  #   paste(., collapse = "\n") |>
  sas_code <-  sas_code |>
    gsub2(pattern = "run\\s?;",  replacement = "run;") |>
    gsub2(pattern = "quit\\s?;", replacement = "quit;")

  code_decoupe <- decouper_SAS(sas_code)

  if (length(code_decoupe$id) > 0) {
    code_decoupe$traduction <- lapply(
      X = 1:length(code_decoupe$id),
      FUN = function(i) {
        id   = code_decoupe$id[i]
        code = code_decoupe$texte[i]
        switch(
          tolower(id),
          "proc sql" = {
            transpile(ProcSQL(code))
          },
          "proc contents" = {
            transpile(ProcContents(code))
          },
          "proc means" = {
            transpile(ProcMeans(code))
          }
        )
      }
    ) |> unlist()

# TODO: REMOVE f*cking dependencies
    stringi::stri_sub_all(str = sas_code,
                          from = code_decoupe$place$start,
                          to = code_decoupe$place$end) <-
      code_decoupe$traduction
  }


  return(sas_code)

}
