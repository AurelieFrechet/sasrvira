reecriture <- function(id, code) {
  switch(
    tolower(id),
    "proc sql" = {
      sasr_sql(code)
    },
    "proc contents" = {
      sasr_contents(code)
    },
    "proc means" = {
      transpile(proc_means(code))
    }
  )
}


#' Transcripteur
#' @include decoupe.R
#' @description traduit du code SAS en R
#'
#' @param code_sas fichier SAS
#' @importFrom stringi stri_sub_all
#'
#' @export
#'
traducteur <- function(code_sas) {
  # TODO : Suppression des options non transcriptibles en R
  code_sas <- code_sas |>
    remove_string(pattern = "noprint", ignore.case = T)
  # code_sas <- readLines(input, encoding = "UTF-8", warn=FALSE) |>
  #   paste(., collapse = "\n") |>
  code_sas <-  code_sas |>
    gsub2(pattern = "run\\s?;",  replacement = "run;") |>
    gsub2(pattern = "quit\\s?;", replacement = "quit;")

  code_decoupe <- decouper_SAS(code_sas)

  if (length(code_decoupe$id) > 0) {
    code_decoupe$traduction <- lapply(
      X = 1:length(code_decoupe$id),
      FUN = function(i) {
        reecriture(id   = code_decoupe$id[i], code = code_decoupe$texte[i])
      }
    ) |> unlist()

# TODO: REMOVE f*cking dependencies
    stringi::stri_sub_all(str = code_sas,
                          from = code_decoupe$place$start,
                          to = code_decoupe$place$end) <-
      code_decoupe$traduction
  }


  return(code_sas)

}
