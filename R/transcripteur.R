globalVariables(c("."))

reecriture <- function(id, code) {
  switch(tolower(id),
         "proc sql" = {
           sasr_sql(code)
         },
         "proc contents" = {
           sasr_contents(code)
         },
         "proc means" = {
           sasr_means(code)
         })
}


#' Transcripteur
#' @include decoupe.R
#' @import dplyr
#' @import stringr
#' @import stringi
#' @description traduit du code SAS en R
#'
#' @param input fichier SAS
#' @param output fichier R
#'
#' @export
#'
traducteur <- function(code_sas) {
  # TODO : Suppression des options non transcriptibles en R
  code_sas <- code_sas %>%
    str_remove_all(pattern = regex("noprint", ignore_case = T))
  # code_sas <- readLines(input, encoding = "UTF-8", warn=FALSE) %>%
  #   paste(., collapse = "\n") %>%
  code_sas <-  code_sas %>%
    str_replace_all(pattern = regex("run\\s?;",
                                    ignore_case = TRUE),
                    replacement = "run;") %>%
    str_replace_all(pattern = regex("quit\\s?;",
                                    ignore_case = TRUE),
                    replacement = "quit;")

  code_decoupe <- decouper_SAS(code_sas)

  if (length(code_decoupe$id) > 0) {
    code_decoupe$traduction <- lapply(
      X = 1:length(code_decoupe$id),
      FUN = function(i) {
        reecriture(id   = code_decoupe$id[i],
                   code = code_decoupe$texte[i])
      }
    ) %>% unlist()


    stri_sub_all(str = code_sas, from = code_decoupe$place$start, to = code_decoupe$place$end) <-
      code_decoupe$traduction
  }


  return(code_sas)

}
