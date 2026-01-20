#' sasr_libname
#' @description transformation l'un libname en une affectation
#' d'un chemin dans un objet R : Convertit LIBNAME nom chemin en nom <- chemin
#' @param code_sas code en entrée contenant le libname
#'
#' @return expression R associée
#' @export
#'
sasr_libname <- function(code_sas) {
  lib_match <- code_sas |>
    # Transformation des \ en /
    transform_path() |>
    # Identification du nom de la librarie et du chemin assosié
    match_multiple_string(pattern = "libname (\\w+) ([\"'][\\S]+[\"'])")

  # Gestion des options ?

  nom    <- lib_match[[1]]
  chemin <- lib_match[[2]]

  return(paste(nom, "<-", chemin))

}
