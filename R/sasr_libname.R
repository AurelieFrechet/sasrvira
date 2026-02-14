#' sasr_libname
#' @description transformation l'un libname en une affectation
#' d'un chemin dans un objet R : Convertit LIBNAME nom chemin en nom <- chemin
#' @param sas_code code en entrée contenant le libname
#'
#' @return expression R associée
#' @export
#'
sasr_libname <- function(sas_code) {
  lib_match <- sas_code |>
    # Transformation des \ en /
    transform_path() |>
    # Identification du nom de la librarie et du chemin assosié
    regex_match_groups(pattern = "libname (\\w+) ([\"'][\\S]+[\"'])")

  # Gestion des options ?

  nom    <- lib_match[[1]]
  chemin <- lib_match[[2]]

  return(paste(nom, "<-", chemin))

}
