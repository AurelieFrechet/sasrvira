#' data_equal_to
#' @description detecte la valeur da data et la renvoie, contenur dans une
#' fonction file.path si elle est associée à une librairie SAS
data_equal_to <- function(code_sas){
  data_equal <- match_multiple_string(x = code_sas,
            pattern = "data\\s?=\\s?([0-9a-zA-Z._]+)")[[1]]

  if (grepl(x = data_equal, pattern = "\\.")){
    data_equal <- data_equal |>
      strsplit(split = "\\.", perl = T) |>
      unlist()

    data_equal <- paste0("file.path(", data_equal[1],", \"", data_equal[2], "\")")
  }

  return(data_equal)
}

#' transform pattern
#' @description remplace les motifs d'intentification de caractères des LIKES
#' dans les clauses WHERE d'une requête SQL
#' @param chaine chaine de caractères de la clause
transform_pattern  <- function(chaine){
#TODO
}


#' transform condition
#' @description remplace les opérateurs de construction des prédicats pour
#' qu'ils soient compatibles avec R
#' @param chaine chaine de caractère contenant les conditions
transform_conditions <- function(chaine){
  chaine |>
    # NULL and .
    gsub2(pattern = "([\\S]+)\\snot\\s?=\\s?\\.", replacement = "!is.na(\\1)") |>
    gsub2(pattern = "([\\S]+)\\s?=\\s?\\.",       replacement = "is.na(\\1)") |>
    gsub2(pattern = "([\\S]+)\\sne\\s?\\.",       replacement = "!is.na(\\1)") |>
    gsub2(pattern = "([\\S]+)\\s?<>\\s?\\.",      replacement = "!is.na(\\1)") |>
    gsub2(pattern = "([\\S]+)\\sis\\snull",       replacement = "is.na(\\1)") |>
    gsub2(pattern = "([\\S]+)\\sis\\snot\\snull", replacement = "!is.na(\\1)") |>

  # Remplacement =/le/ge/<>
  gsub2(pattern = "\\s?=\\s?",  replacement = " == ") |>
    gsub2(pattern = "\\sne\\s",   replacement = " != ") |>
    gsub2(pattern = "\\sge\\s",   replacement = " >= ") |>
    gsub2(pattern = "\\sle\\s",   replacement = " <= ") |>
    gsub2(pattern = "\\s?<>\\s?", replacement = " != ") |>

    # Replacement NOT IN
    gsub2(pattern = "([\\S]+)\\snot\\sin\\s([a-zA-Z0-9,()]+)", replacement = "!(\\1 %in% c\\2)") |>

    # Replacement IN
    gsub2(pattern = "([\\S]+)\\sin\\s([a-zA-Z0-9,()]+)", replacement = "\\1 %in% c\\2") |>

    # Replacement NOT BETWEEN
    gsub2(pattern = "([\\S]+)\\snot\\sbetween\\s(\\w+)\\sand\\s(\\w+)", replacement = "!between(\\1, \\2, \\3)") |>

    # Replacement BETWEEN
    gsub2(pattern = "([\\S]+)\\sbetween\\s(\\w+)\\sand\\s(\\w+)", replacement = "between(\\1, \\2, \\3)") |>

    # Remplacement and et or
    gsub2(pattern = "\\s?\\band\\b\\s?", replacement = " & ") |>
    gsub2(pattern = "\\s?\\bor\\b\\s?",  replacement = " | ") |>
    gsub2(pattern = "\\s?\\bnot\\b\\s?", replacement = " !")
}

transform_casewhen <- function(chaine){
  chaine <- chaine |>
    remove_string(pattern = "\\b(case|end)\\b", ignore.case = T, perl = T) |>
    remove_string(pattern = "\n") |>
    trimws()

  when_then <-
    match_multiple_string(
      x = chaine,
      pattern = "when\\s+([\\S]+)\\s+then\\s+([\\S]+)",
      nb_group = 2,
      ignore.case = T,
      perl = T
    )

  requete <-
    paste(when_then[[1]], when_then[[2]], sep = " ~ ") |>
    paste(collapse = ",\n")

  else_then <-
    match_multiple_string(x = chaine,
                  pattern = "else\\s+([\\S]+)", nb_group = 1,
                  ignore.case = T,
                  perl = T
    )

  if (!is.null(else_then)) {
    requete <- paste0(requete, ",\nTRUE ~ ", else_then[1])
  }

  requete <- paste0("case_when(", requete, ")") |>
    transform_conditions()

  return(requete)

}

#' transform functions
#'
#' @description remplace les fonctions par défaut SAS ou SQL par des fonctions R
#' @param chaine chaine de charactères contenant la fonction
transform_functions <- function(chaine){

  chaine <- chaine |>
    # Fonctions de base SQL
    gsub2(pattern = "\\bfalse\\b",       replacement = "FALSE") |>
    gsub2(pattern = "\\btrue\\b",        replacement = "TRUE")  |>
    gsub2(pattern = "\\bavg\\b",         replacement = "mean")  |>
    gsub2(pattern = "\\bvar_samp\\b",    replacement = "var")   |>
    gsub2(pattern = "\\bstddev_samp\\b", replacement = "sd")    |>
    gsub2(pattern = "\\bcount\\(\\*\\)", replacement = "n()")   |>
    gsub2(pattern = "\\bcount\\(distinct\\(([a-zA-z0-9._]+)\\)\\)",
          replacement = "\\bn_distinct(\\1)")

  # Indicateurs proc means
    chaine <- chaine |>
    gsub2(pattern = "\\bKURT\\b", replacement = "kurtosis") |>
    gsub2(pattern = "\\bLCLM\\b", replacement = "t.test") |>
    gsub2(pattern = "\\bUCLM\\b", replacement = "t.test") |>
    gsub2(pattern = "\\bSKEW\\b", replacement = "skewness") |>
    gsub2(pattern = "\\bSTDDEV\\b", replacement = "sd") |>
    gsub2(pattern = "\\bSTD\\b",    replacement = "sd") |>
    gsub2(pattern = "\\bN\\(([a-zA-z0-9._]+)\\)", replacement = "n()") |>
    gsub2(pattern = "\\bMEAN\\b", replacement = "mean") |>
    gsub2(pattern = "\\bMIN\\b",  replacement = "min") |>
    gsub2(pattern = "\\bMAX\\b",  replacement = "max") |>
    gsub2(pattern = "NMISS\\(([a-zA-z0-9._]+)\\)",        replacement = "sum(is.na(\\1))") |>
    gsub2(pattern = "\\bP([0-9]+)\\(([a-zA-z0-9._]+)\\)", replacement = "quantile(\\2, \\1/100)")

    # Case when

    chaine_casewhen <-
      match_multiple_string(x = chaine,
                            pattern = "case\\s([\\s\\S]+)\\send",
                                            ignore.case = T, perl = T)

  if (!is.null(chaine_casewhen)) {
    chaine <-
      gsub2(
        x = chaine,
        pattern = "case\\s([\\s\\S]+)\\send",
        replacement = transform_casewhen(chaine_casewhen)
      )
  }

  return(chaine)
}


#' Transform listes
#' @description remplace une liste SAS par son équivalent vectoriel R
#' @param chaine liste SAS au format "\{l1 l2 l3\}"
transform_list <- function(chaine){
  valeurs <- chaine |>
    remove_string(pattern = "\\{") |>
    remove_string(pattern = "\\}") |>
    trimws() |>
    strsplit(split = "\\s+") |>
    unlist()

  valeurs_numeric <- suppressWarnings(as.numeric(valeurs))
  if(all(!is.na(valeurs_numeric))){
    valeurs <- valeurs_numeric
  }

  return(paste(list(valeurs), collapse = ", "))
}


#' transform path
#' @description change le chemin d'un fichier de façon compatible à la lecture dans R
#' En changeant \ en /
#' @param chaine chemin du fichier
transform_path <- function(chaine){
  return(gsub("\\", "/", chaine, fixed=TRUE))
}


