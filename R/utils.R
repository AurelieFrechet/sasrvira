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
    regex_replace(pattern = "([\\S]+)\\snot\\s?=\\s?\\.", replacement = "!is.na(\\1)") |>
    regex_replace(pattern = "([\\S]+)\\s?=\\s?\\.",       replacement = "is.na(\\1)") |>
    regex_replace(pattern = "([\\S]+)\\sne\\s?\\.",       replacement = "!is.na(\\1)") |>
    regex_replace(pattern = "([\\S]+)\\s?<>\\s?\\.",      replacement = "!is.na(\\1)") |>
    regex_replace(pattern = "([\\S]+)\\sis\\snull",       replacement = "is.na(\\1)") |>
    regex_replace(pattern = "([\\S]+)\\sis\\snot\\snull", replacement = "!is.na(\\1)") |>

  # Remplacement =/le/ge/<>
  regex_replace(pattern = "\\s?=\\s?",  replacement = " == ") |>
    regex_replace(pattern = "\\sne\\s",   replacement = " != ") |>
    regex_replace(pattern = "\\sge\\s",   replacement = " >= ") |>
    regex_replace(pattern = "\\sle\\s",   replacement = " <= ") |>
    regex_replace(pattern = "\\s?<>\\s?", replacement = " != ") |>

    # Replacement NOT IN
    regex_replace(pattern = "([\\S]+)\\snot\\sin\\s([a-zA-Z0-9,()]+)", replacement = "!(\\1 %in% c\\2)") |>

    # Replacement IN
    regex_replace(pattern = "([\\S]+)\\sin\\s([a-zA-Z0-9,()]+)", replacement = "\\1 %in% c\\2") |>

    # Replacement NOT BETWEEN
    regex_replace(pattern = "([\\S]+)\\snot\\sbetween\\s(\\w+)\\sand\\s(\\w+)", replacement = "!between(\\1, \\2, \\3)") |>

    # Replacement BETWEEN
    regex_replace(pattern = "([\\S]+)\\sbetween\\s(\\w+)\\sand\\s(\\w+)", replacement = "between(\\1, \\2, \\3)") |>

    # Remplacement and et or
    regex_replace(pattern = "\\s?\\band\\b\\s?", replacement = " & ") |>
    regex_replace(pattern = "\\s?\\bor\\b\\s?",  replacement = " | ") |>
    regex_replace(pattern = "\\s?\\bnot\\b\\s?", replacement = " !")
}

transform_casewhen <- function(chaine){
  chaine <- chaine |>
    regex_remove(pattern = "\\b(case|end)\\b", ignore.case = T, perl = T) |>
    regex_remove(pattern = "\n") |>
    trimws()

  when_then <-
    regex_match_groups(
      x = chaine,
      pattern = "when\\s+([\\S]+)\\s+then\\s+([\\S]+)",
      n_groups = 2,
      ignore.case = T,
      perl = T
    )

  query <-
    paste(when_then[[1]], when_then[[2]], sep = " ~ ") |>
    paste(collapse = ",\n")

  else_then <-
    regex_match_groups(x = chaine,
                  pattern = "else\\s+([\\S]+)", n_groups = 1,
                  ignore.case = T,
                  perl = T
    )

  if (!is.null(else_then)) {
    query <- paste0(query, ",\nTRUE ~ ", else_then[1])
  }

  query <- paste0("case_when(", query, ")") |>
    transform_conditions()

  return(query)

}

#' transform functions
#'
#' @description remplace les fonctions par défaut SAS ou SQL par des fonctions R
#' @param chaine chaine de charactères contenant la fonction
transform_functions <- function(chaine){

  chaine <- chaine |>
    # Fonctions de base SQL
    regex_replace(pattern = "\\bfalse\\b",       replacement = "FALSE") |>
    regex_replace(pattern = "\\btrue\\b",        replacement = "TRUE")  |>
    regex_replace(pattern = "\\bavg\\b",         replacement = "mean")  |>
    regex_replace(pattern = "\\bvar_samp\\b",    replacement = "var")   |>
    regex_replace(pattern = "\\bstddev_samp\\b", replacement = "sd")    |>
    regex_replace(pattern = "\\bcount\\(\\*\\)", replacement = "n()")   |>
    regex_replace(pattern = "\\bcount\\(distinct\\(([a-zA-z0-9._]+)\\)\\)",
          replacement = "\\bn_distinct(\\1)")

  # Indicateurs proc means
    chaine <- chaine |>
    regex_replace(pattern = "\\bKURT\\b", replacement = "kurtosis") |>
    regex_replace(pattern = "\\bLCLM\\b", replacement = "t.test") |>
    regex_replace(pattern = "\\bUCLM\\b", replacement = "t.test") |>
    regex_replace(pattern = "\\bSKEW\\b", replacement = "skewness") |>
    regex_replace(pattern = "\\bSTDDEV\\b", replacement = "sd") |>
    regex_replace(pattern = "\\bSTD\\b",    replacement = "sd") |>
    regex_replace(pattern = "\\bN\\(([a-zA-z0-9._]+)\\)", replacement = "n()") |>
    regex_replace(pattern = "\\bMEAN\\b", replacement = "mean") |>
    regex_replace(pattern = "\\bMIN\\b",  replacement = "min") |>
    regex_replace(pattern = "\\bMAX\\b",  replacement = "max") |>
    regex_replace(pattern = "NMISS\\(([a-zA-z0-9._]+)\\)",        replacement = "sum(is.na(\\1))") |>
    regex_replace(pattern = "\\bP([0-9]+)\\(([a-zA-z0-9._]+)\\)", replacement = "quantile(\\2, \\1/100)")

    # Case when

    chaine_casewhen <-
      regex_match_groups(x = chaine,
                            pattern = "case\\s([\\s\\S]+)\\send",
                                            ignore.case = T, perl = T)

  if (!is.null(chaine_casewhen)) {
    chaine <-
      regex_replace(
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
    regex_remove(pattern = "\\{") |>
    regex_remove(pattern = "\\}") |>
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


