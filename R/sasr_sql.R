# Merci à Nolwenn Lannuel :)
#' @include utils.R

# Utils SQL ---------------------------------------------------------------

get_alias <- function(segment_table){
  if (str_detect(segment_table, pattern = "\\bas\\b")) {
    alias_table <- str_match_all(segment_table,
                                  pattern = "[\\S]+\\sas\\s([\\S]+)")[[1]][, 2]

  } else {
    if (str_count(segment_table, "\\w+") == 2) {
      alias_table <- str_match_all(segment_table,
                                    pattern = "[\\S]+\\s([\\S]+)")[[1]][, 2]

    } else {
      alias_table <- segment_table
    }
  }
  return(alias_table)
}

get_table <- function(segment_table){
  if (str_detect(segment_table, pattern = "\\bas\\b")) {
    nom_table <- str_match_all(segment_table,
                                 pattern = "([\\S]+)\\sas\\s[\\S]+")[[1]][, 2]

  } else {
    if (str_count(segment_table, "[\\S]+") == 2) {
      nom_table <- str_match_all(segment_table,
                                   pattern = "([\\S]+)\\s[\\S]+")[[1]][, 2]

    } else {
      nom_table <- segment_table
    }
  }
  return(nom_table)
}


read_join <- function(from_table, join_table, join_expression){

  cles_sql <- join_expression %>%
    str_split(pattern = "\\s*=\\s*") %>%
    unlist() %>%
    str_split(pattern = "\\.")

  id_join        <- sapply(cles_sql, function(i) i[2])
  names(id_join) <- sapply(cles_sql, function(i) i[1])

  jointure <- paste0("\"", id_join[from_table],"\"",
                     " = ",
                     "\"", id_join[join_table],"\"")
  return(jointure)
}


sql_dplyr_select <- function(select_clause) {
  # Détection du ALL
  is_all       <- select_clause == "*"
  contains_all <-
    str_detect(string = select_clause, pattern = "\\*")

  # Détection du DISTINCT
  is_distinct  <- str_detect(string = select_clause,
                             pattern = regex("distinct", ignore_case = T))

  # Découpage de la clause par la virgule
  code <- select_clause %>%
    str_split(pattern = ",") %>%
    unlist() %>%
    str_trim %>%
    transform_functions()

  attribution <- code %>%
    str_split(pattern = "\\sinto\\s?:\\s?|\\sas\\s|\\s")

  attribution <- do.call(rbind, attribution) %>%
    as.data.frame(stringsAsFactors = FALSE)

  if(ncol(attribution) == 2){
    noms_var = attribution[, 2]
    contenu  = ifelse(attribution[, 1] == attribution[, 2],
                      NA,
                      attribution[, 1])
  } else {
    noms_var = attribution[, 1]
    contenu  = rep(NA, length(noms_var))
  }

  # Detection de contenu
  is_create <- ifelse(is.na(contenu), FALSE, TRUE)

  # Détection de fonctions
  # TODO : détection des fonctions d'aggregation uniquement :
  # - AVG()
  # - COUNT()
  # - MAX()
  # - MIN()
  # - SUM()
  is_function <- str_detect(string = code, pattern = "\\(")


  # Préparation du select général
  select_code <- noms_var %>%
    # paste0("\"", ., "\"") %>%
    paste(., collapse = ", ") %>%
    paste0("select(", ., ")")

  # Affectation des noms de variables à leur contenu
  affectation <-
    ifelse(is.na(contenu), NA,
           paste(noms_var, contenu, sep = " = ")) %>%
    {
      .[!is.na(.)]
    } %>%
    paste(., collapse = ", ")


  ## SI ALL
  if (is_all) {
    return_code <- NULL
  } else {
    ## SI DISTINCT
    if (is_distinct) {
      return_code <- select_code %>%
        paste0(., " %>% \n\tdistinct()")
    } else {
      ## SI ne contient que des fonctions d'aggregation
      if (all(is_function)) {
        if(any(is_create)){
          return_code <- affectation %>%
            paste0("summarize(", ., ")")
        } else {
          return_code <- noms_var %>%
            paste(., collapse = ", ") %>%
            paste0("summarize(", ., ")")
        }

      } else {
        ## SI ne contient que des créations de variables
        if (all(is_create)) {
          return_code <- affectation %>%
            paste0("transmute(", ., ")")
        } else {
          # Si contient ALL
          if (contains_all & any(is_create)) {
            return_code <- affectation %>%
              paste0("mutate(", ., ")")
          } else {
            # SI Extraction pure
            if (all(is.na(contenu))) {
              # Pas d'affection de variable
              return_code <- select_code

            } else{
              # Creation de variable
              return_code <- affectation %>%
                paste0("mutate(", ., ") %>%\n\t", select_code)
            }

          }
        }

      }
    }
  }

  return(return_code)
}



#' sql_to_dplyr
#' @include decoupe.R
#' @import dplyr
#' @import stringr
#' @param code_sql : chaine de charactère code SQL
#'
#' @return chaine de charactere
#' @export
#'
#' @examples
sql_to_dplyr <- function(code_sql) {
  # Déclaration des variables
  nom <- colonne <- NULL
  affectation   <- NA
  dplyr_data    <- NA
  dplyr_mutate  <- NA
  dplyr_select  <- NA
  dplyr_data    <- NA
  dplyr_filter  <- NA
  dplyr_arrange <- NA
  dplyr_groupby <- NA
  dplyr_join    <- NA
  affectation   <- NA

  code_sql <- remove_string(code_sql, ";")
  # Initialisation
  sentence <- decoupe_requete(code_sql,
                              keywords = c("select",
                                            "from",
                                            "where",
                                            "order by",
                                            "having",
                                            "group by",
                                            "left join",
                                            "right join",
                                            "inner join",
                                            "full join",
                                            "create table"))



  #  FROM ----
  # TODO : Détecter les abréviations FROM table_machin t1
  # TODO : gestion plusieurs tables
  if (any(sentence$key_word == "from")) {
    from_vector <- sentence$text[(sentence$key_word == "from")] %>%
      str_split(pattern = ",") %>%
      unlist()

    if (length(from_vector) > 1) {
      # TODO les jointures impropres
    } else {
      alias_from <- get_alias(from_vector)
      dplyr_data <- get_table(from_vector)
    }
  }

  # CREATE TABLE ----
  if (any(sentence$key_word == "create table")) {
    lecture <- sentence$text[(sentence$key_word == "create table")] %>%
      str_match(pattern = regex("([\\S]+)\\s(as|like)?(\\s[\\S]+)?", ignore_case = T))

    nom_table <-  lecture[, 2]
    table_like <- lecture[, 4]

    # CAS CREATE TABLE ______ LIKE
    if(str_detect(lecture[, 1], pattern = "\\blike\\b") & !is.na(table_like)){
      dplyr_data <- paste(nom_table, table_like, sep = " <- ")
    }
    else {
      # CAS CREATE TABLE ______ AS
      dplyr_data <- paste(nom_table, dplyr_data, sep = " <- ")
    }

  }

  # JOIN ----
  if (any(str_detect(sentence$key_word, "join"))) {
    join_expression <- sentence$text[str_detect(sentence$key_word, "join")]
    nom_jointures   <- sentence$key_word[str_detect(sentence$key_word, "join")]%>%
      tolower() %>%
      str_replace(pattern = "\\s+", "_")

    # If there is no ON => WHERE
    if (any(str_detect(join_expression, pattern = regex("\\bon\\b", ignore_case = T)))) {
      jointures <- join_expression %>%
        str_split(pattern = regex("\\s+on\\s+", ignore_case = T))

      tables_jointures      <- sapply(jointures, function(i) i[1])
      conditions_jointures  <- sapply(jointures, function(i) i[2])
    } else {
      tables_jointures      <- join_expression %>% unlist()
      conditions_jointures  <- str_extract_all(
        sentence$text[str_detect(sentence$key_word, "where")],
        pattern = "\\w+\\.\\w+\\s*=\\s*\\w+\\.\\w++"
      ) %>%
        unlist()
      sentence$text[str_detect(sentence$key_word, "where")] <- str_remove_all(
        sentence$text[str_detect(sentence$key_word, "where")],
        pattern = "\\w+\\.\\w+\\s*=\\s*\\w+\\.\\w++"
      )
    }


    dplyr_join <-  sapply(1:length(tables_jointures), function(i) {
      alias_jointures       <- get_alias(tables_jointures[i])
      read_join(alias_from,
                alias_jointures,
                conditions_jointures[i]) %>%
        paste(., collapse = ", ") %>%
        paste0(nom_jointures[i],
               "(",
               tables_jointures[i],
               ", ",
               "by = c(",
               .,
               "))")
    }) %>%
      paste(., collapse = " %>%\n\t")



  }


  # WHERE ----
  if (any(sentence$key_word == "where")) {
    # If non-empty WHERE (because of JOIN cleaning)
    if (str_trim(sentence$text[(sentence$key_word == "where")]) != "") {
      dplyr_filter <- sentence$text[(sentence$key_word == "where")] %>%
        transform_conditions() %>%
        paste0("filter(", ., ")")
    }
  }


  # GROUP BY ----
  if (any(sentence$key_word == "group by")) {
    # Soustraction des var du group by au select
    var_groupby <- sentence$text[(sentence$key_word == "group by")] %>%
      str_split(pattern = ',') %>%
      unlist() %>%
      str_trim()


    var_select <- sentence$text[(sentence$key_word == "select")] %>%
      str_split(pattern = ',') %>%
      unlist() %>%
      str_trim()


    sentence$text[(sentence$key_word == "select")] <-
      setdiff(var_select, var_groupby) %>%
      paste(., collapse = ", ")


    dplyr_groupby <- var_groupby %>%
      paste0("group_by(", . , ")")
  }


  # HAVING ----
  if (any(sentence$key_word == "having")) {
    dplyr_filter <- sentence$text[(sentence$key_word == "having")] %>%
      transform_conditions() %>%
      paste0("filter(", ., ")")
  }

  # SELECT ----
  if (sentence$text[(sentence$key_word == "select")] != "*"
      & any(sentence$key_word == "select")) {
    # Détecter les prefixes et les supprimer
    # Note : choix de tout supprimer peut-être à revoire plus tard
    dplyr_select <- sentence$text[(sentence$key_word == "select")] %>%
      str_remove_all(pattern = "\\w+\\.") %>%
      sql_dplyr_select()
  }

  # ORDER BY ----
  if (any(sentence$key_word == "order by")) {
    dplyr_arrange <- sentence$text[(sentence$key_word == "order by")] %>%
      str_replace_all(pattern = regex("([\\S]+)\\sdesc", ignore_case = T),
                      replacement = "-\\1") %>%
      paste0("arrange(", . , ")")
  }







  # Return
  requete_dplyr <- c(dplyr_data,
                     dplyr_join,
                     dplyr_groupby,
                     dplyr_mutate,
                     dplyr_select,
                     dplyr_filter,
                     dplyr_arrange) %>%
    {
      .[!is.na(.)]
    } %>%
    paste(., collapse = " %>%\n\t")

  return(requete_dplyr)

}

#' sasr_sql
#' @include decoupe.R
#' @param code_sas code SAS balisé de proc sql; quit;
#'
#' @return la même requeteen R library dplyr
#' @export
#'
#' @examples
sasr_sql <- function(code_sas) {
  # Séparer les différentes requêtes ----
  requetes <- code_sas %>%
    str_remove(pattern = regex("proc\\s+sql\\s*;", ignore_case = T)) %>%
    str_remove(pattern = regex("quit\\s*;", ignore_case = T)) %>%
    str_split(pattern = ";") %>%
    unlist() %>%
    str_replace_all(pattern = "\n", " ") %>%
    str_trim() %>%
    {
      .[-which(. == "")]
    }

  # Mise en fonction dplyr pour chaque requete
  requetes_dplyr <- lapply(requetes, sql_to_dplyr) %>%
    unlist() %>%
    paste(., collapse = "\n")

  return(requetes_dplyr)

}
