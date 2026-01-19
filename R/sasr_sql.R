# Merci à Nolwenn Lannuel :)
#' @include utils.R

# Utils SQL ---------------------------------------------------------------

get_alias <- function(segment_table){
  if (grepl(x = segment_table, pattern = "\\bas\\b")) {
    alias_table <- match_multiple_string(segment_table,
                                  pattern = "[\\S]+\\sas\\s([\\S]+)")[[1]]

  } else {
    if (count_string(x = segment_table, pattern = "\\w+") == 2) {
      alias_table <- match_multiple_string(segment_table,
                                    pattern = "[\\S]+\\s([\\S]+)")[[1]]

    } else {
      alias_table <- segment_table
    }
  }
  return(alias_table)
}

get_table <- function(segment_table){
  if (grepl(x = segment_table, pattern = "\\bas\\b")) {
    nom_table <- match_multiple_string(segment_table,
                                 pattern = "([\\S]+)\\sas\\s[\\S]+")[[1]]

  } else {
    if (count_string(x = segment_table, pattern = "[\\S]+") == 2) {
      nom_table <- match_multiple_string(segment_table,
                                   pattern = "([\\S]+)\\s[\\S]+")[[1]]

    } else {
      nom_table <- segment_table
    }
  }
  return(nom_table)
}


read_join <- function(from_table, join_table, join_expression){

  cles_sql <- join_expression |>
    strsplit(split = "\\s*=\\s*", perl = T) |>
    unlist() |>
    strsplit(split = "\\.", perl = T)

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
  contains_all <- grepl(x = select_clause, pattern = "\\*")

  # Détection du DISTINCT
  is_distinct  <- grepl(x = select_clause, pattern = "distinct", ignore.case = T)

  # Découpage de la clause par la virgule
  code <- select_clause |>
    strsplit(split = ",", perl = T) |>
    unlist() |>
    trimws() |>
    transform_functions()

  attribution <- code |>
    strsplit(split = "\\sinto\\s?:\\s?|\\sas\\s|\\s", perl = T)

  attribution <- do.call(rbind, attribution) |>
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
  is_function <- grepl(x = code, pattern = "\\(")


  # Préparation du select général
  select_code <- paste0("select(", paste(noms_var, collapse = ", "), ")")


  # Affectation des noms de variables à leur contenu

  a1 <- ifelse(is.na(contenu), NA, paste(noms_var, contenu, sep = " = "))
  a2 <- a1[!is.na(a1)]
  affectation <-  paste(a2, collapse = ", ")


  ## SI ALL
  if (is_all) {
    return_code <- NULL
  } else {
    ## SI DISTINCT
    if (is_distinct) {
      return_code <- paste0(select_code, " %>% \n\tdistinct()")
    } else {
      ## SI ne contient que des fonctions d'aggregation
      if (all(is_function)) {
        if(any(is_create)){
          return_code <- paste0("summarize(", affectation, ")")
        } else {
          return_code <-  paste0("summarize(", paste(noms_var, collapse = ", "), ")")
        }

      } else {
        ## SI ne contient que des créations de variables
        if (all(is_create)) {
          return_code <- paste0("transmute(", affectation, ")")
        } else {
          # Si contient ALL
          if (contains_all & any(is_create)) {
            return_code <- paste0("mutate(", affectation, ")")
          } else {
            # SI Extraction pure
            if (all(is.na(contenu))) {
              # Pas d'affection de variable
              return_code <- select_code

            } else{
              # Creation de variable
              return_code <- paste0("mutate(", affectation, ") %>%\n\t", select_code)
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
#' @param code_sql : chaine de charactère code SQL
#'
#' @return chaine de charactere
#' @export
#'
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
    from_vector <- sentence$text[(sentence$key_word == "from")] |>
      strsplit(split = ",", perl = T) |>
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
    lecture <- sentence$text[(sentence$key_word == "create table")] |>
      match_multiple_string(pattern = "([\\S]+)\\s(as|like)?(\\s[\\S]+)?", ignore.case = T)

    nom_table <-  lecture[[1]]
    table_like <- ifelse(lecture[[3]] == "", NA, lecture[[3]])

    # CAS CREATE TABLE ______ LIKE
    if(!is.na(table_like)){
      dplyr_data <- paste(nom_table, table_like, sep = " <- ")
    }
    else {
      # CAS CREATE TABLE ______ AS
      dplyr_data <- paste(nom_table, dplyr_data, sep = " <- ")
    }

  }

  # JOIN ----
  if (any(grepl(x = sentence$key_word, pattern = "join"))) {
    join_expression <- sentence$text[grepl(x = sentence$key_word, pattern = "join")]
    nom_jointures   <- sentence$key_word[grepl(x = sentence$key_word, pattern = "join")]|>
      tolower() |>
      gsub2(pattern = "\\s+", "_")

    # If there is no ON => WHERE
    if (any(grepl(x = join_expression, pattern = "\\bon\\b", ignore.case = T))) {
      jointures <- join_expression |>
        strsplit(split = "(?i)\\s+on\\s+", perl = T)

      tables_jointures      <- sapply(jointures, function(i) i[1])
      conditions_jointures  <- sapply(jointures, function(i) i[2])
    } else {
      tables_jointures      <- join_expression |> unlist()
      conditions_jointures  <- match_multiple_string(
        sentence$text[grepl(x = sentence$key_word, pattern = "where")],
        pattern = "(\\w+\\.\\w+\\s*=\\s*\\w+\\.\\w++)"
      ) |>
        unlist()
      sentence$text[grepl(x = sentence$key_word, pattern = "where")] <- remove_string(
        sentence$text[grepl(x = sentence$key_word, pattern = "where")],
        pattern = "\\w+\\.\\w+\\s*=\\s*\\w+\\.\\w++"
      )
    }


    dplyr_join <-  sapply(1:length(tables_jointures), function(i) {
      alias_jointures       <- get_alias(tables_jointures[i])
      joingned_expression <- read_join(
        from_table = alias_from,
        join_table = alias_jointures,
        join_expression = conditions_jointures[i]
      )

      paste0(
          nom_jointures[i],
          "(",
          tables_jointures[i],
          ", ",
          "by = c(",
          paste(joingned_expression, collapse = ", "),
          "))"
        )
    })

    dplyr_join <-  paste(dplyr_join, collapse = " %>%\n\t")

  }


  # WHERE ----
  if (any(sentence$key_word == "where")) {
    # If non-empty WHERE (because of JOIN cleaning)
    if (trimws(sentence$text[(sentence$key_word == "where")]) != "") {
      dplyr_filter <- sentence$text[(sentence$key_word == "where")] |>
        transform_conditions()
      dplyr_filter <- paste0("filter(", dplyr_filter, ")")
    }
  }


  # GROUP BY ----
  if (any(sentence$key_word == "group by")) {
    # Soustraction des var du group by au select
    var_groupby <- sentence$text[(sentence$key_word == "group by")] |>
      strsplit(split = ',', perl = T) |>
      unlist() |>
      trimws()


    var_select <- sentence$text[(sentence$key_word == "select")] |>
      strsplit(split = ',', perl = T) |>
      unlist() |>
      trimws()


    sentence$text[(sentence$key_word == "select")] <- paste(setdiff(var_select, var_groupby) , collapse = ", ")


    dplyr_groupby <- paste0("group_by(", var_groupby , ")")
  }


  # HAVING ----
  if (any(sentence$key_word == "having")) {
    dplyr_filter <- sentence$text[(sentence$key_word == "having")] |>
      transform_conditions()
    dplyr_filter <- paste0("filter(", dplyr_filter, ")")
  }

  # SELECT ----
  if (sentence$text[(sentence$key_word == "select")] != "*"
      & any(sentence$key_word == "select")) {
    # Détecter les prefixes et les supprimer
    # Note : choix de tout supprimer peut-être à revoire plus tard
    dplyr_select <- sentence$text[(sentence$key_word == "select")] |>
      remove_string(pattern = "\\w+\\.") |>
      sql_dplyr_select()
  }

  # ORDER BY ----
  if (any(sentence$key_word == "order by")) {
    dplyr_arrange <- sentence$text[(sentence$key_word == "order by")] |>
      gsub2(pattern = "([\\S]+)\\sdesc", replacement = "-\\1", ignore.case = T)
    dplyr_arrange <- paste0("arrange(", dplyr_arrange , ")")
  }







  # Return
  requete_dplyr <- c(dplyr_data,
                     dplyr_join,
                     dplyr_groupby,
                     dplyr_mutate,
                     dplyr_select,
                     dplyr_filter,
                     dplyr_arrange)
  requete_dplyr <- paste(requete_dplyr[!is.na(requete_dplyr)], collapse = " %>%\n\t")

  return(requete_dplyr)

}

#' sasr_sql
#' @include decoupe.R
#' @param code_sas code SAS balisé de proc sql; quit;
#'
#' @return la même requeteen R library dplyr
#' @export
#'
sasr_sql <- function(code_sas) {
  # Séparer les différentes requêtes ----
  requetes <- code_sas |>
    remove_string(pattern = "proc\\s+sql\\s*;", ignore.case = T) |>
    remove_string(pattern = "quit\\s*;", ignore.case = T) |>
    strsplit(split = ";", perl = T) |>
    unlist() |>
    gsub2(pattern = "\n", " ") |>
    trimws()

  requetes <- requetes[-which(requetes == "")]


  # Mise en fonction dplyr pour chaque requete
  requetes_dplyr <- lapply(requetes, sql_to_dplyr) |>
    unlist()
  requetes_dplyr <- paste(requetes_dplyr, collapse = "\n")

  return(requetes_dplyr)

}
