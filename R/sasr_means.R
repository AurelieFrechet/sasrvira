# Difficulté de la proc means : peut être traduite de diverses façon en fonction ds aguments.
# On choisit une traduction via dplyr:summarize quand les indicateurs sont spécifiés,
# et via summary lors qu'ils ne sonts pas présents
sasr_means <- function(code_sas){
  code_net <- code_sas %>%
    str_remove_all(pattern  = regex("proc\\smeans\\s", ignore_case = T)) %>%
    str_remove_all(pattern  = regex("run\\s*;", ignore_case = T)) %>%
    str_remove_all(pattern  = ";") %>%
    str_replace_all(pattern = "\n",   replacement = " ") %>%
    str_replace_all(pattern = "=",    replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    decoupe_requete(keywords = c("data",
                                  "by",
                                  "class",
                                  "format",
                                  "freq",
                                  "id",
                                  "output",
                                  "types",
                                  "var",
                                  "weight",
                                  "ways"))



  # Découpe DATA=data <option> -------------------------------------------------
  means_data <- code_net$text[(code_net$key_word == "data")] %>%
    str_split("\\s+") %>%
    unlist()
  # le premier mot correspond aux données,
  # les suivants aux indicateurs sélectionnés
  means_indic <- means_data[-1]
  dplyr_data  <- means_data[1]


# Sélection des variables ----------------------------------------------------
  means_var <- code_net$text[(code_net$key_word == "var")]
  dplyr_select <- NA
  if (!identical(means_var, character(0))) {
    dplyr_select <- means_var %>%
      str_replace_all(pattern = "-", replacement = ":") %>%
      str_replace_all(pattern = "\\s+", replacement = ", ") %>%
      paste0("select(", ., ")")
  }


# Summarize ---------------------------------------------------------------
  #  distinguer s'il y a une ou plusieurs variables
  nb_vars <- str_count(means_var, pattern = "[A-Za-z0-9._]+") %>%
    ifelse(identical(., integer(0)), 0, .)

  if (nb_vars == 1) {
    dplyr_summarize <- paste0(means_indic, "(", means_var, ")") %>%
      transform_functions() %>%  # réécrire les indicateurs de façon compatibles
      paste(., collapse = ", ") %>%
      paste0("summarize(", ., ")")


  } else{
    # summarize_all
    dplyr_summarize <- means_indic %>%
      paste(., ., sep = "=") %>%
      paste(., collapse = ", ") %>%
      paste0("summarize_all(list(", ., "))") %>%
      c(dplyr_select, .) %>%
      { .[!is.na(.)] } %>%
      paste(., collapse = " %>%\n\t")
  }

  # Cas OUTPUT -----------------------------------------------------------------
  # Défini nom de la table de sortie et les noms de variables crées
  output <- FALSE
  if (any((code_net$key_word == "output"))) {
    output <- TRUE
    means_output <- code_net$text[(code_net$key_word == "output")] %>%
      decoupe_requete(
        requete = .,
        keywords = c("out", "n", "mean", "std", "skewness", "kurtosis") # TODO  préparer un vecteur de mots clés
      )

    variables <- means_var %>%
      str_split(pattern = "\\s+") %>%
      unlist()


    dplyr_summarize <- sapply(
      c("n", "mean", "std", "skewness", "kurtosis"),
      FUN = function(indic) {
        noms_variables <- means_output$text[(means_output$key_word == indic)] %>%
          str_split(pattern = "\\s+") %>%
          unlist()
        nb  <- length(noms_variables)

        if (any(means_output$key_word == indic)) {
          return(paste0(noms_variables, " = ", indic, "(", variables[1:nb], ")"))
        }
      }
    ) %>%
      unlist() %>%
      paste(collapse = ", ") %>%
      paste0("summarize(", ., ")") %>%
      transform_functions()

    dplyr_data <- paste0(means_output$text[(means_output$key_word == "out")],
                         " <- ",
                         dplyr_data)

  }

  # Regroument BY et CLASS -----------------------------------------------------
  dplyr_groupby <- NA
  if (any(code_net$key_word == "by") | any(code_net$key_word == "class")) {
  dplyr_groupby <- paste(code_net$text[(code_net$key_word == "by")],
                         code_net$text[(code_net$key_word == "class")]) %>%
    str_trim() %>%
    str_replace_all(pattern = "\\s+", replacement = ", ") %>%
    paste0("group_by(", ., ")")
  }



  # Gestion des indicateurs -------------------------------------------------
  # TODO : si pas d'indicateurs mais pas de output
  if (identical(means_indic, character(0)) & !output) {
    # Si OUTPUT : means_indic <- output_indic
    requete_dplyr <-
      c(dplyr_data,
        dplyr_groupby,
        dplyr_select,
        "summary()") %>%
      { .[!is.na(.)] } %>%
      paste(., collapse = " %>%\n\t")

  } else {
    # Composition de la sortie
    requete_dplyr <-
      c(dplyr_data,
        dplyr_groupby,
        dplyr_summarize) %>%
      { .[!is.na(.)] } %>%
      paste(., collapse = " %>%\n\t")
  }

  return(requete_dplyr)
}
