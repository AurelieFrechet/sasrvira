# Difficulté de la proc means : peut être traduite de diverses façon en fonction ds aguments.
# On choisit une traduction via dplyr:summarize quand les indicateurs sont spécifiés,
# et via summary lors qu'ils ne sonts pas présents
sasr_means <- function(code_sas){
  code_net <- code_sas |>
    remove_string(pattern  = "proc\\s*means\\s", ignore.case = T) |>
    remove_string(pattern  = "run\\s*;", ignore.case = T) |>
    remove_string(pattern  = ";") |>
    gsub2(pattern = "\n|=|\\s+",   replacement = " ") |>
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
  means_data <- code_net$text[(code_net$key_word == "data")] |>
    strsplit(split = "\\s+", perl = T) |>
    unlist()
  # le premier mot correspond aux données,
  # les suivants aux indicateurs sélectionnés
  means_indic <- means_data[-1]
  dplyr_data  <- means_data[1]

# Sélection des variables ----------------------------------------------------
  means_var <- code_net$text[(code_net$key_word == "var")]
  dplyr_select <- NA
  if (!identical(means_var, character(0))) {
    dplyr_select <- means_var |>
      gsub2(pattern = "-", replacement = ":") |>
      gsub2(pattern = "\\s+", replacement = ", ")

    dplyr_select <- paste0("select(", dplyr_select, ")")
  }


# Summarize ---------------------------------------------------------------
  #  distinguer s'il y a une ou plusieurs variables
  if (identical(means_var, character(0))) {
    nb_vars <- 0
  } else {
    nb_vars <- match_multiple_string(x = means_var, pattern = "([A-Za-z0-9._]+)")[[1]] |>
      length()
  }

  if (nb_vars == 1) {
    d1 <- paste0(means_indic, "(", means_var, ")") |>
      transform_functions()
    d2 <- paste(d1, collapse = ", ")
    dplyr_summarize <-  paste0("summarize(", d2, ")")


  } else{
    # summarize_all
    d1 <- paste(paste(means_indic, means_indic, sep = "="), collapse = ", ")
    d2 <- c(dplyr_select, paste0("summarize_all(list(", d1, "))"))
    d3 <- d2[!is.na(d2)]
    dplyr_summarize <- paste(d3, collapse = " %>%\n\t")
  }

  # Cas OUTPUT -----------------------------------------------------------------
  # Défini nom de la table de sortie et les noms de variables crées
  output <- FALSE
  if (any((code_net$key_word == "output"))) {
    output <- TRUE
    means_output <- code_net$text[(code_net$key_word == "output")] |>
      decoupe_requete(
        requete = _,
        keywords = c("out", "n", "mean", "std", "skewness", "kurtosis") # TODO  préparer un vecteur de mots clés
      )

    variables <- means_var |>
      strsplit(split = "\\s+", perl = T) |>
      unlist()


    d1 <- sapply(
      c("n", "mean", "std", "skewness", "kurtosis"),
      FUN = function(indic) {
        noms_variables <- means_output$text[(means_output$key_word == indic)] |>
          strsplit(split = "\\s+", perl = T) |>
          unlist()
        nb  <- length(noms_variables)

        if (any(means_output$key_word == indic)) {
          return(paste0(noms_variables, " = ", indic, "(", variables[1:nb], ")"))
        }
      }
    ) |>
      unlist() |>
      paste(collapse = ", ")

    dplyr_summarize <- paste0("summarize(", d1, ")") |>
      transform_functions()

    dplyr_data <- paste0(means_output$text[(means_output$key_word == "out")], " <- ", dplyr_data)

  }

  # Regroument BY et CLASS -----------------------------------------------------
  dplyr_groupby <- NA
  if (any(code_net$key_word == "by") | any(code_net$key_word == "class")) {
    d1 <- paste(code_net$text[(code_net$key_word == "by")], code_net$text[(code_net$key_word == "class")]) |>
      trimws() |>
      gsub2(pattern = "\\s+", replacement = ", ")

    dplyr_groupby <- paste0("group_by(", d1 , ")")
  }



  # Gestion des indicateurs -------------------------------------------------
  # TODO : si pas d'indicateurs mais pas de output
  if (identical(means_indic, character(0)) & !output) {
    # Si OUTPUT : means_indic <- output_indic
    r1 <- c(dplyr_data, dplyr_groupby, dplyr_select, "summary()")

    requete_dplyr <- r1[!is.na(r1)] |>
      paste(collapse = " %>%\n\t")

  } else {
    # Composition de la sortie
    r1 <- c(dplyr_data, dplyr_groupby, dplyr_summarize)

    requete_dplyr <- r1[!is.na(r1)] |>
      paste(collapse = " %>%\n\t")
}

  return(requete_dplyr)
}
