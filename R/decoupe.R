#' Decoupe code SAS
#'
#' @import stringr
#' @import stringi
#' @import dplyr
#'
#' @description identifie les blocs de procédures/commentaires/étapes data/etc,
#' les découpe et indique leur position dans le code d'origine
#'
#' @param code_sas : code SAS en entrée, non découpé par lignes
#'
#' @return liste d'élements : place, texte et id
#' place : localisation du code extrait
#' texte : code extrait
#' id : identifiant du code extrait, premier mot ou groupe de mot du bloc,
#' identifiant la procédure ou le commentaire, etc.
#' @export
#'
#' @examples
decouper_SAS <- function(code_sas) {
  # PROCEDURES : proc mot [...] run;/quit;
  locate_proc <- locate_string(x = code_sas, pattern = "(proc \\w+)([\\s\\S]*?)(run;|quit;)", ignore.case = T, perl = T)
  match_proc  <-  match_multiple_string(x = code_sas, pattern = "(proc \\w+)([\\s\\S]*?)(run;|quit;)", ignore.case = T, perl = T)

  # ETAPES DATA : data [...] run;
  locate_data <- locate_string(x = code_sas, pattern = "(data(?!.*=))([\\s\\S]*?)(run;|quit;)", ignore.case = T, perl = T)
  match_data  <- match_multiple_string(x = code_sas, pattern = "(data(?!.*=))([\\s\\S]*?)(run;|quit;)", ignore.case = T, perl = T)

  # COMMENTAIRES 1 LIGNE
  locate_c1 <- locate_string(x = code_sas, pattern = "\\n\\s+?\\*(.*?);\\n", ignore.case = T, perl = T)
  match_c1  <- match_multiple_string(x = code_sas, pattern = "\\n\\s+?\\*(.*?);\\n", ignore.case = T, perl = T)

  # COMMENTAIRES MULTIGNES
  locate_c2 <- locate_string(x = code_sas, pattern = "\\/\\*([\\s\\S]*?)\\*\\/", ignore.case = T, perl = T)
  match_c2  <- match_multiple_string(x = code_sas, pattern = "\\/\\*([\\s\\S]*?)\\*\\/", ignore.case = T, perl = T)

  return(list(
    place = rbind(locate_proc, locate_data, locate_c1, locate_c2),
    texte = trimws(c(match_proc[[2]], match_data[[2]], match_c1[[1]], match_c2[[1]])),
    id = c(
      trimws(match_proc[[1]]),
      if(is.null(locate_data)) NULL else rep("data", nrow(locate_data)),
      if(is.null(locate_c1)) NULL else rep("*;",   nrow(locate_c1)),
      if(is.null(locate_c2)) NULL else rep("/**/", nrow(locate_c2))
    )
  ))
}

#' decoupe_requete
#' @import stringr
#' @import stringi
#' @import dplyr
#' @description lit une requete sql et renvoie une data.frame avec les mots clés (kw)
#' et les valeurs associées (sentence)
#' @param requete une seule requete sql
#' @param key_words : mots clés de découpe (select, from, etc)
#'
#' @return vecteur nommé des blocs, le nom associé correspond aux mots clés
#' @export
#'
#' @examples
decoupe_requete <- function(requete, key_words){
  # Mise sous forme de mots :
  key_words <- paste0("\\b", key_words, "\\b")
  # Definition des mots clés
  pattern_kw <- paste(paste0("(?=", key_words, ")"),
                      collapse = "|")

  # Decoupe
  sentence <- str_split(string = requete,
                        pattern = regex(pattern_kw, ignore_case = T))[[1]] %>%
    str_trim() %>%
    {
      .[!(. == "")]
    }


  # Identification
  kw <- str_extract(string = tolower(sentence),
                    pattern = paste(key_words, collapse = "|"))

  # Nettoyage
  kw_pattern <- paste(key_words, collapse = "|")
  sentence <- sentence %>%
    str_replace_all(pattern = regex(kw_pattern, ignore_case = T),
                    replacement = "") %>%
    str_trim()

  # Messages d'erreur
  if(all(is.na(kw))){
    message("Requete does not contain key words")
    return(NULL)
  }

  return(list(kw = kw, text = sentence))
}
