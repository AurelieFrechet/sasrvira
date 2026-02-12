#' Split SAS code into logical blocks
#'
#' @description
#' Identifies logical blocks in SAS code such as procedures, DATA steps,
#' and comments (single-line and multi-line). Each block is extracted and
#' its position in the original code is recorded.
#'
#' @param sas_code Character string containing raw SAS code
#'   (not pre-split by lines).
#'
#' @return
#' A list with three elements:
#' \itemize{
#'   \item \code{place}: positions of extracted blocks in the original code
#'   \item \code{texte}: extracted code blocks
#'   \item \code{id}: identifier of each block (procedure name, comment type, etc.)
#' }
#'
#' @export
split_sas_code <- function(sas_code) {

  # PROCEDURES : proc mot [...] run;/quit;
  proc_locations <- locate_string(x = sas_code, pattern = "(\\bproc \\w+)([\\s\\S]*?)(run;|quit;)", ignore.case = T, perl = T)
  proc_matches  <-  match_multiple_string(x = sas_code, pattern = "(\\bproc \\w+)([\\s\\S]*?)(run;|quit;)", ignore.case = T, perl = T)

  # ETAPES DATA : data [...] run;
  data_locations <- locate_string(x = sas_code, pattern = "(\\bdata(?!.*=))([\\s\\S]*?)(run;|quit;)", ignore.case = T, perl = T)
  data_matches  <- match_multiple_string(x = sas_code, pattern = "(\\bdata(?!.*=))([\\s\\S]*?)(run;|quit;)", ignore.case = T, perl = T)

  # COMMENTAIRES 1 LIGNE
  single_comment_locations <- locate_string(x = sas_code, pattern = "\\n\\s+?\\*(.*?);\\n", ignore.case = T, perl = T)
  single_comment_matches  <- match_multiple_string(x = sas_code, pattern = "\\n\\s+?\\*(.*?);\\n", ignore.case = T, perl = T)

  # COMMENTAIRES MULTIGNES
  multiline_comment_locations <- locate_string(x = sas_code, pattern = "\\/\\*([\\s\\S]*?)\\*\\/", ignore.case = T, perl = T)
  multiline_comment_matches  <- match_multiple_string(x = sas_code, pattern = "\\/\\*([\\s\\S]*?)\\*\\/", ignore.case = T, perl = T)

  return(list(
    locations = rbind(proc_locations, data_locations, single_comment_locations, multiline_comment_locations),
    text      = trimws(c(proc_matches[[2]], data_matches[[2]], single_comment_matches[[1]], multiline_comment_matches[[1]])),
    block_id  = c(
      trimws(proc_matches[[1]]),
      if(is.null(data_locations)) NULL else rep("data", nrow(data_locations)),
      if(is.null(single_comment_locations)) NULL else rep("*;",   nrow(single_comment_locations)),
      if(is.null(multiline_comment_locations)) NULL else rep("/**/", nrow(multiline_comment_locations))
    )
  ))
}

#' Split an SQL query into keyword-based blocks
#'
#' @description
#' Parses a single SQL query and splits it into logical blocks based on
#' specified SQL keywords (e.g. SELECT, FROM, WHERE). The function returns
#' the extracted keywords and their associated query segments.
#'
#' @param query Character string containing a single SQL query.
#' @param keywords Character vector of SQL keywords used as split points
#'   (e.g. \code{c("select", "from", "where")}).
#'
#' @return
#' A list with two elements:
#' \itemize{
#'   \item \code{key_word}: SQL keywords found in the query (lowercased)
#'   \item \code{text}: query fragments associated with each keyword
#' }
#'
#' Returns \code{NULL} if none of the specified keywords are found.
#'
#' @export
split_sql_query <- function(query, keywords){
  # Clean spaces and newlines
  query <-  clean_newlines(query)
  look_ahead <- paste0("(?<=.)(?=(\\b", keywords, "\\b))")
  look_behind <- paste0("(?=.)(?<=(\\b", keywords, "\\b))")

  pattern_kw <- paste0('(?i)',                         # insensible à la casse
                       '"[^"\']*"(*SKIP)(*F)|',          # ignorer guillemets doubles
                       "'[^']*'(*SKIP)(*F)|",          # ignorer guillemets simples
                       paste(look_ahead, collapse = "|"), '|',
                       paste(look_behind, collapse = "|"))

  if(!grepl(pattern = pattern_kw, x = query, ignore.case = T, perl = T)){
    message("Query does not contain key words")
    return(NULL)
  }

  parts <- strsplit(trimws(query), pattern_kw, perl = TRUE)[[1]]

  key_word = tolower(parts[seq(1, length(parts) - 1, by = 2)])
  content  = trimws(parts[seq(2, length(parts), by = 2)])


  return(list(key_word = key_word, text = content))
}
