#' String and regular expression utility functions
#'
#' @description
#' A collection of internal helper functions for string manipulation and
#' regular-expression-based parsing. These utilities provide thin wrappers
#' around base R functions to simplify common tasks such as pattern removal,
#' substitution, matching, counting, and locating matches in text.
#'
#' These functions are primarily used as building blocks for higher-level
#' SAS and SQL parsing logic.
#'
#' @param x Character vector to process.
#' @param pattern Regular expression pattern.
#' @param replacement Replacement string used in substitutions.
#' @param ignore.case Logical; whether matching should be case-insensitive.
#' @param perl Logical; whether to use Perl-compatible regular expressions.
#' @param group Integer specifying which capture group to extract.
#' @param n_groups Integer specifying the number of capture groups.
#' @param ... Additional arguments passed to underlying functions.
#'
#' @return
#' Depending on the function:
#' Depending on the function:
#' \itemize{
#'   \item a character vector
#'   \item a list of character vectors (one per capture group)
#'   \item an integer count
#'   \item a data frame with match start and end positions
#' }
#'
#' @keywords internal
#'
#' @name regex_utils
NULL


#' @rdname regex_utils
regex_remove <- function(x,
                          pattern,
                          ignore.case = FALSE,
                          perl = TRUE,
                          ...) {
  gsub(
    pattern = pattern,
    replacement = "",
    x = x,
    ignore.case = ignore.case,
    perl =  perl,
    ...
  )
}

#' @rdname regex_utils
regex_replace <- function(x,
                  pattern,
                  replacement,
                  ignore.case = TRUE,
                  perl = TRUE,
                  ...) {
  gsub(
    pattern = pattern,
    replacement = replacement,
    x = x,
    ignore.case = ignore.case,
    perl =  perl,
    ...
  )
}


#' @rdname regex_utils
regex_match_groups <- function(x,
                                  pattern,
                                  n_groups = NULL,
                                  ignore.case = TRUE,
                                  perl = TRUE,
                                  ...) {
  m <- gregexpr(pattern,
                text = x,
                ignore.case = ignore.case,
                perl = perl)
  res <- regmatches(x, m)[[1]]
  if (identical(res, character(0)))
    return(NULL)

  if (is.null(n_groups))
    n_groups <- lengths(regmatches(pattern, gregexpr('\\(', pattern)))

  lapply(1:n_groups, function(i) {
    gsub(
      pattern = pattern,
      replacement = paste0("\\", i),
      x = res,
      ignore.case = ignore.case,
      perl =  perl,
      ...
    )
  })
}


#' @rdname regex_utils
regex_count_matches <- function(x,
                         pattern,
                         ignore.case = TRUE,
                         perl = TRUE,
                         ...) {
  regex_match_groups(
    x = x,
    pattern = pattern,
    ignore.case = ignore.case,
    perl =  perl,
    ...
  )[[1]] |>
    length()
}


#' @rdname regex_utils
regex_locate_matches <- function(x,
                          pattern,
                          ignore.case = TRUE,
                          perl = TRUE,
                          ...) {
  m <- gregexpr(pattern,
                text = x,
                ignore.case = ignore.case,
                perl = perl)
  res <- regmatches(x, m)[[1]]
  if (identical(res, character(0)))
    return(NULL)

  m_start <- as.numeric(m[[1]])
  m_length <- as.numeric(attributes(m[[1]])$match.length)
  m_end <- m_start + m_length - 1

  return(data.frame(start = m_start, end = m_end))
}
