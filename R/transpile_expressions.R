#' Translate SQL and SAS expressions to R syntax
#'
#' @description
#' Internal helper functions used to translate SQL and SAS expressions
#' (conditions, functions, CASE WHEN clauses, lists, and paths)
#' into their R equivalents. These functions are part of the expression
#' transpilation layer used during SQL/SAS to R code conversion.
#'
#' @param expr Character string containing an SQL or SAS expression.
#'
#' @return
#' A character string containing the translated R expression.
#'
#' @keywords internal
#'
#' @name transpile_expressions
NULL


#' @rdname transpile_expressions
#'
#' @description
#' Rewrites SQL and SAS conditional operators into R-compatible logical
#' expressions, including NULL checks, comparison operators, IN / BETWEEN
#' clauses, and logical connectors.
transform_conditions <- function(expr){
  expr |>
    # NULL and .
    regex_replace(pattern = "([\\S]+)\\snot\\s?=\\s?\\.", replacement = "!is.na(\\1)") |>
    regex_replace(pattern = "([\\S]+)\\s?=\\s?\\.",       replacement = "is.na(\\1)") |>
    regex_replace(pattern = "([\\S]+)\\sne\\s?\\.",       replacement = "!is.na(\\1)") |>
    regex_replace(pattern = "([\\S]+)\\s?<>\\s?\\.",      replacement = "!is.na(\\1)") |>
    regex_replace(pattern = "([\\S]+)\\sis\\snull",       replacement = "is.na(\\1)") |>
    regex_replace(pattern = "([\\S]+)\\sis\\snot\\snull", replacement = "!is.na(\\1)") |>

    # Comparison operators (=, <=, >=, <>)
    regex_replace(pattern = "\\s?=\\s?",  replacement = " == ") |>
    regex_replace(pattern = "\\sne\\s",   replacement = " != ") |>
    regex_replace(pattern = "\\sge\\s",   replacement = " >= ") |>
    regex_replace(pattern = "\\sle\\s",   replacement = " <= ") |>
    regex_replace(pattern = "\\s?<>\\s?", replacement = " != ") |>

    # NOT IN / IN clauses
    regex_replace(pattern = "([\\S]+)\\snot\\sin\\s([a-zA-Z0-9,()]+)", replacement = "!(\\1 %in% c\\2)") |>
    regex_replace(pattern = "([\\S]+)\\sin\\s([a-zA-Z0-9,()]+)", replacement = "\\1 %in% c\\2") |>

    # BETWEEN / NOT BETWEEN clauses
    regex_replace(pattern = "([\\S]+)\\snot\\sbetween\\s(\\w+)\\sand\\s(\\w+)", replacement = "!between(\\1, \\2, \\3)") |>
    regex_replace(pattern = "([\\S]+)\\sbetween\\s(\\w+)\\sand\\s(\\w+)", replacement = "between(\\1, \\2, \\3)") |>

    # Logical operators AND / OR / NOT
    regex_replace(pattern = "\\s?\\band\\b\\s?", replacement = " & ") |>
    regex_replace(pattern = "\\s?\\bor\\b\\s?",  replacement = " | ") |>
    regex_replace(pattern = "\\s?\\bnot\\b\\s?", replacement = " !")
}


#' @rdname transpile_expressions
#'
#' @description
#' Converts SQL CASE WHEN expressions into \code{dplyr::case_when()}
#' syntax and applies condition rewriting to ensure R compatibility.
transform_case_when <- function(expr){
  expr <- expr |>
    regex_remove(pattern = "\\b(case|end)\\b", ignore.case = T, perl = T) |>
    regex_remove(pattern = "\n") |>
    trimws()

  when_then <-
    regex_match_groups(
      x = expr,
      pattern = "when\\s+([\\S]+)\\s+then\\s+([\\S]+)",
      n_groups = 2,
      ignore.case = T,
      perl = T
    )

  query <-
    paste(when_then[[1]], when_then[[2]], sep = " ~ ") |>
    paste(collapse = ",\n")

  else_then <-
    regex_match_groups(x = expr,
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



#' @rdname transpile_expressions
#'
#' @description
#' Translates SQL and SAS built-in functions and summary indicators
#' into their R equivalents (e.g. \code{AVG} → \code{mean},
#' \code{COUNT(*)} → \code{n()}).
transform_functions <- function(expr){

  expr <- expr |>
    # SQL base functions
    regex_replace(pattern = "\\bfalse\\b",       replacement = "FALSE") |>
    regex_replace(pattern = "\\btrue\\b",        replacement = "TRUE")  |>
    regex_replace(pattern = "\\bavg\\b",         replacement = "mean")  |>
    regex_replace(pattern = "\\bvar_samp\\b",    replacement = "var")   |>
    regex_replace(pattern = "\\bstddev_samp\\b", replacement = "sd")    |>
    regex_replace(pattern = "\\bcount\\(\\*\\)", replacement = "n()")   |>
    regex_replace(pattern = "\\bcount\\(distinct\\(([a-zA-z0-9._]+)\\)\\)",
                  replacement = "\\bn_distinct(\\1)")

  # proc means indicators
  expr <- expr |>
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
  expr_case_when <-
    regex_match_groups(x = expr,
                       pattern = "case\\s([\\s\\S]+)\\send",
                       ignore.case = T, perl = T)

  if (!is.null(expr_case_when)) {
    expr <-
      regex_replace(
        x = expr,
        pattern = "case\\s([\\s\\S]+)\\send",
        replacement = transform_case_when(expr_case_when)
      )
  }

  return(expr)
}

#' @rdname transpile_expressions
#'
#' @description
#' Converts a SAS-style list (e.g. \code{\{1 2 3\}})
#' into an R vector expression.
transform_list <- function(expr){
  values <- expr |>
    regex_remove(pattern = "\\{") |>
    regex_remove(pattern = "\\}") |>
    trimws() |>
    strsplit(split = "\\s+") |>
    unlist()

  numeric_values <- suppressWarnings(as.numeric(values))
  if(all(!is.na(numeric_values))){
    values <- numeric_values
  }

  return(paste(list(values), collapse = ", "))
}

#' @rdname transpile_expressions
#'
#' @description
#' Normalizes file paths by replacing backslashes with forward slashes
#' to ensure compatibility with R file handling.
transform_path <- function(expr){
  return(gsub("\\", "/", expr, fixed=TRUE))
}
