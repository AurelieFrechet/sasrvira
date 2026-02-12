#' Paste function calls as strings
#'
#' @description
#' Utility functions to build character strings representing common
#' R function calls applied to a given content.
#'
#' @param function_name Character string giving the function name.
#' @param content Character string representing the function argument(s).
#'
#' @return A character string representing a function call.
#'
#' @keywords internal
paste_function <- function(function_name, content) {
  paste0(function_name, "(", content, ")")
}

#' @rdname paste_function
paste_str <- function(content) {
  paste_function("str", content)
}

#' @rdname paste_function
paste_summary <- function(content) {
  paste_function("summary", content)
}

#' @rdname paste_function
paste_table <- function(content) {
  paste_function("table", content)
}
