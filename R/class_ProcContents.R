#' @include class_ProcSAS.R
#'
#' @title Parsed Representation of a SAS PROC CONTENTS Statement
#'
#' @description
#' `ProcContents` is an S7 class representing a parsed SAS `PROC CONTENTS`
#' statement. It extends [ProcSAS] and provides a concrete implementation
#' of the [transpile()] method.
#'
#' @details
#' This class inherits all parsing behavior from [ProcSAS]. The `transpile()`
#' method translates a `PROC CONTENTS` statement into equivalent R code,
#' returning a character string representing the generated R expression.
#'
#' @section Inheritance:
#' `ProcContents` inherits from [ProcSAS] and therefore exposes the same
#' properties:
#' \describe{
#'   \item{source}{Original SAS code supplied to the constructor.}
#'   \item{proc_data}{Name of the dataset specified in the `DATA=` argument.}
#'   \item{proc_options}{Character vector of remaining procedure options.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{transpile()}{Translate the SAS `PROC CONTENTS` statement into R code.}
#' }
#'
#' @import S7
#'
#' @seealso
#' [ProcSAS] for the parent class,
#' [transpile()] for the generic interface.
#'
#' @family SAS procedure classes
#'
#' @keywords internal
#'
#' @examples
#' sas_code <- "proc contents data=mydata;"
#' proc <- ProcContents(sas_code)
#' proc
#'
#' @export
ProcContents <- S7::new_class(
  "ProcContents",
  parent = ProcSAS
)

# Method: transpile ----------------------------------------------------------
S7::method(transpile, ProcContents) <- function(x) {
  code_r <- paste_function("str", x@proc_data)
  return(code_r)
}

