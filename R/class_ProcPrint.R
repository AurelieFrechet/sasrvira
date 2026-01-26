#' @include class_ProcSAS.R
#'
#' @title Parsed Representation of a SAS PROC PRINT Statement
#'
#' @description
#' `ProcPrint` is an S7 class representing a parsed SAS `PROC PRINT`
#' statement. It extends [ProcSAS] and implements procedure-specific
#' transpilation behavior.
#'
#' @details
#' The `transpile()` method translates a `PROC PRINT` statement into
#' equivalent R code. The returned value is a character string representing
#' the generated R expression.
#'
#' @section Inheritance:
#' `ProcPrint` inherits from [ProcSAS] and therefore exposes the same
#' properties:
#' \describe{
#'   \item{source}{Original SAS code supplied to the constructor.}
#'   \item{proc_data}{Name of the dataset specified in the `DATA=` argument.}
#'   \item{proc_options}{Character vector of remaining procedure options.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{transpile()}{Translate the SAS `PROC PRINT` statement into R code.}
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
#' sas_code <- "proc print data=mydata;"
#' proc <- ProcPrint(sas_code)
#' proc
#'
#' @export
ProcPrint <- S7::new_class(
  "ProcPrint",
  parent = ProcSAS
)

# Method: transpile ----------------------------------------------------------

S7::method(transpile, ProcPrint) <- function(x) {
  code_r <- x@proc_data
  return(code_r)
}

