#' @title Base Class for Parsed SAS PROC Statements
#'
#' @description
#' `ProcSAS` is an S7 base class representing a parsed SAS `PROC` statement.
#' It stores the original SAS code and extracts structured metadata such as
#' the input dataset and procedure options.
#'
#' @details
#' The constructor parses the first `PROC` statement found in `code_sas`.
#' The dataset name is extracted from the `DATA=` argument. Any remaining
#' tokens in the `PROC` statement header are stored as procedure options.
#'
#' This class is intended to be subclassed by concrete SAS procedures
#' (e.g. `PROC SORT`, `PROC MEANS`) and provides a common interface for
#' SAS-to-* transpilation workflows.
#'
#' @param code_sas
#' A length-one character vector containing SAS code with at least one
#' `PROC` statement.
#'
#' @return
#' An object of class `ProcSAS`.
#'
#' @section Properties:
#' \describe{
#'   \item{source}{Original SAS code supplied to the constructor.}
#'   \item{proc_data}{Name of the dataset specified in the `DATA=` argument.}
#'   \item{proc_options}{Character vector of remaining procedure options.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{transpile()}{Generic method for translating the SAS procedure
#'   into another target language. Must be implemented by subclasses.}
#' }
#'
#' @import S7
#'
#' @seealso
#' [transpile()] for the generic interface implemented by subclasses.
#'
#' @family SAS procedure classes
#'
#' @keywords internal
#'
#' @examples
#' sas_code <- "proc sort data=mydata out=sorted; by id;"
#' proc <- ProcSAS(sas_code)
#' proc
#'
#' @export
ProcSAS <- new_class(
  "ProcSAS",
  properties = list(
    source       = S7::class_character,
    proc_data    = S7::class_character,
    proc_options = S7::class_character
  ),
  constructor =
    function(code_sas) {
      splitted_proc <- code_sas |> strsplit(split = ";") |> unlist() |> trimws()

      infos_contents <- splitted_proc[1] |>
        remove_string(pattern  = "proc\\s*\\w+\\s", ignore.case = T) |>
        gsub2(pattern = "\\s?=\\s?", replacement = "=") |>
        remove_string(pattern  = "data\\s?=", ignore.case = T) |>
        trimws() |> splitws()

      new_object(
        .parent = S7_object(),
        source = code_sas,
        proc_data    = infos_contents[1],
        proc_options = infos_contents[-1]
      )
    }
)


#' Transpile a SAS Procedure into Another Language
#'
#' @description
#' `transpile()` is a generic function that translates a parsed SAS
#' procedure object into a target language representation. The output
#' is typically a character string containing code in the target language.
#'
#' @param x An object representing a parsed SAS procedure. Must be a subclass
#' of [ProcSAS].
#' @param ... Additional arguments passed to method implementations (currently unused).
#'
#' @return
#' A character string containing the translated code. Subclasses of
#' `ProcSAS` must implement this method to provide procedure-specific
#' translations.
#'
#' @details
#' The default method for `ProcSAS` raises an error. Each subclass (e.g.,
#' `ProcContents`, `ProcPrint`, `ProcMeans`) must provide its own
#' implementation of `transpile()` to generate meaningful output.
#'
#' @seealso
#' [ProcSAS] and its subclasses for parsed SAS procedures.
#'
#' @examples
#' \dontrun{
#' sas_code <- "proc print data=mydata;"
#' proc <- ProcPrint(sas_code)
#' transpile(proc)  # Calls the subclass method
#' }
#'
#' @export
transpile <- S7::new_generic("transpile", "x")

S7::method(transpile, ProcSAS) <- function(x) {
  stop("Not implemented")
}

