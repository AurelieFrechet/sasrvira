#' @include class_ProcSAS.R
#'
# Define class ------------------------------------------------------------

#' @title Parsed Representation of a SAS PROC SORT Statement
#'
#' @description
#' `ProcSort` is an S7 class representing a parsed SAS `PROC SORT`
#' statement. It extends [ProcSAS] and implements procedure-specific
#' parsing and transpilation behavior.
#'
#' @details
#' In addition to the core metadata extracted by [ProcSAS], this class
#' parses common `PROC SORT` statements such as `BY`.
#' These components are stored as structured
#' properties and can be used by the `transpile()` method or downstream
#' workflows.
#'
#' The `transpile()` method translates a `PROC SORT` statement into
#' equivalent R code. The returned value is a character string representing
#' the generated R expression.
#'
#' @section Inheritance:
#' `ProcSort` inherits from [ProcSAS] and therefore exposes the same
#' base properties:
#' \describe{
#'   \item{source}{Original SAS code supplied to the constructor.}
#'   \item{proc_data}{Name of the dataset specified in the `DATA=` argument.}
#'   \item{proc_options}{Character vector of remaining procedure options.}
#' }
#'
#' @section Additional Properties:
#' \describe{
#'   \item{pm_by}{Variables specified in the `BY` statement.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{transpile()}{Translate the SAS `PROC SORT` statement into R code.}
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
#' sas_code <- "
#' proc sort data = iris;
#' by  descending Sepal.Lenght Petal.Lenght;
#' run;"
#' proc <- ProcSort(sas_code)
#' proc
#'
#' @export
ProcSort <- S7::new_class(
  "ProcSort",
  parent = ProcSAS,
  properties = list(
    ps_by  = S7::class_character
  ),
  constructor = function(code_sas) {
    code_net <- code_sas |>
      remove_string(pattern  = "proc\\s*sort\\s", ignore.case = T) |>
      remove_string(pattern  = "run\\s*;", ignore.case = T) |>
      remove_string(pattern  = ";") |>
      gsub2(pattern = "\n|=|\\s+", replacement = " ") |>
      decoupe_requete(
        keywords = c(
          "data",
          "by"
        )
      )

    by_arg <- code_net$text[(code_net$key_word == "by")] |>
    gsub2( pattern = "descending\\s+?", replacement = "descending-", ignore.case = T) |>
      splitws()


    new_object(
      .parent = ProcSAS(code_sas = code_sas),
      ps_by  = by_arg
    )
  }
)


S7::method(transpile, ProcSort) <- function(x) {
  sort_by <- x@ps_by |>
    gsub2(pattern = "descending-([0-9a-zA-Z._]+)", replacement = "desc(\\1)")

  dplyr_groupby <- paste_function(function_name = "arrange",
                                  content = paste(sort_by, collapse = ", "))

  requete_dplyr <- paste(c(x@proc_data, dplyr_groupby), collapse = " %>%\n\t")

  out_table <- x@proc_options[grepl(pattern = "out=", x = x@proc_options)]|>
    remove_string(pattern  = "out=", ignore.case = T)


  if(!identical(out_table, character(0))) {
    requete_dplyr <- paste(out_table, "<-", requete_dplyr)
  }

  return(requete_dplyr)

}
