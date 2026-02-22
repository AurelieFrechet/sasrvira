#' @include class_ProcSAS.R
#'
# Define class ------------------------------------------------------------

#' @title Parsed Representation of a SAS PROC FREQ Statement
#'
#' @description
#' `ProcFreq` is an S7 class representing a parsed SAS `PROC FREQ`
#' statement. It extends [ProcSAS] and implements procedure-specific
#' parsing and transpilation behavior.
#'
#' @details
#' In addition to the core metadata extracted by [ProcSAS], this class
#' parses common `PROC FREQ` statements such as `VAR`, `BY`, `CLASS`,
#' `WEIGHT`, and `OUTPUT`. These components are stored as structured
#' properties and can be used by the `transpile()` method or downstream
#' workflows.
#'
#' The `transpile()` method translates a `PROC FREQ` statement into
#' equivalent R code. The returned value is a character string representing
#' the generated R expression.
#'
#' @section Inheritance:
#' `ProcFreq` inherits from [ProcSAS] and therefore exposes the same
#' base properties:
#' \describe{
#'   \item{source}{Original SAS code supplied to the constructor.}
#'   \item{proc_data}{Name of the dataset specified in the `DATA=` argument.}
#'   \item{proc_options}{Character vector of remaining procedure options.}
#' }
#'
#' @section Additional Properties:
#' \describe{
#'   \item{pf_by}{Variables specified in the `BY` statement.}
#'   \item{pf_exact}{Frequency variable specified in the `EXACT` statement.}
#'   \item{pf_tables}{Frequency variable specified in the `TABLES` statement.}
#'   \item{pf_test}{Frequency variable specified in the `TEST` statement.}
#'   \item{pf_weight}{Weight variable specified in the `WEIGHT` statement.}
#'   \item{pf_output}{List describing the `OUTPUT` statement.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{transpile()}{Translate the SAS `PROC FREQ` statement into R code.}
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
#' proc FREQ data=mydata;
#'   var x y;
#'   class group;
#'   output out=summary mean=;
#' run;
#' "
#' proc <- ProcFreq(sas_code)
#' proc
#'
#' @export
ProcFreq <- S7::new_class(
  "ProcFreq",
  parent = ProcSAS,

  properties = list(
    pf_by      = S7::class_character,
    pf_exact   = S7::class_character,
    pf_tables  = S7::class_list,
    pf_test    = S7::class_character,
    pf_weight  = S7::class_character,
    pf_output  = S7::class_character
  ),

  constructor = function(sas_code) {
    code_net <- sas_code |>
      regex_remove(pattern  = "proc\\s*FREQ\\s", ignore.case = T) |>
      regex_remove(pattern  = "run\\s*;", ignore.case = T) |>
      regex_remove(pattern  = ";") |>
      regex_replace(pattern = "\n|=|\\s+", replacement = " ") |>
      split_sql_query(
        keywords = c(
          "data",
          "by",
          "exact",
          "tables",
          "test",
          "weight",
          "output"
        )
      )

    .extract_args <- function(keyword){
      splitws(code_net$text[(code_net$key_word == keyword)])
    }

    output_args <- .extract_args("output")
    output_args <- output_args[output_args!="out"]

    new_object(
      .parent = ProcSAS(sas_code = sas_code),
      pf_by      = .extract_args("by"),
      pf_exact   = .extract_args("exact"),
      pf_tables  = pairs_tables(code_net$text[(code_net$key_word == "tables")]),
      pf_test    = .extract_args("test"),
      pf_weight  = .extract_args("weight"),
      pf_output  = output_args

    )
  },
  validator = function(self) {
    if (length(self@pf_output) != 0 && length(self@pf_tables) != length(self@pf_output)) {
      "The number of outputs in OUTPUT OUT= must match TABLES"
    }
  }
)


# Method transpile --------------------------------------------------------

S7::method(transpile, ProcFreq) <- function(x) {
  ## Display ---- # TODO CrossTable
  all_tables <- lapply(x@pf_tables, function(table){
    m_tables <- table |>
      as.matrix()
    m_tables[] <- paste0(x@proc_data, "$", m_tables)
    apply(m_tables, 1, paste, collapse = ", ")
  }) |>
    unlist() |>
    paste_function(function_name ="table", content = _)

## Output ---- # TODO dplyr
  if (!identical(x@pf_output, character(0))) {
    all_tables <- paste0(x@pf_output," <- ", all_tables)
  }

  all_tables <- paste(all_tables, collapse = "\n")

  return(all_tables)
}



# Specific utils ----------------------------------------------------------

pairs_tables <- function(text) {
  text <- regex_replace(x = text,
                        pattern = "\\s+",
                        replacement = " ") |>
    regex_replace(pattern = "\\s?\\*\\s?", replacement = "*") |>
    strsplit(split = "([ ](?![^(]*\\)))", perl = TRUE)

  list_vars <- strsplit(x = text[[1]], split = "\\*")

  lapply(list_vars, function(subvar) {
    subtext <- subvar |>
      regex_remove(pattern = "\\(|\\)") |>
      strsplit(split = " ")
    expand.grid(subtext)
  })
}
