#' sasr_contents
#' @import stringr
#' @param code_sas
#' @description transformation de la proc contents en code R equivalent
#' @return code R equivalent de la proc contents avec les arguments
#' @export
#'
sasr_contents <- function(code_sas) {

  code_r <- data_equal_to(code_sas) |>
    paste_str()

  return(code_r)
}
