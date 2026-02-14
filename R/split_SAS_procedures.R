#' split_SAS_procedures
#'
#' @param text character: code sas
#'
#' @return character vector, procedures splitted
split_SAS_procedures <- function(text) {
  # Clean Text
  t1 <- text |>
    tolower() |>
    trimws() |>
    regex_replace(pattern = "run\\s?;", replacement = "run;") |>
    regex_replace(pattern = "quit\\s?;", replacement = "quit;") |>
    regex_replace(pattern = "\\s+", replacement = " ") |>
    strsplit('(?=\\s(proc \\w+))|(?<=run;)|(?=\\s(data \\w+))|(?<=quit;)',
             perl = T) |>
    unlist() |>
    trimws()

    t2 <- t1[!t1==""]
    t3 <- t2[!t2=="quit;"]

    return(t3)
}
