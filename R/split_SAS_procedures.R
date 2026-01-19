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
    gsub2(pattern = "run\\s?;", replacement = "run;") |>
    gsub2(pattern = "quit\\s?;", replacement = "quit;") |>
    gsub2(pattern = "\\s+", replacement = " ") |>
    strsplit('(?=\\s(proc \\w+))|(?<=run;)|(?=\\s(data \\w+))|(?<=quit;)',
             perl = T) |>
    unlist() |>
    trimws()

    t2 <- t1[!t1==""]
    t3 <- t2[!t2=="quit;"]

    return(t3)
}
