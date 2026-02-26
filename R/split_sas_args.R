split_sas_args <- function(sas_query) {
  splitted <- sas_query |>
    regex_replace(pattern = "\\s+", replacement = " ") |>
    regex_remove(pattern = "proc", ignore.case = TRUE) |>
    regex_remove(pattern = "run\\s?;", ignore.case = TRUE) |>
    trimws()|>
    strsplit(split = ";") |>
    unlist() |>
    trimws()


  arg   <- regex_replace(x = splitted, pattern = "^(\\w+).*", replace = "\\1") |> tolower()
  query <- regex_remove(x = splitted, pattern = "^(\\w+)") |> trimws() |> as.list()

  names(query) <- arg

  return(query)
}
