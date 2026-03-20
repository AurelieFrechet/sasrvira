

paste_function <- function(function_name, content){
  paste0(function_name, "(", content, ")")
}

clean_newlines <- function(text, ...){
  regex_replace(x = text, pattern = "\\s|\\r\\n\\t", " ", ...)
}

splitws <- function(text){
  text |>
    strsplit(split = " ") |>
    unlist() |>
    trimws()
}

concatws <- function(text){
  text |>
    trimws() |>
    gsub(pattern = "\\s+",
         replacement = " ",
         x = _,
         perl = TRUE)
}

replace_by_ws <- function(text, pattern) {
  text |>
    trimws() |>
    gsub(pattern = pattern,
         replacement = " ",
         x = _,
         perl = TRUE) |>
    concatws()

}
