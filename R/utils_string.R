

paste_function <- function(function_name, content){
  paste0(function_name, "(", content, ")")
}

clean_newlines <- function(text, ...){
  regex_replace(x = text, pattern = "\\s|\\r\\n\\t", " ", ...)
}

splitws <- function(text){
  text |>
    regex_replace(pattern = "\\s+", " ") |>
    strsplit(split = " ") |>
    unlist() |>
    trimws()
}
