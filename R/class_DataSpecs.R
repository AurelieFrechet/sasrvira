DataSpecs <- new_class(
  "DataSpecs",
  properties = list(
    data  = S7::class_character,
    where = S7::class_character,
    keep  = S7::class_character,
    drop  = S7::class_character,
    rename = S7::class_character,
    firstobs = S7::class_numeric,
    obs      = S7::class_numeric
  )
  ,
  constructor = function(data_statement) {
    data_statement <- regex_replace(data_statement, "\\s+", " ")
    data_info <- regex_match_groups(
      data_statement,
      pattern = "([A-z0-9.]+)\\s?(\\((.*)\\))?"
    )

    dataspecs <- data_info[[3]]

    splitted_specs <- split_sql_query(dataspecs,
                    keywords = c("drop", "keep", "where", "rename", "firstobs", "obs",
                                 "in", "end", "idxname", "idxwhere", "point", "index", # not used
                                 "pw", "read", "write", "alter", "encrypt", "compress", #not used
                                 "label", "genmax", "gennum")) # not used

    splitted_specs$text <- regex_remove(x = splitted_specs$text, pattern = "^=") |> trimws()

    .extract_spec <- function(x) splitted_specs$text[(splitted_specs$key_word == x)]

    new_object(
      .parent = S7_object(),
      data  = data_info[[1]],
      where = regex_remove(.extract_spec("where"), "^\\(|\\)$"),
      keep  = splitws(.extract_spec("keep")),
      drop  = splitws(.extract_spec("drop")),
      rename = regex_remove(.extract_spec("rename"), "^\\(|\\)$"),
      firstobs = as.numeric(.extract_spec("firstobs")),
      obs      = as.numeric(.extract_spec("obs"))
    )
  }
)




transpile_data_specs <- S7::new_generic("transpile_data_specs", "x")

S7::method(transpile_data_specs, DataSpecs) <- function(x) {
  data_output <- x@data

  ## filter ----
  if(!identical(x@where, character(0))){
    where_as_filter <- x@where |>
      transform_conditions() |>
      transform_functions() |>
      paste_function(function_name = "filter", content = _)

    data_output <- paste(data_output, where_as_filter, sep = " %>%\n\t")
  }
  ## select ----
  keep_as_select <- NULL
  drop_as_select <- NULL
  if (!identical(x@keep, character(0))) {
    keep_as_select <- paste(x@keep, collapse = ", ")
  }
  if (!identical(x@drop, character(0))) {
    drop_as_select <- paste(paste0("-", x@drop), collapse = ", ")
  }
  select_statement <- c(keep_as_select, drop_as_select)
  select_statement <- select_statement[!is.null(select_statement)]


  if(!is.null(keep_as_select)|!is.null(drop_as_select)){
    select_statement <- paste_function(function_name = "select",
                                  content = paste(select_statement, collapse = ", "))

    data_output <- paste(data_output, select_statement, sep = " %>%\n\t")
  }

  ## rename ----
  if (!identical(x@rename, character(0))) {
  rename_statement <- x@rename |>
    transform_functions() |>
    regex_replace("\\s+?=\\s+?", "=") |>
    regex_match_groups("(\\w+=\\w+)")

  old_new <- strsplit(rename_statement[[1]], split = "=")
  rename_old <- sapply(old_new, '[[', 1)
  rename_new <- sapply(old_new, '[[', 2)

  rename_statement <-  paste_function(function_name = "rename",
                                      content = paste(paste(rename_new, "=", rename_old), collapse = ", "))

  data_output <- paste(data_output, rename_statement, sep = " %>%\n\t")
  }

  ## slice ----
  if (!identical(x@obs, numeric(0))) {
    slice_value <- x@obs
    if (!identical(x@firstobs, numeric(0))) {
      slice_value <- paste(x@firstobs, x@obs, sep = ":")
    }
    slice_statement <-  paste_function(function_name = "slice", slice_value)
    data_output <- paste(data_output, slice_statement, sep = " %>%\n\t")
  }

  return(data_output)
}
