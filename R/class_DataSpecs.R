DataSpecs <- new_class(
  "ProcSAS",
  properties = list(
    data  = S7::class_character,
    where = S7::class_character,
    keep  = S7::class_character,
    drop  = S7::class_character
  )
  ,
  constructor = function(data_statement) {
    data_statement <- regex_replace(data_statement, "\\s+", " ")
   data_info <- regex_match_groups(
     data_statement,
      pattern = "([A-z0-9.]+)\\s?\\((drop|keep|where)\\s?=\\s?(.*)\\)"
      )

   check_statement <- function(x) if(data_info[[2]]==x) data_info[[3]] else character(0)

   new_object(
     .parent = S7_object(),
     data  = data_info[[1]],
     where = regex_remove(check_statement("where"), "\\(|\\)"),
     keep  = splitws(check_statement("keep")),
     drop  = splitws(check_statement("drop"))
   )
  }
)




transpile_data_specs <- S7::new_generic("transpile_data_specs", "x")

S7::method(transpile_data_specs, DataSpecs) <- function(x) {
  data_output <- x@data

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

    data_output <- paste(data_output, select_statement, sep = " %>%\n")
  }

  ## filter ----
  if(!identical(x@where, character(0))){
  where_as_filter <- x@where |>
    transform_conditions() |>
    paste_function(function_name = "filter", content = _)

  data_output <- paste(data_output, where_as_filter, sep = " %>%\n")
  }

  return(data_output)
}
