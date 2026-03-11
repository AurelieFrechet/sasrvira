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
