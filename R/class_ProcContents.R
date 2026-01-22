ProcContents <- S7::new_class(
  "ProcContents",

  properties = list(
    pc_data    = S7::class_character,
    pc_options = S7::class_character
  )
)


# Constructor -------------------------------------------------------------

proc_contents <- function(code_sas){
  code_net <- code_sas |>
    remove_string(pattern  = "proc\\s*contents\\s", ignore.case = T) |>
    remove_string(pattern  = "run\\s*;", ignore.case = T) |>
    remove_string(pattern  = "data\\s?=", ignore.case = T) |>
    remove_string(pattern  = ";")

  infos_contents <- splitws(code_net)

  ProcContents(
    pc_data    = infos_contents[1],
    pc_options = infos_contents[-1])
}

# Method: transpile ----------------------------------------------------------

transpile2 <- S7::new_generic("transpile2", "x")

S7::method(transpile2, ProcContents) <- function(x) {
  code_r <- paste_function("str", x@pc_data)
  return(code_r)
}

