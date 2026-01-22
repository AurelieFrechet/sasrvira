ProcPrint <- S7::new_class(
  "ProcPrint",

  properties = list(
    pp_data    = S7::class_character,
    pp_options = S7::class_character
  )
)


# Constructor -------------------------------------------------------------

proc_print <- function(code_sas){
  code_net <- code_sas |>
    remove_string(pattern  = "proc\\s*print\\s", ignore.case = T) |>
    remove_string(pattern  = "run\\s*;", ignore.case = T) |>
    remove_string(pattern  = "data\\s?=", ignore.case = T) |>
    remove_string(pattern  = ";")

  infos_contents <- splitws(code_net)

  ProcPrint(
    pp_data    = infos_contents[1],
    pp_options = infos_contents[-1])
}

# Method: transpile ----------------------------------------------------------

transpile3 <- S7::new_generic("transpile3", "x")

S7::method(transpile3, ProcPrint) <- function(x) {
  code_r <- x@pp_data
  return(code_r)
}

