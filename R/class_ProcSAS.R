ProcSAS <- new_class(
  "ProcSAS",
  properties = list(
    proc_data    = S7::class_character,
    proc_options = S7::class_character
  ),
  constructor =
    function(code_sas) {
      code_net <- code_sas |>
        remove_string(pattern  = "proc\\s*\\w+\\s", ignore.case = T) |>
        remove_string(pattern  = "run\\s*;", ignore.case = T) |>
        remove_string(pattern  = "data\\s?=", ignore.case = T) |>
        remove_string(pattern  = ";")

      infos_contents <- splitws(code_net)

      new_object(
        .parent = S7_object(),
        proc_data    = infos_contents[1],
        proc_options = infos_contents[-1]
      )
    }
)

transpile2 <- S7::new_generic("transpile2", "x")

S7::method(transpile2, ProcSAS) <- function(x) {
  stop("Not implemented")
}









ProcContents <- S7::new_class(
  "ProcContents",
  parent = ProcSAS
)

# Method: transpile ----------------------------------------------------------
S7::method(transpile2, ProcContents) <- function(x) {
  code_r <- paste_function("str", x@proc_data)
  return(code_r)
}

