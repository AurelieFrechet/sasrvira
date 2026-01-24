ProcSAS <- new_class(
  "ProcSAS",
  properties = list(
    source       = S7::class_character,
    proc_data    = S7::class_character,
    proc_options = S7::class_character
  ),
  constructor =
    function(code_sas) {
      splitted_proc <- code_sas |> strsplit(split = ";") |> unlist() |> trimws()

      infos_contents <- splitted_proc[1] |>
        remove_string(pattern  = "proc\\s*\\w+\\s", ignore.case = T) |>
        remove_string(pattern  = "data\\s?=", ignore.case = T) |>
        trimws() |> splitws()

      new_object(
        .parent = S7_object(),
        source = code_sas,
        proc_data    = infos_contents[1],
        proc_options = infos_contents[-1]
      )
    }
)

transpile <- S7::new_generic("transpile", "x")

S7::method(transpile, ProcSAS) <- function(x) {
  stop("Not implemented")
}

