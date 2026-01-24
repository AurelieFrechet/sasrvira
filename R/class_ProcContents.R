#' @include class_ProcSAS.R

ProcContents <- S7::new_class(
  "ProcContents",
  parent = ProcSAS
)

# Method: transpile ----------------------------------------------------------
S7::method(transpile, ProcContents) <- function(x) {
  code_r <- paste_function("str", x@proc_data)
  return(code_r)
}

