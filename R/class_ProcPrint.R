#' @include class_ProcSAS.R

ProcPrint <- S7::new_class(
  "ProcPrint",
  parent = ProcSAS
)

# Method: transpile ----------------------------------------------------------

S7::method(transpile, ProcPrint) <- function(x) {
  code_r <- x@proc_data
  return(code_r)
}

