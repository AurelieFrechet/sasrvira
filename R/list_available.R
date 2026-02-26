
# Options -----------------------------------------------------------------
available_proc_means_options <- function() {
  # In transform_functions
  c(
    "\\bKURT\\b",
    "\\bLCLM\\b",
    "\\bUCLM\\b",
    "\\bSKEW\\b",
    "\\bSTDDEV\\b",
    "\\bSTD\\b",
    "\\bN\\(([a-zA-z0-9._]+)\\)",
    "\\bMEAN\\b",
    "\\bMIN\\b",
    "\\bMAX\\b",
    "NMISS\\(([a-zA-z0-9._]+)\\)",
    "\\bP([0-9]+)\\(([a-zA-z0-9._]+)\\)\\b"
  )
}


# Attributes --------------------------------------------------------------

available_proc_means_attributes <- function(){

}
