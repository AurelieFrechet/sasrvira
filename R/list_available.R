
# Options -----------------------------------------------------------------
available_proc_means_stats <- function() {
  # In transform_functions
  c("\\bN\\b",
    "\\bSUM\\b",
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
    "\\bNMISS\\(([a-zA-z0-9._]+)\\)",
    "\\bP([0-9]+)\\b",
    "\\bRANGE\\b"
  )
}


# Attributes --------------------------------------------------------------

available_proc_means_attributes <- function(){

}
