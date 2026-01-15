sasr_import <- function(code_sas) {
  code_net <- code_sas |>
    remove_string(pattern = "proc\\s+import\\s+", ignore.case = T) |>
    remove_string(pattern = "run\\s*;", ignore.case = T) |>
    remove_string(pattern = ";") |>
    gsub2(pattern = "\n", replacement = " ") |>
    gsub2(pattern = "=", replacement = " ") |>
    gsub2(pattern = "\\s+", replacement = " ") |>
    decoupe_requete(
      requete = .,
      keywords = c("datafile",
                    "table",
                    "out",
                    "file",
                    "dbms",
                    "sheet",
                    "delimiter",
                    "getnames")
    )
}
