sasr_import <- function(code_sas) {
  code_net <- code_sas |>
    remove_string(pattern = regex("proc\\s+import\\s+", ignore_case = T)) |>
    remove_string(pattern = regex("run\\s*;", ignore_case = T)) |>
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
