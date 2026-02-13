sasr_import <- function(sas_code) {
  code_net <- sas_code |>
    regex_remove(pattern = "proc\\s+import\\s+", ignore.case = T) |>
    regex_remove(pattern = "run\\s*;", ignore.case = T) |>
    regex_remove(pattern = ";") |>
    regex_replace(pattern = "\n", replacement = " ") |>
    regex_replace(pattern = "=", replacement = " ") |>
    regex_replace(pattern = "\\s+", replacement = " ") |>
    split_sql_query(
      query = _,
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
