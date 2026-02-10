sasr_import <- function(sas_code) {
  code_net <- sas_code |>
    remove_string(pattern = "proc\\s+import\\s+", ignore.case = T) |>
    remove_string(pattern = "run\\s*;", ignore.case = T) |>
    remove_string(pattern = ";") |>
    gsub2(pattern = "\n", replacement = " ") |>
    gsub2(pattern = "=", replacement = " ") |>
    gsub2(pattern = "\\s+", replacement = " ") |>
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
