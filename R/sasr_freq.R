sasr_freq <-function(sas_code){
  code_net <- sas_code |>
    remove_string(pattern = "proc\\s+freq\\s+", ignore.case = T) |>
    remove_string(pattern = "run\\s*;", ignore.case = T) |>
    remove_string(pattern = ";") |>
    gsub2(pattern = "\n", replacement = " ") |>
    gsub2(pattern = "=", replacement = " ") |>
    gsub2(pattern = "\\s+", replacement = " ") |>
    decoupe_requete(
      requete = _,
      keywords = c("data",
                    "by",
                    "exact",
                    "output",
                    "tables",
                    "test",
                    "weight")
    )
}
