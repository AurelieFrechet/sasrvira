sasr_freq <-function(code_sas){
  code_net <- code_sas |>
    remove_string(pattern = "proc\\s+freq\\s+", ignore.case = T) |>
    remove_string(pattern = "run\\s*;", ignore.case = T) |>
    remove_string(pattern = ";") |>
    gsub2(pattern = "\n", replacement = " ") |>
    gsub2(pattern = "=", replacement = " ") |>
    gsub2(pattern = "\\s+", replacement = " ") |>
    decoupe_requete(
      requete = .,
      keywords = c("data",
                    "by",
                    "exact",
                    "output",
                    "tables",
                    "test",
                    "weight")
    )
}
