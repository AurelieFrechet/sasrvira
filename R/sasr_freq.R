sasr_freq <-function(code_sas){
  code_net <- code_sas |>
    remove_string(pattern = regex("proc\\s+freq\\s+", ignore_case = T)) |>
    remove_string(pattern = regex("run\\s*;", ignore_case = T)) |>
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
