sasr_sort <- function(code_sas) {
   # Nettoyage et découpage de la procédure
   code_net <- code_sas |>
     remove_string(pattern = "proc\\ssort\\s", ignore.case = T) |>
     remove_string(pattern = "run\\s*;", ignore.case = T) |>
     remove_string(pattern = ";") |>
     gsub2(pattern = "\n", replacement = " ") |>
     gsub2(pattern = "=", replacement = " ") |>
     gsub2(pattern = "\\s+", replacement = " ") |>
      decoupe_requete(keywords = c("data", "out", "by"))

   # DATA
   new_data <- code_net$text[code_net$key_word == "data"]

   # OUT : Optionnel
   new_out <- code_net$text[code_net$key_word == "out"]


   # BY
   script <- code_net$text[code_net$key_word == "by"] |>
      gsub2(pattern = "descending\\s(\\w+)", replacement = "desc(\\1)") |>
      gsub2(pattern = "\\s+", replacement = ", ")

   script <- paste0(new_data, " %>% arrange(", script , ")")


   # Si option OUT écriture dans un objet
   if(!identical(new_out, character(0))){
      script <- paste(new_out, script, sep = " <- ")
   }

   return(script)

}
