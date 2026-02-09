sasr_data <- function(sas_code){
  # Déclaration des variables
  data_table        <- NA
  set_table         <- NA
  variables_brut    <- NA
  length_variables  <- NA
  input_variabless  <- NA
  datalines         <- NA
  keep_variables    <- NA
  drop_variables    <- NA
  where             <- NA

  # data lib.table;
  data_table <- match_multiple_string(sas_code, pattern   = "^data (\\w+\\.)?(\\w+) ?;")

  # <set lib.table;>
  set_table <- match_multiple_string(x = sas_code, pattern   = "^set (\\w+\\.)?(\\w+) ?;")

  # var = contenu;
  variables_brut <- match_multiple_string(sas_code, pattern   = "^(\\w+) ?= ?(\\w+) ?;", )

  # <length var format;>
  length_variables
  #<input var contenu;>
  input_variabless
  # datalines;
  # matrice donnees;
  datalines

  keep_variables
  drop_variables
  where


}


