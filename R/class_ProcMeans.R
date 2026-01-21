splitws <- function(text){
  text |>
  strsplit(split = " ") |>
    unlist() |>
    trimws()
}

# Define class ------------------------------------------------------------

ProcMeans <- S7::new_class(
  "ProcMeans",

  properties = list(
    pm_data    = S7::class_character,
    pm_stats   = S7::class_character,
    pm_var     = S7::class_character,
    pm_by      = S7::class_character,
    pm_class   = S7::class_character,
    pm_format  = S7::class_character,
    pm_freq    = S7::class_character,
    pm_id      = S7::class_character,
    pm_types   = S7::class_character,
    pm_weight  = S7::class_character,
    pm_ways    = S7::class_character,
    pm_output  = S7::class_list,
    sas_source = S7::class_character
  )
)


# Cosntructor -------------------------------------------------------------

proc_means <- function(code_sas) {
  code_net <- code_sas |>
    remove_string(pattern  = "proc\\s*means\\s", ignore.case = T) |>
    remove_string(pattern  = "run\\s*;", ignore.case = T) |>
    remove_string(pattern  = ";") |>
    gsub2(pattern = "\n|=|\\s+", replacement = " ") |>
    decoupe_requete(
      keywords = c(
        "data",
        "var",
        "by",
        "class",
        "format",
        "freq",
        "id",
        "types",
        "weight",
        "ways",
        "output"
      )
    )

  .extract_args <- function(keyword){
    splitws(code_net$text[(code_net$key_word == keyword)])
  }

  # DATA=data <option>
  net_data <- .extract_args("data")

  ## data=
  proc_means_data  <- net_data[1]

  ## Options (stats)
  proc_means_stats <- net_data[-1]


  ## Output as a list
  if(!identical(.extract_args("output"), character(0))){
    output <- decoupe_requete(
      requete = code_net$text[(code_net$key_word == "output")],
      keywords = c("out", "n", "mean", "std", "skewness", "kurtosis") # TODO  préparer un vecteur de mots clés
    )
    output_list <- as.list(output$text)
    output_list <- lapply(output_list, splitws)
    names(output_list) <- output$key_word

  } else {
    output_list <- list()
  }


  ProcMeans(
    pm_data    = proc_means_data,
    pm_stats   = proc_means_stats,
    pm_var     = .extract_args("var"),
    pm_by      = .extract_args("by"),
    pm_class   = .extract_args("class"),
    pm_format  = .extract_args("format"),
    pm_freq    = .extract_args("freq"),
    pm_id      = .extract_args("id"),
    pm_types   = .extract_args("types"),
    pm_weight  = .extract_args("weight"),
    pm_ways    = .extract_args("ways"),
    pm_output  = output_list,
    sas_source = code_sas

  )
}


# Method: transpile to_dplyr -------------------------------------------------------

transpile <- S7::new_generic("transpile", "x")

S7::method(transpile, ProcMeans) <- function(x) {

  # init output
  dplyr_new_df <- NA
  dplyr_data <- NA
  dplyr_select <- NA
  dplyr_groupby <- NA
  dplyr_summarize <- NA

  # data ----
  nb_vars <- length(x@pm_var)
  more_than_1_var <- nb_vars > 1 || any(grepl("-", x@pm_var))
  is_default_stats <- length(x@pm_stats) == 0
  dplyr_data <- x@pm_data

  # select() ----
  if (!identical(x@pm_var, character(0))) {
    ## If only one VAR, no select()
    if ((nb_vars == 1 & is_default_stats) | more_than_1_var) {
      dplyr_select <- x@pm_var |>
        gsub2(pattern = "-", replacement = ":")

      dplyr_select <- paste_function("select", paste(dplyr_select, collapse = ", "))
    }
  }

  # group_by : Combine BY and CLASS ----
  if (!identical(x@pm_by, character(0)) | !identical(x@pm_class, character(0))) {
    dplyr_groupby <- c(x@pm_by, x@pm_class)

    dplyr_groupby <- paste_function("group_by", paste(dplyr_groupby, collapse = ", "))

  }

  # summarise stats ----
  if (is_default_stats) { # summary
    dplyr_summarize <- "summary()"

  } else if (!more_than_1_var) { # summarize
    dplyr_summarize <- paste0(x@pm_stats, "(", x@pm_var, ")") |>
      transform_functions()
    dplyr_summarize <-  paste_function("summarize", paste(dplyr_summarize, collapse = ", "))

  } else { # summarize_all
    dplyr_summarize <- paste(paste(x@pm_stats, x@pm_stats, sep = "="), collapse = ", ")
    dplyr_summarize <- paste0("summarize_all(list(", dplyr_summarize, "))")
  }

  # output overwrite ----
  if(length(x@pm_output) > 0){
    dplyr_new_df <- x@pm_output$out
    dplyr_select <- NA
    output_stats <- x@pm_output[names(x@pm_output) != "out"]

    named_stats <- lapply(seq_along(output_stats), function(i) {
      stat_function <- transform_functions(names(output_stats)[i])
      name_output <- output_stats[[i]]
      paste(name_output, "=", paste_function(stat_function, x@pm_var[1:length(name_output)]))
    }) |> unlist()

    dplyr_summarize <-  paste_function("summarize", paste(named_stats, collapse = ", "))
  }

  # return ----
  requete_dplyr <- c(dplyr_data, dplyr_groupby, dplyr_select, dplyr_summarize)

  requete_dplyr <- requete_dplyr[!is.na(requete_dplyr)] |>
    paste(collapse = " %>%\n\t")

  if(!is.na(dplyr_new_df)) {
    requete_dplyr <- paste(dplyr_new_df, "<-", requete_dplyr)
  }

  return(requete_dplyr)
}
