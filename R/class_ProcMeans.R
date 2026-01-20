splitws <- function(text){
  text |>
  strsplit(split = " ") |>
    unlist() |>
    trimws()
}

# Define class ------------------------------------------------------------

ProcMeans <- new_class(
  "ProcMeans",

  properties = list(
    pm_data    = class_character,
    pm_stats   = class_character,
    pm_var     = class_character,
    pm_by      = class_character,
    pm_class   = class_character,
    pm_format  = class_character,
    pm_freq    = class_character,
    pm_id      = class_character,
    pm_types   = class_character,
    pm_weight  = class_character,
    pm_ways    = class_character,
    pm_output  = class_character,
    sas_source = class_character
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
  if (identical(net_data[-1], character(0))) {
    proc_means_stats <- "default"
  } else {
    proc_means_stats <- net_data[-1]
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
    pm_output  = .extract_args("output"),
    sas_source = code_sas

  )
}


# Method: transplie -------------------------------------------------------

to_dplyr <- new_generic("to_dplyr", "x")

method(to_dplyr, ProcMeans) <- function(x) {

  # init output
  dplyr_new_var <- NA
  dplyr_data <- NA
  dplyr_select <- NA
  dplyr_groupby <- NA
  dplyr_summarize <- NA

  # data ----
  dplyr_data <- x@pm_data

  # select() ----
  if (!identical(x@pm_var, character(0))) {
    dplyr_select <- x@pm_var %>%
      str_replace_all(pattern = "-", replacement = ":") %>%
      str_replace_all(pattern = "\\s+", replacement = ", ") %>%
      paste0("select(", ., ")")
  }

  # group_by ----
  dplyr_groupby <- NA
  if (any(code_net$kw == "by") | any(code_net$kw == "class")) {
    dplyr_groupby <- paste(x@pm_by,x@pm_class) %>%
      str_trim() %>%
      str_replace_all(pattern = "\\s+", replacement = ", ") %>%
      paste_function(fonction = "group_by", content = _)
  }


  # return ----
  requete_dplyr <- c(dplyr_data, dplyr_select, dplyr_groupby, dplyr_summarize)

  requete_dplyr <- requete_dplyr[!is.na(requete_dplyr)] |>
    paste(collapse = " %>%\n\t")

  if(!is.na(dplyr_new_var)) {
    requete_dplyr <- paste(dplyr_new_var, "<-", dplyr_new_var)
  }

  return(dplyr_new_var)
}
