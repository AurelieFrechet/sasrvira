#' read PROC MEANS
#' @description The means procedure is written as follow :
#' PROC MEANS <options> <statistic-keyword(s)>;
#' BY <DESCENDING> variable-1 <<DESCENDING> variable-2 ...> <NOTSORTED>;
#' CLASS variable(s) </ options>;
#' FREQ variable;
#' ID variable(s);
#' OUTPUT <OUT=SAS-data-set> <output-statistic-specification(s)>
#'   <id-group-specification(s)> <maximum-id-specification(s)>
#'   <minimum-id-specification(s)> </ options> ;
#' TYPES request(s);
#' VAR variable(s) </ WEIGHT=weight-variable>;
#' WAYS list;
#' WEIGHT variable;
#' source : https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/proc/p0f0fjpjeuco4gn1ri963f683mi4.htm
#'
#' This function read a cut sas means procedure, identify each parameter with regex and returns their values.
#' ID, OUTPUT, TYPES and WAYS statements are irrevelant to translate to R. Thus they are not read.
#' @import stringr
#' @param text
#'
#' @return
#' @examples
read_SAS_proc_means <- function(text) {

match_data <- str_match_all(text, "data\\s?=\\s?(\\w+)((\\s\\w+)+)?") %>%  unlist()
pm_data    <- match_data[2]
pm_params  <- match_data[3] %>% str_trim() %>% strsplit(" ") %>% unlist()

match_by     <- str_match_all(text, "by(\\s(\\w+)?\\w+)+") %>%  unlist()

match_class  <- str_match_all(text, "class(\\s\\w+)+(\\s?\\/.+)?") %>%  unlist()

match_freq   <- str_match_all(text, "freq(\\s\\w+)+") %>%  unlist()

match_var    <- str_match_all(text, "var(\\s\\w+)+(\\s?/.+)?") %>%  unlist()

match_weight <- str_match_all(text, "weight(\\s\\w+)+") %>%  unlist()

}
