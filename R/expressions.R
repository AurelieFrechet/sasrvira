#' #' Expressions regex
#' #'
#' #' @description différentes expression regex correspondants aux blocs principaux de code SAS
#' #'
#' #'@references http://www.lsta.upmc.fr/Lejeune/SAS_Lejeune.pdf
#' #'
#'
#' comment1 <-"(?=(^\\*)).*?(?=\\n)" # Commentaire 1 ligne
#' comment2 <-"(?=(^\\/\\*))(\\n|.)*?(?<=\\*\\/)" # Commentaire multilignes
#' libname  <- "(?=(^libname)).*?(?<=;)" # libnanme
#'
#'
#' # Etape Data --------------------------------------------------------------
#' data_run       <- "(?=^data)(\\n|.)*?(?<=run;)"
#'
#' # Procédures --------------------------------------------------------------
#' # PROC CONTENTS
#' proc_contents  <- "(?=^proc contents)(.|\\n)*?(?<=run;)"
#' # PROC PRINT
#' proc_print     <- "(?=(^proc print))(.|\\n)*?(?<=run;)"
#' # PROC SORT
#' proc_sort      <- "(?=(^proc sort))(.|\\n)*?(?<=run;)"
#' # PROC STANDARD
#' proc_standard  <- "(?=^proc standard)(.|\\n)*?(?<=run;)"
#' # PROC TRANSPOSE
#' proc_transpose <- "(?=^proc transpose)(.|\\n)*?(?<=run;)"
#' # PROC MEANS
#' proc_means     <- "(?=^proc means)(.|\\n)*?(?<=run;)"
#' # PROC CORR
#' proc_corr      <- "(?=^proc corr)(.|\\n)*?(?<=run;)"
#' # PROC UNIVARIATE
#' proc_univariate<- "(?=^proc univariate)(.|\\n)*?(?<=run;)"
#' # PROC FREQ
#' proc_freq      <- "(?=^proc freq)(.|\\n)*?(?<=run;)"
#' # PROC REG
#' proc_reg       <- "(?=^proc reg)(.|\\n)*?(?<=run;)"
#' # PROC GLM
#' proc_glm       <- "(?=^proc glm)(.|\\n)*?(?<=run;)"
#' # PROC GENMOD
#' proc_genmod    <- "(?=^proc genmod)(.|\\n)*?(?<=run;)"
#' # PROC PLOT
#' proc_plot      <- "(?=^proc plot)(.|\\n)*?(?<=run;)"
#' # PROC GPLOT
#' proc_gplot     <- "(?=^proc gplot)(.|\\n)*?(?<=run;)"
#' # PROC PLOT
#' proc_chart     <- "(?=^proc chart)(.|\\n)*?(?<=run;)"
#' # PROC GPLOT
#' proc_gchart    <- "(?=^proc gchart)(.|\\n)*?(?<=run;)"
#' # PROC SQL
#' proc_sql       <- "(?=^proc sql)(.|\\n)*?(?<=quit;)"
#'
#'
#'
#' # Regroupement ------------------------------------------------------------
#'
#' cut_expressions <- paste(comment1,
#'                          comment2,
#'                          libname,
#'                          data_run,
#'                          proc_contents,
#'                          proc_print,
#'                          proc_sort,
#'                          proc_standard,
#'                          proc_transpose,
#'                          proc_means,
#'                          proc_corr,
#'                          proc_univariate,
#'                          proc_freq,
#'                          proc_reg,
#'                          proc_glm,
#'                          proc_genmod,
#'                          proc_plot,
#'                          proc_gplot,
#'                          proc_chart,
#'                          proc_gchart,
#'                          proc_sql, sep = "|")
#'
#'
