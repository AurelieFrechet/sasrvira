remove_string <- function(x, pattern, ...){
  gsub(pattern = pattern, replacement = "", x = x, ...)
}

gsub2 <- function(x, pattern,  replacement,  ignore.case = T, perl = T, ...){
  gsub(pattern = pattern, replacement = replacement, x = x,  ignore.case = ignore.case, perl =  perl, ...)
}

match_string <- function(x, pattern, group = 1, ignore.case = T, perl = T, ...){
  gsub(pattern = pattern, replacement = paste0("\\", group), x = x, ignore.case = ignore.case, perl =  perl, ...)
}


match_multiple_string <- function(x, pattern, nb_group = NULL, ignore.case = T, perl = T, ...){
  m <- gregexpr(pattern, text = x, ignore.case = ignore.case, perl = perl)
  res <- regmatches(x, m)[[1]]
  if(identical(res, character(0))) return(NULL)

  if(is.null(nb_group)) nb_group <- lengths(regmatches(pattern, gregexpr('\\(', pattern)))

  sapply(1:nb_group, function(i){
    match_string(x = res, pattern = pattern, group = i, ignore.case = ignore.case, perl = perl)
    })
}

paste_function <- function(fonction, contenu){
  paste0(fonction, "(", contenu, ")")
}
