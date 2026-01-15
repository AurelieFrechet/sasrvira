remove_string <- function(x, pattern,  ignore.case = F, perl = T, ...){
  gsub(pattern = pattern, replacement = "", x = x, ignore.case = ignore.case, perl =  perl,...)
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

  lapply(1:nb_group, function(i){
    match_string(x = res, pattern = pattern, group = i, ignore.case = ignore.case, perl = perl)
    })
}

count_string <- function(x, pattern, ignore.case = T, perl = T, ...){
  match_multiple_string(x = x, pattern = pattern,  ignore.case = ignore.case, perl =  perl, ...)[[1]] |>
    length()
}


locate_string <- function(x, pattern, ignore.case = T, perl = T, ...){
  m <- gregexpr(pattern, text = x, ignore.case = ignore.case, perl = perl)
  res <- regmatches(x, m)[[1]]
  if(identical(res, character(0))) return(NULL)

  m_start <- as.numeric(m[[1]])
  m_length <- as.numeric(attributes(m[[1]])$match.length)
  m_end <- m_start + m_length - 1

  return(data.frame(start = m_start, end = m_end))
  }

paste_function <- function(function_name, content){
  paste0(function_name, "(", content, ")")
}

clean_newlines <- function(text, ...){
  gsub2(x = text, pattern = "\\s|\\r\\n\\t", " ", ...)
}
