last <- function( x ) {
  x[length(x)]
}
first <- function( x ) {
  x[[1]]
}

init_param_with_env <- function( inits) {
  custom_env <- new.env()
  param_names <- names(inits)

  for( name in param_names ) {
    assign( x = name,
            value = eval( inits[name][[1]],
                          envir = custom_env),
            envir = custom_env)
  }
  return(as.list(custom_env))
}

disambiguate_names <- function(names) {
  unique_names <- unique(names)
  counts <- purrr::map_int( unique_names,
                            function(unique_name){ sum(names == unique_name)})
  ambiguous_by_count <- counts > 1
  if( !any(ambiguous_by_count) ) {
    return(names)
  }
  ambiguous <- unique_names[ambiguous_by_count]
  replacements <- purrr::map2(ambiguous, counts[ambiguous_by_count], function( name, copies ) {
    if( copies == 1 ) return(NULL)
    # otherwise
    paste0( name, "_", seq_len(copies))
  })
  names(replacements) <- ambiguous
  for( ambig in ambiguous ) {
    names[ names == ambig ] <- unlist(replacements[ names(replacements) == ambig ])
  }

  return(names)
}
