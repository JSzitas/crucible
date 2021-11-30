# TODO:
merge_identical_agent_types <- function( agents ) {
  # list( a = c(1,2,3), b = c(1,2), a = c(4,5,6) ) ->>
  # list( a = c(1,2,3,4,5,6), b = c(1,2) )
  agent_types <- names(agents)

  # index the agents to prevent unnecessary checks ...
  # agent_type_index <-

  for( agent_type in agent_types ) {
    matches = which( agent_type == agent_types )
  }
  merged <- agents
  return(merged)
}

check_agent_names <- function( maybe_names) {
  purrr::map_lgl( maybe_names, function( maybe_name ) {
    maybe_name != "" & !is.null(maybe_name)
  })
}

# I hate that this almost sounds object oriented
# hilariously, we can exploit that due to all our operators putting the agents
# at the tail of the expression, this allows us to **very** conveniently
# grab the agent names - this always grabs the last thing in the expression
# if this still fails to yield a nice, valid name, we just dub the agent "unnamed"
agent_name_resolver <- purrr::safely( function(agent_expr) {
  len_x <- length(agent_expr)
  if( len_x == 1 ) {
    # we only have a single agent, ie not 10 * agent or such
    return( rlang::as_string(agent_expr) )
  }
  # we have an agent that is a compound expression - we have to index the
  # last part of the expression, i.e. 10 * agent ->> agent, and then we
  # have to index into that (since it is a list)
  return( rlang::as_string( first(last(agent_expr) )))
}, otherwise = "unnamed" )
