init_agent <- function( ... ) {

  e <- rlang::env(
    rlang::caller_env(),
    `*` = function(times, agent) purrr::map( seq_len(times), function(i) agent )
  )
  agents <- eval( rlang::expr(...) , envir = e)
  # check that we actually have multiple agents, not just a single one
  if( check_agents(agents) ) {
    new_agents <- list( actions = agents[[1]][["actions"]],
                        # this super ugly constructor is necessary if you want to have list elements
                        # which we do, since that is how we pass our arguments (as a list)
                        individual_agents = purrr::map( seq_len(length(agents)), ~list()))

    for( agent_id in seq_len(length(agents)) ) {
      # there is some argument for having character ids, but until we need them,
      # lets stick to numeric ids. if we didnt need this id this could be a map,
      # but this way it seems a bit more understandable than map2
      new_agents[["individual_agents"]][[agent_id]][["parameters"]] <- init_param_with_env(
        agents[[agent_id]]$init)
    }
  }
  else {
    new_agents <- list( actions = agents[["actions"]], individual_agents = NULL )
    new_agents[["individual_agents"]][[1]][["parameters"]] <- init_param_with_env(
      agents$init)
  }
  return(new_agents)
}

check_agents <- function( agents ) {
  class(agents) != "crucible_agent"
}
#' Initialize a population of agents
#'
#' @description Initialize an arbitrary number of agents
#' @param ... Agent definitions - see examples.
#' @return Initialized agents.
#' @details Agent definitions are quite flexible - even unnamed agents are
#' permitted. The only requirement is that if the "*" operator is used in
#' agent definitions, these must not be named at the same time, i.e.
#' 100 * some_agent = agent(...) is sadly not permitted. All other variants of
#' agent definitions should work - see examples.
#' @export
#'
#' @examples {
#' # initialized an agent without a name - assigns the "unnamed" name automatically
#' init_agents( agent(z = 100) )
#' # initializes 2 agents at the same time, both unnamed (will be unnamed_1 and unnamed_2)
#' init_agents( agent(z = 100), agent(k = runif(10)) )
#' # initializes 100 unnamed agents of the same type (these will all get the unnamed name, being treated
#' # as the same agent type)
#' init_agents( 100 * agent(z = rnorm(100)) )
#' # initialize some named, and some unnamed agents
#' init_agents( named_agent = agent(k = 1), agent(a = 1) )
#' }
#'
init_agents <- function( ... ) {
  # TODO: figure out how to make this fail when supplied agents arent valid.
  agent_exprs <- rlang::exprs(...)
  agent_population <- purrr::map(agent_exprs, init_agent)
  # if the user specified the names in the expression, this is super convenient >>
  # we can just grab them using the names
  agent_names <- names(agent_exprs)
  # otherwise, it might seem to be a bit more difficult... but not really
  valid_agent_names <- check_agent_names(agent_names)
  # find names for agents for which we werent able to resolve the names already
  if( !all(valid_agent_names) ) {
    agent_names[!valid_agent_names] <-
      purrr::map_chr( agent_exprs[!valid_agent_names],
                                   function(expr) agent_name_resolver(expr)[["result"]] )
  }
  agent_names <- disambiguate_names(agent_names)

  return(stats::setNames( agent_population, agent_names))
}
