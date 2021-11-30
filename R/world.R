# world

# world keeps track of all agents and the environment.
# tracks e.g. parameter evolution, iterations... and so on

world <- function( agents, environment, ... ) {

  structure( list( agents = agents,
                   environment = environment ),
             class = "crucible_world")
}
#' @export
iterate.crucible_world <- function( world, times = 1 ) {
  return(world)
}

#' @importFrom dplyr mutate
#' @export
mutate.world <- function( agent_type,
                          fitness,
                          mutate_dead_agents = FALSE,
                          mutation_rate = 0.001 ) {

}

