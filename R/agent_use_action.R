
agent_eval_action <- function(parameters, action, action_over_vars ) {
  do.call( action, parameters[action_over_vars])
}

assign_results <- function() {

}

use_action <- function( agent_type, agent_id = NULL, world = NULL, action = "some_string" ) {

  # if agent id is NULL, assume that ALL agents perform the action
  if( is.null(agent_id)) {
    agent_id <- seq_len( length(agent_type[["individual_agents"]]) )
  }

  selected_action <- agent_type[["actions"]][[action]]
  if( is.null(selected_action) ) {
    rlang::abort( paste0("Action ",
                         action,
                         " for agent ",
                         deparse(substitute(agent_type)),
                         " not found." )
                  )
  }

  # recover the necessary arguments to proceed
  action_var <- rlang::as_string( selected_action[["variable"]] )
  action_fun <- selected_action[["fn"]]
  action_over_parameters <- names(rlang::fn_fmls( action_fun))

  # evaluate all actions used by agents
  action_results <- purrr::map(
    agent_type[["individual_agents"]][agent_id],
              ~agent_eval_action( c( .x[["parameters"]],
                                     world[["environment"]][["parameters"]],
                                     agent_id = agent_id),
                                  action_fun,
                                  action_over_parameters)
    )
  # this dispatches to assign results of individual actions
  # assign_results()

  agents <- purrr::map( agent_id, function(id) {

    agent_type[["individual_agents"]][[id]][["parameters"]][[action_var]] <-
      action_results[[id]]

    return(agent_type[["individual_agents"]][[id]])
  })
  agent_type[["individual_agents"]] <- agents

  return(agent_type)
}
