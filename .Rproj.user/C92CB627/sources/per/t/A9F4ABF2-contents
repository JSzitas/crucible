# add an action to an agent
#' @export
add_action.crucible_agent <- function( x, alias, action, ... ) {
  # TODO: The x is the agent - document this properly
  # we need enquo since steps will probably contain functions (and optionally parameters)
  # defined in the outside scope - so it would be really dumb to mask them with things defined
  # in whatever environment the steps wind up being evaluated in
  action <- parse_action(alias, action)
  x[["actions"]][[alias]] <- action[c("variable","fn")]
  return(x)
}

parse_action <- function( alias, action ) {

  variable <- rlang::f_lhs(action)
  var_expr <- eval(rlang::f_rhs(action))

  structure( list( alias = alias,
                   variable = variable,
                   fn = var_expr ),
             class = "agent_action")
}
