
env_init <- function( env ) {
  # this should be much simples than agents
  env[["parameters"]] <- init_param_with_env(env[["init"]])
  return( env[c("parameters","actions")] )
}
