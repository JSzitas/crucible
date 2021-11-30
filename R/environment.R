

environment <- function( ... ) {
  structure(
    list( init = rlang::exprs( ... ),
          actions = list() ),
    class = "crucible_environment"
  )
}
