#' Add an action
#'
#' @description Add an action to an object (generic method)
#' @param x The object to add the action to.
#' @param ... Additional arguments (enables methods to use them).
#' @return See specific implementations.
#' @export
add_action <- function( x, ... ) {
  UseMethod("add_action",x)
}

#' Iterate (generic)
#'
#' @description Iterate from an object
#' @param x The thing to iterate from/on
#' @param ... Additional arguments.
#' @return See specific implementations.
#' @export
iterate <- function( x, ... ) {
  UseMethod("iterate",x)
}
