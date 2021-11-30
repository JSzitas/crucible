
remove(list = ls())
pkgload::load_all()

# suitors and acceptors - acceptor can "accept" a suitor (from some suitor pool)
# and this gives the acceptor a hidden reward (dependent on the suitor)
# and a suitor some reward

bounded <- function( x, lower = 0, upper = 1 ) {
  x[ x > upper ] <- upper
  x[ x < lower ] <- lower
  return(x)
}

suitor <- agent( quality = runif(1),
                 expressed_quality = bounded(quality + rnorm(1, sd = 0.15))) %>%
  add_action( "apply",
              # we need the suitor to be able to communicate with the acceptor - thus we
              # need a way to post a variable to the acceptor
              # something like this? (it does complicate our parsing ...)
              application|some(acceptor, 5) ~ function( acceptors ) {
                expressed_quality # start with the simple case where the agent expresses the same
                # quality to all acceptors
               })

acceptor <- agent( quality_requirements = runif(1) ) %>%
  add_action( "select",
              accepted|respond(suitor) ~ function(application) {
                # for now, accept all suitors which pass the requirements
                accepted <- application > quality_requirements
              }
              )












