# test script
remove(list = ls())
pkgload::load_all()

# a minmax function - scales all values to be positive, and bounded between 0 and 1,
# but crucially, these values will not sum to 1 - you need x/sum(x) on top of this for that
minmax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

duopoly_agent <- agent(
  cost = 4,
  sd_mutation = 1e-3,
  strategy = runif(1)
) %>%
  add_action( "set_prices", price ~ function( strategy ) {
    strategy
  }) %>%
  add_action( "die", dead ~ function( profit ) {
    (profit - cost) < 0
  })

mutate_agents( duopoly_agent,  )


example <- init_agents(2*duopoly_agent)


# env <- environment( prices = runif(1) ) %>%
#   env_init()
#
# wrld <- world( example, env )




