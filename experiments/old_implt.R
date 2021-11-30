Nplayers <- 2  # počet hráčů
Nstrategies <- 100  # počet strategií každého hráče
Nsteps <- 10000  # počet kroků simulace (zatím fixní počet kroků, tj. nehlídá se konvergence)


# parametry firmy:

# -- poptávková fce
#    P ... tržní cena; funkce vrací tržní rovnovážne mnozstvi
#    Q[1] = 100 - 5P[1]+2P[2]
A<-c(100,100)
B<-rbind(c(-5,2),c(2,-5))
demand <- function(A,B,P){
  pmax(A + B%*%P,0)
}

max_price <- 100  # max. uvažovaná produkce

# -- funkce zisku jedné firmy při dané ceně
#    price ... tržní cena, quantity ... množství, které firma uvažuje vyrábět;
#    funkce vrací zisk
#    TR = P * q, kde P je tržní cena a q množství, které firma vyrábí
#    TC = c * q, kde c = 10 jsou mezní náklady a q je množství, které firma vyrábí

c<-4
profit <- function(prices, quantity) {
  (prices-c) * quantity
}


# obslužné funkce -------------------------------------------------------------------

# pravděpodobnost, se kterou se zvolí každá strategie v závislosti na jejím fitness
#   fitness ... vektor fitness strategií jednoho hráče;
#   funkce vrací pravděpodobnosti
#   musí vyřešit dvě divné situace:
#   1) když mají všechny strategie stejný fitness -- pak má každá strategie stejnou prst
#   2) když jsou některé fitness záporné -- od fitness každé strategie se odečte fitness
#      nejhorší strategie; pravděpodobnost zvolení je pak proporcionální takto upravené fitness
fitness_probabilities <- function(fitness) {
  # řešení situace, kdy všechny fitness stejné
  if (all(fitness == fitness[1]))
    return(rep(1 / length(fitness), length(fitness)))
  # univerzální řešení i pro záporné fitness (zisky)
  fitness <- fitness - min(fitness)
  fitness / sum(fitness)
}

# aktualizace strategií
sd_mutation <- 1e-3  # parametr: jak moc velké mají být mutace
#    strategies ... vektor strategií, fitness ... vektor odpovídajících fitness;
#    funkce vrací nový vektor strategií
#    strategie se nejdříve replikují -- každá strategie se vybere s pravděpodobností danou
#       svou fitness
#    pak se strategie mutují -- přidá se k nim náhodná složka z N(0, sd_mutation), tj.
#       z normálního rozdělení s nulovou střední hodnotou a směrodatnou odchylkou sd_mutation
#    pak se strateige oříznou do prostoru [0, q_max]
update_strategies <- function(strategies, fitness) {
  Nplayers <- nrow(strategies)
  Nstrategies <- ncol(strategies)
  # replikace
  new_strategies <- t(sapply(seq_len(Nplayers),
                             function(player) sample(strategies[player, ],
                                                     size = Nstrategies,
                                                     replace = TRUE,
                                                     prob = fitness_probabilities(fitness[player, ]))))
  # mutace
  new_strategies <- new_strategies + rnorm(Nplayers * Nstrategies, sd = sd_mutation)
  # oříznutí na přípustná množství (ve skutečnosti tohle není potřeba)
  #new_strategies <- pmax(new_strategies, 0)  # nesmí být záporné
  #new_strategies <- pmin(new_strategies, max_price)  # nesmí být větší než max_quantity
  # return
  new_strategies
}

# výběr ceny, za kterou chce firma prodavat
#   strategies ... matice strategií všech firem, fitness ... matice fitness všech firem
#   funkce vrací vektor produkcí jednotlivých firem
#   pro každou firmu se vybere jedna strategie s pravděpodobností danou minulým odhadem fitness
#      této strategie
select_prices <- function(strategies, fitness) {
  Nplayers <- nrow(strategies)
  sapply(seq_len(Nplayers),
         function(player)
           sample(strategies[player, ],
                  size = 1,
                  prob = fitness_probabilities(fitness[player, ])))
}

# spočtítá nový fitness (= předpověď zisku) pro všechny strategie všech hráčů;
# přitom se (naivně) předpokládá, že tržní cena se nezmění, když daný hráč změní unilaterálně
# svoje množství
#    price ... poslední známá tržní cena
#    strategies ... matice strategií všech hráčů
#    funkce vrací matici fitness

# inicializace simulace -------------------------------------------------------------

# matice strategií (cen) vyráběných všemi firmami; řádky ... firmy, sloupce ... strategie
#   počáteční hodnoty jsou náhodné z U(0, max_quantity), tj. z rovnoměrného rozdělení z [0, max_quantity]
strategies <- matrix(runif(Nplayers * Nstrategies, min = 0, max = max_price),
                     nrow = Nplayers, ncol = Nstrategies)

# matice fitness všech strategií a všech hráčů; řádky ... firmy, sloupce ... strategie;
#   počáteční fitness je stejné pro všechny strategie, tj. každá strategie má stejnou šanci
#   na výběr
actual_fitness <- matrix(1, nrow = Nplayers, ncol = Nstrategies)


# vlastní běh simulace --------------------------------------------------------------
nove_strategie<-matrix(0,Nplayers,Nstrategies)
P<-matrix(0,Nsteps,Nplayers)
Q<-matrix(0,Nsteps,Nplayers)
SD<-matrix(0,Nsteps,Nplayers)
#quantity<-matrix(0,Nplayers)
for (k in seq_len(Nsteps)) {
  # každý hráč zvolí svou cenu
  prices <- select_prices(strategies, actual_fitness)
  # P[k, ]<-prices
  # spočítá se aktuální mnozstvi
  # quantity<-demand(A,B,prices)
  # Q[k, ] <- quantity
  # SD[k,]<-apply(strategies, 1, sd)
  # aktualizuje se fitness jednotlivých strategií
  for (player in 1:Nplayers){
    staronove_ceny <- rbind( strategies[player,],
                             rep( prices[-player],
                                  Nstrategies)
                           )
    if( k == 1 && player == 1){
      print( staronove_ceny)
    }
    # TODO:
    # but agents cant possibly learn this way - they do not know what the
    # demand curve is, to be able to evaluate all strategies at once. that is, sort of, the point
    # the correct way to do genetic algorithms here would be to have some companies "die"
    # and then generate new companies by e.g. combining/perturbing the more successful ones.
    # alternatively if you are playing a two player game, you have to basically replace previous
    # agents by e.g. weighed combinations of previous agents, or constantly permute them somehow.
    # but allowing the agent to basically infer "had I played anything else, I would have gotten this much"
    # amounts to not solving a learning problem. Because you have handed the agent a complete model of the world,
    # never mind that this takes the price chosen by the other player (which can oftentimes be hidden) as a known thing
    # no wonder this converges so quickly - it amounts to doing a grid search (with permutation) by both players,
    # where they could evaluate the exact model of the environment however many times they wanted.
    staronove_mnozstvi<-demand(A,B,staronove_ceny)
    # if( k == 1 && player == 1){
    #   print( profit(strategies[player,],staronove_mnozstvi[1,]))
    # }
    nove_strategie[player,]<-profit(strategies[player,],staronove_mnozstvi[1,])
  }
  actual_fitness<-nove_strategie


  # aktualizují se strategie
  strategies <- update_strategies(strategies, actual_fitness)
  # aktualizuje se fitnes nových strategií
  for (player in 1:Nplayers){
    staronove_ceny<-rbind(strategies[player,],rep(prices[-player],Nstrategies))
    staronove_mnozstvi<-demand(A,B,staronove_ceny)
    nove_strategie[player,]<-profit(strategies[player,],staronove_mnozstvi[1,])
  }
  actual_fitness<-nove_strategie
}
