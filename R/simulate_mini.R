simulate_mini <- function(data, Nsims = 100, groups = 3, factors, burnin, minprob,
                          stratify = NULL, ratio = rep(1, groups)) {

  inputs <- expand.grid(sim.no = 1:Nsims,
                        burnin = c(15, 10),
                        minprob = list(c(0.8, 0.1, 0.1),
                                       c(0.8, 0.15, 0.05)))

  sims <- mapply(
    function(x, y) minimise(data, factors = c("sex", "stage"), burnin = x,
                            minprob = y),
    inputs$burnin, inputs$minprob, SIMPLIFY = FALSE
  )

  inputs$minprob <- sapply(inputs$minprob, paste, collapse = ", ")

  imbalance <- mapply(function(x) balance(x)$imbalance, sims)
  group.sizes <- t(sapply(sims, with, table(Group)))

  out <- list(inputs = inputs, simulations = sims, group.sizes = group.sizes,
              imbalance = imbalance)

  class(out) <- "mini.sim"

  return(out)

}

print.mini.sim <- function(x, ...) {

  temp <- x
  temp$inputs$imbalance <- sims$imbalance
  tab <- with(temp$inputs, tapply(imbalance, list(burnin, minprob), FUN=mean))
  tab <- round(tab, 2)

  cat("Simulation of Multi-arm Minimisation \n")
  cat(rep("-", 80), "\n", sep = "")
  cat("Number of simulations:", max(x$inputs$sim.no), "\n")
  cat("Burnin options:", paste(unique(x$inputs$burnin), collapse = ", "), "\n")
  cat("Minimisation probability options:",
      paste(unique(x$inputs$minprob), ncollapse = "; "), "\n")
  cat("Average imbalance:\n")
  cat(knitr::kable(tab, format = "markdown"), sep = "\n")
}

temp <- sims$inputs
temp$groups <- sims$group.sizes

tab <- aggregate(groups ~ burnin + minprob, data=temp, FUN = function(x) round(mean(x), 2))

knitr::kable(tab, format = "markdown")
tab
