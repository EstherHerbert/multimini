#' Simulation of minimisation
#'
#' `simulate_mini` repeatedly minimises a data set of patients with different
#' specified burnin periods and/or minimisation probabilities. Its purpose is to
#' guide decisions when designing a randomisation strategy for a randomised
#' controlled trial.
#'
#' @param data a `data.frame` object with one line per participant and columns
#'             for the minimisation factors.
#' @param Nsims an integer, the number of simulations to run per scenario,
#'              default is 100.
#' @param groups an integer, the number of groups to randomise, default is 3.
#' @param factors a character vector with the names of the factors for
#'                minimisation
#' @param burnin a vector of integers of possible burnin lengths before
#'               minimisation kicks in. Individual values must be > 0 and
#'               < total sample size. When using stratification burnin must be
#'               smaller than the smallest strata size.
#' @param minprob a list of vectors (each of the same length as `groups`) with
#'                the possible minimisation probabilities.
#' @param stratify if stratification is to be used then a character string
#'                 specifying the name of the stratification variable (e.g.
#'                 "site"). Default is `NULL` for no stratification.
#' @param ratio a numeric vector of randomisation ratios (must be of length
#'              equal to the number of groups).
#'
#' @export
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

#' @export
print.mini.sim <- function(x, ...) {

  temp <- x$inputs
  temp$imbalance <- sims$imbalance
  temp$groups <- sims$group.sizes

  tab_imb <- with(temp, tapply(imbalance, list(burnin, minprob), FUN=mean))
  tab_imb <- round(tab_imb, 2)

  tab_grp <- aggregate(groups ~ burnin + minprob, data=temp,
                       FUN = function(x) round(mean(x), 1))

  cat("Simulation of Multi-arm Minimisation \n")
  cat(rep("-", 80), "\n", sep = "")
  cat("Number of simulations:", max(x$inputs$sim.no), "\n")
  cat("Burnin options:", paste(unique(x$inputs$burnin), collapse = ", "), "\n")
  cat("Minimisation probability options:",
      paste(unique(x$inputs$minprob), ncollapse = "; "), "\n")
  cat("Average group sizes:\n")
  cat(knitr::kable(tab_grp, format = "markdown"), sep = "\n")
  cat("Average imbalance:\n")
  cat(knitr::kable(tab_imb, format = "markdown"), sep = "\n")
}
