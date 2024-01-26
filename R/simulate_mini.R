#' Simulation of minimisation
#'
#' `simulate_mini` repeatedly generates random data and then minimises with
#'   different specified burnin periods and/or minimisation probabilities. Its
#'   purpose is to guide decisions when designing a randomisation strategy for a
#'   randomised controlled trial.
#'
#' @param sampsize the desired sample size of the data, i.e., the sample size
#'   of your prospective trial.
#' @param factors a list of factors, each a list containing two items. The first
#'   is `levels` which is either a vector of the level names OR the number of
#'   levels. The second item is either `props`, a vector of proportions equal to
#'   the number of levels in the factor; OR prop.dist which is a named vector
#'   containing the mean and sd of the proportions.
#' @param Nsims an integer, the number of simulations to run per scenario,
#'              default is 100.
#' @param groups an integer, the number of groups to randomise, default is 3.
#' @param burnin a vector of integers of possible burnin lengths before
#'               minimisation kicks in. Individual values must be > 0 and
#'               < total sample size.
#' @param minprob a list of vectors (each of the same length as `groups`) with
#'                the possible minimisation probabilities.
#' @param ratio a numeric vector of randomisation ratios (must be of length
#'              equal to the number of groups).
#'
#' @examples
#' simulate_mini(sampsize = 150,
#'               factors = list(sex = list(levels = c("M", "F"),
#'                                         props = c(0.5, 0.5)),
#'                              site = list(levels = 12,
#'                                          prop.dist = c(mean = 0.1, sd = 0.05))),
#'               Nsims = 10, groups = 3, burnin = c(10, 15),
#'               minprob = list(c(0.8, 0.1, 0.1), c(0.8, 0.15, 0.05)),
#'               ratio = c(1,1,1))
#'
#' @export
simulate_mini <- function(sampsize, factors, Nsims = 100, groups = 3, burnin,
                          minprob, ratio = rep(1, groups)) {

  inputs <- expand.grid(sim.no = 1:Nsims,
                        burnin = burnin,
                        minprob = minprob)

  sims <- mapply(
    function(x, y) {
      data <- simulate_data(sampsize, factors)
      minimise(data = data, groups = groups, factors = names(factors),
               burnin = x, minprob = y, ratio = ratio)
    },
    inputs$burnin, inputs$minprob,
    SIMPLIFY = FALSE
  )

  imbalance <- mapply(function(x) balance(x)$imbalance, sims)
  group.sizes <- t(sapply(sims, function(x) with(x, table(Group))))

  inputs$minprob <- sapply(inputs$minprob, paste, collapse = ", ")

  out <- list(inputs = inputs, simulations = sims, group.sizes = group.sizes,
              imbalance = imbalance)

  class(out) <- "mini.sim"
  factors(out) <- names(factors)

  return(out)

}

#' @export
print.mini.sim <- function(x, ...) {

  temp <- x$inputs
  temp$imbalance <- x$imbalance
  temp$groups <- x$group.sizes

  tab_imb <- stats::aggregate(imbalance ~ burnin + minprob ,
                              data=temp, FUN = function(x) round(mean(x), 1))

  tab_grp <- stats::aggregate(groups ~ burnin + minprob,
                              data=temp, FUN = function(x) round(mean(x), 1))

  cat("Simulation of Multi-arm Minimisation \n")
  cat(rep("-", 80), "\n", sep = "")
  cat("Number of simulations per scenario:", max(x$inputs$sim.no), "\n")
  cat("Factors:", paste(factors(x), collapse = ", "), "\n")
  cat("Burnin options:", paste(unique(x$inputs$burnin), collapse = ", "), "\n")
  cat("Minimisation probability options:",
      paste(unique(x$inputs$minprob), ncollapse = "; "), "\n")
  cat("Average group sizes:\n")
  cat(knitr::kable(tab_grp, format = "markdown"), sep = "\n")
  cat("Average imbalance:\n")
  cat(knitr::kable(tab_imb, format = "markdown"), sep = "\n")
}
