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
#' @param eligibility logical, should the data include group eligibility.
#'   If `TRUE` then must supply `groups` argument.
#' @param Nsims an integer, the number of simulations to run per scenario,
#'              default is 100.
#' @param groups an integer, the number of groups to randomise, default is 3.
#' @param burnin a vector of integers of possible burnin lengths before
#'               minimisation kicks in. Individual values must be > 0 and
#'               < total sample size.
#' @param minprob a vector of the possible minimisation probabilities.
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
#'               minprob = c(0.7, 0.8, 0.9), ratio = c(1,1,1))
#'
#' @export
simulate_mini <- function(sampsize, factors, eligibility = FALSE, Nsims = 100,
                          groups = 3, burnin, minprob, ratio = rep(1, groups)) {

  inputs <- expand.grid(sim.no = 1:Nsims,
                        burnin = burnin,
                        minprob = minprob)

  sims <- pbapply::pbmapply(
    function(x, y) {
      data <- simulate_data(sampsize, factors, eligibility, groups)
      minimise(data = data, groups = groups, factors = names(factors),
               burnin = x, minprob = y, ratio = ratio,
               check.eligibility = eligibility)
    },
    inputs$burnin, inputs$minprob,
    SIMPLIFY = FALSE
  )

  imbalance <- mapply(function(x) balance(x)$imbalance, sims)
  group.sizes <- t(sapply(sims, function(x) with(x, table(Group))))

  out <- list(inputs = inputs, simulations = sims, group.sizes = group.sizes,
              imbalance = imbalance)

  class(out) <- "mini.sim"
  factors(out) <- names(factors)
  ratio(out) <- ratio
  eligibility(out) <- eligibility

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
  cat("Allocation ratio:", paste(ratio(x), collapse = ", "), "\n")
  if(eligibility(x)) {
    cat("Eligibiliyt for arms was checked\n")
  }
  cat("Burnin options:", paste(unique(x$inputs$burnin), collapse = ", "), "\n")
  cat("Minimisation probability options:",
      paste(unique(x$inputs$minprob), collapse = ", "), "\n")
  cat("Average group sizes:\n")
  cat(knitr::kable(tab_grp, format = "markdown"), sep = "\n")
  cat("Average imbalance:\n")
  cat(knitr::kable(tab_imb, format = "markdown"), sep = "\n")
}

#' @export
plot.mini.sim <- function(x, ...) {

  cbind(x$inputs, x$group.sizes) %>%
    tidyr::pivot_longer(-c(sim.no, burnin, minprob), names_to = "group",
                        values_to = "size") %>%
    ggplot2::ggplot(ggplot2::aes(group, size)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_grid(rows = ggplot2::vars(burnin), cols = ggplot2::vars(minprob)) +
    ggplot2::labs(x = "Group", y = "Group Size") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = NA))

}
