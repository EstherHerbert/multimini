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
#' @param factors a list of character vectors with the names of the factors for
#'                minimisation
#' @param burnin a vector of integers of possible burnin lengths before
#'               minimisation kicks in. Individual values must be > 0 and
#'               < total sample size. When using stratification burnin must be
#'               smaller than the smallest strata size.
#' @param minprob a list of vectors (each of the same length as `groups`) with
#'                the possible minimisation probabilities.
#' @param stratify a list of stratification options, see [minimise()] for more
#'                 information.
#' @param ratio a numeric vector of randomisation ratios (must be of length
#'              equal to the number of groups).
#'
#' @export
simulate_mini <- function(data, Nsims = 100, groups = 3, factors, burnin, minprob,
                          stratify = NULL, ratio = rep(1, groups)) {

  if(any(!unlist(factors) %in% names(data))) {
    stop("Given factors must be variables in the data.")
  }

  if(any(!is.null(stratify))) {
    if(any(!unlist(stratify) %in% names(data))) {
      stop("`stratify` must be either NULL or a variable in the data.")
    }
  }

  if(is.null(stratify)) {
    stratify <- list(NULL)
  }

  inputs <- expand.grid(sim.no = 1:Nsims,
                        factors = factors,
                        burnin = burnin,
                        minprob = minprob,
                        stratify = stratify)

  minimise_s <- purrr::possibly(minimise, otherwise = NA)

  sims <- mapply(
    function(x, y, z, w) minimise_s(data, factors = z, burnin = x,
                                    minprob = y, stratify = w, ratio = ratio),
    inputs$burnin, inputs$minprob, inputs$factors, inputs$stratify,
    SIMPLIFY = FALSE
  )

  if(any(is.na(sims))) {
    warning(paste("Some combinations of inputs could not be used for",
                  "minimisation, they have been removed from the list of",
                  "inputs."), call. = FALSE)
  }

  inputs$minprob <- sapply(inputs$minprob, paste, collapse = ", ")
  inputs$factors <- sapply(inputs$factors, paste, collapse = ", ")
  inputs$stratify[sapply(inputs$stratify, is.null)] <- "NULL"
  inputs$stratify <- unlist(inputs$stratify)

  inputs <- inputs[!is.na(sims),]
  sims <- sims[!is.na(sims)]

  imbalance <- mapply(function(x) balance(x)$imbalance, sims)
  group.sizes <- t(sapply(sims, function(x) with(x, table(Group))))

  out <- list(inputs = inputs, simulations = sims, group.sizes = group.sizes,
              imbalance = imbalance)

  class(out) <- "mini.sim"

  return(out)

}

#' @export
print.mini.sim <- function(x, ...) {

  temp <- x$inputs
  temp$imbalance <- x$imbalance
  temp$groups <- x$group.sizes

  tab_imb <- stats::aggregate(imbalance ~ factors + burnin + minprob + stratify,
                              data=temp, FUN = function(x) round(mean(x), 1))

  tab_grp <- stats::aggregate(groups ~ factors + burnin + minprob + stratify,
                              data=temp, FUN = function(x) round(mean(x), 1))

  cat("Simulation of Multi-arm Minimisation \n")
  cat(rep("-", 80), "\n", sep = "")
  cat("Number of simulations:", max(x$inputs$sim.no), "\n")
  cat("Factor options:", paste(unique(x$inputs$factor), collapse = "; "), "\n")
  cat("Burnin options:", paste(unique(x$inputs$burnin), collapse = ", "), "\n")
  cat("Minimisation probability options:",
      paste(unique(x$inputs$minprob), ncollapse = "; "), "\n")
  cat("Stratification options:",
      paste(unique(x$inputs$stratify), collapse = ", "), "\n")
  cat("Average group sizes:\n")
  cat(knitr::kable(tab_grp, format = "markdown"), sep = "\n")
  cat("Average imbalance:\n")
  cat(knitr::kable(tab_imb, format = "markdown"), sep = "\n")
}
