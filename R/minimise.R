#' Minimisation for a dataset
#'
#' `minimise` randomises patients in a data frame using minimisation
#'
#' @param data a `data.frame` object with one line per participant and columns
#'             for the minimisation factors.
#' @param groups an integer, the number of groups to randomise to, default is 3
#' @param factors a character vector with the factors for minimisation
#' @param burnin an integer, the burnin length before minimisation kicks in,
#'               default is 10
#' @param minprob a vector of the same length as `groups` with the minimisation
#'                probabilities. The default is to give 0.8 probability to the
#'                group which would lead to the least imbalance and to allocate
#'                the remaining probability equally to the other groups.
#'
#' @returns (Invisibly) the data.frame with an additional column `Group` indicating
#' numerically which group has been allocated.
#'
#' @export
minimise <- function(data, groups = 3, factors, burnin = 10,
                     minprob = c(0.8, rep(0.2/(groups - 1), groups - 1))){

  sampsize <- nrow(data)
  n.factors <- length(factors)

  out <- data

  out$Group <- rep(NA, sampsize)

  # Burn-in phase
  out$Group[1:burnin] <- sample(1:groups, burnin, replace = T)

  # Minimisation
  for(i in (burnin + 1):sampsize){

    c_factors <- out[i,] # new participant
    p_factors <- head(out, i-1) # previous participants

    counts <- matrix(NA, groups, n.factors)
    for (j in 1:groups) {
      for (k in 1:n.factors) {
        factor <- factors[k]
        counts[j,k] <- sum(p_factors[,factor] == c_factors[,factor] &
                             p_factors$Group == j)
      }
    }; rm(j, k)

    scenarios <- list()
    for(j in 1:groups){
      scenarios[[j]] <- counts
      scenarios[[j]][j,] <- counts[j,] + rep(1, n.factors)
    }; rm(j)

    SD <- matrix(NA, groups, n.factors)
    for(j in 1:groups){
      SD[j,] <- apply(scenarios[[j]], 2, sd)
    }; rm(j)

    scores <- apply(SD, 1, sum) # sum across SDs for each scenario


    if (var(scores) == 0) { # i.e., if they're all equal
      probs <- rep(1/groups, groups)
    } else {
      probs <- minprob[rank(scores)]
    }

    out$Group[i] <- sample(1:groups, 1, replace = T, prob = probs)

  }

  mini <- list(data = out, groups = groups, factors = factors, burnin = burnin,
               minprob = minprob)

  class(mini) <- "mini"

  return(mini)

}

#' @export
print.mini <- function(x, ...){

  cat("Multi-arm Minimisation \n")
  cat(rep("-", 80), "\n", sep = "")
  cat("Number of groups:", x$groups, "\n")
  cat("Factors:", paste(x$factors, collapse = ", "), "\n")
  cat("Burnin:", x$burnin, "\n")
  cat("Minimisation probabilities:",
      paste(round(x$minprob, 2), collapse = ", "), "\n")
  cat("Group sizes:", paste(table(x$data$Group), collapse = ", "))

  return(invisible(x))

}
