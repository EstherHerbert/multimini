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
#' @param stratify if stratification is to be used then a character string
#'                 specifying the name of the stratification variable (e.g.,
#'                 "site"). Default is `NULL` for no stratification.
#'
#' @returns (Invisibly) the data.frame with an additional column `Group` indicating
#' numerically which group has been allocated.
#'
#' @export
minimise <- function(data, groups = 3, factors, burnin = 10,
                     minprob = c(0.8, rep(0.2/(groups - 1), groups - 1)),
                     stratify = NULL){

  sampsize <- nrow(data)
  n.factors <- length(factors)

  out <- data

  out$Group <- rep(NA, sampsize)

  if (!is.null(stratify)) {
    out <- split(out, out[, stratify])
  } else {
    out <- list(out)
  }

  for(s in 1:length(out)) {
    sampsize <- nrow(out[[s]])
    # Burn-in phase
    out[[s]]$Group[1:burnin] <- sample(1:groups, burnin, replace = T)

    # Minimisation
    for(i in (burnin + 1):sampsize){

      c_factors <- out[[s]][i,] # new participant
      p_factors <- utils::head(out[[s]], i-1) # previous participants

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
        SD[j,] <- apply(scenarios[[j]], 2, stats::sd)
      }; rm(j)

      scores <- apply(SD, 1, sum) # sum across SDs for each scenario


      if (stats::var(scores) == 0) { # i.e., if they're all equal
        probs <- rep(1/groups, groups)
      } else {
        probs <- minprob[rank(scores)]
      }

      out[[s]]$Group[i] <- sample(1:groups, 1, replace = T, prob = probs)

    }
  }

  out <- do.call(rbind, out)
  row.names(out) <- NULL

  class(out) <- c("mini", "data.frame")
  groups(out) <- groups
  factors(out) <- factors
  burnin(out) <- burnin
  minprob(out) <- minprob
  strata(out) <- stratify

  return(out)

}

#' @export
print.mini <- function(x, ...){

  cat("Multi-arm Minimisation \n")
  cat(rep("-", 80), "\n", sep = "")
  cat("Number of groups:", groups(x), "\n")
  cat("Factors:", paste(factors(x), collapse = ", "), "\n")
  if (!is.null(strata(x))) {
    cat("Stratified by:", strata(x), "\n")
  }
  cat("Burnin:", burnin(x), "\n")
  cat("Minimisation probabilities:",
      paste(round(minprob(x), 2), collapse = ", "), "\n")
  cat("Group sizes:", paste(table(x$Group), collapse = ", "))

  return(invisible(x))

}
