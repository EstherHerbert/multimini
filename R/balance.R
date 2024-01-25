#' Get balance of minimisation
#'
#' Get balance minimisation randomisation
#'
#' @param object An object of class `mini`
#'
#' @return An object of class `balance.mini`
#'
#' @examples
#' # Minimisation to 3 groups, with two factors and a burnin of 15
#' mini <- minimise(patients, groups = 3, factors = c("sex", "stage"),
#'                  burnin = 15)
#'
#' balance(mini)
#'
#' @export
balance <- function(object) {

  # Balance across factors
  factors <- factors(object)
  f_tab <- list()
  for(i in 1:length(factors)) {
    f_tab[[i]] <- table(object[,c("Group", factors[i])])
    names(f_tab)[i] <- factors[i]
  }

  imbalance <- rep(NA, length(factors))
  for(i in 1:length(factors)) {
    temp <- t(f_tab[[i]]) %*% diag(1/ratio(object))
    imbalance[i] <- sum(apply(temp, 1, stats::sd))
  }

  # this currently assumes equal weight for each factor
  imbalance <- sum(imbalance)

  out <- list(factor_balance = f_tab, imbalance = imbalance)

  class(out) <- "balance.mini"
  groups(out) <- groups(object)
  factors(out) <- factors
  ratio(out) <- ratio(object)

  return(out)

}

#' @export
print.balance.mini <- function(x, ...) {

  f_temp <- list()
  for(i in 1:length(x$factor_balance)) {
    f_temp[[i]] <- knitr::kable(x$factor_balance[[i]], format = "markdown")
  }

  cat("Balance of factors (", paste(factors(x), collapse = ", "), ") over ",
      groups(x), " groups (", paste(ratio(x), collapse = ":"), ")\n", sep = "")
  cat(rep("-", 80), "\n", sep = "")
  cat(do.call("paste", c(f_temp, sep = "    ")), sep = "\n")
  cat("\nTotal imbalance:", round(x$imbalance, 3))
}
