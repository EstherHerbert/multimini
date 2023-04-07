#' Get balance of minimisation
#'
#' Get balance minimisation randomisation
#'
#' @param object An object of class `mini`
#'
#' @return An object of class `balance.mini`
#'
#' @export
balance <- function(object) {

  factors <- factors(object)
  tab <- list()
  for(i in 1:length(factors)) {
    tab[[i]] <- table(object[,c("Group", factors[i])])
    names(tab)[i] <- factors[i]
  }

  imbalance <- rep(NA, length(factors))
  for(i in 1:length(factors)) {
    imbalance[i] <- sum(apply(tab[[i]], 2, stats::sd))
  }
  imbalance <- sum(imbalance)

  out <- list(balance = tab, imbalance = imbalance, groups = groups(object),
              factors = factors)
  class(out) <- "balance.mini"

  return(out)

}

#' @export
print.balance.mini <- function(x, ...) {
  temp <- list()

  for(i in 1:length(x$balance)) {
    temp[[i]] <- knitr::kable(x$balance[[i]],
                              format = "markdown")
  }
  cat("Balance of factors (", paste(x$factors, collapse = ", "), ") over",
      x$groups, "groups\n")
  cat(rep("-", 80), "\n", sep = "")
  cat(do.call("paste", c(temp, sep = "    ")), sep = "\n")
  cat("\nTotal imbalance:", round(x$imbalance, 3))
}
