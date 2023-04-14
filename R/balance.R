#' Get balance of minimisation
#'
#' Get balance minimisation randomisation
#'
#' @param object An object of class `mini`
#'
#' @return An object of class `balance.mini`
#'
#' @examples
#'
#' patients <- data.frame(sex = sample(c("M", "F"), 150, replace = TRUE),
#'                        stage = sample(c("I", "II", "III"), 150,
#'                                       replace = TRUE,
#'                                       prob = c(0.5, 0.3, 0.2)),
#'                        site = sample(1:10, 150, replace = TRUE))
#'
#' # Minimisation to 3 groups, with two factors and a burnin of 15
#' mini <- minimise(patients, groups = 3, factors = c("sex", "stage"),
#'                  burnin = 15)
#'
#' balance(mini)
#' # When stratification is used `balance` gives the balance across strata too
#' mini <- minimise(patients, groups = 3, factors = c("sex", "stage"),
#'                  burnin = 5, stratify = "site")
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

  imbalance <- sum(imbalance)

  out <- list(factor_balance = f_tab, imbalance = imbalance)

  # Balance across strata
  if(!is.null(strata(object))) {
    s_tab <- table(object[,c("Group", strata(object))])
    out <- c(out, strata_balance = list(s_tab))
    strata(out) <- strata(object)
  }

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

  if(!is.null(strata(x))) {
    s_temp <- knitr::kable(x$strata_balance, format = "markdown")
  }

  cat("Balance of factors (", paste(factors(x), collapse = ", "), ") over ",
      groups(x), " groups (", paste(ratio(x), collapse = ":"), ")\n", sep = "")
  cat(rep("-", 80), "\n", sep = "")
  cat(do.call("paste", c(f_temp, sep = "    ")), sep = "\n")
  if(!is.null(strata(x))) {
    cat("\nBalance of strata (", strata(x), ")\n", sep = "")
    cat(rep("-", 80), "\n", sep = "")
    cat(s_temp, sep = "\n")
  }
  cat("\nTotal imbalance:", round(x$imbalance, 3))
}
