#' Simulate data for minimisation simulations
#'
#' Given a list of factors with specified levels and proportions, and a sample
#'   size, this function simulates data for use in [simulate_mini()].
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
#' @param groups either a character vector of group names or an integer
#'   specifying the number of groups.
#'
#' @examples
#' simulate_data(factors = list(sex = list(levels = c("M", "F"),
#'                                         props = c(0.5, 0.5)),
#'                              site = list(levels = 12,
#'                                          prop.dist = c(mean = 0.1, sd = 0.05))),
#'               sampsize = 150)
#'
#' simulate_data(factors = list(sex = list(levels = c("M", "F"),
#'                                         props = c(0.5, 0.5)),
#'                              site = list(levels = 12,
#'                                          prop.dist = c(mean = 0.1, sd = 0.05))),
#'               sampsize = 100, eligibility = TRUE, groups = 4)
#'
#' @export
simulate_data <- function(sampsize, factors, eligibility = FALSE, groups = NULL) {

  if(eligibility & is.null(groups)) {
    stop("Must supply `groups` if `eligibility = TRUE`")
  }

   data <- lapply(factors, gen_factor, sampsize = sampsize)

  data <- data.frame(ID = 1:sampsize, data)

  if (eligibility) {
    if (length(groups) == 1 & rlang::is_integerish(groups)) {
      groups <- LETTERS[1:groups]
    }
    temp <- list()
    for(i in 2:length(groups)) {
      temp <- c(temp, utils::combn(groups, i, simplify = F))
    }
    groups <- sapply(temp, paste, collapse = "")
    data$eligible <- sample(groups, sampsize, replace = T)
  }

    return(data)

}

estBetaParams <- function(mean, sd) {
  var <- sd^2
  alpha <- ((1 - mean) / var - 1 / mean) * mean ^ 2
  beta <- alpha * (1 / mean - 1)
  return(list(shape1 = alpha, shape2 = beta))
}

gen_factor <- function(factor, sampsize) {

  if(length(factor$levels) == 1) {
    factor$levels <- 1:factor$levels
  }

  if("prop.dist" %in% names(factor)) {
    shapes <- rlang::inject(estBetaParams(!!!factor$prop.dist))
    factor$props <- rlang::inject(stats::rbeta(n = length(factor$levels), !!!shapes))
  }

  data <- sample(factor$levels, size = sampsize, replace = TRUE, prob = factor$props)

  return(data)

}
