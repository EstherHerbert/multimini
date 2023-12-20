#' Minimisation for a dataset
#'
#' `minimise` randomises patients in a data frame using minimisation
#'
#' @param data a `data.frame` object with one line per participant and columns
#'             for the minimisation factors.
#' @param groups an integer, the number of groups to randomise to, default is 3
#' @param factors a character vector with the factors for minimisation
#' @param burnin an integer, the burnin length before minimisation kicks in,
#'               default is 10. Must be >0 and < total sample size. When using
#'               stratification burnin must be smaller than the smallest strata
#'               size.
#' @param minprob a vector of the same length as `groups` with the minimisation
#'                probabilities. The default is to give 0.8 probability to the
#'                group which would lead to the least imbalance and to allocate
#'                the remaining probability equally to the other groups.
#' @param stratify if stratification is to be used then a character string
#'                 specifying the name of the stratification variable (e.g.,
#'                 "site"). Default is `NULL` for no stratification.
#' @param ratio a numeric vector of randomisation ratios (must be of length
#'              equal to the number of groups)
#' @param group.names optional, a character vector with the group names, must be
#'                    the same length as `groups`.
#' @param seed optional, an integer that is used with `set.seed()` for
#'             offsetting the random number generator.
#'
#' @returns (Invisibly) the data.frame with an additional column `Group` indicating
#'   numerically which group has been allocated.
#'
#' @examples
#' # Minimisation to 3 groups, with two factors and a burnin of 15, using the
#' # patients data from the package
#' (mini <- minimise(patients, groups = 3, factors = c("sex", "stage"),
#'                   burnin = 15))
#'
#' # View data with group info
#' as.data.frame(mini)
#'
#' # Stratify minimisation by site
#' minimise(patients, groups = 3, factors = c("sex", "stage"), burnin = 5,
#'          stratify = "site")
#'
#' # Use 2:1:1 ratio
#' minimise(patients, groups = 3, factors = c("sex", "stage"), burnin = 5,
#'          stratify = "site", ratio = c(2,1,1))
#'
#' @export
minimise <- function(data, groups = 3, factors, burnin = 10,
                     minprob = c(0.8, rep(0.2/(groups - 1), groups - 1)),
                     stratify = NULL, ratio = rep(1, groups),
                     group.names = NULL, seed = NULL){

  # Check inputs
  if(groups < 2) {
    stop("Must be randomising to two or more groups.")
  }

  if(!all(minprob <= 1  & minprob >= 0) | length(minprob) != groups)
    stop(paste("minprob should be a vector of values between 0-1 (0 <= x <= 1)",
               "and should be of length equal to the number of groups."))

  if(sum(minprob) != 1)
    stop("minprob must sum to 1")

  if(!all(factors %in% names(data)))
    stop("The factors must be variables in the provided data")

  if(!is.null(stratify)) {
    if(!stratify %in% names(data)) {
      stop(paste("stratify must either be NULL (for no stratification) or a",
                 "variable provided in the data"))
    }
  }

  if(length(ratio) != groups) {
    stop("ratio should have length equal to the number of groups.")
  }

  if(burnin == 0) {
    warning("Burnin must be greater than 0, it has been updated to 1.")
    burnin <- 1
  }

  if(!is.null(group.names)) {
    if(length(group.names) != groups) {
      stop("group.names should have length equal to the number of groups.")
    }
  }

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

    if(!is.null(seed)) {
      set.seed(seed)
    }

    sampsize <- nrow(out[[s]])
    # Burn-in phase
    if(burnin >= sampsize) {
      stop(paste0("Specified burnin must be less than the smallest strata",
                  "size (", min(sapply(out, nrow)), ")."))
    }

    out[[s]]$Group[1:burnin] <- sample(1:groups, burnin, replace = T,
                                       prob = ratio/sum(ratio))

    # Minimisation
    for(i in (burnin + 1):sampsize){

      c_factors <- out[[s]][i,] # new participant
      p_factors <- utils::head(out[[s]], i-1) # previous participants

      counts <- matrix(NA, n.factors, groups)
      for (j in 1:n.factors) {
        for (k in 1:groups) {
          factor <- factors[j]
          counts[j,k] <- sum(p_factors[,factor] == c_factors[,factor] &
                               p_factors$Group == k)
        }
      }; rm(j, k)

      scores <- rep(NA, groups)
      for (j in 1:groups) {
        temp <- counts
        temp[, j] <- temp[, j] + 1
        num_level <- temp %*% diag(1/ratio)
        sd_level <- apply(num_level, 1, stats::sd)
        scores[j] <- sum(sd_level)
      }


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

  if(!is.null(group.names)) {
    out$Group <- factor(out$Group, levels = 1:groups, labels = group.names)
  }

  class(out) <- c("mini", "data.frame")
  groups(out) <- groups
  factors(out) <- factors
  burnin(out) <- burnin
  minprob(out) <- minprob
  strata(out) <- stratify
  ratio(out) <- ratio
  seed(out) <- seed

  return(out)

}

#' @export
print.mini <- function(x, ...){

  cat("Multi-arm Minimisation \n")
  cat(rep("-", 80), "\n", sep = "")
  if(!is.null(seed(x))) {
    cat("Seed:", seed(x), "\n")
  }
  cat("Number of groups:", groups(x), "\n")
  cat("Randomisation ratio:", paste(ratio(x), collapse = ":"), "\n")
  cat("Factors:", paste(factors(x), collapse = ", "), "\n")
  if (!is.null(strata(x))) {
    cat("Stratified by:", strata(x), "\n")
  }
  cat("Burnin:", burnin(x), "\n")
  cat("Minimisation probabilities:",
      paste(round(minprob(x), 2), collapse = ", "), "\n")
  cat("Group sizes:", paste(table(x$Group), collapse = ", "), "\n")

  return(invisible(x))

}

#' Plots to explore minimisation results
#'
#' @param x an object of class `mini` as a result of `minimises`
#' @param show.plots logical; if `FALSE` plots won't be displayed, useful when
#'                   assigning the plots for future use.
#' @param ... other parameters to be passed through to plotting functions.
#'
#' @export
plot.mini <- function(x, show.plots = TRUE, ...) {

  plots <- list()

  out <- data.frame(x)

  out$factors <- do.call(paste, c(out[factors(x)], sep=":"))
  out$Group <- factor(out$Group)

  ratio <- paste("Allocation ratio:", paste(ratio(x), collapse = ":"))

  plots$factors <-
    ggplot2::ggplot(out, ggplot2::aes(factors, fill = Group, group = Group)) +
    ggplot2::geom_bar(position = "dodge") +
    ggplot2::annotate("label", x = Inf, y = Inf, label = ratio, vjust = 1.5,
                      hjust = 1.5) +
    ggplot2::labs(x = paste("Combinations of factors:",
                            paste(factors(x),collapse = ", ")),
                  y = "Count", fill = "Group") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  if(!is.null(strata(x))) {
    out$strata <- out[,strata(x)]
    plots$strata <-
      ggplot2::ggplot(out, ggplot2::aes(factor(strata), fill = Group,
                                        group = Group)) +
      ggplot2::geom_bar(position = "dodge") +
      ggplot2::annotate("label", x = Inf, y = Inf, label = ratio, vjust = 1.5,
                        hjust = 1.5) +
      ggplot2::labs(x = paste("Strata:", strata(x)), y = "Count", fill = "Group") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")
  }

  if(show.plots) {
    print(plots[[1]])
    if(length(plots) > 1) {
      for(i in 2:length(plots)) {
        invisible(readline("Press [enter] to see next plot:"))
        print(plots[[i]])
      }
    }
  }

  return(invisible(plots))

}
