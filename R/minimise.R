#' Minimisation for a dataset
#'
#' `minimise` randomises patients in a data frame using minimisation
#'
#' @param data a `data.frame` object with one line per participant and columns
#'             for the minimisation factors.
#' @param groups an integer, the number of groups to randomise to, default is 3
#' @param factors a character vector with the factors for minimisation
#' @param burnin an integer, the burnin length before minimisation kicks in,
#'               default is 10. Must be > 0 and < total sample size.
#' @param minprob the minimisation probability. The default is to give 0.8
#'                probability to the group which would lead to the least
#'                imbalance.
#' @param ratio a numeric vector of randomisation ratios in ascending order
#'              (must be of length equal to the number of groups)
#' @param group.names optional, a character vector with the group names, must be
#'                    the same length as `groups`.
#' @param seed optional, an integer that is used with `set.seed()` for
#'             offsetting the random number generator.
#'
#' @returns (Invisibly) the data.frame with an additional column `Group`
#'   indicating numerically which group has been allocated.
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
#' # Use 1:1:2 ratio
#' minimise(patients, groups = 3, factors = c("sex", "stage"), burnin = 5,
#'          ratio = c(1,1,2))
#'
#' @export
minimise <- function(data, groups = 3, factors, burnin = 10,
                     minprob = 0.80, ratio = rep(1, groups), group.names = NULL,
                     seed = NULL){

  # Check inputs
  if(groups < 2) {
    stop("Must be randomising to two or more groups.")
  }

  if(minprob > 1)
    stop("minprob must be less than or equalto 1")

  if(!all(factors %in% names(data)))
    stop("The factors must be variables in the provided data")

  if(length(ratio) != groups) {
    stop("ratio should have length equal to the number of groups.")
  }

  if(burnin == 0) {
    warning("Burnin must be greater than 0, it has been updated to 1.")
    burnin <- 1
  }

  if(burnin >= nrow(data)) {
    stop("Specified burnin must be less than the sample size")
  }

  if(!is.null(group.names)) {
    if(length(group.names) != groups) {
      stop("group.names should have length equal to the number of groups.")
    }
  }

  if(!is.null(group.names)) {
    groups <- group.names
  } else {
    groups <- 1:groups
  }

  names(ratio) <- groups
  Rsum <- sum(ratio)
  Rsum_1 <- Rsum - ratio

  Popt <- rep(minprob, length(groups))
  for (i in 2:length(groups)) {
    Popt[i] <- 1 - (1 - minprob) * Rsum_1[i] / Rsum_1[1]
  }; rm(i)

  Pnon <- matrix(nrow = length(groups), ncol = length(groups))
  for (i in 1:length(groups)) {
    for (j in 1:length(groups)) {
      Pnon[i,j] <- (1 - Popt[j]) * ratio[i]/Rsum_1[j]
    }
  }; rm(i,j)
  diag(Pnon) <- Popt

  if(!is.null(seed)) {
    set.seed(seed)
  }

  out <- data

  out$Group <- NA

  out$Group[1:burnin] <- sample(groups, burnin, replace = T, prob = ratio/Rsum)

  for (i in (burnin + 1):nrow(data)) {

    c_factors <- out[i,]
    p_factors <- out[1:(i-1),]

    counts <- matrix(NA, length(factors), length(groups),
                     dimnames = list(factors, groups))
    for (j in factors) {
      for (k in groups) {
        counts[j,k] <- sum(p_factors[,j] == c_factors[,j] &
                             p_factors$Group == k)
      }
    }; rm(j, k)

    m <- array(dim = c(length(factors),length(groups),length(groups)),
               dimnames = list(factors, groups, groups))

    for (f in factors) {
      for (t in groups) {
        for (s in groups) {
          if(t == s) {
            m[f,t,s] <- (counts[f,t] + 1) / ratio[t]
          } else {
            m[f,t,s] <- (counts[f,t]) / ratio[t]
          }
        }
      }
    }; rm(f,t,s)

    M <- matrix(nrow = length(factors), ncol = length(groups),
                dimnames = list(factors, groups))
    for (f in factors) {
      for (s in groups) {
        M[f,s] <- sum(m[f,,s])
      }
    }; rm(f,s)

    Mean <- M/length(groups)

    SD <- matrix(nrow = length(factors), ncol = length(groups),
                 dimnames = list(factors, groups))
    for (f in factors) {
      for (s in groups) {
        SD[f,s] <- sqrt((1/length(groups)) * sum((m[f,,s] - Mean[f,s])^2))
      }
    }

    SD <- apply(SD, 2, sum)
    J <- (SD == min(SD)) * 1

    if(sum(J) == 1) {
      P <- Pnon %*% J
    } else if (sum(J) < length(groups)){
      P <- (Pnon/sum(J)) %*% J
    } else {
      P <- ratio/Rsum
    }

    out$Group[i] <- sample(groups, 1, prob = P)

  }

  class(out) <- c("mini", "data.frame")
  groups(out) <- groups
  factors(out) <- factors
  burnin(out) <- burnin
  minprob(out) <- minprob
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
  cat("Groups:", groups(x), "\n")
  cat("Randomisation ratio:", paste(ratio(x), collapse = ":"), "\n")
  cat("Factors:", paste(factors(x), collapse = ", "), "\n")
  cat("Burnin:", burnin(x), "\n")
  cat("Minimisation probability:", minprob(x), "\n")
  cat("Group sizes:", paste(table(x$Group), collapse = ", "), "\n")

  return(invisible(x))

}

#' Plots to explore minimisation results
#'
#' @param x an object of class `mini` as a result of `minimises`
#' @param show.plot logical; if `FALSE` the plot won't be displayed, useful when
#'                   assigning the plot for future use.
#' @param ... other parameters to be passed through to plotting functions.
#'
#' @export
plot.mini <- function(x, show.plot = TRUE, ...) {

  plots <- list()

  out <- data.frame(x)

  out$factors <- do.call(paste, c(out[factors(x)], sep=":"))
  out$Group <- factor(out$Group)

  ratio <- paste("Allocation ratio:", paste(ratio(x), collapse = ":"))

  plot <- ggplot2::ggplot(out, ggplot2::aes(factors, fill = Group,
                                            group = Group)) +
    ggplot2::geom_bar(position = "dodge") +
    ggplot2::annotate("label", x = Inf, y = Inf, label = ratio, vjust = 1.5,
                      hjust = 1.5) +
    ggplot2::labs(x = paste("Combinations of factors:",
                            paste(factors(x),collapse = ", ")),
                  y = "Count", fill = "Group") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  if(show.plot) {
    print(plot)
  }

  return(invisible(plot))

}
