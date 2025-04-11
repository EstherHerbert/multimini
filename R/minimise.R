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
#' @param check.eligibility logical indicating whether participants have varying
#'                          eligibility for different groups. If `TRUE` then the
#'                          column `eligible` should exist in the data.
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
#' # Account for different group eligibility
#' minimise(patients, groups = 3, factors = c("sex", "stage"), burnin = 5,
#'          ratio = c(1,1,2), check.eligibility = TRUE)
#' @export
minimise <- function(data, groups = 3, factors, burnin = 10, minprob = 0.80,
                     ratio = rep(1, groups), group.names = NULL, seed = NULL,
                     check.eligibility = FALSE) {

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

  if(is.unsorted(ratio)) {
    ask <- utils::askYesNo(
      msg = paste("ratio should be in ascending order, would you like to",
                  "automatically sort the vector?"),
      prompts = "Y/N/c"
    )
    if (!is.na(ask) && ask) {
      ratio <- sort(ratio)
    } else {
      stop("ratio should be given in ascending order", call. = F)
    }
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

  if (check.eligibility) {
    if(!"eligible" %in% names(data)) {
      stop(paste("The data must contain a variable called `eligible` to check",
                 "eligiblity"))
    }
  }

  if(!is.null(group.names)) {
    groups <- group.names
  } else {
    groups <- LETTERS[1:groups]
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

  if (check.eligibility) {
    out$eligible <- strsplit(out$eligible, "")
  } else {
    out$eligible <- list(groups)
  }

  out$Group <- NA

  for(i in 1:burnin) {
    I <- groups %in% out$eligible[[i]]
    out$Group[i] <- sample(groups[I], 1, prob = ratio[I]/sum(ratio[I]))
  }

  for (i in (burnin + 1):nrow(data)) {

    elig <- out$eligible[[i]]
    c_factors <- out[i,]
    p_factors <- out[1:(i-1),]
    p_factors$Group <- factor(p_factors$Group, levels = groups)
    p_factors <- p_factors %>%
      dplyr::filter(purrr::map_lgl(eligible, ~all(elig %in% .x)))

    I <- groups %in% elig

    if (sum(I) < length(groups)) {
      Ptot <- 1/(I %*% Pnon)
      colnames(Ptot) <- groups
      Ptot[,!I] <- 0

      P <- Pnon %*% diag(c(Ptot))
      P <- P[I,I]
    } else {
      P <- Pnon
    }

    counts <- matrix(NA, length(factors), length(groups[I]),
                     dimnames = list(factors, groups[I]))
    for (j in factors) {
      for (k in groups[I]) {
        counts[j,k] <- sum(p_factors[,j] == c_factors[,j] &
                             p_factors$Group == k)
      }
    }; rm(j, k)

    m <- array(dim = c(length(factors),length(groups[I]),length(groups[I])),
               dimnames = list(factors, groups[I], groups[I]))

    for (f in factors) {
      for (t in groups[I]) {
        for (s in groups[I]) {
          if(t == s) {
            m[f,t,s] <- (counts[f,t] + 1) / ratio[t]
          } else {
            m[f,t,s] <- (counts[f,t]) / ratio[t]
          }
        }
      }
    }; rm(f,t,s)

    M <- matrix(nrow = length(factors), ncol = length(groups[I]),
                dimnames = list(factors, groups[I]))
    for (f in factors) {
      for (s in groups[I]) {
        M[f,s] <- sum(m[f,,s])
      }
    }; rm(f,s)

    Mean <- M/length(groups[I])

    SD <- matrix(nrow = length(factors), ncol = length(groups[I]),
                 dimnames = list(factors, groups[I]))
    for (f in factors) {
      for (s in groups[I]) {
        SD[f,s] <- sqrt((1/length(groups[I])) * sum((m[f,,s] - Mean[f,s])^2))
      }
    }

    SD <- apply(SD, 2, sum)
    J <- (SD == min(SD)) * 1

    if(sum(J) == 1) {
      P <- P %*% J
    } else if (sum(J) < length(groups[I])){
      P <- (P/sum(J)) %*% J
    } else {
      P <- unname(ratio[I]/sum(ratio[I]))
    }

    rn <- stats::runif(1)
    P <- cumsum(P)

    out$Group[i] <- groups[I][findInterval(rn, P, rightmost.closed = T) + 1]

  }

  class(out) <- c("mini", "data.frame")
  groups(out) <- groups
  factors(out) <- factors
  burnin(out) <- burnin
  minprob(out) <- minprob
  ratio(out) <- ratio
  seed(out) <- seed
  eligibility(out) <- check.eligibility

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
