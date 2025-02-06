#' Update a minimisation with new data
#'
#' `update` updates the minimisation with new participant data, using the
#' existing group allocations and factor information to allocate the new
#' participants using minimisation.
#'
#' @param object an object of class mini, i.e., resulting from [minimise()]
#' @param new.data a `data.frame` object with one line per new participant and
#'                 columns for the minimisation factors. Must be of the same
#'                 format as the `data.frame` in `object` (minus `Group`)
#' @param ... unused arguments passed to `update.default()`
#'
#' @returns (Invisibly) the data.frame of all participants with group existing
#'   and new group allocations
#'
#' @seealso [update()] for the generic function
#'
#' @examples
#' mini <- minimise(patients, groups = 3, factors = c("sex", "stage"),
#'                  burnin = 10)
#'
#' new.patients <- data.frame(
#'   sex = c("F", "F", "M"),
#'   stage = c("II","I", "I"),
#'   site = c(9, 9, 1)
#' )
#'
#' update(mini, new.patients)
#'
#' @export
update.mini <- function(object, new.data, ...) {

  if(!all(factors(object) %in% names(new.data))) {
    stop("All minimisation factors must be included as columns in `new.data`",
         call. = F)
  } else if ("Group" %in% names(new.data)) {
    warning("`new.data` already contains a Group variable, this will be ",
            "overwritten", call. = F)
  }

  new.data$Group <- NA

  out <- dplyr::bind_rows(as.data.frame(object), new.data)

  if(!is.null(seed(object))) {
    set.seed(seed(object))
  }

  minprob <- minprob(object)
  groups <- groups(object)
  factors <- factors(object)
  ratio <- ratio(object)
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

  burnin.remaining <- min(burnin(object) - nrow(object), nrow(new.data))

  if(burnin.remaining > 0) {
    out$Group[(nrow(object) + 1):(nrow(object) + burnin.remaining)] <-
      sample(groups, burnin.remaining, replace = T, prob = ratio/Rsum)
  } else {
    burnin.remaining <- 0
  }

  if(any(is.na(out$Group))) {
    for(i in (nrow(object) + burnin.remaining + 1):nrow(out)){

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

      m <- array(dim = c(2,3,3), dimnames = list(factors, groups, groups))

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
        P <- unname(ratio/sum(ratio))
      }

      rn <- runif(1)
      P <- cumsum(P)

      out$Group[i] <- groups[findInterval(rn, P, rightmost.closed = T) + 1]

    }
  }

  class(out) <- class(object)
  groups(out) <- groups(object)
  factors(out) <- factors(object)
  burnin(out) <- burnin(object)
  minprob(out) <- minprob(object)
  ratio(out) <- ratio(object)
  seed(out) <- seed(object)

  return(out)

}
