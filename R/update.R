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



  # TODO add check for stratification - with warning that it will be ignored

  out <- dplyr::bind_rows(as.data.frame(object), new.data)

  n.factors <- length(factors(object))

  if(!is.null(seed(object))) {
    set.seed(seed(object))
  }

  burnin.remaining <- min(burnin(object) - nrow(object), nrow(new.data))

  if(burnin.remaining > 0) {
    out$Group[(nrow(object) + 1):(nrow(object) + burnin.remaining)] <-
      sample(1:groups(object), burnin.remaining, replace = T,
             prob = ratio(object)/sum(ratio(object)))
  } else {
    burnin.remaining <- 0
  }

  if(any(is.na(out$Group))) {
    for(i in (nrow(object) + burnin.remaining + 1):nrow(out)){

      c_factors <- out[i,] # new participant
      p_factors <- utils::head(out, i-1) # previous participants

      counts <- matrix(NA, n.factors, groups(object))
      for (j in 1:n.factors) {
        for (k in 1:groups(object)) {
          factor <- factors(object)[j]
          counts[j,k] <- sum(p_factors[,factor] == c_factors[,factor] &
                               p_factors$Group == k)
        }
      }; rm(j, k)

      scores <- rep(NA, groups(object))
      for (j in 1:groups(object)) {
        temp <- counts
        temp[, j] <- temp[, j] + 1
        num_level <- temp %*% diag(1/ratio(object))
        sd_level <- apply(num_level, 1, stats::sd)
        scores[j] <- sum(sd_level)
      }


      if (stats::var(scores) == 0) { # i.e., if they're all equal
        probs <- rep(1/groups(object), groups(object))
      } else {
        probs <- minprob(object)[rank(scores)]
      }

      out$Group[i] <- sample(1:groups(object), 1, replace = T, prob = probs)

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
