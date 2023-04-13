groups <- function(x) {
  return(attr(x, "groups"))
}

`groups<-` <- function(x, value) {
  attr(x, "groups") <- value
  return(x)
}

factors <- function(x) {
  return(attr(x, "factors"))
}

`factors<-` <- function(x, value) {
  attr(x, "factors") <- value
  return(x)
}

burnin <- function(x) {
  return(attr(x, "burnin"))
}

`burnin<-` <- function(x, value) {
  attr(x, "burnin") <- value
  return(x)
}

minprob <- function(x) {
  return(attr(x, "minprob"))
}

`minprob<-` <- function(x, value) {
  attr(x, "minprob") <- value
  return(x)
}

strata <- function(x) {
  return(attr(x, "strata"))
}

`strata<-` <- function(x, value) {
  attr(x, "strata") <- value
  return(x)
}

ratio <- function(x) {
  return(attr(x, "ratio"))
}

`ratio<-` <- function(x, value) {
  attr(x, "ratio") <- value
  return(x)
}
