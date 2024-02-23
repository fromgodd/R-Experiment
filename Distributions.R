distfunc <- function(coefbase, size = NULL, prob = NULL, rate = NULL, type) {
  if (type == 'binomial') {
    return(dbinom(coefbase, size, prob))
  } else if (type == 'poisson') {
    return(dpois(coefbase, rate))
  } else if (type == 'uniform') {
    return(dunif(coefbase, min = size, max = prob))
  } else if (type == 'normal') {
    return(dnorm(coefbase, mean = size, sd = prob))
  } else {
    stop("Invalid distribution type")
  }
}

# Example usage:
# For binomial distribution
distfunc(5, size = 10, prob = 0.7, type = 'binomial')

# For poisson distribution
distfunc(5, rate = 2, type = 'poisson')

# For uniform distribution
distfunc(5, size = 1, prob = 10, type = 'uniform')

# For normal distribution
distfunc(5, size = 0, prob = 1, type = 'normal')
