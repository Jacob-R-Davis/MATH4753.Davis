#' My binomial simulation
#'
#' @param iter Number of trials to run
#' @param n Size of sample
#' @param p Probability of success (1)
#'
#' @return A table and barplot with the simulated frequency distribution
#' @export
#'
#' @examples
#' mybin(iter=10000, n=20, p=0.5)
#'
mybin = function(iter=100, n=10, p=0.5){ # iter = iterations, n = sample size

  # Make a matrix to hold the samples
  sample.matrix=matrix(NA, nr=n, nc=iter, byrow=TRUE) # Start out filled with "NA"

  # Make a vector to hold the number of successes in each trial
  success=c()

  for(i in 1:iter){

    # Fill each column with a new sample
    sample.matrix[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))

    # Calculate a statistic from the sample (in this case it is the sum)
    success[i]=sum(sample.matrix[,i])
  }

  # Make a table of successes
  success.table=table(factor(success,levels=0:n))

  # Make a barplot of the proportions
  barplot(success.table/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  success.table/iter
}
