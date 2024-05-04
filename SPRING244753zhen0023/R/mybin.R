#' Creating sample and corresponding table and bar graph
#'
#' @param iter integer, number of iterations
#' @param n integer, sample size
#' @param p integer, percentage of success
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot
#'
#' @return a table and bar graph corresponding to the sample
#' @export
#'
#' @examples mybin(iter = 200, 10, 0.8)


mybin=function(iter=100, n=10, p=0.7){

  # matrix to hold samples, initialized with NULL
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)

  # vector to hold number of successes for each trial
  succ=c()

  for(i in 1:iter){

    # filling each column with a sample
    sam.mat[,i] = sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))

    # calculating sum of the sample
    succ[i]=sum(sam.mat[,i])
  }

  # table displaying successes
  succ.tab=table(factor(succ,levels=0:n))

  # barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
