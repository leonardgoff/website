#First example: tossing a coin

numsims<-1000
n<-100

results<-data.frame(simulation_num=integer(), sample_mean=double())
for (x in 1:numsims) {
  thissample<-sample(c(0, 1), size = n, replace=TRUE)
  samplemean<-mean(thissample)
  results[x,] = c(x,samplemean)
}

hist(results$sample_mean, xlim=c(0,1))
