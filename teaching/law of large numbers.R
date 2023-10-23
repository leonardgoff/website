numsims<-1000
x<- seq(-4, 4, length=100)
y <- dnorm(x)

par(mfrow=c(2,2))
#Loop over sample sizes
for (n in c(2,10,100,1000)){
  
  results<-data.frame(simulation_num=integer(), sample_mean=double())
  
  #Simulate the random sample 1000 times
  for (x in 1:numsims) {
    #Draw sample of n 0/1 coin flips
    thissample<-sample(c(0, 1), size = n, replace=TRUE)
    
    #calculate sample mean and store it in results dataframe
    samplemean<-mean(thissample)
    results[x,] = c(x,samplemean)
  }
  
  #plot results along with vertical lines depicting the set of values within .05 of 1/2
  h<-hist(results$sample_mean, plot=FALSE, breaks = seq(from=0, to=1, by=.01))
  h$density = h$density/100
  plot(h, freq=FALSE, main=paste0("Distribution of sample means, n=",n," coin flips"), xlab="Sample mean", ylab="Proportion of samples", col="green")
  abline(v=c(.45,.55), col=c("red", "red"))  
}
