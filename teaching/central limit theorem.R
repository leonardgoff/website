set.seed(519)
numsims<-1000
t<- seq(-4, 4, length=100)

par(mfrow=c(2,2))
#Loop over sample sizes
for (n in c(2,10,100,1000)){
  results<-data.frame(simulation_num=integer(), sample_mean=double())
  
  #Simulate the random sample 1000 times
  for (x in 1:numsims) {
    thissample<-sample(c(0, 1), size = n, replace=TRUE)
    
    #calculate sample mean and store it in results dataframe
    samplemean<-mean(thissample)
    results[x,] = c(x,sqrt(n)*(samplemean-1/2))
  }
  
  #plot results along with theoretical normal density according to CLT (green)
  h<-hist(results$sample_mean, plot=FALSE, breaks = seq(from=-5, to=5, by=.01))
  h$density = h$density/100
  y<-dnorm(t,0,1/2)/sqrt(n)
  plot(h, freq=FALSE, col = "black", main=paste0("n=",n," coin flips"), xlab="Sample mean", ylab="Proportion of samples")
  lines(t,y, type = "l", lty=8, col="green", lwd = 2)
}
