#Statistical Inference Course - Simulation Exercise
#by Preetika Srivastava

#Multiple Simulations will be ran to compare the theoretical mean and standard deviation of exponential function, to that the one obtained
#by simulations. This exercise leverages the idea of 'Central Limit Theorem'

set.seed(3007)
lambda = 0.2
n = 40
simNum = 1000
plot(rexp(10000,lambda ), pch = 20, cex=0.6, main = "Exponential Dist with rate as 0.2 and 10,000 obs")
plot(rexp(10000,lambda ), pch = 20, cex=0.6, main = "Exponential Dist with rate as 0.2 and 10,000 obs", col= "red")

#See fig1 for the output of above code.

#generating the collection of means for 1000 simulations  of exp. distribution
myMean = NULL
for(k in 1:simNum) myMean = c(myMean, mean(rexp(n, lambda)))
hist(myMean, col="green", main= "rexp Mean Dist", breaks = 50)
rug(myMean)
#See fig 2 for the output of above code.

#Calculating the mean by simulated values and visualizing it

round(mean(myMean), 3)
hist(myMean, col="green", main="Comparing theoretical vs actual Mean", breaks = 50)
abline(v = mean(myMean), lwd ="5", col="red")

#See fig 3 for the output of the above code.

#Check for Standard Deviation
#theoretical
round( (1/lambda)/sqrt(n) ,4)
#Actual
round(sd(myMean) ,4)


#If the distribution is noraml ?

hist(myMean, prob=TRUE, col="darkblue", main="mean distribution for rexp()", breaks=50)
lines(density(myMean), lwd=5, col="red")

#See fig 5 for the output of the above code. 

