lambda = 0.2
n = 40 
nosim = 1000

set.seed(300)


sim <- data.frame(ncol=1,nrow=1000)
names(sim) <- c("Mean")

for (i in 1:nosim)
{
 
sim[i,1] <- mean(rexp(n,lambda))
}

sample_mean <- mean(sim$Mean)
sample_mean

theor_mean <- 1/lambda
theor_mean

hist(sim$Mean, 
     breaks = 100, 
     prob = TRUE, 
     main="Exponential Distribution n = 1000", 
  )
abline(v = theor_mean, 
       col= 5,
       lwd = 2)
abline(v = sample_mean, 
       col = 6,
       lwd = 2)

legend('topright', c("Sample Mean", "Theoretical Mean"), 
       bty = "n",       
       lty = c(1,1), 
       col = c(col = 5, col = 6))

sample_var <- var(sim$Mean)
theor_var <- ((1/lambda)^2)/40

sample_var

theor_var

hist(sim$Mean, 
     breaks = 100, 
     prob = TRUE, 
     main = "Exponential Distribution n = 1000", 
     )
xfit <- seq(min(sim$Mean), max(sim$Mean), length = 100)
yfit <- dnorm(xfit, mean = 1/lambda, sd = (1/lambda/sqrt(40)))
lines(xfit, yfit, pch = 22, col = 3, lwd = 2)
legend('topright', c("Theoretical Curve"), 
       lty = 1,lwd = 2, bty = "n", col = 3)



hist(sim$Mean, 
     breaks = 100, 
     prob = TRUE, 
     main = "Exponential Distribution n = 1000", 
     xlab = "Spread")
lines(density(sim$Mean))
abline(v = 1/lambda, col = 3)
xfit <- seq(min(sim$Mean), max(sim$Mean), length = 100)
yfit <- dnorm(xfit, mean = 1/lambda, sd = (1/lambda/sqrt(40)))
lines(xfit, yfit, pch = 22, col = 4, lty = 2)
legend('topright', c("Simulated Values", "Theoretical Values"), 
       bty = "n", lty = c(1,2), col = c(4, 3))



qqnorm(sim$Mean, 
       main ="Normal Q-Q Plot")
qqline(sim$Mean, 
       col = "3")




