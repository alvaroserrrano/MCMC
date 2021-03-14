######################
# Rejection Sampling #
######################

# Generate 1000 random samples from the Beta(3, 2) density
# Assume we are dealing with unknown normalizing constant

# Plot density function curve in R
y <- seq(0, 1, 0.01)
fy <- (y^2)*(1-y)
plot(y, fy, type = "l", col = "red", ylim = c(0, 7))
# Plot proposal distribution g(y) which is Uniform(0, 1)
lines(y, rep(1, length(y)), col = "blue") # 1*g(y)

# Rejection sampling method
n <- 1000
k <- 0 #counter for accepted
j <- 0 #counter for iterations
x <- rep(0, n)

while(k < n){
  y <- runif(1) #random sample from g(y) which is Uniform(0, 1)
  u <- runif(1)
  j <- j + 1
  if (u < (y^2)*(1-y)){
    #we accept y
    k <- k + 1
    x[k] <- y
  }
}

# Compare empirical and theoretical percentiles
p <- seq(0.1, 0.9, 0.01)
Qhat <- quantile(x, p) # quantiles from sample
Q <- qbeta(p, 3, 2) # theoretical quantiles

round(rbind(Qhat, Q), 3)

# Draw a Q-Q plot
plot(Q, Qhat)


####################################################
# Monte Carlo Probability with Importance Sampling #
####################################################

# Calculate P(X > 3) where X ~ N(0,1)
# P(X > 3) = E(I(X > 3)), q(x) = I(X > 3) 
set.seed(23)
n <- 10000

# Naive Monte Carlo estimate
x <- rnorm(n, mean = 0, sd = 1)
theta.hat <- mean(x > 3)
theta.hat

1 - pnorm(3, mean = 0, sd = 1)

# Use Importance Sampling method with importance function, N(3, 1).
x1 <- rnorm(n, mean = 3, sd = 1)
theta.hat <- mean(((x1 > 3) * dnorm(x1, mean = 0, sd = 1))/dnorm(x1, mean = 3, sd = 1))
theta.hat

# Use importance sampling method with importance function, N(1, 1).
x2 <- rnorm(n, mean = 1, sd = 1)
theta.hat <- mean(((x2 > 3) * dnorm(x2, mean = 0, sd = 1))/dnorm(x2, mean = 1, sd = 1))
theta.hat

# A plot to show the convergence of the importance sampling approximation.
par(mfrow =c(1,3))

# Naive Monte Carlo estimate
plot(cumsum(x > 3)/(1:n), type = "l", ylab = "", ylim = c(0, 0.005))
abline(h = 1 - pnorm(3, mean = 0, sd = 1), col = "red")

# Importance Sampling with importance function, N(3, 1)
plot(cumsum(((x1 > 3) * dnorm(x1, mean = 0, sd = 1))/dnorm(x1, mean = 3, sd = 1))/(1:n), type = "l", ylab = "", ylim = c(0, 0.005))
abline(h = 1 - pnorm(3, mean = 0, sd = 1), col = "red")

# Importance Sampling with importance function, N(1, 1)
plot(cumsum(((x2 > 3) * dnorm(x2, mean = 0, sd = 1))/dnorm(x2, mean = 1, sd = 1))/(1:n), type = "l", ylab = "", ylim = c(0, 0.005))
abline(h = 1 - pnorm(3, mean = 0, sd = 1), col = "red")



