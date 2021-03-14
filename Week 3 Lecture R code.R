###########################################
# Inverse Transform Method, Discrete Case #
###########################################

# Generate random sample from the Bernoulli distribution with success probability p = 0.4
n <- 10000
u <- runif(n)
x <- rep(0, n)

for (i in 1:n){
  if (u[i] <= 0.6){
    x[i] <- 0
  } else{
    x[i] <- 1
  }
}

table(x) # you can find how many zeros and ones we have

mean(x) # Theoretical mean of Bernoulli distribution is p = 0.4 and our sample mean is very close to 0.4.


# Generate random sample from the binomial distribution with number of trials = 3 and success probabiilty = 0.2.

# dbinom(0, 3, 0.2)
# dbinom(1, 3, 0.2)
# dbinom(2, 3, 0.2)
# dbinom(3, 3, 0.2)

# pbinom(0, 3, 0.2)
# pbinom(1, 3, 0.2)
# pbinom(2, 3, 0.2)
# pbinom(3, 3, 0.2)

F_x <- c(0.512, 0.896, 0.992, 1)
num_success <- c(0, 1, 2, 3)

n <- 10000
u <- runif(n)
x <- rep(0, n)

for (i in 1:n){
  if (u[i] <= F_x[1]){
    x[i] <- 0
  } else if (u[i] > F_x[1] & u[i] <= F_x[2]){
    x[i] <- 1
  } else if (u[i] > F_x[2] & u[i] <= F_x[3]){
    x[i] <- 2
  } else {
    x[i] <- 3
  }
}

table(x) # you can find how many zeros and ones we have

mean(x) # Theoretical mean of the binomial distribution is N*p = 3*0.2 = 0.6 and our sample mean is very close to 0.6.



######################
# Rejection Sampling #
######################

# Generate 1000 beta vairiates from the Beta(2, 2)

# Plot density function curve in R
y <- seq(0, 1, 0.01)
fy <- 6*y*(1-y)
plot(y, fy, type = "l", col = "red", ylim = c(0, 7))
# Plot proposal distribution g(y) which is Uniform(0, 1)
lines(y, rep(6, length(y)), col = "blue") # 6*g(y)

# Rejection sampling method
n <- 1000
k <- 0 # counter for accepted
j <- 0 # counter for iterations
x <- rep(0, n)

while(k < n){
  y <- runif(1) # random variate from proposal distribution g(y) which is Uniform(0, 1)
  u <- runif(1)
  j <- j + 1
  if (u < y*(1-y)){
    # we accept y
    k <- k + 1
    x[k] <- y
  }
}

# Compare empirical and theoretical percentiles
p <- seq(0.1, 0.9, 0.01)
Qhat <- quantile(x, p) # quantiles from sample
Q <- qbeta(p, 2, 2) # theoretical quantiles

round(rbind(Qhat, Q), 3)

# Draw a Q-Q plot
plot(Q, Qhat)

