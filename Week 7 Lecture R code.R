################################################################
# Metropolis-Hastings Algorithm for Continuous Random Variable #
################################################################
set.seed(2)

# Example
# the function evaluating the Rayleigh density
f_x <- function(x, sigma){
  (x/sigma^2)*exp(-x^2/(2*sigma^2))
}
# plot the target function
# Rayleigh distribution with scale parameter 4.
y <- seq(0, 12, 0.01)
plot(y, f_x(y, 4), type = "l", col = "red", xlab = "", ylab = "")

# proposal distribution
# Chi-squared distribution with degrees of freedom at current time point (X_t)
# plot the chi-squared distribution (not a symmetric distribution)
y <- seq(0, 12, 0.01)
plot(y, dchisq(y, df = 6), type = "l", col = "red", xlab = "", ylab = "")


#the function sampling new point from the proposal distribution
q <- function(n, shape, rate){
  rgamma(n, shape = shape, rate = rate)
}

# the function evaluating the proposal function density
q_x <- function(x_t, shape, rate ){
  dgamma(x_t, shape = shape, rate = rate)
}

n <- 10000
x <- rep(0, n)

# initial value
x[1] <- 2

for(i in 1:n-1){
  xnew = q(1, shape = x[i], rate = 1)
  if(runif(1) < (f_x(xnew, sigma = 2)*q_x(x[i], shape = x[i], rate = 1))/(f_x(x[i], sigma = 2)*q_x(x[i], shape = x[i], rate = 1))){
    x[i+1] = xnew
  }else{
    x[i+1] = x[i]
  }
}

# # the function sampling new point from the proposal distribution
# q <- function(df){
#   rchisq(1, df = df)
# }
# 
# # the function evaluating the proposal function density
# q_x <- function(x, df){
#   dchisq(x, df = df)
# }
# 
# n <- 10000
# x <- rep(0, n)
# 
# # initial value
# x[1] <- 2
# 
# for(i in 1:(n-1)){
#   # pick new sample
#   xnew <- q(x[i])
#   # acceptance condition
#   if (runif(1) < (f_x(xnew, sigma = 4)*q_x(x[i], df = xnew))/(f_x(x[i], sigma = 4)*q_x(xnew, df = x[i]))){
#     x[i+1] <- xnew
#   }
#   else{
#     x[i+1] <- x[i]
#   }
# }

# draw the histogram and add the target function to the histogram
hist(x, prob = TRUE, ylim = c(0, 0.2))
y <- seq(0, 15, 0.01)
lines(y, f_x(y, 4), col = "red")


