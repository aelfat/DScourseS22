library(tidyverse); library(nloptr)

## Q4
set.seed(100)

# X is a matrix of dimension N = 100, 000 by K = 10
# containing normally distributed random numbers, 
# except the first column which should be a column of
# 1’s.
N = 100000 
K = 10
sd1 <- 0.25

X <- matrix(data = rnorm(N*K, mean = 0, sd = sd1), N,K)
X[,1] <- 1


eps <- matrix(data = rnorm(N, mean = 0, sd = sd1), N,1)


beta_val <- matrix(data = c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2) ,10,1)

## generating Y
y <- crossprod(t(X),beta_val) + eps 

## Q5
beta_hat <- crossprod(X) %>% solve() %*%  crossprod(X,y) 

## Q6

y <- y %>% as.vector()
  
 # set up a stepsize
  alpha <- 0.00003
# set up a number of iterations
maxiter <- 500000

# Our objective function
objfun <- function(beta,y,X) {
  return ( sum((y-X%*%beta)^2) )
}
# define the gradient of our objective function
gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}


beta <- runif(dim(X)[2])

iter  <- 1
beta0 <- 0*beta_hat


while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,y,X)
  #beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))


## Q7

library(nloptr)

#----------------------------------
# OLS estimation with L-BFGS
#----------------------------------

## Our objective function
objfun <- function(beta,y,X) {
  return (sum((y-X%*%beta)^2))
  # equivalently, if we want to use matrix algebra:
  # return ( crossprod(y-X%*%beta) )
}

## Gradient of our objective function
gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}

## initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
print(result)



#----------------------------------
# MLE estimation with Nelder Mead
#----------------------------------
## Our objective function
objfun  <- function(theta,y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}



## initial values
theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)

## Optimize!
result <- nloptr( x0=theta0,eval_f=objfun,opts=options,y=y,X=X)
print(result)
betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]


## Q8

gradient <- function (theta ,Y,X) {
  grad <- as.vector ( rep (0, length (theta )))
  beta <- theta [1:( length ( theta) -1)]
  sig <- theta [ length (theta )]
  grad [1:( length ( theta) -1)] <- -t(X)%*%(Y - X%*%beta )/(sig ^2)
  grad[ length (theta )] <- dim (X)[1] /sig - crossprod (Y-X%*%beta )/(sig^3)
                                                                       
  return ( grad )
}

## initial values
theta0 <- runif(dim(X)[2]+1)

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)

## Optimize!
result <- nloptr( x0=theta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
print(result)
betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]

## Q9
model1 <- lm(y ~ X -1)

modelsummary(model1,output = "table1.tex")


#summary(model1)
