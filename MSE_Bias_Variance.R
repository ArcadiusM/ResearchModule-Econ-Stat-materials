
# Fix values for the mean, variance and the sample size

mu <- 3
sd <- 1 

n <- 30
R <- 5000

#X <- rnorm()

sn <- matrix(nrow = n, ncol = R)

for ( i in 1:n ) {
  
  for ( j in 1:R ) {
    
    X <- rnorm(i, mean = mu, sd = sd)
   
    X_n <- mean(X)
    
    Y <- X - X_n
    
    sn[i,j] <- sqrt(1/i*sum(Y^2))
    
  }
  
}

MSE <- matrix(nrow = n, ncol = 1)

for (i in 1:n) {
  
  z <- sn[,i] - sd 
  MSE[i] <- 1/R*sum(z^2)
  
}


Bias <- matrix(nrow = n, ncol = 1)

for (i in 1:n) {
  
  mean_sn <- mean(sn[,i]) 
  Bias[i] <- (mean_sn - sd)^2
  
}


Var <- matrix(nrow = n, ncol = 1)

for (i in 1:n) {
  
  mean_sn <- mean(sn[,i])
  
  Var[i] <- mean((sn[,i] - mean_sn)^2)
  
  
}

plot(1:n, MSE)
