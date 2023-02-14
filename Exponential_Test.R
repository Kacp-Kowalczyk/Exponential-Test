
MC <- 100000 # amount of Monte Carlo iterations needed to calculate 
# empirical distribution of test statistics that is used to calculate p-values

stat <- function(X){   #function that calculates test statistics
  
  n <- length(X)
  
  Z <- X/mean(X)
  
  Z_ord <- sort(Z)
  
  s <- c()
  
  for(i in 1:n){
    
    one <- abs( i/n - pexp(Z_poz[i]) )
    
    two <- abs( (i-1)/n - pexp(Z_poz[i]) )
    
    s[i] <- max(one, two)
  }
  
  return (max(s));
  
}

n <- 200 # sample size

TMC <- c() # list with values of test statistics under the null

for(i in 1:MC){
  
  X <- rexp(n)
  
  TMC[i] <- stat(X)
  
  
}

plot(ecdf(TMC))

m <- 100000  # amount of test conducted to calculate empirical power of the test

a <- c(seq(0.05, 3, 0.05)) # list with different parameters "a"

POW <- c() # list with different values of empirical power for different "a"'s

alpha = 0.05 #significanse level

for(j in 1:60){
  
  M <- 0
  
  for(i in 1:m){
  
    X <- rgamma(n,a[j],1)  #distribution on which we calculate the power
                           #for a = 1 it is exponential distribution 
    p <- (sum(stat(X)<TMC))/MC #p-value calculated using MC method
    
    if(p < alpha){M <- M + 1}

  }
  
  POW[j] <- M/m
  
}
plot(a,POW, type = 'l')  #plot of empirical power 

POW

