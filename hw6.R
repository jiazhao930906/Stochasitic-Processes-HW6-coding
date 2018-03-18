rm(list=ls())

all_x <- c(0:20)
mu <- dbinom(all_x, 20, 0.4)
nu <- dbinom(all_x, 20, 0.6)

min_mu_nu <- apply(cbind(mu, nu), 1, min) 
p <- sum(min_mu_nu)

gamma <- min_mu_nu/p
gamma1 <- sapply(mu-nu, function(t){max(0,t)})/(1-p)
gamma2<- sapply(nu-mu, function(t){max(0,t)})/(1-p)

size = 10000
sample <- matrix(NA, size, 2)

for (i in 1:size){
  if (rbinom(1,1,p)==1) {
    u <- runif(1,0,1)
    sample[i,1] <- sample[i,2] <- sample(c(0:20), size = 1, prob = gamma)    
    # sample[i,1] <- sample[i,2] <- min(which(cdf_z > u)-1)  
  } else {
    u <- runif(1,0,1)
    sample[i,1] <- sample(c(0:20), size = 1, prob = gamma1)
    # sample[i,1] <- min(which(cdf_mu > u)-1)
    sample[i,2] <- sample(c(0:20), size = 1, prob = gamma2)
    # sample[i,2] <- min(which(cdf_nu > u)-1)
  } 
}

plot(sample[,1], sample[,2])

X<-rbinom(10000, 20, 0.4)
Y<-rbinom(10000, 20, 0.6)
plot(X,Y, xlim=c(0,20), ylim=c(0,20))