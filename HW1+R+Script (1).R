# Standing ovation simulation
# G.P. Learmonth
#
# Monte Carlo simulation of Expected Number of audience members in
# standing ovation.
#
# N.B. This result is trivial hence this "simulation" is essentially # useless; however, it provides the basis for the questions posed 
# in HW1. 

# N in audience; M independent iterations

N <- 100;  M <- 1000

# Quality signal received = q; variance = sigma2;  
# threshhold for standing = threshhold

q <- 6; sigma2 <- 2

threshhold <- 6

# Iterate M times; initialize size vector

size <- rep(0, N)

for (j in 1:M)  {

  s <- ceiling((q + rnorm(N,0,sigma2)))

# Determine those standing

  k <- (sum(s>threshhold));
  size[k] <- size[k]+1;

  }

plot(1:100,size[1:100]/M)