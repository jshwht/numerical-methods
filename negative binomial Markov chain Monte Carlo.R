data <- c() ## data represents the number of failures given r successes

r_vec <- c()
p_vec <- c()

lim <- 50 ## total number of samples of r and p

for (k in 1:lim){
  p0 <- 0.5
  r0 <-5
  
  for (j in 1:10000){ ## number of times to alternate sampling 
    ## from conditionals
    
    ## Metropolis-Hastings for sampling from r|x,p
    for (i in 1:500){ ## 500 to prevent runaway iterations 
      rt <- rnorm(n=1,mean=r0,sd=3) # proposal distribution
      r <- (prod(choose(rt+data-1,data)*(p0^rt)*(1-p0)^data)* 
              ifelse(rt>=0 & rt<=10, 1/(10-0+1), 0))/ 
        (prod(choose(r0+data-1,data)*(p0^rt)*(1-p0)^data)* 
           ifelse(r0>=0 & r0<=10, 1/(10-0+1), 0)) 
      
      if (r>runif(n=1,0,1)){
        r0 <- rt 
        ## r0 is a sample from the conditional
        break
      }
    }
    
    ## Metropolis-Hastings for sampling from p|x,r
    for (i in 1:500){
      pt <- rnorm(1,p0,0.1) # proposal distribution 
      r <- (prod(choose(r0+data-1,data)*(pt^r0)*(1-pt)^data)*
              dunif(x=pt))/
        (prod(choose(r0+data-1,data)*(p0^r0)*(1-p0)^data)*
           dunif(x=p0))
      
      if (r>runif(n=1,0,1)){
        p0 <- pt
        ## p0 is a sample from the conditional
        break
      }
    }
  }
  r_vec <- c(r_vec,r0)
  p_vec <- c(p_vec,p0)
}

## data frame containing parameter estimates
df <- data.frame(r = r_vec, p = p_vec) 

r_final <- mean(df$r)
p_final <- mean(df$p)

## Comparison with maximum likelihood estimates (Poisson and negative binomial)
plot(dpois(0:20,3.3),type='h',col='blue',lwd=3,xlab="",ylab="")
lines(dnbinom(0:20,size=mean(df$r),p=mean(df$p)),col='green',lwd=4)
lines(dnbinom(0:20,size=6.631,p=0.668),col='red',lwd=3)
legend(x = 15.63, y = 0.23, c('Poisson','MCMC neg. binomial',
                              'MLE neg. binomial'), lty = c(1,1,1), 
       col = c('blue','green','red'), lwd = c(3, 3,3))

