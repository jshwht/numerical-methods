## Gradient descent for estimation of negative binomial
## parameter 'r' given analytical MLE for 'p' 

data <- c() 

fcn_val <- function(x){
  fcn_list <- c()
  for (i in 1:20){
    fcn_list <- c(fcn_list,choose(x+data[i]-1,data[i])*
                    ((x/(3.3+x))^x)*(1-x/(3.3+x))^(data[i]))
  }
  return(prod(fcn_list))
}

calc <- function(data,x,itr){
  avg <- mean(data)
  converged=FALSE
  alpha <- 0.9
  f<-fcn_val(x)
  i=0
  
  while(converged==FALSE){
    i=i+1
    
    x_new <- x + alpha*((sum((log(data+x))-1/(2*(data+x))))-
                          20*((log(x))-1/(2*(x)))+
                          20*log(x/(3.3+x)))
    f_new <- fcn_val(x_new)
    
    while(f_new<f){
      alpha <- alpha*0.95
      
      x_new <- x + alpha*((sum((log(data+x))-1/(2*(data+x))))-
                            20*((log(x))-1/(2*(x)))+
                            20*log(x/(3.3+x)))
      f_new <- fcn_val(x_new)
    }
    
    x_dif <- abs(x-x_new)
    
    if ((i==itr)|(x_dif<1e-10)){
      converged=TRUE
    }
    
    x <- x_new
    f<-f_new
    
  }
  
  return(list(r=x,fcn_val=f,iterations=i,alpha=alpha))
}

calc(data,__,10000) ## enter initial value for x (the parameter). 
