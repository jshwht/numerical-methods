## Optimizing f(x) = e^x / (1 + e^x)^2 
fcn <- function(x,itr){
  init=x
  evalvec <- c()
  xvec <- c()
  for (i in 1:itr){
    term1 = (exp(init)-exp(2*init))/((1+exp(init))^3)
    term2 = (exp(init)-4*exp(2*init)+exp(3*init))/((1+exp(init))^4)
    
    x_new = init - term1/term2
    
    eval <- exp(x_new)/((1+exp(x_new))^2)
    evalvec <- c(evalvec,eval)
    init = x_new
    xvec <- c(xvec,init)
  }
  result=list(X_values=xvec,f_x=evalvec)
  return(result)
}

call <- fcn(-0.6,10) ## Converges in less than 10 iterations
                     ## Local maximum at x=6.439751e-17
call 
