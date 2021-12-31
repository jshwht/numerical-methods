library(matlib)

## Newton-Raphson for minimizing f(x1,x2) = 100(x2-x1^2)^2 + (1-x1)^2
fcn <- function(x,itr){
  init=x
  evalvec <- c()
  x1vec <- c()
  x2vec <- c()
  for (i in 1:itr){
    term1 = matrix(c(1200*init[1]^2-400*init[2]+2,
                     -400*init[1],
                     -400*init[1],
                     200),ncol=2,byrow=TRUE)
    term2 = c(-400*init[1]*init[2]+400*init[1]^3+2*init[1]-2, 
              200*init[2]-200*init[1]^2)
    
    x_new = init - inv(term1)%*%term2
    
    eval <- 100*(x_new[2]-x_new[1]^2)^2 + (1-x_new[1])^2
    evalvec <- c(evalvec,eval)
    init = x_new
    x1vec <- c(x1vec,init[1])
    x2vec <- c(x2vec,init[2])
  }
  x_matrix <- cbind(x1 = x1vec, x2 = x2vec)
  result=list(x_matrix,f_x=evalvec) ## gives the parameters and the function values
  return(result)
}

call <- fcn(c(2,2),20) ## initial (x1,x2) is (2,2) 
call

## (1,1) is a local minimizer 