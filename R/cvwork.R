
#' @title k-fold cross validation
#' @description Calculate cross-validation error
#' @export cv.work
#' @import ggplot2
#' @return A single cross-validated error
#' @author Xiaoyao Yang
#' @param fun The model function to call on the data
#' @param k The number of folds
#' @param data The data
#' @param cost Cost function for the error:'cost.mse','cost.01'
#' @param response Character vector indicating which column is the response
#' @param \dots Extra arguments for model function
#' @examples
#' 
#' data(diamonds)
#' dat <- diamonds[1:1000,]
#' cv.work(fun=lm,k=5,data=dat,cost=cost.mse,response='price',formula=price~carat)
#' 
cv.work <- function(fun,k=5,data,cost,response='y', ...)
{
  #generate folds
  folds <- data.frame(Fold=sample(rep(1:k, length.out=NROW(data))),
                      Row=1:NROW(data))
  error <- 0
  
  for(f in 1:max(folds$Fold))
  {
    theRows <- folds$Row[folds$Fold == f]
    
    mod <- fun(data[-theRows,],...)
    pred <- predict(mod, data[theRows,])
    
    theCost <- cost(data[theRows, response],pred)
    error <- error + theCost*(length(theRows)/NROW(data))
  }
  return(error)
}




