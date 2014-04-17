





#' @title Generic classify function
#' @aliases classify
#' @author Xiaoyao Yang
#' @description Returning classification result
#' @return An label obtained by classification method determined by pars
#' @details
#' Returning label for classification problem
#'
#' This is the S3 generic method for returning classification result.
#'
#' For more information \code{\link{classify.default}}
#' @export classify
#' @param pars Result returned by training algorithm
#' @param dat Data set to be classified
#' @examples
#'
#' set.seed(1024)
#' z <- runif(n=5)
#' mydata <- fakedata(w=z,n=100)
#' X<- mydata$S[,1:4]
#' y <- mydata$y
#' w <- rep(1/100,100)
#' pars <- DecisionStump(X=X,w=w,y=y)
#' classify(pars,X)
#' 
classify <- function(pars,dat)
{
    UseMethod('classify')
}






#' @title Default method for classify
#' @description Returning classification result
#' @aliases classify.default
#' @return An label obtained by classification method determined by pars
#' @author Xiaoyao Yang
#' @param pars Result returned by training algorithm
#' @param dat Data set to be classified
#' @export classify.default
#' @method classify default
#' @S3method classify default
#' @examples
#' 
#' set.seed(1024)
#' z <- runif(n=5)
#' mydata <- fakedata(w=z,n=100)
#' X<- mydata$S[,1:4]
#' y <- mydata$y
#' w <- rep(1/100,100)
#' pars <- DecisionStump(X=X,w=w,y=y)
#' classify.default(pars,X)

classify.default <- function(pars,dat)
{
    #     X must have same rcol with training data
    rcol <- pars[['j']]
    theta <- pars$theta
    yhat <- 2*(dat[,rcol]>theta)-1
    return(as.vector(yhat))
}

#' @title classify.ds
#' 
#' @description Returning classification for DecisionStump
#'
#'@details
#' Returning label for classification problem
#'
#' \code{\link{classify}} is the S3 generic method for returning classification result.
#'
#' For more information \code{\link{classify.default}}
#' 
#' @aliases classify.ds
#' @export classify.ds
#' @method classify ds
#' @S3method classify ds
#' @author Xiaoyao Yang
#' @param \dots See \code{\link{classify.default}} for argument details
#' @return See \code{\link{classify.default}} for more information.
#' @examples
#' 
#' set.seed(1024)
#' z <- runif(n=5)
#' mydata <- fakedata(w=z,n=100)
#' X<- mydata$S[,1:4]
#' y <- mydata$y
#' w <- rep(1/100,100)
#' pars <- DecisionStump(X=X,w=w,y=y)
#' classify.ds(pars,X)

classify.ds <- function(...)
{
    classify.default(...)
}




#' @title Classify for perceptrain in package
#' @description Returning classification result
#' @aliases classify.pt
#' @return An label obtained by classification method determined by pars
#' @author Xiaoyao Yang
#' @details
#' For consistency, using X(data set) instead of S ([dataset,1]) for classify function.
#' 
#' For more information \code{\link{perceptrain}}
#' @param pars Result returned by training algorithm
#' @param dat Data set to be classified (Here cbind(dat,1)=S)
#' @export classify.pt
#' @method classify pt
#' @S3method classify pt
#' @examples
#' 
#' set.seed(1024)
#' z <- runif(n=3)
#' mydata <- fakedata(w=z,n=100)
#' r <- perceptrain(S=mydata$S,y=mydata$y,alpha_k=1,endcost=0)
#' classify.pt(r,mydata$S[,1:(NCOL(mydata$S)-1)])


classify.pt<-function(pars,dat){
    #z<-c(-c,Vh) x<-c(x,1)
    if (length(z)!=(NCOL(dat)+1)) stop ('input error: dimension of hyperplane must be NCOL(dat)+1')
    z <- pars$z;  S <- cbind(dat,1)
    d<-length(z)-1;
    yy<-as.vector(sign(z%*%rbind(1,t(S)[1:d,])))
    return(yy)
}





