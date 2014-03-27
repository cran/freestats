\name{cv.work}
\alias{cv.work}
\title{k-fold cross validation}
\usage{
cv.work(fun, k = 5, data, cost, response = "y", ...)
}
\arguments{
  \item{fun}{The model function to call on the data}

  \item{k}{The number of folds}

  \item{data}{The data}

  \item{cost}{Cost function for the
  error:'cost.mse','cost.01'}

  \item{response}{Character vector indicating which column
  is the response}

  \item{\dots}{Extra arguments for model function}
}
\value{
A single cross-validated error
}
\description{
Calculate cross-validation error
}
\examples{
data(diamonds)
dat <- diamonds[1:1000,]
cv.work(fun=lm,k=5,data=dat,cost=cost.mse,response='price',formula=price~carat)
}
\author{
Xiaoyao Yang
}

