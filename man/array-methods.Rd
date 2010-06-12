\name{array-methods}
\docType{methods}
\alias{array-methods}
\alias{array,ANY-method}
\alias{array,mondate-method}
\alias{array}
\alias{matrix-methods}
\alias{matrix,ANY-method}
\alias{matrix,mondate-method}
\alias{matrix}
\title{Array Methods for Mondate's}
\description{
Apply array attributes to a \code{mondate}.
}
\section{Methods}{
\describe{

\item{\code{array(data="mondate", dim = length(data), dimnames = NULL)}}{
Shapes \code{data} as an array. 
Inherits \code{data}'s properties.
See \code{\link{array}} for further details.
}

\item{\code{matrix(data, nrow, ncol, byrow=FALSE, dimnames=NULL)}}{
Shapes \code{data} as a matrix. 
Inherits \code{data}'s properties.
See \code{\link{matrix}} for further details.
}
}}
\examples{
(M<-mondate.mdy(12,31,2000:2009)) # 10 year-ends
# rows representing (actuarial) accident years, every other year
matrix(M, nrow=5, ncol=2, dimnames=list(AccYrs=LETTERS[1:5],
          YearBounds=c("Begin","End")), byrow=TRUE)
# the first 27 days of 2000 in a three-dimensional array
array(mondate(1:27, timeunits="days"), rep(3,3))
}
\keyword{methods}
