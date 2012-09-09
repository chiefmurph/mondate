\name{matrix-methods}
\docType{methods}
\alias{matrix-methods}
\alias{matrix,mondate-method}
\alias{matrix}
\title{Matrix Methods for Mondate's}
\description{
Apply matrix attributes to a \code{mondate}.
}
\section{Methods}{
\describe{

\item{\code{matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)}}{
Shapes \code{mondate} \code{data} as a matrix. 
Inherits the \code{mondate}'s other properties.
See \code{\link{matrix}} for further details.
}
}}
\examples{
m <- mondate.mdy(12, 31, 2001:2006) # 6 year-ends
matrix(m)                           # a one-column matrix
matrix(m, 2, byrow=TRUE)            # a two-row matrix stored in row-order
}
\keyword{methods}
