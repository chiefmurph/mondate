\name{array-method}
\docType{methods}
\alias{array-methods}
\alias{array,mondate-method}
\alias{array}
\title{Array Methods}
\description{
Apply array attributes to a \code{mondate}.
}
\section{Methods}{
\describe{

\item{\code{array(data = NA, dim = length(data), dimnames = NULL)}}{
Shapes \code{mondate} \code{data} as a matrix. 
Inherits the \code{mondate}'s other properties.
See \code{\link{array}} for further details.
}

}}
\examples{
M <- mondate.mdy(12, 31, 2006:2011, timeunits = "years") # 6 year-ends
array(M, c(2,3))
}
\keyword{methods}
