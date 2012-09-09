\name{Summary-methods}
\docType{methods}
\alias{Summary-methods}
\alias{Summary,mondate-method}
\title{Summary Methods}
\description{
Methods for the \code{Summary} group of functions.
}
\section{Methods}{
\describe{

\item{\code{signature(x = "mondate")}}{
summarizes a \code{mondate}.

The result will be a \code{mondate} with the same 
\code{displayFormat} and \code{timeunits} properties. 

The usual rules apply as to the shape of the result.
}

}}
\examples{
A<-mondate.ymd(2001:2010,12,31) # ten yearends
min(A)                  # December 31, 2001
max(A)                  # December 31, 2010
range(A)
}
\keyword{methods}
