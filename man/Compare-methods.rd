\name{Compare-methods}
\docType{methods}
\alias{Compare-methods}
\alias{Compare,mondate,mondate-method}
\title{Comparison Methods}
\description{
Methods for the \code{Compare} group of functions.
}
\section{Methods}{
\describe{

\item{\code{signature(e1 = "mondate", e2 = "mondate")}}{
compares two \code{mondate}s.
The usual recycling rules apply to the shorter of the two \code{mondate}s.
The result will be logical. 
The usual rules apply as to the shape of the result.
}

}}
\seealso{
\code{\link{Compare}}
}
\examples{
A<-mondate.ymd(2001:2003,12,31) # three year ends
B<-mondate.ymd(2001:2003, 6,30) # three mid-years
B<A                             # c(TRUE, TRUE, TRUE)
}
\keyword{methods}
