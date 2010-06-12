\name{[-methods}
\docType{methods}
\alias{[-methods}
\alias{[,mondate-method}
\title{Extraction Method for Mondates}
\description{
Method for function \code{[} for a \code{mondate}
}
\section{Methods}{
\describe{

\item{\code{[(x = "mondate")}}{
See \code{[} for more details.
}
}}
\examples{
(M<-structure(mondate.ymd(2001:2005,12,31),names=LETTERS[1:5]))
M[1]
M[2:5]
M<-cbind(begin=M-12,end=M)
M[1:3,]
}
\keyword{methods}
