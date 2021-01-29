\name{[-methods}
\docType{methods}
\alias{[-methods}
\alias{[,mondate-method}
\alias{head}
\alias{head.mondate}
\alias{tail}
\alias{tail.mondate}
\title{Extraction Method for Mondates}
\description{
Methods to extract portions of a \code{mondate}
}
\section{Methods}{
\describe{

\item{\code{[(x = "mondate")}}{
See \code{[} for more details.
}

}}
\usage{
\method{head}{mondate}(x, \dots)
\method{tail}{mondate}(x, \dots)
}
\arguments{
\item{x}{
a \code{mondate}.
}
\item{\dots}{
See the \code{base} functions for details.
}
}
\details{
Whether the \code{mondate x} is shaped as a \code{vector} or a \code{matrix}, 
the \code{head} and \code{rbind} methods will behave just as they
would if \code{x} were \code{numeric}.
}
\examples{
(m<-structure(mondate.ymd(2001:2010,12,31),names=LETTERS[1:10]))
m[1]
m[2:5]
head(m)
tail(m,2)

(M<-cbind(m-12,m,m+12, deparse.level=2))  # a matrix
M[1:5,1:2]    # '[' works with matrix mondates
head(M,2)  # as does 'head'

}
\keyword{methods}
