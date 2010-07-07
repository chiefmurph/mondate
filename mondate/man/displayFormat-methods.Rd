\name{mondateDisplayFormat-methods}
\docType{methods}
\alias{mondateDisplayFormat-methods}
\alias{mondateDisplayFormat,mondate-method}
\alias{mondateDisplayFormat,ANY-method}
\alias{mondateDisplayFormat}
\alias{mondateDisplayFormat<--methods}
\alias{mondateDisplayFormat<-,mondate-method}
\alias{mondateDisplayFormat<-}
\title{Methods to Access 'displayFormat' Property}
\description{
Methods to get and set the \code{displayFormat} value of a \code{mondate}.
}
\usage{
\S4method{mondateDisplayFormat}{mondate}(x)
\S4method{mondateDisplayFormat}{ANY}(x)
\S4method{mondateDisplayFormat}{mondate}(x)<-value
}
\arguments{
\item{x}{
a \code{mondate}.
}
\item{value}{
For the "get" method, 
a \code{character} string indicating the date format with which to
display the \code{mondate}.
Choices are currently
    \enumerate{
    \item "%m/%d/%Y"
    \item "%m-%d-%Y"
    \item "%Y-%m-%d"
    \item "%Y/%m/%d"
    }
If \code{x} is not a \code{mondate}, 
the "get" value returned is \code{NULL}.
}
}
\examples{
x<-mondate("2010-6-30")                   # The middle of 2010
mondateDisplayFormat(x)                   # "\%Y-\%m-\%d"
mondateDisplayFormat(x) <- "\%m/\%d/\%Y"
x                                         # x now displays as 06/30/2011
}
\keyword{methods}
