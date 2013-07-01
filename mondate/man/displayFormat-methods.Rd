\name{displayFormat-methods}
\docType{methods}
\alias{displayFormat-methods}
\alias{displayFormat,mondate-method}
\alias{displayFormat,ANY-method}
\alias{displayFormat}
\alias{displayFormat<--methods}
\alias{displayFormat<-,mondate-method}
\alias{displayFormat<-}
\title{Methods to Access 'displayFormat' Property}
\description{
Methods to get and set the \code{displayFormat} value of a \code{mondate}.
}
\usage{
\S4method{displayFormat}{mondate}(x)
\S4method{displayFormat}{ANY}(x)
\S4method{displayFormat}{mondate}(x)<-value
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
    \item "\%m/\%d/\%Y"
    \item "\%m-\%d-\%Y"
    \item "\%Y-\%m-\%d"
    \item "\%Y/\%m/\%d"
    }
If \code{x} is not a \code{mondate}, 
the "get" value returned is \code{NULL}.
}
}
\note{
The \code{mondateDisplayFormat} versions have been deprecated.
}
\examples{
x<-mondate("2010-6-30")            # The middle of 2010
displayFormat(x)                   # "\%Y-\%m-\%d"
displayFormat(x) <- "\%m/\%d/\%Y"
x                                  # x now displays as 06/30/2010
}
\keyword{methods}
