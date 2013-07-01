\name{timeunits-methods}
\docType{methods}
\alias{timeunits-methods}
\alias{timeunits,mondate-method}
\alias{timeunits,ANY-method}
\alias{timeunits}
\alias{timeunits<--methods}
\alias{timeunits<-,mondate-method}
\alias{timeunits<-}
\title{Methods to Access 'timeunits' Property}
\description{
Methods to get and set the \code{timeunits} value of a \code{mondate}.
}
\usage{
\S4method{timeunits}{mondate}(x)
\S4method{timeunits}{ANY}(x)
\S4method{timeunits}{mondate}(x)<-value
}
\arguments{
\item{x}{
a \code{mondate}.
}
\item{value}{
For the "get" method, 
a \code{character} string indicating the units with which to measure
time as a result of operations on a \code{mondate}.
Choices are currently
    \enumerate{
    \item "months"
    \item "years"
    \item "days"
    }
If \code{x} is not a \code{mondate}, 
the "get" value returned is \code{NULL}.
}
}
\note{
The \code{mondateTimeunits} versions have been deprecated.
}
\examples{
x <- mondate("2010-6-30") # The middle of 2010
timeunits(x)              # "months"
y <- x + 12               # One year (12 months) later.
timeunits(y)              # "months"
y - x                     # Result is 12 months
timeunits(y) <- "years"
y - x                     # Result is 1 year, with a warning because 
                          # x@timeunits != y@timeunits. Units of result
                          # comes from the first argument.
timeunits(y) <- "days"
suppressWarnings(y - x)   # Result is 365 days -- warning suppressed
}
\keyword{methods}
