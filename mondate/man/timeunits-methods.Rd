\name{timeunits-methods}
\docType{methods}
\alias{mondateTimeunits-methods}
\alias{mondateTimeunits,mondate-method}
\alias{mondateTimeunits,ANY-method}
\alias{mondateTimeunits}
\alias{mondateTimeunits<--methods}
\alias{mondateTimeunits<-,mondate-method}
\alias{mondateTimeunits<-}
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
\S4method{mondateTimeunits}{mondate}(x)
\S4method{mondateTimeunits}{ANY}(x)
\S4method{mondateTimeunits}{mondate}(x)<-value
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
The \code{mondateTimeunits} versions are being deprecated.
}
\examples{
x<-mondate("2010-6-30") # The middle of 2010
mondateTimeunits(x)     # "months"
y<-x+12                 # One year (12 months) later.
mondateTimeunits(y)     # "months"
y-x                     # Result is 12 months
mondateTimeunits(y) <- "years"
y-x                     # Result is 1 year, with a warning because 
                        # x@timeunits != y@timeunits. Units of result
                        # comes from the first argument.
mondateTimeunits(y) <- "days"
suppressWarnings(y-x)   # Result is 365 days -- warning suppressed
}
\keyword{methods}
