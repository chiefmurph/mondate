\name{mondate-methods}
\docType{methods}
\alias{mondate}
\alias{mondate-methods}
\alias{mondate,ANY-method}
\alias{mondate,array-method}
\alias{mondate,character-method}
\alias{mondate,Date-method}
\alias{mondate,mondate-method}
\alias{mondate,numeric-method}
\alias{mondate,POSIXt-method}
\title{Create an instance of the mondate class}
\description{
All purpose constructor for an instance of the \code{mondate} class.
}
\usage{
mondate(x, displayFormat=.default.displayFormat, 
           timeunits=.default.timeunits, ...)
}
\arguments{
  \item{x}{
a R object to convert to a \code{mondate}. Can be another \code{mondate},
a character representing a date, a date, a numeric, or an object which
converts to a numeric with as.numeric(x).
More details below.
}
  \item{displayFormat}{
character string representing the date format with which to display 
the \code{mondate}. 
The default.displayFormat is determined according to Sys.getlocale("LC_TIME"):
if it contains the words "United States", the default will be 
"\%m/\%d/\%Y" (MM/DD/YYYY), otherwise "\%Y-\%m-\%d" (YYYY-MM-DD). 
Other choices are "\%m-\%d-\%Y" and "\%Y/\%m/\%d".
}
  \item{timeunits}{
character string "months" (default), "years", or "days" indicating the units
in which date arithmetic will be carried out.
}
  \item{\dots}{
arguments to be passed to other methods.
}
}
\section{Methods}{
\describe{

\item{\code{signature(x = "numeric")}}{
For \code{numeric} x, the simplest case is when \code{timeunits} = "months", 
in which case
the value of x and properties \code{displayFormat} and \code{timeunits} 
are simply stored. 
If \code{timeunits} = "years" then it 
is presumed that the value of x represents the number of years since the
beginning of the millennium, 
in which case the value of x is multiplied by 12
and then stored. 
If \code{timeunits} = "days" then it is presumed 
that the value of x represents the number of days since the beginning
of the millennium, 
in which case the value is calculated using \code{as.Date}.
}

\item{\code{signature(x = "Date")}}{
}
\item{\code{signature(x = "POSIXt")}}{
For a date x, \code{as.POSIXlt} is used to convert to an ISO standard date,
from whence the number of months of that day since the beginning of the
millennium is calculated. 
}

\item{\code{signature(x = "character")}}{
For character x, if \code{displayFormat} is provided as an argument, 
that format is used with
\code{as.Date} to convert to a \code{mondate}.
If \code{displayFormat} is not provided, 
an attempt is made to convert to a date
using the default.displayFormat. If unsuccessful, then each of the four 
optional \code{displayFormat}s (see \code{displayFormat} above) 
is attempted for conversion to a date, 
and the successful format is retained as that \code{mondate}'s 
\code{displayFormat}.
If none of the possible \code{displayFormat}s is successful 
in converting to a date, 
then as a last resort
the character string is attempted to be converted to a 
\code{numeric} and then
to a \code{mondate}.
}

\item{\code{signature(x = "mondate")}}{
For \code{mondate} x, this would be the way to change
the \code{mondate}'s \code{displayFormat} or \code{timeunits}.
}

\item{\code{signature(x = "array")}}{
Used to preserve array attributes, after which other signatures take over.
}

\item{\code{signature(x = "ANY")}}{
For any other class of x an attempt will be made to convert
to \code{Date} ("\code{as.Date}"). 
If unsuccessful, an attempt will be made to convert to \code{numeric};
if successful, a warning will be issued to check the results
relative to the \code{numeric} conversion, otherwise execution will
be \code{stop}ped.
}

}}
\seealso{
\code{\link{POSIXt}}, \code{\link[zoo]{yearmon}}, \code{\link[zoo]{yearqtr}}
}
\examples{
mondate("1-31-2010") # Jan. 31, 2010
mondate(60)          # 60 months after 12/31/1999, so Dec. 31, 2004
dat<-as.Date("2010-1-31")
(M<-mondate(dat))    # Jan. 31, 2010
x <- 12*1:6
y<-x+12
mondate(cbind(x,y))  # bounding dates of first 6 years of millennium
mondate(x)           # first 6 yearends
(y<-mondate(1:6,timeunits="years")) # the same, but will show the results
                     # of date arithmetic in "years" rather than "months"
as.numeric(x)
as.numeric(y)        # the underlying numeric representations are the same

x<-ts(1:10, frequency = 4, start = c(1959, 2)) # start=second quarter 1959
mondate(x)           # thinks start=January 31, 2000; see warning
library(zoo)
mondate(x)           # zoo has an as.Date method for a time series, 
                     # so start now shows as April 1, 1959.
# This points out a difference between zoo and mondate:
#   zoo assumes that the zeroth part of a month or quarter is the first
#   day of the month or quarter, whereas mondate assumes that it is 
#   the instant before the first day of the month or quarter.
# Since frac=0 is zoo's as.Date coersion default, a month or quarter in 
#   zoo's sense converts to the end of the first day rather than
#   the beginning.
mondate(as.yearmon("2010-6-15")) # end of the first day of the month
mondate(as.yearmon("2010-6-15"), frac=1) # end of the last day
mondate(as.yearqtr("2010-2"), frac=1)    # same
}

\keyword{methods}
