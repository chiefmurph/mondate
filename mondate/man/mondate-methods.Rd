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
\alias{mondate,matrix-method}
\alias{mondate,POSIXt-method}
\alias{mondate,missing-method}
\title{Create an instance of the mondate class}
\description{
All purpose \bold{mondate} constructor / coercer.
}
\usage{
    mondate(x, 
        displayFormat = getOption("mondate.displayFormat", default = .default.displayFormat), 
        timeunits = getOption("mondate.timeunits", default = .default.timeunits),
        ...)
    \S4method{mondate}{mondate}(x, displayFormat, timeunits, ...)
    \S4method{mondate}{numeric}(x, displayFormat, timeunits, ...)
    \S4method{mondate}{Date}(x, displayFormat, timeunits, ...)
    \S4method{mondate}{POSIXt}(x, displayFormat, timeunits, ...)
    \S4method{mondate}{character}(x, displayFormat, timeunits, format = "", ...)
    \S4method{mondate}{array}(x, displayFormat, timeunits, ...)
    \S4method{mondate}{missing}(x, displayFormat, timeunits, ...)
    \S4method{mondate}{ANY}(x, displayFormat, timeunits, ...)
    }
\arguments{
    \item{x}{
        an \R object to convert to a \code{mondate}. 
        Can be another \code{mondate},
        a character representing a date, a date, a numeric, or an object which
        converts to a numeric with as.numeric(x).
        More details below.
        }
    \item{displayFormat}{
        character string representing the date format with which to display 
        the \code{mondate}. 
        The default.displayFormat is determined according to Sys.getlocale("LC_TIME"):
        if it contains the words "United States", the default will be 
        "\%m/\%d/\%Y" 
        \cr
        (MM/DD/YYYY), otherwise "\%Y-\%m-\%d" (YYYY-MM-DD). 
        Other choices are "\%m-\%d-\%Y" and "\%Y/\%m/\%d".
        }
    \item{timeunits}{
        character string "months" (default), "years", or "days" indicating the units
        in which date arithmetic will be carried out.
        }
    \item{format}{
        format string for converting a character to a Date 
        (using \code{as.Date, x, format, ...})
        from which the mondate value is determined.
        }
    \item{\dots}{
    arguments to be passed to other methods.
    }
    }
\section{Methods}{
\describe{

\item{\code{signature(x = "mondate")}}{
For \code{mondate} x, this could be a way to copy a \code{mondate}
and perhaps change 
the \code{mondate}'s \code{displayFormat} or \code{timeunits} slots
in the process.
For any class that extends \code{mondate},
use of this method will return the underlying mondate class without
additional slots (if any) of the subclass.
}

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

Note that infinite values of \code{x} are allowed 
("infinite" dates are helpful in modeling events "at ultimate"
in actuarial, longitudinal, and time series analyses).
}

\item{\code{signature(x = "Date")}}{
}
\item{\code{signature(x = "POSIXt")}}{
For a date x, \code{as.POSIXlt} is used to convert to an ISO standard date,
from which the number of months of that day since the beginning of the
millennium is calculated. 
}

\item{\code{signature(x = "character")}}{
If \code{format} is provided, then that format
is used to attempt to convert the character value to a date.
Otherwise, 
characters are converted to dates using the first format found in the 
set of valid formats. 
The current set of valid formts is
"\%m/\%d/\%Y", "\%m-\%d-\%Y", "\%Y-\%m-\%d", and "\%Y/\%m/\%d".
If none of the valid formats successfully converts \code{x} to a date, 
then as a last resort
the character string is attempted to be coerced to a 
\code{numeric} and then
to a \code{mondate}.
If a format is found that converts x to a date, 
then the successful format can be retained as the \code{displayFormat}
of the \code{mondate} by specifying 
\code{displayFormat = "keep"}.
}

\item{\code{signature(x = "array")}}{
If an object \code{x} is an \code{array}, 
then this method enables the \code{mondate} to inherit its shape. 
After that, other "signatures" take over.
}

\item{\code{signature(x = "missing")}}{
Enables the call \code{mondate()} to work.
Useful for prototypes, e.g.
Body of method is simply \code{new("mondate")}.
}

\item{\code{signature(x = "ANY")}}{
For any other class of x an attempt will be made to convert
to \code{Date} ("\code{as.Date(x)}"). 
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
dat <- as.Date("2010-1-31")
(M <- mondate(dat))    # Jan. 31, 2010
x <- 12 * 1:6
mondate(x)           # first 6 yearends in 2000's
y <- x + 12
mondate(cbind(x,y))  # bounding dates of first 6 years of millennium
(y <- mondate(1:6,timeunits="years")) # first 6 yearends, 'years' timeunits 
# The results of date arithmetic on y will be displayed in "years".
# E.g., the differences of y can be calculated as:
tail(y,-1) - head(y,-1)# vector of five 1's, with "timeunits" attribute = "years"
as.numeric(x)
as.numeric(y)        # the underlying numeric representations are the same

# Demonstrating "infinite" dates
y <- c(y,Inf)
y                    # last element shows as Inf
tail(y,-1) - head(y,-1)# last element is now infinity

# The zoo examples point out a difference between zoo and mondate.
#   zoo assumes that the zero-th part of a month or quarter is the first
#   day of the month or quarter, whereas mondate assumes that it is 
#   the instant before the first day of the month or quarter.
# Since frac=0 is zoo's as.Date coersion default, a month or quarter in 
#   zoo's sense converts to the end of the first day rather than
#   the beginning.
library(zoo)
x <- ts(1:10, frequency = 4, start = c(1959, 2)) # starting 2nd qtr of 1959
x
# There is no method for class 'ts' so x is coerced (successfully) 
#   because that class has an as.Date method, but with a warning.
# The result is a vector of length 10 representing the close of business
#   at the end of the first day of each of the given quarters.
mondate(x)      

# The yearmon class will identify any day in June 2010 with that month.
as.yearmon("2010-6-15") 
mondate(as.yearmon("2010-6-15"))         # end of first day of June 2010
mondate(as.yearmon("2010-6-15"), frac=1) # end of  last day of June 2010
mondate(as.yearqtr("2010-2"), frac=1)    # same

# The if missing, displayFormat will be determined from the character input
x <- mondate("2010-12-31")
x               # x displays in the input European format
# The provided, displayFormat must match the format of the character input
#   or NA's will result.
mondate("2010-12-31", displayFormat = "\%m-\%d-\%Y") # results in NA

# Always display x using just the year
x <- mondate(as.Date("2012-3-1"), displayFormat="\%Y")
x                # shows as the year 2012, but month and day are nevertheless retained
month(x)         # 3
day(x)           # 1

}

\keyword{methods}
