\name{Arith-methods}
\docType{methods}
\alias{Arith-methods}
\alias{Arith,mondate,mondate-method}
\alias{Arith,mondate,numeric-method}
\alias{Arith,numeric,mondate-method}
\alias{Arith,mondate,array-method}
\alias{Arith,array,mondate-method}
\alias{MonthsBetween}
\alias{MonthsBetween-methods}
\alias{MonthsBetween,mondate,mondate-method}
\alias{YearsBetween}
\alias{YearsBetween-methods}
\alias{YearsBetween,mondate,mondate-method}
\alias{DaysBetween}
\alias{DaysBetween-methods}
\alias{DaysBetween,mondate,mondate-method}
\alias{+-methods}
\alias{+,mondate,difftime-method}
\alias{--methods}
\alias{-,mondate,difftime-method}
\alias{-,mondate,mondate-method}
\title{Methods for Function Group Arith on mondate Objects}
\description{
Arithmetic methods for class \code{mondate}.
Includes three special "units between" methods.
}
\section{Methods}{
\describe{

\item{\code{signature(e1 = "mondate", e2 = "mondate")}}{
Performs arithmetic operations on two \code{mondate}s. 
The result will be \code{numeric} with attribute \code{timeunits}
belonging to the slot of the first argument 
(with a warning if the two arguments have different \code{timeunits}).
The only operation that would seem useful for two 
\code{mondate}s is subtraction.
}

\item{\code{signature(e1 = "mondate", e2 = "numeric")}}{
}
\item{\code{signature(e1 = "numeric", e2 = "mondate")}}{
}
\item{\code{signature(e1 = "mondate", e2 = "array")}}{
}
\item{\code{signature(e1 = "array", e2 = "mondate")}}{
Performs arithmetic operations of a \code{numeric} on a 
\code{mondate} where the units
of the \code{numeric} is taken from the 
\code{timeunits} slot of the \code{mondate}.
The result will be a \code{mondate} with the same properties 
as the \code{mondate}
in the function call.

The only operations that would seem useful for a \code{mondate} and a numeric
are addition and subtraction.

Most of the time it is expected that \code{timeunits}="months" so that, 
for example,
adding/subtracting a number to/from that \code{mondate} adds/subtracts that
number of months.
If the \code{mondate}'s \code{timeunits}="years" 
then the perceived intention is to
add/subtract that number of years. 
To accomplish that, the \code{mondate}'s numeric 
value is divided by 12, the operation is performed, and the result converted
to a \code{mondate}.
If the \code{mondate}'s \code{timeunits}="days", 
then the \code{mondate}'s value is converted to the number
of days since the start of the millennium, 
the operation is performed
(e.g., the \code{numeric} number of days is added or subtracted), 
and the result converted to a \code{mondate}. 
(See the \code{convert} option of the \code{as.numeric("mondate")} function.)
}

\item{\code{+signature(e1 = "mondate", e2 = "difftime")}}{
  }
\item{\code{-signature(e1 = "mondate", e2 = "difftime")}}{
  Use a difftime object to add and subtract
  secs, days, weeks, months, and years
  to or from a \code{mondate}.
  }

\item{\code{-signature(e1 = "mondate", e2 = "mondate")}}{
  Returns a difftime object equal to the signed number of units between
  \code{e1} and \code{e2}, 
  where "units" = \code{timeunits(e1)}, with a warning
  if \code{e1} and \code{e2} have differing \code{timeunits}.
  }

\item{\code{MonthsBetween(from = "mondate", to = "mondate")}}{
Same as \code{abs(from - to)} in months.
}

\item{\code{YearsBetween(from = "mondate", to = "mondate")}}{
Same as \code{abs(from - to)} in years, 
which is also the number of months between divided by 12.
}

\item{\code{DaysBetween(from = "mondate", to = "mondate")}}{
Same as \code{abs(from - to)} in days,
which is also the difference between the \code{as.Date}
representation of \code{from} and \code{to}.
}

}}
\seealso{
\code{\link{Arith}}
}
\examples{
M <- mondate("1-1-2010") # will display in U.S. date format
M - 1:12 # the 1st of the month for all months in 2009
       # in reverse chronological order; inherits M's displayFormat

x <- mondate(matrix(12 * 1:4, 2, 2)) # 2x2 matrix of 2000-2003 year ends
x                                
y <- x + 12                          # one year later, also a matrix
y
y - x                      # 2x2 matrix of 12s, with an attribute ("months")
MonthsBetween(x, y)        # same, without the attribute
YearsBetween(x, y)
DaysBetween(x, y)

## Use difftime object to add, subtract secs, days, weeks, months, years
x <- mondate(1)            # January 31, 2000
y <- as.difftime(1, units = "days")
x + y
x - y
yw <- as.difftime(1, units = "weeks")
x + yw
x - yw
x + as.difftime(1, , "days")
x + as.difftime(86400, , "secs")

x <- mondate.ymd(2012, 2, 29)  # leap day
x + as.difftime(1, , "years")  # last day of February 2013, not a leap day
x - as.difftime(1, , "months") # not the last day of January

}
\keyword{methods}
\keyword{arith}
