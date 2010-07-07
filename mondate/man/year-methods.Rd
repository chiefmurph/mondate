\name{year/month/day-methods}
\docType{methods}
\alias{year-methods}
\alias{year,mondate-method}
\alias{year}
\alias{month-methods}
\alias{month,mondate-method}
\alias{month}
\alias{day-methods}
\alias{day,mondate-method}
\alias{day}
\alias{mondate.mdy}
\alias{mondate.ymd}
\title{Useful Methods for Class mondate}
\description{
The methods in this section facilitate \code{mondate} construction
using the year, month, and day numbers of the date 
(as those numbers would be seen in the character representation of the date,
i.e.., "January" = 1 and the first day of a month = 1).
}
\usage{
mondate.mdy(m, d, y, displayFormat=.default.displayFormat,
            timeunits=.default.timeunits, ...)

mondate.ymd(y, m, d, displayFormat=.default.displayFormat,
            timeunits=.default.timeunits, ...)

}
\arguments{
  \item{m}{
the month: 1, ..., 12
}
  \item{d}{
the day: 1, ..., 31, depending on the month
}
  \item{y}{
the four-digit year
}
  \item{displayFormat}{
the date format with which to display 
the \code{mondate}.
}
  \item{timeunits}{
character string "months" (default), "years", or "days" indicating the units
in which date arithmetic will be carried out.
}
  \item{\dots}{
arguments to be passed to the \code{mondate} constructor method.
}
}
\examples{
M<-mondate.mdy(6,30,2008)
year(M)     # 2008
month(M)    # 6
day(M)      # 30
mondate.ymd(2008,6,30)  # the same
mondate.ymd(2008,6)     # the same; missing day indicates last day of the month
mondate.ymd(2008,1:12)  # all month-ends of 2008; note leapyear
}
\keyword{methods}
