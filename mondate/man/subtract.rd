\name{subtract}
\alias{subtract}
\title{Subtract numerics from mondates using day-of-month logic}
\description{
  Subtract numerics from mondates. 
  When units = "months" and the \code{numeric} is a whole number
  the result has the same 
  day of the month subject to the number of days in the month,
  thus abandoning \code{mondate}'s
  approach of representing days as fractional months.
  See examples.
}
\usage{
subtract(e1, e2, units, forcelastday = FALSE)
}
\arguments{
\item{e1}{a \code{mondate}
}
\item{e2}{a\code{numeric}
}
\item{units}{
  Anything allowed by \code{base:::as.difftime}.
  In addition, can be "months" or "years".
  If missing, defaults to \code{timeunits(e1)}.
}
\item{forcelastday}{
  If FALSE, the result will have the same day of the month subject to
  the numbere of days in the month.
  If TRUE, the day of the month of the result will be the last day of the
  month if \code{e1} is on the last day of the month.
}
}
\value{
A \code{mondate}.
}
\author{
Dan Murphy.
}
\examples{
x <- mondate(0:12)
subtract(x, 1)             # The third date will the the 29th of March
x <- mondate.ymd(2013, 2:12, 15)
subtract(x, 1)             # Always the 15th of the month. Compare to ...
x - 1
stopifnot(subtract(x, 1, units = "months") == mondate.ymd(2013, 1:11, 15))

}
