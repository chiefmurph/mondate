\name{seq.mondate}
\alias{seq}
\alias{seq.mondate}
\title{
Mondate Sequence Generation
}
\description{
Generate regular \code{mondate} sequences. 
}
\usage{
\method{seq}{mondate}(from, to, by, \dots, right = TRUE)
}
\arguments{
\item{from}{
coercible to a \code{mondate}. May be "missing".
}
\item{to}{
coercible to a \code{mondate}. May be "missing".
}
\item{by}{
  increment of the sequence. 
  If numeric, a sequence is generated using the underlying numeric value(s)
  of the coerced argument(s) and converted to a mondate object 
  (see \code{\link{seq}}).
  If "days" or "weeks", a sequence is generated using "Date" objects
  coerced from the mondate-coerced arguments
  (see \code{\link{seq.Date}}).
  If "months", "years", or "quarters", \code{by} is converted to 1, 12, or 3, 
  respectively, and the numeric case continues.
  Furthermore, the characters can optionally be 
  preceded by a (positive or negative) integer and a space,
  and the singular form may also be used.
  }
\item{\dots}{
optional arguments passed to \code{\link{seq}} or,
if by "day" or "week",
\code{\link{seq.Date}}
}
\item{right}{
  in the case when \code{by} specifies units of multiples of months 
  ("months", "years", or "quarters") should increments be considered as
  attaching to the end of the day (the \code{mondate} default).
  If \code{FALSE}, the increment attaches to the beginning of the day 
  (see "Examples")
  }
}
\details{
For more details about sequence generation, see \code{\link{seq}}.

If \code{from} and \code{to} are both provided,
the \code{displayFormat} and \code{timeunits} properties are
taken from \code{from}, 
without a warning if \code{from}'s properties differ from 
\code{to}'s.
}
\value{
A \code{mondate} vector with slots 
(including \code{displayFormat}, \code{timeunits}, \code{formatFUN})
from argument \code{from}, if provided, otherwise from argument \code{to}.
}
\author{
Dan Murphy
}
\seealso{
\code{\link{seq}}, \code{\link{seq.Date}} and \code{\link{seqmondate}}
}
\examples{
x <- mondate.ymd(2010, 1)   # January 31, 2014
y <- mondate.ymd(2010, 12)  # December 31, 2014
seq(from = x, to = y, by = 1)  # all month-ends in 2014
# 8 quarter-ends beginning 1st quarter 2009; US displayFormat
seq(mondate("3/31/2009"), by = 3, length.out = 8) 
# 8 quarter-ends ending year-end 2009; R's date format
seq(to = mondate("2009-12-31"), by = 3, length.out = 8)
#
# Use of RIGHT = FALSE in seq.mondate is depracated. 
# Instead, to generate sequences of 
#   Dates corresponding to the first days of the months
# use the seq.Date method.
seqmondate(as.Date("2014-01-01"), by = "months", length = 12)
# To generate the last day of a sequence of months, 
#   use the default right = TRUE:
(m <- seq.mondate(as.Date("2014-01-31"), by = "months", length = 12))
# Coerce back to Date if necessary:
as.Date(m) 
# Note how the Date method yields a different sequence, not corresponding to
#   the last day of the month. 
seq(as.Date("2014-01-31"), by = "months", length = 12)

}