\name{cut.mondate}
\alias{cut}
\alias{cut.mondate}
\title{Convert a mondate Object to a Factor}
\description{
Method for \code{cut} applied to mondate objects.
}
\usage{
 \method{cut}{mondate}(x, breaks, labels = NULL, 
      include.lowest = FALSE, right = TRUE, 
      start.on.monday = TRUE, startmonth = NULL, startyear = NULL,
      attr.breaks = FALSE, \dots)
}
\arguments{
\item{x}{
a \code{mondate}
}
\item{breaks}{
a vector of cut points or number giving the number of intervals which x 
is to be cut into or an interval specification, 
one of "day", "week", "month", "quarter" or "year", 
optionally preceded by an integer and a space, 
or followed by "s" (pluralized).
}
\item{labels}{
labels for the levels of the resulting category. 
By default, labels are constructed from the right-hand end of the intervals 
(which are included for the default value of right). 
If \code{labels = FALSE}, simple integer codes are returned instead of a factor. 
}
\item{include.lowest}{
logical, indicating if an 'x[i]' equal to the lowest 
(or highest, for \code{right = FALSE}) 'breaks' value should be included.
(Note: Beginning with version 0.11 of mondate,
\code{!include.lowest} is invalid when \code{breaks} is \code{character}.)
}
\item{right}{
logical, indicating if the intervals should be closed on the right 
(and open on the left) or vice versa.
}
\item{start.on.monday}{
logical. If \code{breaks = "weeks"}, should the week start on Mondays or Sundays?
}
\item{startmonth}{
If not \code{NULL}, the number of the month 
(1 = January, ..., 12 = December)
corresponding to the beginning of the year/fiscal-year.
If \code{NULL}, the resulting intervals begin or end depending on
the minimum
(\code{!right}) or maximum
(\code{right}) value, respectively, of \code{x}.
This argument only has an impact when \code{character} \code{breaks}
defines multi-month intervals.
}
\item{startyear}{
If not \code{NULL}, the year 
corresponding to the left-most level.
If \code{NULL}, the resulting intervals begin or end depending on
the minimum
(\code{!right}) or maximum
(\code{right}) value, respectively, of \code{x}.
This argument only has an impact when \code{character} \code{breaks}
defines month or multi-month intervals.
}
\item{attr.breaks}{
logical. If \code{TRUE} the result has a "breaks" attribute which is
a \code{mondate} whose pairwise values determine a covering of \code{x}.
Most helpful when \code{breaks} = 
"days", "weeks", "months", "quarters", or "years".
}
\item{\dots}{
optional arguments passed to or from other methods.
}
}
\details{
This method converts a \code{mondate} to a factor 
corresponding to time intervals as determined by 
the argument \code{breaks}.
In the "end-of-business-day" spirit of the mondate package,
the intervals (levels) are labeled by the last day in the interval.
If \code{right = FALSE} the levels are labeled by the 
left endpoint of the interval, 
which is \bold{the day immediately preceding the beginning of the period}.
For methods that work in the spirit of Date and POSIXt --
i.e., a time interval is better labeled by the first day of the period --
see \code{\link{cutmondate}}.

For \code{numeric} breaks -- which case also includes \code{mondate}s -- 
the method calls \code{cut.default} and the intervals encoded in the levels 
are converted to date format using \code{mondate} logic.
In the spirit of \code{mondate}, the default 
\code{right = TRUE} ensures that the intervals are left-open/right-closed.
The default \code{include.lowest = TRUE}
ensures that \code{min(x)} is included in the first interval.

For breaks = "days" or "weeks", the method calls cut(as.Date(x)).
For breaks = "months", "quarters", and "years", 
numeric breaks are determined from the range of x.

If \code{breaks} is preceded by an integer, call it \code{step},
then the period of the first level is determined by \code{min(x)}
and subsequent "day", "week", "month", "quarter", and "year" periods
are determined sequentially per
seq(min(x), max(x), by = step).

When \code{attr.breaks = TRUE}, the result will have a "breaks" attribute
(attr(., "breaks")) which pairwise "cover" \code{x}.
Such "breaks" are suitable be use by 'hist', for example.

In all cases, the formats of the dates representing the levels are 
inherited from \code{displayFormat(x)}.
Accordingly, if such resulting potential levels would be non-unique, 
\code{cut.mondate} resorts to the fall-back scheme of \code{cut.default},
i.e., "labels such as "Range3" will be used."
}
\value{
A factor is returned, unless \code{labels = FALSE} which returns the integer level codes.
}
\author{
Dan Murphy. Many thanks to the R-development team for cut and cut.Date.
}
\seealso{
\code{\link{cut}}, \code{\link{cut.Date}}, and
\code{\link{cutmondate}} for methods that work better with Dates.
}
\examples{
# days
x <- mondate.ymd(2013, 1, 1:7)
cut(x, breaks = "days", include.lowest = TRUE)
cut(x, breaks = "days", include.lowest = TRUE)

# weeks
x <- mondate.ymd(2013, 1, 1:31)  # days in January 2013
# labeled by the first 5 Sundays of 2013
cut(x, breaks = "weeks", include.lowest = TRUE, right = TRUE) 

x <- mondate.ymd(2015, 1, 5:25)  # days in January 2013
# labeled by the 1st and 3rd Mondays of 2015
cut(x, breaks = "2 weeks", include.lowest = TRUE, right = FALSE) 

# months
x <- mondate.ymd(2013, 1:12, 15) # 15th day of each month in 2013
# labeled by the left endpoint of the interval = last day of the month
cut(x, breaks = "months", include.lowest = TRUE)
# labeled by the right endpoint of the interval = last day of the prior month
cut(x, breaks = "months", right = FALSE, include.lowest = TRUE)
cut(x, breaks = "2 months", include.lowest = TRUE) 

# quarters
x <- mondate.ymd(2013, 1:12, 15)          # 15th day of each month in 2013
cut(x, "quarters", include.lowest = TRUE) # labeled by last day of the quarter
cut(x, "2 quarters", include.lowest = TRUE)

# years
m <- mondate(0:12)
cut(m, "years", include.lowest = TRUE)  # labeled by last day of the year 2000
cut(m, "years", right = FALSE, include.lowest = TRUE) # labeled by first day of 2000
displayFormat(m) <- "\%Y"
cut(m, "years", include.lowest = TRUE)  # labeled by just the year of the date

# numeric scalar breaks

# Four intervals starting with 2015-01-15 (exclusive) and ending with
# 2015-12-15 (inclusive)
# First factor value is NA because include.lowest is FALSE (default)
x <- mondate.ymd(2015, 1:12, 15)
cut(x, breaks = 4)
# Now 2015-01-15 will be included in the first left-closed interval.
cut(x, breaks = 4, include.lowest = TRUE)

# specific break boundaries -- month-ends
cut(x, breaks = mondate("2014-12-31") + 0:12) # levels show interval boundaries
# Compare with next, where levels are labeled by interval endpoints
cut(x, breaks = "month", include.lowest = TRUE) 

}
\keyword{methods}
