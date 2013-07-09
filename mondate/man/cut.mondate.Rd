\name{cut.mondate}
\docType{methods}
\alias{cut}
\alias{cut.mondate}
\title{Convert a mondate Object to a Factor}
\description{
Method for \code{cut} applied to mondate objects.
}
\usage{
 \S3method{cut}{mondate}(x, breaks, labels = NULL, 
      include.lowest = TRUE, right = TRUE, 
      start.on.monday = TRUE, attr.breaks = FALSE, \dots)
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
}
\item{right}{
logical, indicating if the intervals should be closed on the right 
(and open on the left) or vice versa.
}
\item{start.on.monday}{
logical. If \code{breaks = "weeks"}, should the week start on Mondays or Sundays?
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
Convert a \code{mondate} to a factor where,
in the "end-of-business-day" spirit of \code{mondate},
the levels are labeled by the last day in the period.
If \code{right = FALSE} the levels are labeled by the first day in the period.

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
\code{\link{cut}} and \code{\link{cut.Date}}
}
\examples{
x <- mondate(0:12)
cut(x, breaks = 4)
cut(x, mondate.ymd(1999) + c(0, 3, 6, 9, 12)) # same as above

# days
x <- mondate.ymd(2013, 1, 1:7)
cut(x, breaks = "days")

# weeks
x <- mondate.ymd(2013, 1, 1:31)  # days in January 2013
cut(x, breaks = "weeks") # labeled by the first 5 Sundays of 2013
cut(x, breaks = "2 weeks") # labeled by 2nd, 4th, and 6th Sundays of 2013

# months
x <- mondate.ymd(2013, 1:12, 15) # 15th day of each month in 2013
cut(x, breaks = "months") # labeled by last day of the month
cut(x, breaks = "months", right = FALSE) # labeled by first day of the month
cut(x, breaks = "2 months") # labeled by last day of the month
x1 <- mondate.ymd(2008, 6)
x2 <- mondate.ymd(2008:2011, 6)
cx1 <- cut(x1, "month", right = FALSE)
cx2 <- cut(x2, "month", right = FALSE)
stopifnot(levels(cx1)[cx1] == levels(cx2)[cx2[1]])

# quarters
x <- mondate.ymd(2013, 1:12, 15) # 15th day of each month in 2013
cut(x, "quarters")       # labeled by last day of the quarter
cut(x, "quarters", right = FALSE) # labeled by first day of the quarter
cut(x, "2 quarters")
cut(x, "2 quarters", right = FALSE) # labeled by first day of the quarter

# years
m <- mondate(0:12)
cut(m, "years")           # labeled by last day of the year
cut(m, "years", right = FALSE) # labeled by last day of the year
displayFormat(m) <- "\%Y"
cut(m, "years")           # labeled by just the year
x <- mondate.ymd(2008:2013, 6)
cut(x, "years")                  # 6 levels labeled by last day of the year
cut(x, "years", right = FALSE)   # 6 levels labeled by first day of the year
cut(x, "3 years")                # 2 levels
cut(x, "3 years", right = FALSE) # 2 levels
x1 <- mondate.ymd(2008, 12)
x2 <- mondate.ymd(2008:2011, 12)
cx1 <- cut(x1, "year", right = FALSE)
cx2 <- cut(x2, "year", right = FALSE)
stopifnot(levels(cx1)[cx1] == levels(cx2)[cx2[1]])
}
\keyword{methods}
