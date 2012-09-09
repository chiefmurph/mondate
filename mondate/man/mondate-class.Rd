\name{mondate-class}
\Rdversion{1.1}
\docType{class}
\alias{mondate-class}

\title{Class "mondate"}
\description{
A \code{mondate} represents a date as a \code{numeric} equalling
the number of months since the beginning of
the current millennium (the "mondate epoch"). 
Somewhat arbitrarily, 
and at the risk of reopening a decade-old debate, 
"the beginning of the current millennium"
is defined as the instant between December 31, 1999
and January 1, 2000.

The need for a "mondate" class arises in the area of actuarial analysis,
and other areas of financial modeling that need to reconcile
to a company's book of accounts.
Its motivation is based on the following presumptions:
\enumerate{
    \item Business accounting-wise, the closing of the books for a month, 
    quarter, and year are the important milestones for measuring time.
    \item For accountants -- and actuaries -- it is usually not important
    to measure events on an hourly basis.
    \item All events that occur during a business day, up to and including 
    the closing of the books for a day, are all "accounted for" as
    having occurred "at the same time."
    }

To appreciate the difficulty in measuring the passage of time in days,
note that there are typically three fewer days in the first half of the year
(January 1 through June 30) than there are in the second half.
Yet accountants will say that on June 30th the year is half over.
For another example,  
note that -- 
with the exception of July/August and December/January -- 
the same days of the month for two consecutive months are not 
one "month" apart if measured in days because, with those exceptions,
consecutive months contain differing numbers of days,
so which of the two months do you choose as the yardstick?
Since changes in accounts over the course of a month, quarter and year
are the amounts by which financial results are measured,
it is important to be able to measure the passage of time 
where a year is comprised of twelve months of equal "accounting weight."

That gives rise to a date as measured in months, coined "mondate".

A \code{mondate} is simply a real number whose fractional part represents 
the fraction of the month as of the end of the day. 
E.g., the fractional part of January 1st = 1/31; the fractional part of 
February 1st = 1/28 or 1/29, depending on the year.
A \code{mondate} which is a whole number 
(i.e., no fractional part) corresponds to a month
that is fully completed, whose subsequent month has not yet begun;
i.e., the instant in time between
one month and the next. 

The length of time (in months) between \code{mondate x} and 
\code{mondate y} is simply \code{y-x}.
An interval of time is represented in a mathematical sense
by the half-open/half-closed 
interval \strong{(x,y]}. 
For example, calendar year 2009 is
the interval
\tabular{c}{ 
\strong{(2008-12-31, 2009-12-31]}
}
i.e., all
events \emph{after} the close of business \strong{2008-12-31}
and \emph{through and including} the close
of business \strong{2009-12-31}.
The mondate vector
\tabular{c}{ 
c(mondate("2008-12-31"), mondate("2009-12-31"))
}
could be used to represent such an interval.

A \code{mondate} allows "infinite dates",
which are helpful constructs for modeling "ultimate" events
in actuarial, longitudinal, and time series analyses
(see the \code{mondate} method for signature "numeric").

A \code{mondate} has two important properties (in S4 parlance, "slots").
The first is \code{displayFormat} which is the format 
of the date when the \code{mondate} is printed or \code{show}n.
The other is \code{timeunits} which is the "units" reported 
when date arithmetic is performed.
The default units is "months", but "years" and "days" are also allowed, 
in which case the difference of two dates, say, would be reported
in "years" or "days", respectively.

}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{mondate(x, ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"numeric"} or a numeric array }
    \item{\code{displayFormat}:}{Object of class \code{"character"}.
        The is the format of the date when displayed. 
        Currently, there are four choices:
        \enumerate{
            \item "%m/%d/%Y"
            \item "%m-%d-%Y"
            \item "%Y-%m-%d"
            \item "%Y/%m/%d"
            }
        Currently, the default displayFormat for a "United States" locale is
        mm/dd/YYYY ("%m/%d/%Y"), 
        in all other locales YYYY-mm-dd ("%Y-%m-%d").
        The default can be changed to another format
        using the \code{options(mondate.displayFormat = myFormat)} command.
        See Examples.
        }
    \item{\code{timeunits}:}{Object of class \code{"character"}
        There are three options:
        \describe{
            \item{"months":}{the default}
            \item{"years":}{although it is assumed that "month" 
                is the fundamental unit of time, 
                sometimes it is more convenient to report the result of 
                date arithmetic in terms of "years", 
                for example, in actuarial analyses when 
                events are measured over the course of years, not months. 
                Of course, one "year" = twelve "months".
                }
            \item{"days":}{mostly for convenience when the number of days
                between events needs to be reported.
                Of course, unlike with "years", there is no simple relationship
                between "days" and "months" -- it depends on the month and year of
                the date.
                If the \emph{fundamental} unit of time for a particular problem 
                is "days", not "months", then a different date class
                (e.g., class \code{Date}) might be a better tool for modeling
                the passage of time for that problem.
                }
            }
        The default can be changed to a different unit
        using the \code{options(mondate.timeunits = myUnits)} command.
        See Examples.
        }
    \item{\code{.S3Class}:}{Object of class \code{"logical"}}
    Currently \code{TRUE} so that S3 method-based functionality that 
    could not be duplicated in an S4 method would still be available.
  }
}
\section{Extends}{
Class \code{c("numeric","array")}, from data part.
}
\references{
For information about how month-based time measurement can help with
standardizing rates of return, see Damien Laker, 
"Time Calculations for Annualizing Returns: 
the Need for Standardization,"
\emph{The Journal of Performance Measurement}, Summer 2008, pp. 45-54.
}
\author{
Dan Murphy
}
\seealso{
\code{\link[zoo]{yearmon}} in the zoo package.
}
\examples{
# See the \code{mondate-methods} section for an
# explanation of the \code{mondate} method below.

# January 1, 2010 in US displayFormat
mondate("1-1-2010") 

# generate 10 random dates in calendar year 2000; will be
# displayed in local format
mondate(runif(10,0,12)) 

# Change the default format so that the character representation of the date
#   sorts in chronological order.
options(mondate.displayFormat = "\%Y-\%m-\%d")
# January
mondate(runif(10,0,12)) 

# generate 10 random dates in calendar year 2010;
# date arithmetic results will be reported in units of "years"
mondate(10+runif(10),timeunits="years") 
}
\keyword{classes}
