\name{Combining-methods}
\docType{methods}
\alias{c-methods}
\alias{c,mondate-method}
\alias{cbind}
\alias{cbind-methods}
\alias{cbind,ANY-method}
\alias{cbind,mondate_possible-method}
\alias{cbind,mondate_possible_w_date-method}
\alias{rbind}
\alias{rbind-methods}
\alias{rbind,ANY-method}
\alias{rbind,mondate_possible-method}
\alias{rbind,mondate_possible_w_date-method}
\alias{rep-methods}
\alias{rep,mondate-method}
\title{Methods for Combining Mondates}
\description{
Methods to combine \code{mondate}s.
}
\details{
The \code{cbind} and \code{rbind} methods 
use the \code{base} \code{cbind} and \code{rbind} functions,
respectively,
to combine the arguments.
If the first argument in \code{\dots} is not a \code{mondate},
that combination is the value returned.
If the first argument in \code{\dots} is a \code{mondate},
the combination is converted to a \code{mondate}
with \code{displayFormat} and \code{timeunits} properties
equal to those of the first argument
(see method \code{mondate} to see how the conversion
takes place depending on \code{timeunits}; an example is below).

In the default case (\code{timeunits}="months") the result
will have the same column names as would the result of 
\code{base::c-rbind}.
When the method must process convert the objects in \code{\dots} to 
\code{mondate}s individually,
the ability to discern the symbol of an unnamed argument is lost
in the process of the S4 method call.
See an example below.
}
\section{Methods}{
\describe{

\item{\code{c(x = "mondate", \dots)}}{
Combine \code{mondate}s into a \code{vector}. 
\code{\dots} any R object(s) that can
be coerced to a \code{mondate}.
The behavior mimics that of the \code{base} function.
The result will be a \code{mondate} with properties equal to those of 
\code{x}.
}

\item{\code{c-rbind(... = "mondate_possible")}}{
Slightly faster version when a "date class" is not in \code{\dots}.
Faster because \code{base::c-rbind} can be called without looping through
the individual objects in \code{\dots} in the default case
(\code{timeunits} = "months").
Currently, "date classes" are
"Date", "POSIXt", "POSIXct", and "POSIXlt".
}

\item{\code{c-rbind(... = "mondate_possible_w_date")}}{
Same as above, but with at least one "date class" in \code{\dots}. 
}

\item{\code{c-rbind(... = "ANY")}}{
Default \code{base::c-rbind} method when neither of the above two cases occur.
Probably will never be called since most objects that are c-r-combine'd
will be a vector or a matrix, and therefore qualify as a "mndate_possible".
}

\item{\code{rep(x = "mondate", \dots)}}{
Replicates a \code{mondate}.
The behavior mimics that of the \code{base} function.
See \code{\link{rep}} for further details.
The result will be a \code{mondate} with properties equal to those of 
\code{x}.
}
}}
\value{
A \code{mondate}. 
For \code{cbind} and \code{rbind}, a \code{matrix}.
For \code{c} and \code{rep}, a \code{vector}.
}
\seealso{
\code{\linkS4class{mondate_possible}} and
\code{\linkS4class{mondate_possible_w_date}}.
}
\examples{
x <- mondate(1:6) # first 6 month-ends of the year 2000
c(x,x+6)          # all month-ends of 2000
c(0,x)            # result is "numeric", determined by the first argument

M<-mondate.ymd(2001:2005,12,31) # 5 year-ends
names(M)<-LETTERS[1:5]
cbind(M)
rbind(M,M)
begin_date <- M-12
cbind(begin_date,end_date=M)  # 5 pairs of year boundary-dates. Columns are
                              # "automatically" named in the default case 
                              # (all mondates with timeunits="months").
cbind(x,as.Date("2010-6-30")) # column names show as ..1 and ..2
cbind(x=x,DateCol=as.Date("2010-6-30")) # columns are named

# Examples of "cbind-ing" mondates with numerics
cbind(begin_date, 1:5)        # second column = Jan - May month-ends of 2000
mondateTimeunits(begin_date)<-"years"
# Intention now is to measure time in years, so
# the second column below will be 2000 - 2004 year-ends
cbind(begin_date, 1:5) 
                              
rep(mondate("2010-2-14"), 3)

(M<-seq(from=mondate("1/1/2010"),length=2)) # Jan. and Feb. 1st
rep(M,3)                                    # three pairs
rep(M,each=3)                               # three Jan.'s, three Feb.'s
}
\keyword{methods}
