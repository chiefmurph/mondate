\name{Combining-methods}
\docType{methods}
\alias{c-methods}
\alias{c,mondate-method}
\alias{cbindmondate}
\alias{rbindmondate}
\alias{rep.mondate}
\title{Methods for Combining Mondates}
\description{
Methods to combine \code{mondate}s.
}
\details{
The package calls \code{setGeneric("c-rbind")}.
}

\usage{
 cbindmondate(\dots, deparse.level = 1)
 rbindmondate(\dots, deparse.level = 1)
 \S3method{rep}{mondate}(x, \dots)
 \S4method{c}{mondate}(x, ..., recursive = FALSE)
}

\arguments{
    \item{x}{a mondate}
    \item{deparse.level}{see \code{base::cbind}}
    \item{recursive}{see \code{base::c}}
    \item{\dots}{arguments passed to and from other methods}
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

\item{\code{rep(x = "mondate", \dots)}}{
Replicates a \code{mondate}.
The behavior mimics that of the \code{base} function.
See \code{\link{rep}} for further details.
The result will be a \code{mondate} with properties equal to those of 
\code{x}.
}
}}

\value{
\item{\code{c-rbindmondate(...)}}{
The \code{cbindmondate} and \code{rbindmondate} functions
are similar to the \code{base} \code{cbind} and \code{rbind} functions,
respectively,
to combine the arguments.
If all arguments are \code{mondate}s
then the result is converted to a \code{mondate}
with \code{displayFormat} and \code{timeunits} properties
equal to those of the first argument in \code{\dots}.
If not all arguments are \code{mondate}s
then the result is a \code{data.frame} by virtue of the call
\code{cbind.data.frame(...)}.
}

A \code{mondate} (or a data.frame from 
\code{c-rbindmondate} when \dots holds non-\code{mondate} arguments).
For \code{c} and \code{rep}, a \code{vector}.
}
\examples{
x <- mondate(1:6) # first 6 month-ends of the year 2000
c(x,x+6)          # all month-ends of 2000
c(0,x)            # result is "numeric", as determined by the first argument

M<-mondate.ymd(2001:2005,12,31) # 5 year-ends
names(M)<-LETTERS[1:5]
cbindmondate(M)                      # as a 5x1 matrix
rbindmondate(M,M)
begin_date <- M-12
cbindmondate(begin_date,end_date=M)  # 5 pairs of year boundary-dates. Columns 
                              # are "automatically" named in the default case 
                              # (all mondates with timeunits="months").
dayt <- as.Date("2010-6-30")
cbindmondate(x,mondate(dayt))        # column names show as 'x' and blank
cbindmondate(x=x,DateColumn=mondate("2010-6-30")) # both columns are named

rep(mondate("2010-2-14"), 3)

(M<-seq(from=mondate("1/1/2010"),length=2)) # Jan. and Feb. 1st
rep(M,3)                                    # three pairs
rep(M,each=3)                               # three Jan.'s, three Feb.'s
}
\keyword{methods}
