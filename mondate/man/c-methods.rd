\name{Combining-methods}
\docType{methods}
\alias{c-methods}
\alias{c,mondate-method}
\alias{cbind}
\alias{cbind,mondate-method}
\alias{cbind.mondate}
\alias{rbind}
\alias{rbind,mondate-method}
\alias{rbind.mondate}
\alias{rep.mondate}
\title{Methods for Combining Mondates}
\description{
Methods to combine \code{mondate}s.
}
\details{
The package calls \code{setGeneric("c-rbind")}.
}

\usage{
 \S3method{rep}{mondate}(x, \dots)
 \method{cbind}{mondate}(\dots, deparse.level = 0)
 \method{rbind}{mondate}(\dots, deparse.level = 1)
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

\item{\code{c-rbind(... = "mondate")}}{
The \code{cbind} and \code{rbind} methods 
use the \code{base} \code{cbind} and \code{rbind} functions,
respectively,
to combine the arguments
and the result is converted to a \code{mondate}
with \code{displayFormat} and \code{timeunits} properties
equal to those of the first argument in \code{\dots}.
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
\examples{
x <- mondate(1:6) # first 6 month-ends of the year 2000
c(x,x+6)          # all month-ends of 2000
c(0,x)            # result is "numeric", as determined by the first argument

M<-mondate.ymd(2001:2005,12,31) # 5 year-ends
names(M)<-LETTERS[1:5]
cbind(M)                      # as a 5x1 matrix
rbind(M,M)
begin_date <- M-12
cbind(begin_date,end_date=M)  # 5 pairs of year boundary-dates. Columns are
                              # "automatically" named in the default case 
                              # (all mondates with timeunits="months").
dayt <- as.Date("2010-6-30")
cbind(x,mondate(dayt))        # column names show as 'x' and blank
cbind(x=x,DateColumn=mondate("2010-6-30")) # both columns are named

rep(mondate("2010-2-14"), 3)

(M<-seq(from=mondate("1/1/2010"),length=2)) # Jan. and Feb. 1st
rep(M,3)                                    # three pairs
rep(M,each=3)                               # three Jan.'s, three Feb.'s
}
\keyword{methods}
