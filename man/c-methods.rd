\name{Combining-methods}
\docType{methods}
\alias{c-methods}
\alias{c,mondate-method}
\alias{cbind.mondate}
\alias{cbind}
\alias{cbind-methods}
\alias{cbind,ANY-method}
\alias{cbind,mondate-method}
\alias{rbind.mondate}
\alias{rep-methods}
\alias{rep,mondate-method}
\title{Methods for Combining Mondates}
\description{
Methods to combine \code{mondate}s.
}
\section{Methods}{
\describe{
\item{\code{c(x = "mondate", ...)}}{
combine \code{mondate}s. \code{\dots} can be any R object(s) that can
be coerced to a \code{mondate}.
The result will be a \code{mondate} with properties equal to those of 
\code{x}.
}

\item{\code{cbind}/\code{rbind(...)}}{
Take a sequence of arguments \code{\dots},
at least one of which is a \code{mondate},
and combine by columns/rows.
The result will be a \code{mondate} with properties equal to those of 
\code{x}.
Non-\code{mondate} arguments will be coerced to a \code{mondate},
if necessary using the \code{timeunits} of the first \code{mondate} in
\code{\dots}.
}

\item{\code{rep(...)}}{
Replicates \code{mondate}s.
See \code{\link{rep}} for further details.
}
}}
\examples{
M   <-mondate.mdy(6,30,2006:2010)
Myrs<-mondate.mdy(6,30,2006:2010,timeunits="years") 
c(M,   Myrs) # will be in "months"
c(Myrs,M   ) # will be in "years"

M<-mondate.ymd(2001:2005,12,31) # 5 year-ends
names(M)<-LETTERS[1:5]
M
cbind(begin=M-12,end=M) # 5 pairs of year boundary-dates
begin<-mondate(M-12,timeunits="years")
cbind(begin=begin,end=M) # same dates, but timeunits is in years
cbind(Year2End=2,begin=begin,end=M) # Since 'begin' is first mondate in
                                    # argument list, timeunits of result
                                    # will be in "years." Therefore, 
                                    # column 1 dates are 2 years after 
                                    # the beginning of the millennium.
cbind(as.Date("2000-1-1"),M) # displayFormat of the result will be that of M

rep(mondate("2-14-2010"), 3)
M<-seq(from=mondate("1/1/2010"),length=2) # Jan. and Feb. 1st
rep(M,3)                                  # three pairs
rep(M,length.out=5)                       # ends with Jan. 1
rep(M,each=3)                             # three Jan.'s, three Feb.'s
}
\keyword{methods}
