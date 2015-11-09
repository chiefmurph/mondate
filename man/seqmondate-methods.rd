\name{seqmondate-methods}
\docType{methods}
\alias{seqmondate}
\alias{seqmondate-methods}
\alias{seqmondate,mondate,mondate-method}
\alias{seqmondate,mondate,missing-method}
\alias{seqmondate,missing,mondate-method}
\alias{seqmondate,Date,Date-method}
\alias{seqmondate,Date,missing-method}
\alias{seqmondate,missing,Date-method}
\alias{seqmondate,POSIXlt,POSIXlt-method}
\alias{seqmondate,POSIXlt,missing-method}
\alias{seqmondate,missing,POSIXlt-method}
\alias{seqmondate,POSIXct,POSIXct-method}
\alias{seqmondate,POSIXct,missing-method}
\alias{seqmondate,missing,POSIXct-method}
\alias{seqmondate,ANY,ANY-method}
\alias{seqmondate,ANY,missing-method}
\alias{seqmondate,missing,ANY-method}
\title{Methods to Generate Date Sequences}
\description{
Methods to generate date sequences.
This is essentially a wrapper for \code{seq.mondate} that requires
\code{from} and \code{to} to be of the same class --
when both are specified --
and returns a sequence of that class.
The primary purpose is to generate sequences
in units of "months" 
(the default value of the unnamed argument \code{by}).
}

\arguments{
\item{from}{
}
\item{to}{
objects of class
\code{mondate}, \code{Date}, \code{POSIXlt}, or \code{POSIXct}.
May be "missing".
If both present, must be of the same class.
}
\item{\dots}{
optional arguments passed to \code{\link{seq.mondate}}
}
}

\value{
  A sequence of the same class as \code{from\\to}.
}

\section{Methods}{
\describe{

\item{\code{signature(from = "mondate", to = "mondate", ...)}}{
%%  ~~describe this method here~~
}

\item{\code{signature(from = "Date", to = "Date", ...)}}{
%%  ~~describe this method here~~
}

\item{\code{signature(from = "POSIXlt", to = "POSIXlt", ...)}}{
%%  ~~describe this method here~~
}

\item{\code{signature(from = "POSIXct", to = "POSIXct", ...)}}{
%%  ~~describe this method here~~
}

\item{\code{signature(from = "ANY", to = "ANY", ...)}}{
%%  ~~describe this method here~~
}

}}
\examples{
# 13 month-end dates starting with the end of 2014 and 
#   ending with the end of 2015
seqmondate(mondate.ymd(2014), mondate.ymd(2015))

# In most situations, seq.Date and seqmondate return identical sequences,
# as when 'from' is at the beginning of the month.
s1 <- seq(as.Date("2015-01-01"), as.Date("2015-12-01"), by = "month")
s2 <- seqmondate(as.Date("2015-01-01"), as.Date("2015-12-01"))
stopifnot(identical(s1, s2))

# But when 'from' is near the end of a long month, seq.Date, seq.POSIXct, etc
# can step into subsequent months (as documented in ?seq.POSIXt).
seq(as.POSIXct("2015-01-31"), by = "month", length.out = 12)
# ... contrasted with ...
seqmondate(as.POSIXct("2015-01-31"), by = "month", length.out = 12)
}
\keyword{methods}
