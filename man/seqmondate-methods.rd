\name{seqmondate-methods}
\docType{methods}
\alias{seqmondate}
\alias{seqmondate-methods}
\alias{seqmondate,mondate,mondate-method}
\alias{seqmondate,mondate,missing-method}
\alias{seqmondate,missing,mondate-method}
\alias{seqmondate,Date,Date-method}
\alias{seqmondate,POSIXlt,POSIXlt-method}
\alias{seqmondate,POSIXct,POSIXct-method}
\title{Methods to Generate Date Sequences}
\description{
Methods to generate sequences for objects of class
\code{mondate}, \code{Date}, \code{POSIXlt}, and \code{POSIXct}
in units of "months" in the sense of the 'mondate' package
when so specified by (unnamed) argument \code{by}.
This is essentially a wrapper for \code{seq.mondate} that requires
\code{from} and \code{to} to be of the same class,
and returns a sequence of that class.
}

\arguments{
\item{from}{
coercible to a \code{mondate}. May be "missing".
}
\item{to}{
coercible to a \code{mondate}. May be "missing".
}
\item{\dots}{
optional arguments passed to \code{\link{seq.mondate}}
}
}

\value{
  A sequence of the same class as argument \code{from}.
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

}}
\examples{
d1 <- as.Date("2015-01-31") # last days of the month
d2 <- as.Date("2015-12-31")
seqmondate(d1, d2, by = "months") # always the last day of the month
# Contrast with the base method
seq(d1, d2, by = "months") # No date in any month with less than 31 days
}
\keyword{methods}
