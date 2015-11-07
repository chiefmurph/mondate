\name{cutmondate-methods}
\docType{methods}
\alias{cutmondate}
\alias{cutmondate-methods}
\alias{cutmondate,mondate,ANY-method}
\alias{cutmondate,Date,character-method}
\alias{cutmondate,Date,missing-method}
\alias{cutmondate,Date,ANY-method}
\alias{cutmondate,POSIXt,ANY-method}
\title{Methods to Generate Date Cuts}
\description{
Methods to cut sequences of mondate, Date, and POSIXt objects.
This is essentially a wrapper for the \code{cut.mondate} method
with more intuitive defaults for the arguments 
depending on the object being cut.
}
\section{Methods}{
\describe{

\item{\code{signature(x = "mondate")}}{
calls the cut.mondate function
}

\item{\code{signature(x = c("Date", "character"))}}{
checks for \code{breaks} = "months", "years", or "quarters". 
If so, calls the cut.mondate function using x = mondate(x, displayFormat = "%Y-%m-%d");
otherwise, simply passes the arguments on to the cut.Date method.
The display argument to "mondate" ensures that the levels will display
according to the default "Date" format.
}

\item{\code{signature(x = c("Date", "missing"))}}{
if \code{breaks} is omitted, "months" is assumed.
}

\item{\code{signature(x = c("Date", "ANY"))}}{
if \code{breaks} is anything else, 
the entire argument list is passed on to cut.Date.
}

\item{\code{signature(x = "POSIXt")}}{
cutmondate is called again with x = as.Date(x).
}

}}
\keyword{methods}
