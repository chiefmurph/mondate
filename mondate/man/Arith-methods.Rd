\name{Arith-methods}
\docType{methods}
\alias{Arith-methods}
\alias{Arith,mondate,mondate-method}
\alias{Arith,mondate,numeric-method}
\alias{Arith,numeric,mondate-method}
\title{Methods for Function Group Arith}
\description{
Arithmetic methods for class \code{mondate}.
}
\section{Methods}{
\describe{

\item{\code{signature(e1 = "mondate", e2 = "mondate")}}{
Performs arithmetic operations on two \code{mondate}s. 
The result will be \code{numeric} with attribute \code{timeunits}
belonging to the slot of the first argument 
(with a warning if the two arguments have different \code{timeunits}).
The only operation that would seem useful for two 
\code{mondate}s is subtraction.
}

\item{\code{signature(e1 = "mondate", e2 = "numeric")}}{
}

\item{\code{signature(e1 = "numeric", e2 = "mondate")}}{
Performs arithmetic operations of a \code{numeric} on a 
\code{mondate} where the units
of the \code{numeric} is taken from the 
\code{timeunits} slot of the \code{mondate}.
The result will be a \code{mondate} with the same properties 
as the \code{mondate}
in the function call.

The only operations that would seem useful for a \code{mondate} and a numeric
are addition and subtraction.

Most of the time it is expected that \code{timeunits}="months" so that, 
for example,
adding/subtracting a number to/from that \code{mondate} adds/subtracts that
number of months.
If the \code{mondate}'s \code{timeunits}="years" 
then the perceived intention is to
add/subtract that number of years. 
To accomplish that, the \code{mondate}'s numeric 
value is divided by 12, the operation is performed, and the result converted
to a \code{mondate}.
If the \code{mondate}'s \code{timeunits}="days", 
then the \code{mondate}'s value is converted to the number
of days since the start of the millennium, 
the operation is performed
(e.g., the \code{numeric} number of days is added or subtracted), 
and the result converted to a \code{mondate}. 
(See the \code{convert} option of the \code{as.numeric("mondate")} function.)
}
}}
\seealso{
\code{\link{Arith}}
}
\examples{
M<-mondate("1-1-2010") # will display in U.S. date format
M-1:12 # the 1st of the month for all months in 2009
       # in reverse chronological order; inherits M's displayFormat
}
\keyword{methods}
\keyword{arith}
