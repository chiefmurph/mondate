\name{seq.mondate}
\alias{seq.mondate}
\alias{seq}
\title{
Sequence Generation
}
\description{
Generate regular \code{mondate} sequences. 
}
\usage{
## S3 method for class 'mondate':
seq.mondate(from=NULL, to, ...)
}
\arguments{
  \item{from}{
a \code{mondate}. Optional, but at least one of 
\code{from} and \code{to} must be provided.
}
  \item{to}{
a \code{mondate}. Optional, but at least one of 
\code{from} and \code{to} must be provided.
}
  \item{\dots}{
arguments passed to or from methods.
Examples include \code{by} and \code{length.out}. 
See \code{\link{seq}} for more details.
}
}
\details{
For more details about sequence generation, see \code{\link{seq}}.

If \code{from} and \code{to} are both provided,
the \code{displayFormat} and \code{timeunits} properties are
taken from \code{from}, without a warning if they differ from 
\code{to}'s.
}
\value{
A \code{mondate} with \code{displayFormat} and \code{timeunits}
from \code{from}, if provided, otherwise from \code{to}.
}
\author{
Dan Murphy
}
\seealso{
\code{\link{seq}}
}
\examples{
# 8 quarter-ends beginning 1st quarter 2009; US displayFormat
seq(mondate("3/31/2009"), by=3, length.out=8) 
# 8 quarter-ends ending year-end 2009; non-US displayFormat
seq(to=mondate("2009/12/31"), by=3, length.out=8) 
}