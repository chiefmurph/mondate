\name{seq.mondate}
\alias{seq}
\alias{seq.mondate}
\title{
Mondate Sequence Generation
}
\description{
Generate regular \code{mondate} sequences. 
}
\usage{
\method{seq}{mondate}(from, to, \dots)
}
\arguments{
\item{from}{
a \code{mondate}. May be "missing".
}
\item{to}{
a \code{mondate}. May be "missing".
}
\item{\dots}{
optional arguments passed to \code{seq.default},
including \code{by}, \code{length.out}, and \code{along.with}.
See \code{\link{seq}} for more details.
}
}
\details{
For more details about sequence generation, see \code{\link{seq}}.

If \code{from} and \code{to} are both provided,
the \code{displayFormat} and \code{timeunits} properties are
taken from \code{from}, 
without a warning if \code{from}'s properties differ from 
\code{to}'s.
}
\value{
A \code{mondate} vector with \code{displayFormat} and \code{timeunits}
from argument \code{from}, if provided, otherwise from argument \code{to}.
}
\author{
Dan Murphy
}
\seealso{
\code{\link{seq}}
}
\examples{
x<-mondate.ymd(2010,1)
y<-mondate.ymd(2010,12)
seq(from=x, to=y)  # all month-ends in 2010
# 8 quarter-ends beginning 1st quarter 2009; US displayFormat
seq(mondate("3/31/2009"), by=3, length.out=8) 
# 8 quarter-ends ending year-end 2009; non-US displayFormat
seq(to=mondate("2009/12/31"), by=3, length.out=8) 
}