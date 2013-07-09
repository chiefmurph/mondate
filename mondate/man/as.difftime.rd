\name{as.difftime}
\alias{as.difftime}
\title{Difftime with units Months and Years}
\description{
Expand difftime units to include months and years .
}
\usage{
as.difftime(tim, format = "\%X", units = "auto")
}
\arguments{
\item{tim}{
}
\item{format}{
}
\item{units}{
  Anything allowed by \code{base:::as.difftime}.
  In addition, can be "months" or "years" in which case 
  \code{tim} must be numeric.
}
}
\details{
Primarily used to facilitate adding months and years to 
\code{mondate}s.
See base:::as.difftime.
}
\value{
See base:::as.difftime.
}
\author{
Dan Murphy.
}
\seealso{
Base \code{\link{as.difftime}}
}
\examples{
x <- mondate(0:12)
y <- as.difftime(1, , "months")
x + y
x - y
}
