\name{as.mondate}
\alias{as.mondate}
\title{
As.Mondate Method
}
\description{
Coerce an object to class mondate. 
}
\usage{
as.mondate(x, \dots)
}
\arguments{
\item{x}{
an \R object.
}
\item{\dots}{
optional arguments passed to other methods.
}
}
\details{
This is a convenience function that simply calls the 
appropriate \code{mondate} conversion method depending
on the class of \code{x}.
}
\value{
A \code{mondate} if coersion is successful.
}
\author{
Dan Murphy
}
\seealso{
\code{\link{mondate-methods}}
}
\examples{
y <- as.Date("2012-12-31")
as.mondate(y)
}