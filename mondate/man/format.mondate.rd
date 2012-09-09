\name{format.mondate}
\alias{format.mondate}
\title{Format a mondate}
\description{
Function to format a \code{mondate} into its character
representation according to the \code{displayFormat} property.
}
\usage{
\method{format}{mondate}(x, \dots)
}
\arguments{
\item{x}{
a \code{mondate}.
}
\item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
For more details see \code{\link{format}} and especially \code{\link{strptime}}.
}
\value{
\code{character} representation of the \code{mondate}.
}
\seealso{
\code{\link{strptime}}.
}
\examples{
(b<-mondate(1))   # end of first month of millennium
format(b)         # "01/31/2000" -- with quotes -- in the U.S. locale
format(b, format="\%Y-\%m-\%d")  # "2000-12-31"
}
