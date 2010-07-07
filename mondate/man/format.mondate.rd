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
For more details see \code{base::format}.
}
\value{
\code{character} representation of the \code{mondate}.
}
\examples{
(b<-mondate(1))   # end of first month of millennium
format(b)         # "01/31/2000" -- with quotes -- in the U.S. locale
}
