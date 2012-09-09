\name{names method}
\alias{names<-.mondate}
\title{
Assign names to a mondate.
}
\description{
Function to assign names to a mondate.
}
\usage{
\method{names}{mondate}(x) <- value
}
\arguments{
\item{x}{
a \code{mondate}.
}
\item{value}{
the names to assign to \code{x}
}
}
\details{
Assigns the names attribute to the .Data part of \code{x}.
}
\examples{
YE <- mondate.mdy(12, 31, 2011:2012)
names(YE) <- c("A", "B")
}
