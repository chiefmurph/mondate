\name{as.list method}
\alias{as.list.mondate}
\title{
Construct a list from a mondate
}
\description{
Function to construct a list from a mondate.
}
\usage{
\S3method{as.list}{mondate}(x, \dots)
}
\arguments{
\item{x}{
a \code{mondate}.
}
\item{\dots}{
optional additional arguments to be passed to or from methods.
}
}
\details{
Constructs a list from a mondate object.
The length of the list equals the length of \code{x}.
Each sublist contains an individual element of \code{x}.
This method is most useful when a mondate is the \code{X} argument
of one of the \code{*apply} functions.
See an example of a recommended call to \code{sapply} in "Examples" below.
}
\examples{
YE <- mondate.mdy(12, 31, 2011:2012)
sapply(YE, class)    # "mondate" "mondate"
sapply(YE, month)   # same as month(YE)
month(YE)
}
