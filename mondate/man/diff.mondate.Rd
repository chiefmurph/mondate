\name{diff.mondate}
\alias{diff.mondate}
\title{
'diff' for package mondate
}
\description{
Returns suitably lagged and iterated differences of an object of class \bold{mondate}.
}
\usage{
\method{diff}{mondate}(x, lag = 1L, differences = 1L, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a mondate vector or matrix containing the values to be differenced.
}
  \item{lag}{
an integer indicating which lag to use.
}
  \item{differences}{
an integer indicating the order of the difference.
}
  \item{\dots}{
further arguments to be passed to or from methods.
}
}
\details{
See the \code{diff} function in \code{base}.
}
\value{
If \code{x} is a vector of length \code{n} and \code{differences=1}, 
then the computed result is equal to the successive differences 
\code{x[(1+lag):n] - x[1:(n-lag)]}.

If \code{difference} is larger than one this algorithm is applied 
recursively to \code{x}. 
Note that the returned value is a vector which is shorter than \code{x}.

If \code{x} is a matrix then the difference operations 
are carried out on each column separately.

}
\author{
Dan Murphy
}

\seealso{
\code{\link[base]{diff}}
}
\examples{
evalDate<-mondate(c(12+12*1:10))  # year ends 2001, ..., 2010
diff(evalDate)                    # vector of length 9 holding the number 12, 
                                  # with "timeunits" attribute = "months"
}
