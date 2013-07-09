\name{sieve}
\alias{sieve}
\alias{sieve.mondate}
\alias{sieve.Date}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sieve
%%  ~~function to do ... ~~
}
\description{A method to produce breaks for a vector of dates.
Adjacent entries determine the endpoints of intervals that cover the dates
and are suitable for methods that need such \code{breaks} arguments,
such as \code{hist} and \code{cut}.
}
\usage{
sieve(x, breaks, ...)
\method{sieve}{mondate}(x, breaks, ...)
\method{sieve}{Date}(x, breaks, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    an \R object representing dates. 
    Currently methods exist for objects of class \code{Date} and \code{mondate}
    }
  \item{breaks}{
    a vector of cut points or number giving the number of intervals 
    which \code{x} is to be cut into or an interval specification, 
    one of "days", "weeks", "months", "quarters" or "years", 
    optionally singularlized.
    }
  \item{\dots}{
    arguments to be passed to or from other methods, such as \code{cut}
    }
}
\details{
This is a convenient method of producing breaks suitable for 
\code{hist} and \code{cut}.
}
\value{
A date of the same class as \code{x}.
}
\author{
Dan Murphy
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cut}}, \code{\link{cut.mondate}}, \code{\link{cut.Date}}
}
\examples{
# Intervals so determined are left-open, right-closed
sieve(mondate.ymd(2100:2013, 6, 15), "months")
# Intervals so determined are left-closed, right-open
sieve(as.Date(mondate.ymd(2100:2013, 6, 15)), "months")
}

\keyword{methods}

