\name{Miscellaneous-methods}
\docType{methods}
\alias{mean.mondate}
\alias{pmean}
\alias{pmean-methods}
\alias{pmean,mondate-method}
\alias{unique.mondate}
\alias{quarters.mondate}
\title{Miscellaneous Methods for mondate's}
\description{
Miscellaneous \code{mondate} methods.
}
\usage{
 \S3method{mean}{mondate}(x, trim = 0, na.rm = FALSE, \dots)
 \S3method{unique}{mondate}(x, \dots)
 \S3method{quarters}{mondate}(x, abbreviate)
}

\arguments{
    \item{x}{a mondate}
    \item{trim}{see \code{base::mean}}
    \item{na.rm}{see \code{base::mean}}
    \item{abbreviate}{\code{logical}. Should the names be abbreviated?}
    \item{\dots}{arguments passed to and from other methods}
    }

\section{Methods}{
\describe{

    \item{\code{mean(x = "mondate", ...)}}{
        Calculate the mean date of \code{mondate}s. 
        Arguments \code{trim} and \code{na.rm} have the usual meaning
        (see \code{base::mean}).
        }

    \item{\code{pmean(\dots = "mondate")}}{
        Calculate the "parallel" mean date of \code{mondate}s. 
        Arguments in \code{\dots}
        must all be \code{mondate}s.
        Result will be a \code{mondate}
        with properties equal to those of the first
        \code{mondate} in \code{\dots}.
        }

    \item{\code{unique(x = "mondate", ...)}}{
        Returns a \code{mondate} but with duplicate
        elements/rows removed.
        For an explanation of the arguments in \code{\dots}, 
        see \code{base::unique}.
        }

    \item{\code{quarters(x = "mondate", abbreviate)}}{
        Returns a \code{character} vector of "Q1" to "Q4".
        See \code{\link{quarters}}.
        }
}}
\examples{
(M<-mondate.mdy(12,31,2001:2003))
mean(M)                           # the middle value, Dec. 31, 2002
(M<-c(M,mondate.mdy(12,31,2004))) # tack on another yearend
mean(M)                           # mid-year 2003
mean(M,12)                        # 12 is coerced to Dec. 31, 2000, so the 
                                  # mean is again Dec. 31, 2002

x <- mondate.ymd(2001:2005,12)    # five year ends
y <- x-12                         # one year earlier
pmean(x,y)                        # 2001-06-30 ... 2005-06-30

unique(M,M)                       # just M
(M<-matrix(M,nrow=2))             # now a matrix
rbind(M,M)                        # 2 M's, stacked
unique(rbind(M,M))                # M again, still a matrix

m <- mondate.ymd(2013, 1:12)      # end of the months of 2013
quarters(m)
}
\keyword{methods}
