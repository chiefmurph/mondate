\name{Miscellaneous-methods}
\docType{methods}
\alias{mean-methods}
\alias{mean,ANY-method}
\alias{mean,mondate-method}
\alias{unique-methods}
\alias{unique,ANY-method}
\alias{unique,mondate-method}
\title{Miscellaneous Methods for Mondates}
\description{
Miscellaneous \code{mondate} methods.
}
\section{Methods}{
\describe{

    \item{\code{mean(x = "mondate", ...)}}{
        Calculate the mean date of \code{mondate}s. 
        Arguments in \code{\dots}
        can be \code{mondate}s or R objects 
        that can be coerced to a \code{mondate}.
        Result will be a \code{mondate}
        with properties equal to those of \code{x}.
        }

    \item{\code{unique(x = "mondate", ...)}}{
        Returns a \code{mondate}s but with duplicate
        elements/rows removed.
        For an explanation of the arguments in \code{\dots}, 
        see \code{\link{unique}}.
        }
}}
\examples{
(M<-mondate.mdy(12,31,2001:2003))
mean(M)                           # the middle value, Dec. 31, 2002
(M<-c(M,mondate.mdy(12,31,2004))) # tack on another yearend
mean(M)                           # mid-year 2003
mean(M,12)                        # 12 is coerced to Dec. 31, 2000, so the 
                                  # mean is again Dec. 31, 2002   

unique(M,M)                       # just M
(M<-matrix(M,nrow=2))             # now a matrix
rbind(M,M)                        # 2 M's, stacked
unique(rbind(M,M))                # M again, still a matrix

}
\keyword{methods}
