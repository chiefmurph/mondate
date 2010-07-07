\name{Coersion-from-mondate methods}
\docType{methods}
\alias{as.character-methods}
\alias{as.character,mondate-method}
\alias{as.numeric,mondate-method}
\alias{as.Date-methods}
\alias{as.Date,ANY-method}
\alias{as.Date,mondate-method}
\alias{as.POSIXct,mondate-method}
\alias{as.POSIXct.mondate}
\alias{as.POSIXlt,mondate-method}
\alias{as.POSIXlt.mondate}
\title{Coersion Methods for Mondates}
\description{
Methods to coerce a \code{mondate} to other R objects.
Currently that includes numbers, characters, 
and three classes of dates.
}
\section{Methods}{
\describe{

\item{\code{as.character(x = "mondate", displayFormat, ...)}}{
    Coerce \code{mondate} to class \code{character}.
    Uses the \code{format} function.
    \describe{
        \item{\code{displayFormat}}{
            If \code{missing} the value is drawn from 
            the property of the \code{mondate}.
            }
        \item{\code{\dots}}{
            arguments passed to other methods (e.g., \code{format}).
            }
        }
    }

    \item{\code{as.numeric(x = "mondate",}}{ \code{convert=FALSE,}
        \code{timeunits=c("months", "years", "days"), ...)}
        Coerce \code{mondate} to class \code{numeric}.
        \describe{
            \item{\code{convert:}}{ 
            \code{FALSE} (the default)
            is equivalent to \code{getDataPart}.
            If \code{TRUE} the result will be converted to
            the number of years since 
            the beginning of the millennium if
            \code{timeunits}="years";
            to the number of days since
            the beginning of the millennium if
            \code{timeunits}="days".
            Also in the case that \code{convert=TRUE}
            the \code{numeric} returned will have
            "timeunits" as an attribute.
            }
        \item{\code{timeunits}}{
            If \code{missing} the value is drawn from 
            the property of the \code{mondate}.
            }
        }
    }

    \item{\code{as.Date(x = "mondate")}}{
        Coerce \code{mondate} to class \code{Date}
        }

    \item{\code{as.POSIXlt(x = "mondate")}}{
        Coerce \code{mondate} to class \code{POSIXlt}
        }
    
    \item{\code{as.POSIXct(x = "mondate")}}{
        Coerce \code{mondate} to class \code{POSIXct}
        }
}}
\examples{
(b<-mondate(1))              # end of first month of current millennium
as.numeric(b)                # 1
as.numeric(b, convert=TRUE, timeunits="years") # converts to 1/12 "years"
(b<-mondate(1, timeunits="days")) # end of first day of millennium
as.numeric(b)                # 1/31
as.numeric(b, convert=TRUE)  # 1 (with a "days" attribute)
as.Date(b)                   # displays as "2000-01-31"
as.POSIXct(b)                # displays as "2000-01-31 UTC"
weekdays(as.POSIXct(b))      # January 1, 2000 was a "Monday" (in English)
as.POSIXlt(b)$hour           # zero, as are ...$min and ...$sec
}
\keyword{methods}
\keyword{chron}
