\name{print-methods}
\docType{methods}
\alias{print.mondate}
\alias{show,mondate-method}
\title{Methods to Display a Mondate}
\description{
Methods to display a \code{mondate} in an R session.
}
\usage{
    \S3method{print}{mondate}(x, \dots)
    \S4method{show}{mondate}(object)
}

\arguments{
    \item{x}{a mondate}
    \item{object}{a mondate}
    \item{\dots}{arguments passed to and from other methods}
    }

\section{Methods}{
\describe{

    \item{\code{print(x = "mondate", ...)}}{
        Print the date in its character representation using the
        \cr 
        \code{displayFormat} property. 
        Uses the \code{noquote} function. 
        Arguments in \code{\dots} are passed to \code{base::print}.
        }

    \item{\code{show(object = "mondate")}}{
        Same as \code{print} above,
        but with a "header" showing the 
        \code{timeunits} property. 
        }
}}
\keyword{methods}
