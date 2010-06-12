\name{print-methods}
\docType{methods}
\alias{print-methods}
\alias{print,ANY-method}
\alias{print,mondate-method}
\alias{show,mondate-method}
\title{Methods to Display a Mondate}
\description{
Methods to display a \code{mondate} in an R session.
}
\section{Methods}{
\describe{

    \item{\code{print(x = "mondate", ...)}}{
        Print the date in its character representation using the
        \cr 
        \code{displayFormat} property. 
        Uses the \code{noquote} function. 
        Arguments in \code{\dots} are passed to \code{print}.
        }

    \item{\code{show(x = "mondate")}}{
        Same as \code{print} above,
        but with a "header" showing the 
        \code{timeunits} property. 
        }
}}
\keyword{methods}
