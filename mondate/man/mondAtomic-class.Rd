\name{mondAtomic-class}
\Rdversion{1.1}
\docType{class}
\alias{mondAtomic-class}
\alias{mondArray_or_Atomic-class}
\alias{mondate_possible-class}
\alias{mondAtomic_w_date-class}
\alias{mondArray_or_Atomic_w_date-class}
\alias{mondate_possible_w_date-class}

\title{Classes for cbind and rbind signatures}
\description{
As noted in the help for "setGeneric", 
"Starting with version 2.8.0 of R, ... 
may be signature of the generic function. 
Methods will then be selected if their signature matches 
\emph{all} the ... arguments." 
(Emphasis added.)
To allow \code{cbind} and \code{rbind} to act on \code{mondates} 
as well as other objects that can be coerced to \code{mondate},
the union classes below describe
groups of objects that are "c-rbind-compatible" with class \code{mondate}.
This approach to handling \code{\dots} signatures was copied from
see the "Rmpfr" package.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Extends}{
Class \code{"\linkS4class{mondArray_or_Atomic}"}, directly.
Class \code{"\linkS4class{mondAtomic_w_date}"}, directly.
Class \code{"\linkS4class{mondate_possible}"}, by class "mondArray_or_Atomic", distance 2.
Class \code{"\linkS4class{mondArray_or_Atomic_w_date}"}, by class "mondAtomic_w_date", distance 2.
Class \code{"\linkS4class{mondate_possible_w_date}"}, by class "mondArray_or_Atomic_w_date", distance 3.
}
\section{Methods}{
\code{cbind} and \code{rbind} methods are defined for classes
"mondate_possible" and "mondate_possible_w_date".
}
\note{
The above classes are defined as follows:

setClassUnion("mondAtomic",
    members = c("logical", "integer", "numeric", "character"))
setClassUnion("mondArray_or_Atomic",
    members = c("array", "matrix", "mondAtomic"))
setClassUnion("mondate_possible",
    members = c("mondArray_or_Atomic", "mondate"))

setClassUnion("mondAtomic_w_date",
    members = c("mondAtomic", .date.classes))
setClassUnion("mondArray_or_Atomic_w_date",
    members = c("array", "matrix", "mondAtomic_w_date"))
setClassUnion("mondate_possible_w_date",
    members = c("mondArray_or_Atomic_w_date", "mondate"))

where currently

    .date.classes <- c("Date","POSIXt","POSIXct","POSIXlt")    

}
\references{
Martin Maechler's package "Rmpfr."
}
\author{
Dan Murphy
}
\examples{
showClass("mondAtomic")
}
\keyword{classes}
