\name{numarray-class}
\Rdversion{1.1}
\docType{class}
\alias{numarray-class}

\title{Class "numarray"}
\description{
Definition: \code{setClassUnion("numarray",c("numeric","array"))}.

An S4 class that \code{contains="numarray"} can have a \code{.Data} part
that is either \code{numeric}, an \code{array}, or both.
Since a class that only extends \code{numeric} cannot have "shape"
(a \code{dim} attribute),
this is one way to enable "shape" characteristics of an object
that should otherwise act like a \code{numeric}.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Methods}{
No methods defined with class "numarray" in the signature.
}
\details{
Note that since \code{setClassUnion} is a "union",
a class that extends \code{numarray} could conceivably be a character array.
(To enforce both \code{numeric} \emph{and} \code{array},
one may define a \code{validity} method that checks for \code{numeric}.)

The \code{mondate} class is defined as a \code{numarray},
but has its own methods to coerce, say, a \code{character} date
to a \code{mondate}.
}
\author{
Dan Murphy
}

\seealso{
\code{\linkS4class{mondate}}
}
\examples{
showClass("numarray")
setClass("A", contains="numarray")
a<-new("A", 1:4)                        # a numeric vector
a
dim(a@.Data) <- c(2,2)                  # now a 2x2 matrix
a
b<-new("A", matrix(LETTERS[1:4],2,2))   # a character matrix is also valid
b
## Not run. Removal of the dim attribute of the underlying object
##          would turn it into a character vector, which
##          class "B" does not extend.
# dim(b@.data)<-NULL
}
\keyword{classes}
