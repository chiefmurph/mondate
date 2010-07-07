\name{data.frame-methods}
\alias{as.data.frame.mondate}
\title{
Insert a mondate into a data.frame
}
\description{
Function to coerce a mondate for containment in a data.frame.
}
\usage{
\method{as.data.frame}{mondate}(x, row.names=NULL, optional=FALSE, \dots)
}
\arguments{
\item{x}{
a \code{mondate}.
}
\item{row.names}{
\code{NULL} or a character vector giving the row names 
for the data frame. 
Missing values are not allowed.
}
\item{optional}{
logical. 
See \code{base::as.data.frame} for details.
}
\item{\dots}{
optional additional arguments to be passed to or from methods.
}
}
\details{
Although users would normally call \code{data.frame}, 
the \code{as.data.frame} function is the underlying workhorse that
enables a \code{mondate} to be stored in a \code{data.frame}.
When array-shaped mondates are stored, 
the number of rows is retained and if the dimension > 2 it is 
"flattened" in the
usual \code{data.frame} sense.
The \code{mondate} method is fashioned after 
the \code{as.data.frame.Date} method.
}
\seealso{
\code{\link{data.frame}},
\code{\link{as.data.frame}},
\code{\link{format}}
}
\examples{
M<-mondate.mdy(12,31,2001:2005)
data.frame(YearEnd=M, Liability=1000*1.05^(1:5)) # 5% annual inflation
}
