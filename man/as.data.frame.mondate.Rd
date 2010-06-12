\name{data.frame-methods}
\alias{as.data.frame.mondate}
\title{
Coerce to a data.frame
}
\description{
Functions to coerce a mondate for containment in a data.frame.
}
\section{Methods}{
\describe{
\item{\code{data.frame(x = "mondate")}}{
Coerce \code{mondate} to a \code{data.frame}.
This is the function that users would normally call.
For the full argument list, see \code{data.frame}.
}

\item{\code{as.data.frame(x = "mondate", ...)}}{
Coerce \code{mondate} to a \code{data.frame}.
This function, not normally called directly, 
is the workhorse of \code{data.frame} coersion.
This is the function that \code{data.frame} will call when 
it encounters a \code{mondate} in its argument list.
Using the S3 method approach, 
the \code{as.data.frame.mondate} method goes through the 
necessary contortions to enable a \code{mondate}
stored in a \code{data.frame} to display as a date 
when the \code{data.frame} is displayed.
Fashioned after \code{as.data.frame.Date},
\cr 
\code{as.data.frame.mondate}
enables \code{array}s to also be stored in a \code{data.frame},
retaining the number of rows and "flattening" in the
usual \code{data.frame} sense.
For the full argument list, see \code{as.data.frame}.
}

}}
\seealso{
\code{\link{data.frame}},
\code{\link{as.data.frame}},
\code{\link{format}}
}
\examples{
M<-mondate.mdy(12,31,2001:2005)
data.frame(YearEnd=M, Liability=1000*1.05^(1:5)) # 5% annual inflation
}
