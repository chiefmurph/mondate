\name{get.mondate.displayFormats}
\alias{get.mondate.displayFormats}
\alias{set.mondate.displayFormats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get or set the vector of formats to use when converting character to mondate
}
\description{
Methods to access the vector of \code{displayFormat}s
used by default.
}
\usage{
get.mondate.displayFormats()
set.mondate.displayFormats(x, clear = FALSE)
}
\arguments{
\item{x}{a \code{character} value(s) holding additional formats
to be used for converting \code{character} to \code{mondate}
}
\item{clear}{
\code{logical} indicating if the current formats should be "cleared" and
completely replaced by the value(s) in \code{x}. 
If \code{FALSE}, the value(s) in \code{x} are appended to the current set.
}
}
\value{
A vector of formats.
}
\author{
Dan Murphy
}
\examples{
get.mondate.displayFormats()
# Attempting to convert a date given in the French format dd/mm/yyyy actually
# works because the European format %Y/%m/%d successfully translates
# that character string in the same way as.Date translates it.
m <- mondate("31/08/2015")
m
year(m) # 31 .. probably an unreasonable value
d <- as.Date("31/08/2015")
d
year(d)
# Ensure the appropriate format is found first by 
# reprioritizing it in the list
set.mondate.displayFormats(c(Frb = "\%d/\%m/\%Y", get.mondate.displayFormats()), 
                          clear = TRUE)
m <- mondate("31/08/2015")
m
year(m) # 2015
}
\keyword{ ~format }
