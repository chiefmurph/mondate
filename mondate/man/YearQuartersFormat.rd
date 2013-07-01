\name{YearQuartersFormat}
\alias{YearQuartersFormat}
\title{Formatting Functions for mondate Objects}
\description{
Functions to format a \code{mondate} into its character
representation according to the \code{displayFormat} property.
}
\usage{
YearQuartersFormat(x)
}
\arguments{
\item{x}{
a \code{mondate} or a \code{Date} or a \code{POSIXt}.
}
}
\details{
YearQuartersFormat is an example of a special formatting function
that can be provided to a \code{mondate} object when created.
It will represent the date as YYYYQ* where * is 1-4.
See Examples
}
\examples{
b <- mondate(1:12, formatFUN = YearQuartersFormat)   # end of first 12 months of 2000
b         
}
