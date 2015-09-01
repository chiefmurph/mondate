test.POSIXct.to.mondate <- function() {
  d <- as.Date("2014-06-01")
  d
  as.POSIXct(d)
  as.Date(as.POSIXct(d))
  mondate(as.POSIXct(d))
  checkEqualsNumeric(mondate(as.POSIXct(d)), mondate("2014-06-01"))
  }
