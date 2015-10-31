# email to Gabor 5/7/14: revised
test.cutmondate.Date <- function() {
  from <- as.POSIXlt("2014-01-01")
  to <- as.POSIXlt("2015-01-01")
  xdata <- seq(from, to, by = "months")
  res1 <- cutmondate(xdata, breaks = xdata)
  # versus
  res2 <- cutmondate(xdata, breaks = "months")
  checkTrue(is.na(tail(res1, 1)))
  checkTrue(is.na(tail(res2, 1)))
  checkTrue(identical(as.numeric(res1), as.numeric(res2)))

}

# cutmondate.Date like cutmondate.mondate?
#test