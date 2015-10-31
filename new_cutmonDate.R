library(mondate)
# when breaks is character, include.lowest s/b TRUE by default
cutmonDate <- function(x, breaks, ...) {
  if (inherits(breaks, "character"))
    if (breaks %in% c("month", "months",
                      "year", "years",
                      "quarter", "quarters"))
      return(cutmondate(mondate(x), breaks = breaks, ...))
  cut(x, breaks = breaks, ...)
} 
# email to Gabor 5/7/14: revised
(from <- as.POSIXlt("2014-01-01"))
(to <- as.POSIXlt("2015-01-01"))
(xdata <- seq(from, to, by = "months"))
res1 <- cutmondate(xdata, breaks = xdata)
res1 # NA last
res1b <- cutmonDate(xdata, breaks = xdata)
res1b # NA last
identical(res1, res1b) # FALSE bc labels are different
identical(as.numeric(res1), as.numeric(res1b)) # factor levels are the same
# versus
res2 <- cutmondate(xdata, breaks = "months")
res2 # NA last
res3 <- cutmondate(xdata, breaks = "months", include.lowest = TRUE)
res3 # not NA last
res2b <- cutmonDate(xdata, breaks = "months")
res2b # not NA last ???
(mxdata <- mondate(xdata))

cut(mxdata, "month")
# bug! 01/01/2015 should be NA
cut(mxdata, "month", right = FALSE, include.lowest = FALSE)
# cutmondate.Date like cutmondate.mondate?
#test

mdata <- mondate.ymd(2014) + 0:12
mdata
# This works as it should
cutmondate(mdata, "month")
cutmondate(mdata, "month", include.lowest = TRUE) # s/b default
