test.cut.mondate <- function() {

  #require(RUnit)
  y <- cut(mondate(12:24), breaks = 12:24)
  checkEquals(length(levels(y)), 12)

  # numeric, cut default. For comparison, not testing.
  x <- 0:4
  cut(x, breaks = x)
  cut(x, breaks = x, include.lowest = T)
  cut(x, breaks = 2)
  cut(x, breaks = 2, include.lowest = T)
  # 4 levels, but not of unit width. Weird.
  cut(x, breaks = 4)

  seq(min(x), max(x), length = 3)
  cut(x, breaks = seq(min(x), max(x), length = 3))
  cut(x, breaks = seq(min(x), max(x), length = 3), include.lowest = T)

  
  # mondate

  x <- mondate(0:4)
  (y <- cut(x, x))
  checkEquals(levels(y), c("(12/31/1999,01/31/2000]","(01/31/2000,02/29/2000]","(02/29/2000,03/31/2000]","(03/31/2000,04/30/2000]"))
  (y <- cut(x, x, right = FALSE))
  checkEquals(levels(y), c("[12/31/1999,01/31/2000)","[01/31/2000,02/29/2000)","[02/29/2000,03/31/2000)","[03/31/2000,04/30/2000)"))

  # 4 levels of unit width. Differs from cut.default.
  (y <- cut.mondate(x, breaks = 4, attr.breaks = TRUE))
  checkTrue(is.na(y[1]))
  checkEquals(levels(y), c("(12/31/1999,01/31/2000]", "(01/31/2000,02/29/2000]", "(02/29/2000,03/31/2000]", "(03/31/2000,04/30/2000]"))
  (y <- cut.mondate(x, breaks = 4, include.lowest = TRUE))
  checkTrue(!is.na(y[1]))
  checkEquals(levels(y), c("[12/31/1999,01/31/2000]", "(01/31/2000,02/29/2000]", "(02/29/2000,03/31/2000]", "(03/31/2000,04/30/2000]"))




  (y <- cut(x, "months"))
  checkTrue(is.na(y[1]))
  checkEquals(levels(y), c("01/31/2000", "02/29/2000", "03/31/2000", "04/30/2000"))
  (y <- cut(x, "months", right = FALSE))
  checkTrue(is.na(y[1]))
  checkEquals(levels(y), c("12/31/1999", "01/31/2000", "02/29/2000", "03/31/2000"))
  (y <- cut(x, "months", right = TRUE, include.lowest = TRUE))
  checkTrue(!is.na(y[1]))
  checkEquals(levels(y), c("12/31/1999", "01/31/2000", "02/29/2000", "03/31/2000", "04/30/2000"))
  (y <- cut(x, "months", right = TRUE, include.lowest = TRUE, attr.breaks = TRUE))
  checkEqualsNumeric(attr(y, "breaks"), mondate(-1:4))
  
  (y <- cut(x, attr(y, "breaks"), right = TRUE, include.lowest = TRUE))
  checkEquals(levels(y), c("[11/30/1999,12/31/1999]", "(12/31/1999,01/31/2000]", "(01/31/2000,02/29/2000]", "(02/29/2000,03/31/2000]", "(03/31/2000,04/30/2000]"))

  # Test for non-NA when scalar x on month boundary
  x <- mondate.ymd(2008, 6)
  y <- cut(x, "month", right = TRUE)
  checkTrue(!is.na(y))
  checkEquals(levels(y), "06/30/2008")
  y <- cut(x, "month", right = FALSE)
  checkTrue(!is.na(y))
  checkEquals(levels(y), "05/31/2008")

  # "weeks"
  x <- c(mondate.ymd(2014, 3, 30:31), mondate.ymd(2014, 4, 1:30))
  (y <- cut.mondate(x, "weeks", attr.breaks = TRUE))
  checkTrue(is.na(y[1]))
  checkEquals(levels(y), c("04/06/2014", "04/13/2014", "04/20/2014", "04/27/2014", "05/04/2014"))
  (y <- cut.mondate(x, "weeks", attr.breaks = TRUE, right = FALSE))
  checkTrue(is.na(y[1]))
  checkEquals(levels(y), c("03/30/2014", "04/06/2014", "04/13/2014", "04/20/2014", "04/27/2014"))

  x <- c(mondate.ymd(2014, 3, 31), mondate.ymd(2014, 4, 1:30))
  (y <- cut.mondate(x, "weeks", attr.breaks = TRUE))
  checkTrue(!is.na(y[1]))
  checkEquals(levels(y), c("04/06/2014", "04/13/2014", "04/20/2014", "04/27/2014", "05/04/2014"))

  x <- c(mondate.ymd(2014, 3, 30:31), mondate.ymd(2014, 4, 1:30))
  (y <- cut.mondate(x, "weeks", include.lowest = TRUE, attr.breaks = TRUE))
  checkTrue(!is.na(y[1]))
  checkEquals(levels(y), c("03/30/2014", "04/06/2014", "04/13/2014", "04/20/2014", "04/27/2014", "05/04/2014"))
  
  x <- c(mondate.ymd(2014, 3, 30:31), mondate.ymd(2014, 4, 1:30))
  (y <- cut.mondate(x, "weeks", start.on.monday = FALSE, attr.breaks = TRUE))
  checkTrue(!is.na(y[1]))
  checkEquals(as.character(y[1]), "04/05/2014")
  checkEquals(levels(y), c("04/05/2014", "04/12/2014", "04/19/2014", "04/26/2014", "05/03/2014"))
  
  # 2 weeks
  x <- c(mondate.ymd(2014, 3, 30:31), mondate.ymd(2014, 4, 1:30))
  (y <- cut.mondate(x, "2 weeks", attr.breaks = TRUE))
  checkTrue(is.na(y[1]))
  checkEquals(levels(y), c("04/13/2014", "04/27/2014", "05/11/2014"))
  (y <- cut.mondate(x, "2 weeks", start.on.monday = FALSE, attr.breaks = TRUE))
  checkTrue(!is.na(y[1]))
  checkEquals(levels(y), c("04/12/2014", "04/26/2014", "05/10/2014"))
  
  # "days"
  x <- mondate.ymd(2014, 4, c(1, 7))
  (y <- cut.mondate(x, "days", attr.breaks = TRUE))
  checkEquals(as.character(y), c(NA, "04/07/2014"))
  checkEquals(levels(y), c("04/02/2014", "04/03/2014", "04/04/2014", "04/05/2014", "04/06/2014", "04/07/2014"))
  (y <- cut.mondate(x, "days", include.lowest = TRUE, attr.breaks = TRUE))
  checkEquals(as.character(y), c("04/01/2014", "04/07/2014"))
  checkEquals(levels(y), c("04/01/2014", "04/02/2014", "04/03/2014", "04/04/2014", "04/05/2014", "04/06/2014", "04/07/2014"))

  x <- mondate.ymd(2014, 4, c(1, 8))
  (y <- cut.mondate(x, "days", attr.breaks = TRUE))
  checkEquals(as.character(y), c(NA, "04/08/2014"))
  checkEquals(levels(y), c("04/02/2014", "04/03/2014", "04/04/2014", "04/05/2014", "04/06/2014", "04/07/2014", "04/08/2014"))

  (y <- cut.mondate(x, "days", include.lowest = TRUE, attr.breaks = TRUE))
  checkEquals(as.character(y), c("04/01/2014", "04/08/2014"))
  checkEquals(levels(y), c("04/01/2014", "04/02/2014", "04/03/2014", "04/04/2014", "04/05/2014", "04/06/2014", "04/07/2014", "04/08/2014"))

  x <- mondate.ymd(2014, 4, c(1, 7))
  (y <- cut.mondate(x, "2 days", attr.breaks = TRUE))
  checkEquals(as.character(y), c(NA, "04/08/2014"))
  checkEquals(levels(y), c("04/04/2014", "04/06/2014", "04/08/2014"))

  (y <- cut.mondate(x, "2 days", include.lowest = TRUE, attr.breaks = TRUE))
  checkEquals(as.character(y), c("04/02/2014", "04/08/2014"))
  checkEquals(levels(y), c("04/02/2014", "04/04/2014", "04/06/2014", "04/08/2014"))

  x <- mondate.ymd(2014, 4, c(1, 8))
  (y <- cut.mondate(x, "2 days", attr.breaks = TRUE))
  checkEquals(as.character(y), c(NA, "04/08/2014"))
  checkEquals(levels(y), c("04/04/2014", "04/06/2014", "04/08/2014"))

  (y <- cut.mondate(x, "2 days", include.lowest = TRUE, attr.breaks = TRUE))
  checkEquals(as.character(y), c("04/02/2014", "04/08/2014"))
  checkEquals(levels(y), c("04/02/2014", "04/04/2014", "04/06/2014", "04/08/2014"))
  checkEqualsNumeric(attr(y, "breaks"), mondate(c("03/31/2014", "04/02/2014", "04/04/2014", "04/06/2014", "04/08/2014")))

  x <- mondate.ymd(2014, 4, c(1, 7))
  (y <- cut.mondate(x, "days", attr.breaks = TRUE, right = FALSE))
  checkEquals(as.character(y), c(NA, "04/06/2014"))
  (z <- cut.mondate(x, attr(y, "breaks"), right = FALSE))
  checkTrue(is.na(tail(z, 1)))
  (z <- cut.mondate(x, attr(y, "breaks"), right = FALSE, include.lowest = TRUE))
  checkEquals(as.character(z), c("[04/01/2014,04/02/2014)", "[04/06/2014,04/07/2014]"))
  (y <- cut.mondate(x, "days", include.lowest = TRUE, attr.breaks = TRUE, right = FALSE))
  checkEquals(as.character(y), c("03/31/2014", "04/06/2014"))
  
  ## Date
  
  x <- as.Date(mondate(0:4)) + 1
  x

  # Check for improper numeric vector
  checkException(cut(x, breaks = c(2, 4)))

  # Note how no NA when breaks is a scalar
  (y1 <- cutmondate(x, breaks = 4, attr.breaks = TRUE))
  checkTrue(is.na(tail(y1, 1)))
  checkEquals(levels(y1), c("[2000-01-01,2000-02-01)", "[2000-02-01,2000-03-01)", "[2000-03-01,2000-04-01)", "[2000-04-01,2000-05-01)"))

  # Contrast when breaks is character
  (y2 <- cutmondate(x, breaks = "months", attr.breaks = TRUE))
  checkTrue(is.na(tail(y2, 1)))
  checkTrue(all(!is.na(head(y2, -1))))
  checkEquals(levels(y2), c("2000-01-01", "2000-02-01", "2000-03-01", "2000-04-01"))
  # Class of 'breaks' attribute is Date
  checkEquals(class(attr(y2, "breaks")), "Date")
  # Cut x again with those Date interval endpoints
  (y3 <- cutmondate(x, breaks = attr(y2, "breaks")))
  checkTrue(is.na(tail(y3, 1)))
  checkTrue(all(!is.na(head(y3, -1))))
  checkEquals(levels(y3), c("[2000-01-01,2000-02-01)", "[2000-02-01,2000-03-01)", "[2000-03-01,2000-04-01)", "[2000-04-01,2000-05-01)"))
  
  # Default for right is FALSE when x is Date
  # If change right = TRUE, levels labeled with date corresponding to 
  #   the right endpoint of the interval .. which looks odd for Date x, so may
  #   not be the best choice for Date x.
  (y4 <- cutmondate(x, breaks = "months", right = TRUE, attr.breaks = TRUE))
  checkTrue(is.na(tail(y4, 1)))
  checkTrue(all(!is.na(head(y4, -1))))
  checkEquals(levels(y4), c("2000-02-01", "2000-03-01", "2000-04-01", "2000-05-01"))
  # Cut x again with those Date interval endpoints
  (y5 <- cutmondate(x, breaks = attr(y4, "breaks")))
  checkTrue(is.na(tail(y5, 1)))
  checkTrue(all(!is.na(head(y5, -1))))
  checkEquals(levels(y5), c("[2000-01-01,2000-02-01)", "[2000-02-01,2000-03-01)", "[2000-03-01,2000-04-01)", "[2000-04-01,2000-05-01)"))
  
  # days
  x <- as.Date(mondate.ymd(2014, 4, c(1, 5)))
  (y <- cutmondate(x, breaks = "2 days", attr.breaks = TRUE))
  checkTrue(is.na(tail(y, 1)))
  checkEquals(levels(y), c("2014-04-01", "2014-04-03"))
  (z <- cutmondate(x, breaks = attr(y, "breaks")))
  checkTrue(is.na(tail(z, 1)))
  checkEquals(levels(z), c("[2014-04-01,2014-04-03)", "[2014-04-03,2014-04-05)"))
  (z <- cutmondate(x, breaks = attr(y, "breaks"), include.lowest = TRUE))
  checkTrue(!is.na(tail(z, 1)))
  checkEquals(levels(z), c("[2014-04-01,2014-04-03)", "[2014-04-03,2014-04-05]"))


  (y <- cutmondate(x, breaks = "days", attr.breaks = TRUE))
  checkTrue(is.na(tail(y, 1)))
  checkEquals(levels(y), c("2014-04-01", "2014-04-02", "2014-04-03", "2014-04-04"))

  (y <- cutmondate(x, breaks = "days", include.lowest = TRUE, attr.breaks = TRUE))
  checkTrue(!is.na(tail(y, 1)))
  checkEquals(levels(y), c("2014-04-01", "2014-04-02", "2014-04-03", "2014-04-04", "2014-04-05"))

  # weeks
  
  # right endpoint on a boundary -> NA
  x <- as.Date(c(mondate.ymd(2014, 3, 31), mondate.ymd(2014, 4, c(1, 5, 14))))
  (y <- cutmondate(x, breaks = "weeks", attr.breaks = TRUE))
  checkEquals(y[1], y[2])
  checkEquals(levels(y), c("2014-03-31", "2014-04-07"))

  (y <- cutmondate(x, breaks = "weeks", right = TRUE, attr.breaks = TRUE))
  checkEquals(y[1], y[2])
  checkEquals(levels(y), c("2014-04-07", "2014-04-14"))

  (y <- cutmondate(x, breaks = "2 weeks", attr.breaks = TRUE))
  checkEquals(y[1], y[2])
  checkEquals(levels(y), c("2014-03-31"))
  
  # right endpoint not on a boundary -> no NA
  x <- as.Date(mondate.ymd(2014, 4, c(1, 5, 15)))
  (y <- cutmondate(x, breaks = "weeks", attr.breaks = TRUE))
  checkEquals(y[1], y[2])
  checkTrue(!is.na(tail(y, 1)))
  checkEquals(levels(y), c("2014-03-31", "2014-04-07", "2014-04-14"))
  
  x <- as.Date(c("2013-07-01", "2014-04-02"))
  (y <- cutmondate(x, "quarters", attr.breaks = TRUE))
  checkEquals(levels(y), c("2013-07-01", "2013-10-01", "2014-01-01", "2014-04-01"))
  checkEquals(c(attr(y, "breaks")), c(as.Date(c("2013-07-01", "2013-10-01", "2014-01-01", "2014-04-01", "2014-07-01"))))

  # POSIXlt

  x <- as.POSIXlt(as.Date(mondate(0:4)) + 1)
  x
   # Note how no NA when breaks is a scalar
  (z1 <- cutmondate(x, breaks = 4, attr.breaks = TRUE))
  checkEquals(z1, y1)
  # Contrast when breaks is character
  (z2 <- cutmondate(x, breaks = "months", attr.breaks = TRUE))
  checkEquals(c(z2), c(y2))
  # Cut x again with those Date interval endpoints
  (z3 <- cutmondate(x, breaks = attr(z2, "breaks")))
  checkEquals(z3, y3)
  (z4 <- cutmondate(x, breaks = "months", right = TRUE, attr.breaks = TRUE))
  checkEquals(c(z4), c(y4))
  # Cut x again with those Date interval endpoints
  (z5 <- cutmondate(x, breaks = attr(z4, "breaks")))
  checkEquals(z5, y5)
  
  # POSIXct

  x <- as.POSIXct(as.Date(mondate(0:4)) + 1)
  x
   # Note how no NA when breaks is a scalar
  (z1 <- cutmondate(x, breaks = 4, attr.breaks = TRUE))
  checkEquals(z1, y1)
  # Contrast when breaks is character
  (z2 <- cutmondate(x, breaks = "months", attr.breaks = TRUE))
  checkEquals(z2, y2)
  # Cut x again with those Date interval endpoints
  (z3 <- cutmondate(x, breaks = attr(z2, "breaks")))
  checkEquals(z3, y3)
  (z4 <- cutmondate(x, breaks = "months", right = TRUE, attr.breaks = TRUE))
  checkEquals(z4, y4)
  # Cut x again with those Date interval endpoints
  (z5 <- cutmondate(x, breaks = attr(z4, "breaks")))
  checkEquals(z5, y5)

  # email to Gabor 5/7/14: revised

  from <- as.POSIXlt("2014-01-01")
  to <- as.POSIXlt("2015-01-01")
  xdata <- seq(from, to, by = "months")
  cutmondate(xdata, breaks = xdata)
  # versus
  cutmondate(xdata, breaks = "months")
  
  from <- as.Date("2014-01-01")
  to <- as.Date("2015-01-01")
  mf <- mondate(from-1)
  mt <- mondate(to-1)
  cutmondate(seq(from, to, by = "months"), breaks = seq(mf, mt), right = FALSE)

  ## end of email

}
