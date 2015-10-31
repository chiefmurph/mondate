test.cut.mondate <- function() {

  # numeric, cut default. For comparison, not testing.
  x <- 0:4
  cut(x, breaks = x)
  cut(x, breaks = x, include.lowest = T)
  cut(x, breaks = 2)
  cut(x, breaks = 2, include.lowest = T)
  # 4 levels, but not of unit width. Explanation from ?cut
  #   When breaks is specified as a single number, 
  #   the range of the data is divided into breaks pieces 
  #   of equal length, and then the outer limits are moved 
  #   away by 0.1% of the range to ensure that the extreme 
  #   values both fall within the break intervals. 
  cut(x, breaks = 4)

  seq(min(x), max(x), length = 3)
  cut(x, breaks = seq(min(x), max(x), length = 3))
  cut(x, breaks = seq(min(x), max(x), length = 3), include.lowest = T)

  
  # mondate

  x <- mondate(0:4)
  (y <- cut(x, x))
#  checkEquals(levels(y), c("(12/31/1999,01/31/2000]","(01/31/2000,02/29/2000]","(02/29/2000,03/31/2000]","(03/31/2000,04/30/2000]"))
  (y <- cut(x, x, right = FALSE))
#  checkEquals(levels(y), c("[12/31/1999,01/31/2000)","[01/31/2000,02/29/2000)","[02/29/2000,03/31/2000)","[03/31/2000,04/30/2000)"))

  # 4 levels of unit width. Differs from cut.default.
  (y <- cut.mondate(x, breaks = 4, attr.breaks = TRUE))
#  checkTrue(is.na(y[1]))
#  checkEquals(levels(y), c("(12/31/1999,01/31/2000]", "(01/31/2000,02/29/2000]", "(02/29/2000,03/31/2000]", "(03/31/2000,04/30/2000]"))
  (y <- cut.mondate(x, breaks = 4, include.lowest = TRUE))
#  checkTrue(!is.na(y[1]))
#  checkEquals(levels(y), c("[12/31/1999,01/31/2000]", "(01/31/2000,02/29/2000]", "(02/29/2000,03/31/2000]", "(03/31/2000,04/30/2000]"))




  (y <- cut(x, "months"))
#  checkTrue(is.na(y[1]))
#  checkEquals(levels(y), c("01/31/2000", "02/29/2000", "03/31/2000", "04/30/2000"))
  (y <- cut(x, "months", right = FALSE))
#  checkTrue(is.na(y[1]))
#  checkEquals(levels(y), c("12/31/1999", "01/31/2000", "02/29/2000", "03/31/2000"))
  (y <- cut(x, "months", right = TRUE, include.lowest = TRUE))
#  checkTrue(!is.na(y[1]))
#  checkEquals(levels(y), c("12/31/1999", "01/31/2000", "02/29/2000", "03/31/2000", "04/30/2000"))
  (y <- cut(x, "months", right = TRUE, include.lowest = TRUE, attr.breaks = TRUE))
#  checkEqualsNumeric(attr(y, "breaks"), mondate(-1:4))
  
  (y <- cut(x, attr(y, "breaks"), right = TRUE, include.lowest = TRUE))
#  checkEquals(levels(y), c("[11/30/1999,12/31/1999]", "(12/31/1999,01/31/2000]", "(01/31/2000,02/29/2000]", "(02/29/2000,03/31/2000]", "(03/31/2000,04/30/2000]"))

  # Test for non-NA when scalar x on month boundary
  x <- mondate.ymd(2008, 6)
  y <- cut(x, "month", right = TRUE)
#  checkTrue(!is.na(y))
#  checkEquals(levels(y), "06/30/2008")
  y <- cut(x, "month", right = FALSE)
#  checkTrue(!is.na(y))
#  checkEquals(levels(y), "05/31/2008")

  # "weeks"
  x <- c(mondate.ymd(2014, 3, 30:31), mondate.ymd(2014, 4, 1:30))
  (y <- cut.mondate(x, "weeks", attr.breaks = TRUE))
#  checkTrue(is.na(y[1]))
#  checkEquals(levels(y), c("04/06/2014", "04/13/2014", "04/20/2014", "04/27/2014", "05/04/2014"))
  (y <- cut.mondate(x, "weeks", attr.breaks = TRUE, right = FALSE))
#  checkTrue(is.na(y[1]))
#  checkEquals(levels(y), c("03/30/2014", "04/06/2014", "04/13/2014", "04/20/2014", "04/27/2014"))

  x <- c(mondate.ymd(2014, 3, 31), mondate.ymd(2014, 4, 1:30))
  (y <- cut.mondate(x, "weeks", attr.breaks = TRUE))
#  checkTrue(!is.na(y[1]))
#  checkEquals(levels(y), c("04/06/2014", "04/13/2014", "04/20/2014", "04/27/2014", "05/04/2014"))

  x <- c(mondate.ymd(2014, 3, 30:31), mondate.ymd(2014, 4, 1:30))
  (y <- cut.mondate(x, "weeks", include.lowest = TRUE, attr.breaks = TRUE))
#  checkTrue(!is.na(y[1]))
#  checkEquals(levels(y), c("03/30/2014", "04/06/2014", "04/13/2014", "04/20/2014", "04/27/2014", "05/04/2014"))
  
  x <- c(mondate.ymd(2014, 3, 30:31), mondate.ymd(2014, 4, 1:30))
  (y <- cut.mondate(x, "weeks", start.on.monday = FALSE, attr.breaks = TRUE))
#  checkTrue(!is.na(y[1]))
#  checkEquals(as.character(y[1]), "04/05/2014")
#  checkEquals(levels(y), c("04/05/2014", "04/12/2014", "04/19/2014", "04/26/2014", "05/03/2014"))
  
  # 2 weeks
  x <- c(mondate.ymd(2014, 3, 30:31), mondate.ymd(2014, 4, 1:30))
  (y <- cut.mondate(x, "2 weeks", attr.breaks = TRUE))
#  checkTrue(is.na(y[1]))
#  checkEquals(levels(y), c("04/13/2014", "04/27/2014", "05/11/2014"))
  (y <- cut.mondate(x, "2 weeks", start.on.monday = FALSE, attr.breaks = TRUE))
#  checkTrue(!is.na(y[1]))
#  checkEquals(levels(y), c("04/12/2014", "04/26/2014", "05/10/2014"))
  
  # "days"
  x <- mondate.ymd(2014, 4, c(1, 7))
  (y <- cut.mondate(x, "days", attr.breaks = TRUE))
#  checkEquals(as.character(y), c(NA, "04/07/2014"))
#  checkEquals(levels(y), c("04/02/2014", "04/03/2014", "04/04/2014", "04/05/2014", "04/06/2014", "04/07/2014"))
  (y <- cut.mondate(x, "days", include.lowest = TRUE, attr.breaks = TRUE))
#  checkEquals(as.character(y), c("04/01/2014", "04/07/2014"))
#  checkEquals(levels(y), c("04/01/2014", "04/02/2014", "04/03/2014", "04/04/2014", "04/05/2014", "04/06/2014", "04/07/2014"))

  x <- mondate.ymd(2014, 4, c(1, 8))
  (y <- cut.mondate(x, "days", attr.breaks = TRUE))
#  checkEquals(as.character(y), c(NA, "04/08/2014"))
#  checkEquals(levels(y), c("04/02/2014", "04/03/2014", "04/04/2014", "04/05/2014", "04/06/2014", "04/07/2014", "04/08/2014"))

  (y <- cut.mondate(x, "days", include.lowest = TRUE, attr.breaks = TRUE))
#  checkEquals(as.character(y), c("04/01/2014", "04/08/2014"))
#  checkEquals(levels(y), c("04/01/2014", "04/02/2014", "04/03/2014", "04/04/2014", "04/05/2014", "04/06/2014", "04/07/2014", "04/08/2014"))

  x <- mondate.ymd(2014, 4, c(1, 7))
  (y <- cut.mondate(x, "2 days", attr.breaks = TRUE))
#  checkEquals(as.character(y), c(NA, "04/08/2014"))
#  checkEquals(levels(y), c("04/04/2014", "04/06/2014", "04/08/2014"))

  (y <- cut.mondate(x, "2 days", include.lowest = TRUE, attr.breaks = TRUE))
#  checkEquals(as.character(y), c("04/02/2014", "04/08/2014"))
#  checkEquals(levels(y), c("04/02/2014", "04/04/2014", "04/06/2014", "04/08/2014"))

  x <- mondate.ymd(2014, 4, c(1, 8))
  (y <- cut.mondate(x, "2 days", attr.breaks = TRUE))
#  checkEquals(as.character(y), c(NA, "04/08/2014"))
#  checkEquals(levels(y), c("04/04/2014", "04/06/2014", "04/08/2014"))

  (y <- cut.mondate(x, "2 days", include.lowest = TRUE, attr.breaks = TRUE))
#  checkEquals(as.character(y), c("04/02/2014", "04/08/2014"))
#  checkEquals(levels(y), c("04/02/2014", "04/04/2014", "04/06/2014", "04/08/2014"))
#  checkEqualsNumeric(attr(y, "breaks"), mondate(c("03/31/2014", "04/02/2014", "04/04/2014", "04/06/2014", "04/08/2014")))

  x <- mondate.ymd(2014, 4, c(1, 7))
  (y <- cut.mondate(x, "days", attr.breaks = TRUE, right = FALSE))
#  checkEquals(as.character(y), c(NA, "04/06/2014"))
  (z <- cut.mondate(x, attr(y, "breaks"), right = FALSE))
#  checkTrue(is.na(tail(z, 1)))
  (z <- cut.mondate(x, attr(y, "breaks"), right = FALSE, include.lowest = TRUE))
#  checkEquals(as.character(z), c("[04/01/2014,04/02/2014)", "[04/06/2014,04/07/2014]"))
  (y <- cut.mondate(x, "days", include.lowest = TRUE, attr.breaks = TRUE, right = FALSE))
#  checkEquals(as.character(y), c("03/31/2014", "04/06/2014"))
}
