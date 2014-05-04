test.cut.mondate <- function() {
  y <- cut(mondate(12:24), breaks = 12:24)
  checkEquals(length(levels(y)), 12) 
  }
test.add.mondate <- function() {
  m <- mondate("7/9/2014")
  y <- add(m, 0:12)
  checkEquals(day(y), rep(9, 13))
  m <- mondate("7/31/2014")
  y <- add(m, 0:12)
  checkEquals(day(y), c(31, 31, 30, 31, 30, 31, 31, 28, 31, 30, 31, 30, 31))
  # Check forcelastday
  m <- mondate("2/28/2014")
  y <- add(m, 0:12, forcelastday = FALSE) # default
  checkEquals(day(y), rep(28, 13))
  m <- mondate("2/28/2014")
  y <- add(m, 0:12, forcelastday = TRUE)
  checkEquals(day(y), c(28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 28))
  m <- mondate("2/28/2012")
  y <- add(m, 0:12, forcelastday = TRUE)
  checkEquals(day(y), rep(28, 13)) # TRUE b/c 2012 is a leapyear
  #
  # Check different units
#  y <- add(m, 1:7, units = "days")
#  checkEquals(y, mondate.ymd(2014, 3, 1:7), checkNames = FALSE)
  }
test.subtract.mondate <- function() {
  m <- mondate("2/28/2014")
  y <- subtract(m, 0:12, forcelastday = TRUE)
  checkEquals(day(y), rev(c(28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 28)))
  }
