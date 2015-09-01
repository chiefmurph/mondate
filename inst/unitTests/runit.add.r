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
  m <- mondate("2/28/2014")
  y <- add(m, 1:7, units = "days")
  checkEquals(y, mondate.ymd(2014, 3, 1:7))
  m <- mondate("3/31/2014")
  y <- add(m, 1:4, units = "years")
  checkEquals(y, mondate.ymd(2015:2018, 3))
  m <- mondate("3/31/2014")
  y <- add(m, 1:8, units = "quarters")
  checkEquals(y, c(mondate.ymd(2014, c(6, 9, 12)), 
                   mondate.ymd(2015, seq(3, 12, by = 3)), 
                   mondate("3/31/2016")))
  }
test.subtract.mondate <- function() {
  m <- mondate("2/28/2014")
  y <- subtract(m, 0:12, forcelastday = TRUE)
  checkEquals(day(y), rev(c(28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 28)))
  y <- subtract(m, 1:4, units = "years")
  checkEquals(y, mondate.ymd(2013:2010, 2, 28))
  y <- subtract(m, 1:4, units = "years", forcelastday = TRUE)
  checkEquals(y, add(mondate.ymd(2013:2010, 2, 28), c(0, 1, 0, 0), units = "days"))
  }
