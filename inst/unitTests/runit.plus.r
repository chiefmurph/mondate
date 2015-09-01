test.plus.mondate <- function() {
  # mondate arithmetic is not distributive.
  checkEquals(mondate("01/01/2014") + 2,     mondate.ymd(2014, 3, 1))
  }
