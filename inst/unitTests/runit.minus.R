test.minus.mondate <- function() {
  # mondate arithmetic is not distributive.
  checkEquals(mondate.ymd(2014, 3, 1) - mondate("01/01/2014"), as.difftime(2, units = "months"))
  }
