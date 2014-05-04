test.cut.mondate <- function() {
  y <- cut(mondate(12:24), breaks = 12:24)
  checkEquals(length(levels(y)), 12) 
  }
