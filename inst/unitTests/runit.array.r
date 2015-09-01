test.array.mondate <- function() {
  z <- mondate("01/01/2014")
  A <- array(0, c(2, 2))
  checkEquals(dim(z + A), c(2, 2))
  checkEquals(c(as.numeric(z + A)), c(rep((2014 - 2000) * 12 + 1 / 31, 4)))
  }
