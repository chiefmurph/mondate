test.cut.mondate <- function() {
  #require(RUnit)
  x <- mondate.ymd(2015, 1:12)
  res <- cut(x, breaks = 1)
  checkTrue(is.null(attr(res, "breaks")))
  res <- cut(x, breaks = 1, attr.breaks = TRUE)
  checkTrue(!is.null(attr(res, "breaks")))
}
