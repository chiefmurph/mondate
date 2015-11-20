asof <- function(x, format) {
  if (!missing(format)) {
    h <- gregexpr("%H", format)[[1]][1]
    if (h > 0) format <- substr(format, 1, h - 1)
  }
  as.Date(x, format = format) + 1
}
x <- "2015-12-31"
asof(x)
asof("2015-12-31 24:00:00", format = "%Y-%m-%d %H:%M:%S")
