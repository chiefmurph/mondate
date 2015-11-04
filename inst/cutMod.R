gcut <- function(x, step = 1, startmonth = NULL, include.lowest = TRUE,
                 right = TRUE, attr.breaks = FALSE) {
  if (!include.lowest) stop(
    "!include.lowest is ignored when breaks is character")
  
  x <- as.numeric(x)
  nx <- ceiling(x)
  rngnx <- range(nx)
  rngx <- range(x)
  res <- 
    if (is.null(startmonth)) {
      if (right) {
        maxx <- rngx[2L]
        factor(maxx - (maxx - x) %/% step * step)
      }
      else {
        minx <- rngx[1L]
        factor(minx + (x - minx) %/% step * step)
      }
    }
  else
  if (right) {
    intv <- (rngnx - startmonth) %/% step * step + startmonth + step - 1 - c(step, 0)
    breaks <- seq(intv[1L], intv[2L], by = step)
    cut(x, breaks)
  }
  else {
    intv <- (rngnx - startmonth) %/% step * step + startmonth + c(0, step)
    breaks <- seq(intv[1L], intv[2L], by = step)
    cut(x, breaks)
    #    factor((x - startmonth) %/% step * step + startmonth)
  }
  breaks <- mondate(breaks)
  levels(res) <- if (right) breaks[-1L] else 
    add(breaks[-length(breaks)] - 1, 1, "days")
  if (attr.breaks) attr(res, "breaks") <- breaks
  res
}