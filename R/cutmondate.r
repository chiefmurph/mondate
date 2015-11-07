setGeneric("cutmondate", function(x, breaks = "months", right = TRUE, 
                                  include.lowest = TRUE, ...) 
  standardGeneric("cutmondate"))
setMethod("cutmondate", c("Date", "character"), function(x, breaks, right = FALSE, 
                                                         include.lowest = TRUE, ...) {
  if (length(breaks) > 1) stop("invalid specification for 'breaks'")
  by2 <- strsplit(breaks[1L], " ", fixed = TRUE)[[1L]]
  if (length(by2) > 2L || length(by2) < 1L) stop("invalid specification for 'breaks'")
  valid <- pmatch(by2[length(by2)], c("days", "weeks", "months", "years", "quarters"))
  if (valid %in% 3:5)
    cut.mondate(mondate(x, displayFormat = "%Y-%m-%d"), breaks = breaks, right = right, 
                include.lowest = include.lowest, ...)
  else cut(x, breaks = breaks, include.lowest = include.lowest, ...)
})
setMethod("cutmondate", c("Date", "missing"), function(x, breaks, right = FALSE, 
                                                       include.lowest = TRUE, ...) {
  cut.mondate(mondate(x, displayFormat = "%Y-%m-%d"), breaks = "months", right = right, 
              include.lowest = include.lowest, ...)
})
setMethod("cutmondate", c("Date", "ANY"), function(x, breaks, right = FALSE, 
                                                   include.lowest = TRUE, ...) {
  cut(x, breaks = breaks, include.lowest = include.lowest, ...)
})
setMethod("cutmondate", "mondate", function(x, breaks, right = TRUE, 
                                            include.lowest = TRUE, ...){
  cut.mondate(x, breaks = breaks, right = right, include.lowest = include.lowest, ...)
})
setMethod("cutmondate", "POSIXt", function(x, breaks, right = FALSE, 
                                           include.lowest = TRUE, ...) {
  cutmondate(as.Date(x), breaks = breaks, include.lowest = include.lowest, ...)
})

