setGeneric("seqmondate", function(from, to, ...) standardGeneric("seqmondate"))
setMethod("seqmondate", c("mondate", "mondate"), function(from, to, ...) seq.mondate(from, to, ...))
setMethod("seqmondate", c("Date", "Date"), function(from, to, ...)
  as.Date(seq(mondate(as.numeric(mondate(from))),# - .02), 
              mondate(as.numeric(mondate(to  ))),# - .02),
              ...)
          )
  )
setMethod("seqmondate", c("POSIXlt", "POSIXlt"), function(from, to, ...) 
  structure(as.POSIXlt(seqmondate(as.Date(from), as.Date(to), ...)), tzone = attr(from, "tzone"))
  )
setMethod("seqmondate", c("POSIXct", "POSIXct"), function(from, to, ...) 
  as.POSIXct(seqmondate(as.POSIXlt(from), as.POSIXlt(to), ...))
  )
