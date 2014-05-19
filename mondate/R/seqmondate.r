setGeneric("seqmondate", function(from, ..., right) standardGeneric("seqmondate"))
setMethod("seqmondate", "mondate", function(from, ..., right = TRUE) seq(from, ..., right = right))
setMethod("seqmondate", "Date", function(from, ..., right = FALSE) as.Date(seq(mondate(from - 1), ..., right = right)) + 1)
setMethod("seqmondate", "Date", function(from, ..., right = FALSE) as.Date(seq(mondate(from - 1), ..., right = right)) + 1)
setMethod("seqmondate", "POSIXlt", function(from, ..., right = FALSE) structure(as.POSIXlt(seqmondate(as.Date(from), ..., right = right)), tzone = attr(from, "tzone")))
setMethod("seqmondate", "POSIXct", function(from, ..., right = FALSE) as.POSIXct(seqmondate(as.POSIXlt(from), ..., right = right)))

