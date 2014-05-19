setGeneric("cutmondate", function(x, breaks, labels= NULL,
                                          include.lowest = FALSE, 
                                          right = TRUE,
                                          start.on.monday = TRUE,
                                          attr.breaks = FALSE,
                                          ...) standardGeneric("cutmondate"))

setMethod("cutmondate", "Date", function(x, breaks, labels = NULL,
                                          include.lowest = FALSE, 
                                          right,
                                          start.on.monday = TRUE,
                                          attr.breaks = FALSE,
                                          ...) {
  if (missing(right)) right <- FALSE
  if (inherits(breaks, "POSIXt")) breaks <- as.Date(breaks)
  breaksDate <- inherits(breaks, "Date")
  if (breaksDate) {
    brksDate <- breaks
    breaks <- mondate(breaks - 1, displayFormat = .displayFormat["EU"])
    res <- cut(mondate(x - 1, displayFormat = .displayFormat["EU"]), 
           breaks = breaks, labels = labels, 
           include.lowest = include.lowest, right = right, 
           attr.breaks = TRUE, ...)
    b <- attr(res, "breaks")
    le <- attr(b, "lechar")
    re <- attr(b, "rechar")
    b <- as.Date(attr(res, "breaks")) + 1
    inside <- sapply(1:(length(b) - 1), function(i)
                     paste(as.character(c(b[i], b[i+1L])), collapse = ","))
    levels(res) <- paste(le, inside, re, sep = "")
    if (attr.breaks) attr(res, "breaks") <- brksDate
    else attr(res, "breaks") <- NULL
    }
  else 
  if (is.character(breaks)) { #character
    if (length(breaks) > 1) stop("invalid specification for 'breaks'")
    by2 <- strsplit(breaks, " ", fixed = TRUE)[[1L]]
    if (length(by2) > 2L || length(by2) < 1L) stop("invalid specification for 'breaks'")
    valid <- pmatch(by2[length(by2)], c("days", "weeks", "months", "years", "quarters"))
    if (is.na(valid)) stop("invalid specification for 'breaks'")
    step <- ifelse(length(by2) == 2L, as.integer(by2[1L]), 1L)
    
    if (length(x) == 1) { # must force include.lowest = TRUE for "integral" x
                          # = 1 second of the shortest day = 1/31/3600
      if (abs(x - round(x, 0)) < 8.960573e-06 &
          !missing(include.lowest) & 
          !include.lowest) 
        stop("include.lowest = FALSE and scalar x are incompatible when x is near a month boundary")
      include.lowest <- TRUE
      }

    if (valid == 1) { # days
      x <- as.numeric(x)
      int <- step
      rngx <- range(x)
      breaks <- seq(from = rngx[1],
                    to =   ceiling(rngx[2] / int) * int + ifelse(include.lowest, int, 0), 
                    by = int)
      res <- cut.default(x, breaks = breaks, 
                         labels = labels, right = FALSE, 
                         include.lowest = FALSE, ...)
      breaks <- as.Date(breaks, "1970-01-01")
      # label appropriately
      if (is.null(labels)) levels(res) <- if (right) breaks[-1]
                                          else head(breaks, -1)
      if (attr.breaks) {
        n <- length(breaks) - 1
        lechar <- rep(ifelse(right, "(", "["), n)
        rechar <- rep(ifelse(right, "]", ")"), n)
        if (include.lowest) 
          if (right) lechar[1] <- "["
          else rechar[n] <- "]"
        attr(breaks, "lechar") <- lechar
        attr(breaks, "rechar") <- rechar
        attr(res, "breaks") <- breaks
        }
      }
    else
    if (valid == 2) { # weeks
      x <- as.numeric(as.Date(x))
      int <- step * 7
      rngx <- range(x)
      # 3 is the magic number for adjusting to mondays
      breaks <- seq(from = ((rngx[1] - 4) %/% int) * int + 4 - ifelse(start.on.monday, 0, 1),
                    to =   ceiling((rngx[2] - 4) / int) * int + 4 + ifelse(include.lowest, int, 0) , 
                    by = int)
      res <- cut.default(x, breaks = breaks, 
                         labels = labels, right = FALSE, 
                         include.lowest = FALSE, ...)
      breaks <- as.Date(breaks, "1970-01-01")
      if (is.null(labels)) levels(res) <- if (right) breaks[-1]
                                          else head(breaks, -1)
      if (attr.breaks) {
        n <- length(breaks) - 1
        lechar <- rep(ifelse(right, "(", "["), n)
        rechar <- rep(ifelse(right, "]", ")"), n)
        attr(breaks, "lechar") <- lechar
        attr(breaks, "rechar") <- rechar
        attr(res, "breaks") <- breaks
        }
      }
    else { # 3: month, 4: year, 5: quarter
      valid <- valid - 2
      int <- c(1, 12, 3)[valid] * step
      mx <- mondate(x - 1, displayFormat = .displayFormat["EU"])
      rngx <- range(mx)
      breaks <- seq(from = floor(rngx[1] / int) * int,
                    to =   ceiling(rngx[2] / int) * int + ifelse(include.lowest, int, 0), 
                    by = breaks)
      res <- cut(mx, 
                 breaks = breaks, labels = labels, 
                 include.lowest = include.lowest, right = FALSE, 
                 start.on.monday = start.on.monday, attr.breaks = TRUE, ...)
      if (is.null(labels)) levels(res) <- if (right) as.character(as.Date(attr(res, "breaks")) + 1)[-1]
                                          else as.character(head(as.Date(attr(res, "breaks")) + 1, -1))
      if (attr.breaks) attr(res, "breaks") <- structure(as.Date(attr(res, "breaks")) + 1, 
                                              lechar = attr(attr(res, "breaks"), "lechar"), 
                                              rechar = attr(attr(res, "breaks"), "rechar"))
      else attr(res, "breaks") <- NULL
      }
    } # end character
  else 
  if (is.numeric(breaks)) {# numeric. Must be a scalar
    if (length(breaks) > 1) stop("Only scalars allowed for numeric 'breaks'")
    res <- cut.mondate(mondate(x - 1, displayFormat = .displayFormat["EU"]), 
           breaks = breaks, labels = labels, 
           include.lowest = include.lowest, right = right, 
           start.on.monday = start.on.monday, attr.breaks = TRUE, ...)
    b <- attr(res, "breaks")
    lechar <- attr(b, "lechar")
    rechar <- attr(b, "rechar")
    b <- as.Date(b) + 1
    if (is.null(labels)) {
      inside <- sapply(1:(length(b) - 1), function(i)
                       paste(as.character(c(b[i], b[i+1L])), collapse = ","))
      levels(res) <- paste(lechar, inside, rechar, sep = "")
      }
    if (attr.breaks) attr(res, "breaks") <- b
    else attr(res, "breaks") <- NULL
  }

  else stop("Invalid mode for 'breaks'")

  res
  
})

setMethod("cutmondate", "POSIXlt", function(x,  breaks, labels = NULL,
                                          include.lowest = FALSE, 
                                          right = FALSE,
                                          start.on.monday = TRUE,
                                          attr.breaks = FALSE,
                                          ...) {
  res <- cutmondate(as.Date(x),  breaks= breaks, 
              labels = labels,
              include.lowest = include.lowest, 
              right = right,
              start.on.monday = start.on.monday,
              attr.breaks = attr.breaks,
              ...)
  if (attr.breaks) {
    z <- as.POSIXlt(attr(res, "breaks"), tzone = attr(x, "tzone"))
    attr(z, "lechar") <- attr(attr(res, "breaks"), "lechar")
    attr(z, "rechar") <- attr(attr(res, "breaks"), "rechar")
    attr(res, "breaks") <- z
    }
  res
  })
  
setMethod("cutmondate", "POSIXct", function(x,  breaks, labels = NULL,
                                          include.lowest = FALSE, 
                                          right = FALSE,
                                          start.on.monday = TRUE,
                                          attr.breaks = FALSE,
                                          ...) {
  res <- cutmondate(as.Date(x),  breaks= breaks, 
              labels = labels,
              include.lowest = include.lowest, 
              right = right,
              start.on.monday = start.on.monday,
              attr.breaks = attr.breaks,
              ...)
#  if (attr.breaks) attr(res, "breaks") <- structure(as.POSIXct(res), lechar=attr(res,"lechar"), rechar=attr(res,"lechar"))
  if (attr.breaks) attr(res, "breaks") <- as.POSIXct(attr(res, "breaks"))
  res
  })
  
setMethod("cutmondate", "mondate", function(x,  breaks, labels = NULL,
                                          include.lowest = FALSE, 
                                          right = TRUE,
                                          start.on.monday = TRUE,
                                          attr.breaks = FALSE,
                                          ...) 
  cut.mondate(x,  breaks= breaks, 
              labels = labels,
              include.lowest = include.lowest, 
              right = right,
              start.on.monday = start.on.monday,
              attr.breaks = attr.breaks,
              ...))
