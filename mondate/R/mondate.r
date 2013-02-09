### mondate.r
### S4 class to store and calculate dates in terms of months, 
###     and fractional parts thereof.

### - Dan Murphy, June 1, 2010

##    Copyright (C) <2010>  <Daniel Murphy>

##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


##  SCALARS

.mondate.tolerance <- .Machine$double.eps^0.5
.motbl<-c(31,28,31,30,31,30,31,31,30,31,30,31)

.mondate.origin <- "1999-12-31"
    .mondate.year.zero <- 2000
    .ISO.origin <- "1899-12-31"
    .ISO.year.zero <- 1900
    .origin.diff.years <- .mondate.year.zero-.ISO.year.zero
    .mondate.days.zero <- as.numeric(as.Date(.mondate.origin))
    .as.Date.origin <- "1970-1-1"

.displayFormat <- c(US="%m/%d/%Y", # US format
                   USb="%m-%d-%Y", 
                   EU="%Y-%m-%d", # EU format
                   EUb="%Y/%m/%d")

.default.displayFormat <- ifelse (
    (length(grep("United States",Sys.getlocale("LC_TIME"))) +      # windows
     length(grep("en_US",        Sys.getlocale("LC_TIME")))) > 0L, # mac os X
        .displayFormat[1L], .displayFormat[3L]
    )
.default.timeunits <- "months"

.date.classes <- c("Date","POSIXt","POSIXct","POSIXlt")

.infinite.strings <- c("Inf", "-Inf")

##  USEFUL INTERNAL FUNCTIONS

# fyi: yr=Inf is not a leap year
.trim <- function(x) sub("[[:space:]]+$", "", sub("^[[:space:]]+", "", x))
.is.leapyear<-function(yr) yr%%400==0 | (yr%%4==0 & yr%%100!=0)
.daysinmonth<-function(yr,mo){
    # fixed bug when mo is NA 8/21/2010
    if (length(yr) > length(mo)) mo <- rep(mo, length(yr) / length(mo) + 1)[1:length(yr)]
    else
    if (length(yr) < length(mo)) yr <- rep(yr, length(mo) / length(yr) + 1)[1:length(mo)]
    nna <- !is.na(yr) & !is.na(mo)
    # fixed a bug when yr or mo is NA 1/22/12: assigning numnna inside the 
    #   following if statement doesn't work sometimes
    numnna <- sum(nna)
    if (numnna == 0L) return(rep(NA, length(mo)))
    ina <- !nna
    N <- length(mo)
    mo <- mo[nna]
    yr <- yr[nna]
    # infinite month will produce NA's with a warning
    days <- .motbl[mo]
    days[.is.leapyear(yr)&mo==2] <- 29
    days[is.infinite(yr)]<-Inf # new as of 8/19/2010
    if (numnna > 0) {
        daze <- rep(NA, N)
        daze[nna] <- days
        daze
        }
    else days
    }

##  THE CLASS

setClass("mondate",
    representation(
        displayFormat = "character",
        timeunits = "character"),
    contains = "numeric",
    prototype = prototype(
        numeric(0),
        displayFormat = character(0),
        timeunits = character(0)),
    S3methods = TRUE
    )

## S4 METHODS

## SLOT ACCESS
setGeneric("mondateDisplayFormat", function(x) standardGeneric("mondateDisplayFormat"))
setMethod("mondateDisplayFormat","mondate", function(x) x@displayFormat)
setMethod("mondateDisplayFormat","ANY", function(x) NULL)
setGeneric("mondateDisplayFormat<-",function(x,value) standardGeneric("mondateDisplayFormat<-"))
setReplaceMethod("mondateDisplayFormat", "mondate", function(x, value) { 
    x@displayFormat <- value
    x 
    })
setGeneric("displayFormat", function(x) standardGeneric("displayFormat"))
setMethod("displayFormat","mondate", function(x) x@displayFormat)
setMethod("displayFormat","ANY", function(x) NULL)
setGeneric("displayFormat<-",function(x,value) standardGeneric("displayFormat<-"))
setReplaceMethod("displayFormat", "mondate", function(x, value) { 
    x@displayFormat <- value
    x 
    })

setGeneric("mondateTimeunits", function(x) standardGeneric("mondateTimeunits"))
setMethod("mondateTimeunits","mondate", function(x) x@timeunits)
setMethod("mondateTimeunits","ANY", function(x) NULL)
setGeneric("mondateTimeunits<-",function(x,value) standardGeneric("mondateTimeunits<-"))
setReplaceMethod("mondateTimeunits", "mondate", function(x, value) { 
    x@timeunits <- value
    x 
    })
setGeneric("timeunits", function(x) standardGeneric("timeunits"))
setMethod("timeunits","mondate", function(x) x@timeunits)
setMethod("timeunits","ANY", function(x) NULL)
setGeneric("timeunits<-",function(x,value) standardGeneric("timeunits<-"))
setReplaceMethod("timeunits", "mondate", function(x, value) { 
    x@timeunits <- value
    x 
    })

# CONVERSION TO MONDATE

#setGeneric("mondate", function(x, displayFormat=.default.displayFormat, 
#                               timeunits=.default.timeunits, ...) 
#                      standardGeneric("mondate"))
setGeneric("mondate", function(x, 
    displayFormat = getOption("mondate.displayFormat", default = .default.displayFormat), 
    timeunits = getOption("mondate.timeunits", default = .default.timeunits), 
    ...) standardGeneric("mondate"))

setMethod("mondate", "mondate", function(x, displayFormat, timeunits, ...) {
    new("mondate",
        x@.Data,
        displayFormat = if (missing(displayFormat)) x@displayFormat else displayFormat,
        timeunits = if(missing(timeunits)) x@timeunits else timeunits)
    })

setMethod("mondate", "numeric", function(x, displayFormat, timeunits, ...) {
    if (timeunits == "months") new("mondate", x, displayFormat = displayFormat, timeunits = timeunits, ...)
    else
    if (timeunits=="years")  new("mondate", 12 * x, displayFormat = displayFormat, timeunits = timeunits, ...)
    else {
        # x represents the number of days since beginning of 01/01/2000
        x <- as.POSIXlt(as.Date(x, origin = .mondate.origin))
        new("mondate", (x$year - .origin.diff.years) * 12 + x$mon + x$mday /
                       .daysinmonth(x$year + .ISO.year.zero, x$mon + 1),
            displayFormat = displayFormat,
            timeunits = timeunits)
        }
    })

# Date conversion
# At this writing (2/10/2012) mondates work best if they correspond to
#   a day, specifically, the instant of time as of the close of business
#   on a day. Technically, since the underlying data type is numeric,
#   mondates "could" represent time to minutes and seconds, but that is
#   not currently the intended use of the mondate class.
.date.to.mondate <- function(x, displayFormat, timeunits, ...) {
    x <- as.POSIXlt(x, ...)
    # Note that per ISO standard, x is time since 1900; i.e.,
    #   close of business 12/31/1899.
    new("mondate", (x$year - .origin.diff.years) * 12 + x$mon + x$mday /
                   .daysinmonth(x$year + .ISO.year.zero, x$mon + 1),
                   displayFormat = displayFormat,
                   timeunits = timeunits)
    }
setMethod("mondate", "Date",   .date.to.mondate)
setMethod("mondate", "POSIXt", .date.to.mondate)

setMethod("mondate", "character", function(x, displayFormat = "keep", timeunits, format, ...) {
    # format is the user's requested format for converting the character
    #   into a date
    # If missing, then we'll attempt to determing the best conversion format
    #   based on the first non-NA value in x, 
    #   and we'll retain that format as the display format (default = "keep").
    # m is the first non-NA value in x. 
    isnax <- is.na(x)
    m <- match(TRUE, !isnax)
    if (is.na(m)) # all-NA input
        return(mondate(as.Date(rep(NA, length(x))), displayFormat = displayFormat, timeunits = timeunits, ...))
    # When no date conversion format is specified, find the first format that
    #   can convert the input to a Date
  mf <- missing(format)
  if (!mf && length(format) > 1) {
    format <- format[1L]
    warning("length(format) > 1, only first element used.")
    }
    if (mf) {
        # Find best format for converting this character to date
        for (i in 1:length(.displayFormat)) {
            d <- as.Date(x[m], format = .displayFormat[i], ...)
            if (!is.na(d)) break
            }
        if (is.na(d)) {
            msg <- paste("mondate character: first non-NA element '", x[m], "' is not a date.", sep = "")
            msg <- c(msg, "\nConverting to numeric, then to mondate. Try specifying 'format'.")
            if (displayFormat == "keep") {
                displayFormat <- getOption("mondate.displayFormat", default = .default.displayFormat)
                msg <- c(msg, '\ndisplayFormat = "keep" not applicable, using default.')
                }
            warning(msg) 
            return(mondate(as.numeric(x), displayFormat = displayFormat, timeunits = timeunits, ...))
            }
        format <- .displayFormat[i]
        if (displayFormat == "keep") displayFormat <- format # else goes to default
        }
    # Use format to convert the character
    y <- as.Date(x, format = format, ...)
  isnay <- is.na(y)
  wisnayx <- which(isnay & !isnax)
  if (length(wisnayx)) {
    # Check if the "new" NA's after conversion coincide with Inf
    ty <- .trim(x[wisnayx])
    infty <- ty %in% .infinite.strings
    winfty <- which(infty)
    }
  if (!mf) displayFormat <- format
  z <- .date.to.mondate(y, displayFormat = displayFormat, timeunits = timeunits)
  if (length(wisnayx)) {
    if (length(winfty)) z[wisnayx[winfty]]@.Data <- as.numeric(ty[infty])
    
    }
  if (any(is.na(z) & !isnax)) warning("format '", format, "' did not convert some characters into dates")
  z
  })

setMethod("mondate", "factor", function(x, displayFormat = "keep", timeunits, ...) mondate(as.character(x), displayFormat, timeunits, ...))

# mondates can hold their shape if they have dim attributes
setMethod("mondate", "array", function(x, displayFormat, timeunits, ...)
    structure(mondate(c(x), displayFormat, timeunits, ...), dim = dim(x), dimnames = dimnames(x)))

setMethod("mondate", "missing", function(x, displayFormat, timeunits, ...) new("mondate", displayFormat = displayFormat, timeunits = timeunits))

setMethod("mondate", "ANY", function(x, displayFormat, timeunits, ...) {
    warning("Attempting to convert class '", class(x),
                "' to 'mondate' via 'as.Date' then 'as.numeric'. Check results!",
                call. = FALSE)
    y <- tryCatch(as.Date(x, ...),
        error = function(e) tryCatch(as.numeric(x),
            error = function(e)
                stop("Cannot convert class '", class(x), "' to class 'mondate'")
            )
        )
    mondate(y, displayFormat = displayFormat, timeunits = timeunits)
    })


# Use S4 to simulate S3-type "as.mondate" behavior
as.mondate <- function(x, ...) mondate(x, ...)

# CONVERSION FROM MONDATE

as.Date.mondate <- function(x, ...) {
    # 1/22/12 Removed attributes of x since Dates can't hold their shape
    x <- c(unclass(x))
    # 3/8/12 If a mondate is assigned to the 'names' attribute of an ojbect 
    #   within a 'structure' call, then when 'attributes(object)' is 
    #   subsequently called, 'x' (the 'names' mondate) arrives here with mode 
    #   'character' whose value = character representation of the mondate's 
    #   underlying double. (I don't know why, but I believe it's buried in the 
    #   primitive 'attributes' function.)
    #   This generates an error with the 'floor' function. 
    #   The following test avoids the error. 
    #   Unfortunately by assigning 'names' via 'structure' the object's 
    #   'names' display as the underlying double value, 
    #   not the date value, when the object is printed. That is why this test
    #   test simply returns the unclassed value of x.
    #   Better to assign mondate names with the 'names' function:
    #       names(object) <- mondate
    if (mode(x) != "numeric") return(x)
    ym <- floor(x)
    y <- ym %/% 12L + .mondate.year.zero
    m <- ym %% 12 + 1
    d <- ceiling(round((x - ym) * .daysinmonth(y, m), 7))
    nna <- !is.na(x)
    i <- (abs(x - round(x)) < .mondate.tolerance)
    d[i&nna] <- 1
    z <- as.Date(rep(NA, length(x)))
    z[nna] <- as.Date(paste(y[nna], m[nna], d[nna], sep = "-"), format = "%Y-%m-%d")
    z[i&nna] <- z[i&nna] - 1
    z
    }

as.POSIXlt.mondate <- function(x, ...) as.POSIXlt(as.Date(x), ...)
as.POSIXct.mondate <- function(x, ...) as.POSIXct(as.POSIXlt(x), ...)  

# Per ?as.numeric: "as.numeric and is.numeric are internally S4 generic and 
#   so methods can be set for them via setMethod."
setMethod("as.numeric", "mondate", function(x, 
               convert = FALSE, stripdim = FALSE,  
               timeunits = c("months", "years", "days"), 
               ...) {
    # If convert == FALSE, just strip out data part
    # If convert==TRUE, change units if necessary.
    # If stripdim, strip dim and names (like base::as.numeric)
    # Otherwise, keep shape.
    if (missing(timeunits)) timeunits <- slot(x,"timeunits")
    if (!convert) {
        if (stripdim) y <- x@.Data
        else y <- structure(x@.Data, dim = dim(x), dimnames = dimnames(x)) 
        }
    else # convert
    # may not need arithmetic
    if (timeunits == "months") {
        if (stripdim) y <- x@.Data
        else y <- structure(x@.Data, dim = dim(x), dimnames = dimnames(x)) 
        }
    else # need arithmetic
    if (stripdim) {
        if (timeunits == "years") y <- structure(x@.Data / 12, timeunits = timeunits)
        else y <- structure(as.numeric(as.Date(x)) - .mondate.days.zero, timeunits = timeunits)
        }
    else {# keep shape
        dims <- dim(x)
        dimnms <- dimnames(x)
        if (timeunits == "years") y <- structure(x@.Data / 12, timeunits = timeunits, dim = dims, dimnames = dimnms)
        else y <- structure(as.numeric(as.Date(x)) - .mondate.days.zero, timeunits = timeunits, dim = dims, dimnames = dimnms)
        }
    y
    })

as.character.mondate <- function(x, format, ...) {
    if (missing(format)) format <- slot(x, "displayFormat")
    dims <- dim(x)
    dimnms <- dimnames(x)
    nams <- names(x)
    y <- character(length(x))
    if (any(i <- is.infinite(x))) {
        y[!i] <- format(as.Date(x[!i]), format = format, ...)
        ipos <- x[i] > 0
        y[i][ipos] <- "Inf"
        y[i][!ipos] <- "-Inf"
        }
    else y <- format(as.Date(x), format = format, ...)
    dim(y) <- dims
    if (is.null(dims)) names(y) <- nams
    else dimnames(y) <- dimnms
    y
    }

## DATE ARITHMETIC

setMethod("Compare", "mondate", function(e1, e2) { 
    callGeneric(getDataPart(e1), getDataPart(e2))
    })

setMethod("Arith", c("mondate", "mondate"), function(e1, e2) {
    if (missing(e2)) 
        x <- mondate(callGeneric(as.numeric(e1, convert = TRUE)), 
                timeunits = e1@timeunits, displayFormat = e1@displayFormat)
    else {
        timeunits <- e1@timeunits
        if (timeunits != e2@timeunits) 
            warning("Unequal timeunits, using first mondate's= ", timeunits)
        if (timeunits == "months"){ 
            x <- structure(callGeneric(unclass(e1), unclass(e2)), displayFormat = NULL, timeunits = NULL, .S3Class = NULL, timeunits = timeunits)
            }
        else
        if (timeunits == "years")
            x <- structure(callGeneric(unclass(e1), unclass(e2)) / 12, displayFormat = NULL, timeunits = NULL, .S3Class = NULL, timeunits = timeunits)
        else {
            dims <- dim(x)
            dimnms <- dimnames(x)
            x <- structure(unclass(callGeneric(as.Date(e1), as.Date(e2))), units = NULL, displayFormat = NULL, timeunits = NULL, .S3Class = NULL, timeunits = timeunits, dim = dims, dimnames = dimnms)
            }
        }
    x
    })
setMethod("Arith", c("numeric", "mondate"), function(e1, e2) {
    mondate(callGeneric(e1, as.numeric(e2, convert= TRUE)), 
            timeunits = e2@timeunits, displayFormat = e2@displayFormat)
    })
setMethod("Arith", c("mondate", "numeric"), function(e1, e2) {
    mondate(callGeneric(as.numeric(e1, convert = TRUE), e2), 
            timeunits = e1@timeunits, displayFormat = e1@displayFormat)
    })

setMethod("Summary","mondate", function(x, ..., na.rm = FALSE) 
    mondate(callGeneric(x@.Data, ..., na.rm = na.rm),  
            timeunits = x@timeunits, displayFormat = x@displayFormat)
    )

# OTHER ARITHMETIC FUNCTIONS

mean.mondate<-function(x, trim = 0, na.rm = FALSE,...) 
    mondate(mean(unclass(x), trim = trim, na.rm = na.rm,...))

# New "parallel mean", analogous to pmin and pmax
setGeneric("pmean", function(...) standardGeneric("pmean"))
setMethod("pmean", signature="mondate", function(...) {
    mondate(apply(cbind(...), 1, mean), displayFormat = displayFormat(..1), timeunits = timeunits(..1)) 
    })

## COMBINING, EXTRACTING, SHAPING, ETC.
# Per ?c: "This function is S4 generic, but with argument list (x, ..., recursive = FALSE)."
setMethod("c", "mondate", function(x, ..., recursive = FALSE) {
    L <- list(...)
    if (length(L) > 0L) 
        new("mondate", sapply(unlist(list(x, L)), getDataPart), 
                       displayFormat = x@displayFormat, 
                       timeunits = x@timeunits)
    else
        new("mondate", c(getDataPart(x)), 
                       displayFormat = x@displayFormat, 
                       timeunits = x@timeunits)
    })

# Per help("["): "These operators are also implicit S4 generics, but as primitives, 
#   S4 methods will be dispatched only on S4 objects x. "
setMethod("[", "mondate", function(x, i, j, ..., drop) { 
    y <- callNextMethod()
    structure(
        new("mondate", y,
            displayFormat = x@displayFormat, 
            timeunits = x@timeunits
            ),
        dim = dim(y),
        dimnames = dimnames(y)
        )
    })

setMethod("show", "mondate", function(object) {
    cat('mondate: timeunits="', object@timeunits, '"\n', sep="")
    print(noquote(as.character(object)))
    })

# C/RBIND array, matrix subsection

#setGeneric("cbind", function(..., deparse.level = 1) standardGeneric("cbind"), signature = c("..."))# -> message about creating generic, signatures differ
#setGeneric("cbind")  # just this alone doesn't work...cannot "find" cbind(mondate) method below
#setMethod("cbind","mondate", function (..., deparse.level = 0) {
#    y <- .Internal(cbind(deparse.level = deparse.level, ...))
#    structure(
#        new("mondate", 
#            c(y), 
#            displayFormat = displayFormat(..1),
#            timeunits = timeunits(..1)
#            ),
#        dim = dim(y), 
#        dimnames = dimnames(y)
#        )
#    })
    
#setGeneric("rbind", function(..., deparse.level = 1) standardGeneric("rbind"), signature = c("..."))# -> message about creating generic, signatures differ
#setGeneric("rbind")
#setMethod("rbind","mondate", function (..., deparse.level = 0) {
#    y <- .Internal(rbind(deparse.level = deparse.level, ...))
#    structure(
#        new("mondate", 
#            c(y), 
#            displayFormat = displayFormat(..1),
#            timeunits = timeunits(..1)
#            ),
#        dim = dim(y), 
#        dimnames = dimnames(y)
#        )
#    })

setGeneric("array")
setMethod("array", "mondate", function(data = NA, dim = length(data), dimnames = NULL) 
    mondate(callNextMethod(), displayFormat = displayFormat(data), timeunits = timeunits(data)))

setGeneric("matrix")
setMethod("matrix", "mondate", function(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL) 
    mondate(callNextMethod(), displayFormat = displayFormat(data), timeunits = timeunits(data)))

##
## S3 METHODS
##

as.data.frame.mondate <- function(x, row.names = NULL, optional = FALSE, ...) {
    dims <- dim(x)
    if (is.null(dims)) nrows <- length(x)
    else
    if (length(dims) == 1L) nrows <- length(x)
    else
    if (length(dims) == 2L) nrows <- dims[1L]
    else { # flatten like data.frame does
        nrows <- dims[1L]
        dim(x) <- c(dims[1L], length(x) / dims[1L])
        }
    nm <- paste(deparse(substitute(x), width.cutoff = 500), collapse = " ")
    # determine row.names
    if (is.null(row.names)) {
        if (nrows == 0) row.names <- character(0)
        else if(length(row.names <- names(x)) == nrows &&
                !any(duplicated(row.names))) {
            }
        else if(optional) row.names <- character(nrows)
        else row.names <- seq_len(nrows)
        }
    names(x) <- NULL
    value <- list(x)
    if(!optional) names(value) <- nm
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
    }

format.mondate<- function(x, ...) as.character(x, ...)

unique.mondate<-function(x, ...) mondate(unique(unclass(x), ...))

cbindmondate <- function(..., deparse.level = 1) {
  y <- list(...)
  dspFmt <- displayFormat(y[[1L]])
  if (is.null(dspFmt)) dspFmt <- .default.displayFormat
  tu <- timeunits(y[[1L]])
  if (is.null(tu)) tu <- .default.timeunits
  cls <- sapply(y, function(x) class(x)[1L])
  if (all(cls == cls[1L])) mondate(cbind(..., deparse.level = deparse.level), displayFormat = dspFmt, timeunits = tu)
  else cbind.data.frame(...)
  }

rbindmondate <- function(..., deparse.level = 1) {
  y <- list(...)
  dspFmt <- displayFormat(y[[1L]])
  if (is.null(dspFmt)) dspFmt <- .default.displayFormat
  tu <- timeunits(y[[1L]])
  if (is.null(tu)) tu <- .default.timeunits
  cls <- sapply(y, function(x) class(x)[1L])
  if (all(cls == cls[1L])) mondate(rbind(..., deparse.level = deparse.level), displayFormat = dspFmt, timeunits = tu)
  else rbind.data.frame(...)
  }

rep.mondate<-function(x, ...) mondate(rep(unclass(x), ...), 
    displayFormat = x@displayFormat, timeunits = x@timeunits)

seq.mondate<-function(from = NULL, to, ...) {
    if (missing(from)) mondate(seq(to = as.numeric(to, convert = TRUE), ...),
            timeunits=to@timeunits,
            displayFormat=to@displayFormat)
    else 
    if (missing(to)) mondate(seq(from = as.numeric(from, convert = TRUE), ...),
                timeunits = from@timeunits,
                displayFormat = from@displayFormat)
    else mondate(seq(from = as.numeric(from, convert = TRUE), to = as.numeric(to, convert = TRUE), ...),
                timeunits = from@timeunits,
                displayFormat = from@displayFormat)
    }

head.mondate <- function(x, ...) {
    if (is.matrix(x)) head.matrix(x, ...) 
    else NextMethod()
    }

tail.mondate <- function(x, ...) {
    if (is.matrix(x)) tail.matrix(x, ...) 
    else NextMethod()
    }

diff.mondate <- function(x, lag = 1L, differences = 1L, ...)
    structure(diff(as.numeric(x, convert = TRUE)
                   , lag = lag
                   , differences = differences)
              , timeunits = timeunits(x)
              )

print.mondate <- function(x, ...) {
    print(noquote(as.character(x)), ...)
    invisible(x)
    }

# 3/8/12 So mondates can be X arguments in '*apply' functions:
as.list.mondate <- function(x, ...) lapply(X = NextMethod(), FUN = mondate)
# 3/8/12 So mondates can be assigned names (which worked before but caused
#           an error message    
`names<-.mondate` <- function(x, value) {
    names(x@.Data) <- value
    x
    }

## HELPFUL USER FUNCTIONS

## Pulling out month, year, and day numbers
ymd <- function(x) {
    nms <- names(x)
    x <- mondate(x)
    xd <- x@.Data
    ym <- floor(xd)
    y <- ym %/% 12L + .mondate.year.zero
    m <- ym %% 12 + 1
    d <- ceiling(round((xd - ym) * .daysinmonth(y, m), 7))
    nna <- !is.na(xd)
    xdnna <- xd[nna]
    mnna <- m[nna]
    ynna <- y[nna]
    dnna <- d[nna]
    # find dates where month is fully completed
    i <- (abs(xdnna - round(xdnna)) < .mondate.tolerance)
    # when that happens, the division algorithm puts 'm' into the next month,
    #   so decrement back to correct month number
    mnna[i] <- mnna[i] - 1
    # if decrementing m puts us into "month 0", set month to december and
    #   decrement the year too
    ndx <- mnna < 1
    mnna[ndx] <- 12
    ynna[ndx] <- ynna[ndx] - 1
    # now know correct month and year for "completed months", so set day
    dnna[i] <- .daysinmonth(ynna[i], mnna[i])
    y[nna] <- ynna
    m[nna] <- mnna
    d[nna] <- dnna
    names(y) <- nms
    cbind(year = y, month = m, day = d)
    }

# 3/8/12 Modified so results inherit names, dim, dimnames.
setGeneric("year", function(x, ...) standardGeneric("year"))
setMethod("year", "mondate", function(x) {
    dm <- dim(x)
    dmnms <- dimnames(x)
    nms <- names(x)
    y <- ymd(x)[, "year"]
    if (!is.null(dm)) {
        dim(y) <- dm
        dimnames(y) <- dmnms
        }
    else names(y) <- nms
    y
    })
setMethod("year", "Date", function(x) {
    dm <- dim(x)
    dmnms <- dimnames(x)
    nms <- names(x)
    y <- as.numeric(format(x, "%Y"))
    if (!is.null(dm)) {
        dim(y) <- dm
        dimnames(y) <- dmnms
        }
    else names(y) <- nms
    y
    })
setMethod("year", "POSIXt", function(x) {
    dm <- dim(x)
    dmnms <- dimnames(x)
    nms <- names(x)
    y <- as.numeric(format(x, "%Y"))
    if (!is.null(dm)) {
        dim(y) <- dm
        dimnames(y) <- dmnms
        }
    else names(y) <- nms
    y
    })
setGeneric("month", function(x, ...) standardGeneric("month"))
setMethod("month", "mondate", function(x) {
    dm <- dim(x)
    dmnms <- dimnames(x)
    nms <- names(x)
    y <- ymd(x)[, "month"]
    if (!is.null(dm)) {
        dim(y) <- dm
        dimnames(y) <- dmnms
        }
    else names(y) <- nms
    y
    })
setMethod("month", "Date", function(x) {
    dm <- dim(x)
    dmnms <- dimnames(x)
    nms <- names(x)
    y <- as.numeric(format(x, "%m"))
    if (!is.null(dm)) {
        dim(y) <- dm
        dimnames(y) <- dmnms
        }
    else names(y) <- nms
    y
    })
setMethod("month", "POSIXt", function(x) {
    dm <- dim(x)
    dmnms <- dimnames(x)
    nms <- names(x)
    y <- as.numeric(format(x, "%m"))
    if (!is.null(dm)) {
        dim(y) <- dm
        dimnames(y) <- dmnms
        }
    else names(y) <- nms
    y
    })
setGeneric("day", function(x, ...) standardGeneric("day"))
setMethod("day", "mondate", function(x) {
    dm <- dim(x)
    dmnms <- dimnames(x)
    nms <- names(x)
    y <- ymd(x)[, "day"]
    if (!is.null(dm)) {
        dim(y) <- dm
        dimnames(y) <- dmnms
        }
    else names(y) <- nms
    y
    })
setMethod("day", "Date", function(x) {
    dm <- dim(x)
    dmnms <- dimnames(x)
    nms <- names(x)
    y <- as.numeric(format(x, "%d"))
    if (!is.null(dm)) {
        dim(y) <- dm
        dimnames(y) <- dmnms
        }
    else names(y) <- nms
    y
    })
setMethod("day", "POSIXt", function(x) {
    dm <- dim(x)
    dmnms <- dimnames(x)
    nms <- names(x)
    y <- as.numeric(format(x, "%d"))
    if (!is.null(dm)) {
        dim(y) <- dm
        dimnames(y) <- dmnms
        }
    else names(y) <- nms
    y
    })

setGeneric("MonthsBetween", function(from, to) standardGeneric("MonthsBetween"))
setMethod("MonthsBetween", c("mondate", "mondate"), function(from, to) {
    if (length(from) >= length(to)) {
        dims <- dim(from)
        dimnams <- dimnames(from)
        }
    else {
        dims <- dim(to)
        dimnams <- dimnames(to)
        }
    y <- c(as.numeric(to)) - c(as.numeric(from))
    structure(
        y
        , dim = dims
        , dimnames = dimnams
        , timeunits = "months"
        , .S3Class = NULL
        )
    })
setGeneric("YearsBetween", function(from, to) standardGeneric("YearsBetween"))
setMethod("YearsBetween", c("mondate", "mondate"), function(from, to) {
    if (length(from) >= length(to)) {
        dims <- dim(from)
        dimnams <- dimnames(from)
        }
    else {
        dims <- dim(to)
        dimnams <- dimnames(to)
        }
    y <- c(as.numeric(to)) - c(as.numeric(from))
    y <- y / 12
    structure(
        y
        , dim = dims
        , dimnames = dimnams
        , timeunits = "years"
        , .S3Class = NULL
        )
    })
setGeneric("DaysBetween", function(from, to) standardGeneric("DaysBetween"))
setMethod("DaysBetween", c("mondate", "mondate"), function(from, to) {
    if (length(from) >= length(to)) {
        dims <- dim(from)
        dimnams <- dimnames(from)
        }
    else {
        dims <- dim(to)
        dimnams <- dimnames(to)
        }
    structure(
        round(abs(unclass(as.Date(to) - as.Date(from))))
        , dim = dims
        , dimnames = dimnams
        , units = NULL
        , timeunits = "days"
        )
    })

## Constructing with month, year, and day numbers
mondate.mdy <- function(m, d, y, displayFormat = .default.displayFormat, 
                               timeunits = .default.timeunits, ...) 
    mondate(ISOdate(y, m, d), 
            displayFormat = displayFormat, 
            timeunits = timeunits,
            ...)
mondate.ymd <- function(y, m, d, displayFormat = .default.displayFormat, 
                               timeunits = .default.timeunits, ...) {
    if (missing(d)) {
        if (missing(m)) m <- 12
        else m <- as.numeric(m)
        y <- as.numeric(y)
        d <- .daysinmonth(y, m) # as of 8/19/2010 d=inf if y=inf
        # daysinmonth forces length d = max of y and m
        # R considers NA's as "not finite", but ISOdate returns NA's
        #   for NA days, so we'll allow NA values of d to be TRUE for "isf"
        isf <- is.finite(d) | is.na(d) 
        if (all(isf)) mondate(ISOdate(y, m, d),
                              displayFormat = displayFormat, 
                              timeunits = timeunits,
                              ...)
        else {
            md <- mondate(ISOdate(y, m, d),
                              displayFormat = displayFormat, 
                              timeunits = timeunits,
                              ...)
            nisf <- !isf
            md[nisf] <- Inf
            md[nisf & y[nisf] < 0] <- -Inf
            md
            }
        } 
    else {
        isinf <- is.infinite(y)
        if (any(isinf)) {
            z <- rep(NA, max(length(y), length(m), length(d)))
            isfin <- !isinf
            z[isfin] <- ISOdate(y[isfin], m[isfin], d[isfin])
            z <- mondate(z, displayFormat = displayFormat, timeunits = timeunits, ...)
            z[isinf] <- Inf
            if (any(neg <- (y[isinf] < 0))) z[isinf][neg] <- -Inf
            z
            }
        else mondate(ISOdate(y, m, d), 
                 displayFormat = displayFormat, 
                 timeunits = timeunits,
                 ...)
        }
    }

