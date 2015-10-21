seq.mondate <- function(from = NULL, to, by, ..., right = TRUE) {
  if (!(missing(right))) 
    warning("As of October 2015, use of 'right' is deprecated. Value ignored.")
  if (missing(from)) {
    if (missing(to)) stop("either 'from' or 'to' must be specified")
    mto <- mondate(to)
    tu <- mto@timeunits
    dF <- mto@displayFormat
    fF <- mto@formatFUN
    }
  else {
    mfrom <- mondate(from)
    tu <- mfrom@timeunits
    dF <- mfrom@displayFormat
    fF <- mfrom@formatFUN
    if (!missing(to)) mto <- mondate(to)
    }
  if (missing(by)) {
    if (missing(from)) return(mondate(seq(to = as.numeric(mto, convert = TRUE), ...), timeunits = tu, displayFormat = dF, formatFUN = fF))
    if (missing(to)) return(mondate(seq(from = as.numeric(mfrom, convert = TRUE), ...), timeunits = tu, displayFormat = dF, formatFUN = fF))
    return(mondate(seq(from = as.numeric(mfrom, convert = TRUE), to = as.numeric(mto, convert = TRUE), ...), timeunits = tu, displayFormat = dF, formatFUN = fF))
    }
  if (is.numeric(by)) {
    if (missing(from)) return(mondate(seq(to = as.numeric(mto, convert = TRUE), by = by, ...), timeunits = tu, displayFormat = dF, formatFUN = fF))
    if (missing(to)) return(mondate(seq(from = as.numeric(mfrom, convert = TRUE), by = by, ...), timeunits = tu, displayFormat = dF, formatFUN = fF))
    return(mondate(seq(from = as.numeric(mfrom, convert = TRUE), to = as.numeric(mto, convert = TRUE), by = by, ...), timeunits = tu, displayFormat = dF, formatFUN = fF))
    }
  if (!is.character(by)) stop ("invalid mode for 'by'")
  if (is.na(by)) stop("'by' may not be NA")

  # "days", "weeks", "months", "years", "quarters"
  by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
  if (length(by2) > 2L || length(by2) < 1L) stop("invalid 'by' string")
  valid <- pmatch(by2[length(by2)], c("days", "weeks", "months", "years", "quarters"))
  if (is.na(valid)) stop("invalid string for 'by'")
  if (valid <= 2) {
    if (missing(from) | missing(to)) stop(paste("both 'from' and 'to' must be specified when by = '", by, "'", sep = ""))
    if (missing(to)) return(mondate(seq(from = as.numeric(mfrom, convert = TRUE), ...), timeunits = tu, displayFormat = dF, formatFUN = fF))
    return(mondate(seq(from = as.Date(mfrom), to = as.Date(mto), by = by, ...), timeunits = tu, displayFormat = dF, formatFUN = fF))
    }
  else by <- if (length(by2) == 2L) as.integer(by2[1L]) else 1 # base unit of "1 month"
  by <- by * c(NA, NA, 1, 12, 3)[valid]    
  if (missing(from)) return(mondate(seq(to = as.numeric(mto, convert = TRUE), by = by, ...), timeunits = tu, displayFormat = dF, formatFUN = fF))
  if (missing(to)) return(mondate(seq(from = as.numeric(mfrom, convert = TRUE), by = by, ...), timeunits = tu, displayFormat = dF, formatFUN = fF))
  return(mondate(seq(from = as.numeric(mfrom, convert = TRUE), to = as.numeric(mto, convert = TRUE), by = by, ...), timeunits = tu, displayFormat = dF, formatFUN = fF))
  }
