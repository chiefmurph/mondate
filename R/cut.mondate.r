# Find the quarter- or year-index of the mondate, return the mondate.
# I.e., find the mondate(s) of the quarter-end or year-end that x is in.
# Will be used in a sequence, so don't worry about x's mondate slots.
# cut.mondate.r
cut.mondate <- function (x, breaks, labels = NULL,
                         include.lowest = FALSE, 
                         right = TRUE,
                         start.on.monday = TRUE,
                         startmonth = NULL,
                         startyear = NULL,
                         attr.breaks = FALSE
                         , ...) {
  # factor x with a sequence of contiguous, non-overlapping intervals 
  #   that "cover" x
  #   (see, e.g., www.wikipedia.org/wiki/Cover_(topology))
  # In conformance with cut.default, the value of x that will not be
  #   covered by default (include.lowest is FALSE) will be the 
  #   the leftmost, minimum value or rightmost, maximum value depending
  #   on right or !right, respectively. If include.lowest is TRUE, all
  #   values of x will be included in one of the invervals.
  # All intervals will be (open, closed] or [closed, open) when 
  #   right or !right, respectively, with the exception that when
  #   include.lowest is TRUE the leftmost interval will be closed on
  #   both ends (rightmost interval when !right).
  # The factor labels will show the endpoints of the intervals:
  #   When breaks is numeric, both endpoints will be shown, with
  #   parens ('(' or ')') indicating the open endpoing and brackets ('[', ']')
  #   indicating the closed endpoint.
  #   When breaks is character, only the right or !right endpoint 
  #     will be shown.
  #
  # When breaks is numeric, the method inherits from cut.default;
  #   see ?base::cut for information on its behavior to supplement the above
  #
  # When breaks is character, the intervals will be days, weeks, months,
  #   etc. defined by a sequence of days that determine the breakpoints.
  #   For example, suppose that x holds the first 5 days of November 2015,  
  #     and that breaks = "days" and right and include.lowest are their 
  #     default values (TRUE and FALSE, respectively).
  #     Then the breakpoints will be the sequence
  #       (2015-11-01, 2015-11-02, 2015-11-03, 2015-11-04, 2015-11-05), 
  #     the four intervals so determined will be 
  #       (2015-11-01, 2015-11-02], (2015-11-02, 2015-11-03], 
  #       (2015-11-03, 2015-11-04], (2015-11-04, 2015-11-05],
  #     and the labels of the factor will be
  #       mondate(c("2015-11-02", "2015-11-03", "2015-11-04", "2015-11-05"))
  #   Note that in the example, the minimum value of x, 2015-11-01, 
  # `   is not included in an interval so its factor value will be <NA>.
  #     This conforms with cut.default's behavior, 
  #     but perhaps not necessarily with "intuitive" behavior 
  #     for a cover of "days", "months", etc., when one might expect all
  #     values of x to be included in some "day", "month", etc.
  #     For S4 methods with more intuitive defaults for character breaks,
  #     see the generic cutmondate with methods for "mondate", "Date", etc.
  #   When character breaks spans multiple days (e.g., "week", "month")
  #     and include.lowest is FALSE, the maximum (when right) or
  #     minimum (when !right) value of x will be ignored when determining
  #     the intervals covering x. Therefore, in this case, if x is
  #     comprised of a single value, !include.lowest (which is NOT
  #     the default) generates an error. 
  #     Again, see cutmondate for better defaults.
  #   Continuing the is.character(breaks) case ...
  #     If right, the labels for the factor will the the last day
  #       of the interval, and, if attr.breaks = TRUE, the breaks
  #       returned can be used to re-factor x as
  #         cut.mondate(x, breaks).
  #     If !right, the labels for the factor will be the first day
  #       of the interval, and, if attr.breaks = TRUE, the breaks
  #       returned can be used to re-factor x as
  #         cut(as.Date(x), as.Date(breaks))
  #   
  #   
  # right = TRUE: non-intersecting, half-open/half-closed intervals 
  #   are considered closed on right;
  #   when breaks is character, level represented by date of right endpoint
  # if !right, same as above, but with left replacing right
  #
  # include.lowest
  # Case A: breaks is numeric
  # include.lowest works as with cut's default method:
  # include.lowest = TRUE: 
  #   When 'right', the leftmost interval is considered closed and
  #     the minimum value of x is considered a member of that interval
  #   When !right, the rightmost interval is considered closed and
  #     the maximum value of x is considered a member of that interval
  # include.lowest = FALSE: 
  #   When right: min(x) is considered the leftmost *open* endpoint, so
  #     this data point does not correspond to any interval determined by 
  #     the cut. Therefore, the factor value for min(x) is NA.
  #   When !right, just as above but applicable to the maximum value of x.
  # This behavior inherits from the default method; consider 
  #   cut(1:5, breaks = 1:5, include.lowest = TRUE)
  #    vs
  #   cut(1:5, breaks = 1:5, include.lowest = FALSE)
  #   
  # Case B: breaks is character
  #   Case B.1: include.lowest is FALSE
  #     The minimum value of x (when right, max when !right) is excluded
  #     when considering the periods covering x
  #   Case B.2: include.lowest is TRUE
  #     All values of x are considered
  #     
  # breaks:
  #   vector of length > 1: sequential pairs determine the intervals
  #     into which objects of class x is to be cut.
  #     Only allowed for mondate's.
  #   scalar: the range of x is divided into breaks intervals of equal length
  #     via seq(min(x), max(x), length = breaks + 1), then the vector case
  #     above takes over 
  #     [Note: This differs from cut.default in that the outer limits are
  #     not moved away to ensure the extreme values are included in some
  #     interval.]
  #   character: "days", "weeks", "months", "quarters", "years"
  #     min(x) determines the leftmost interval (e.g., month), not endpoint
  #     max(x) determines the rightmost interval (e.g., month), not endpoint
  #     In the case where breaks is of mode character, include.lowest has a
  #       special meaning:
  #       if min(x) falls on an interval boundary, then the interval
  #         into which min(x) falls is included in the levels of the result
  #         if and only if include.lowest is TRUE
  #     if right = TRUE, the levels of the result is labeled with the date
  #       corresponding to the right endpoint of the interval;
  #     if right = FALSE, the levels of the result is labeled with the date
  #       corresponding to the left endpoint of the interval.
  #     include.lowest:
  #       if right = TRUE and min(x) lies on an interval boundary, 
  #         an extra interval is created to the right to include it
  #       if right = FALSE and max(x) lies on an interval boundary, 
  #         an extra interval is created to the left to include it
  # start.on.monday: only applicable for "weeks". See ?cut.Date.
  # yearend.month = the month number of the last day of the year
  # attr.breaks = TRUE: a "breaks" attribute is returned whose value
  #   can be used to reproduce the same factors whose labels hold the interval
  #   representation of the cuts in date format.
  if (missing(breaks)) stop("argument 'breaks' is missing, with no default")
  tu <- timeunits(x)
  dF <- displayFormat(x)
  fF <- x@formatFUN
  if (is.numeric(breaks)) {
    if (length(breaks) == 1L) {
      rngx <- range(x)
      breaks <- seq(from = rngx[1], length.out = breaks + 1, by = diff(rngx) / breaks)
      res <- cut(x, breaks = breaks, include.lowest = include.lowest, right = right, labels = labels, ...)
      n <- length(breaks) - 1
      lechar <- rep(ifelse(right, "(", "["), n)
      rechar <- rep(ifelse(right, "]", ")"), n)
      if (include.lowest) 
        if (right) lechar[1] <- "["
        else rechar[n] <- "]"
      if (attr.breaks) attr(res, "breaks") <- structure(breaks, lechar = lechar, rechar = rechar)
      }
    else {
      breaks <- sort(breaks)
      res <- cut.default(as.numeric(x), breaks = breaks,
                        include.lowest = include.lowest, right = right, 
                        dig.lab = 16, ...)
      if (!is.factor(res)) return(res) # see code for cut.default to determine what causes this
      lval <- .intervalsplit(res)
      breaks <- mondate(c(lval[1L], lval[, 2L]), 
                        displayFormat = dF, formatFUN = fF)
      attr(breaks, "lechar") <- attr(lval, "lechar")
      attr(breaks, "rechar") <- attr(lval, "rechar")
      if (is.null(labels)) levels(res) <- .intervalsplitToChar(lval)
      else levels(res) <- labels
      if (attr.breaks) attr(res, "breaks") <- breaks  
      }
    }
  else 
  if (!is.character(breaks)) stop("'breaks' must be numeric or character")
  else { #character
    if (length(breaks) > 1) stop("invalid specification for 'breaks'")
    by2 <- strsplit(breaks, " ", fixed = TRUE)[[1L]]
    if (length(by2) > 2L || length(by2) < 1L) stop("invalid specification for 'breaks'")
    valid <- pmatch(by2[length(by2)], c("days", "weeks", "months", "years", "quarters"))
    if (is.na(valid)) stop("invalid specification for 'breaks'")
    step <- ifelse(length(by2) == 2L, as.integer(by2[1L]), 1L)
    
  # 11/3/15: !include.lowest is error when breaks is char
    if (!include.lowest) stop(
      "!include.lowest is invalid when breaks is character")

    # if days
    if (valid == 1) { # days
      x <- as.numeric(as.Date(x))
      int <- step
      rngx <- range(x)
      breaks <- if (right) rev(seq(rngx[2L], 
                                   rngx[1L] - int, 
#                                   rngx[1L] - ifelse(include.lowest, int, 0), 
                                   by = -int))
                else seq(rngx[1L]-1, 
                         rngx[2L] + int, 
#                         rngx[2L] + ifelse(include.lowest, int, 0), 
                         by = int)
      
      res <- cut.default(x, breaks = breaks, 
                         labels = labels)
      # label appropriately 
      breaks <- mondate(as.Date(breaks, "1970-01-01"), displayFormat = dF, formatFUN = fF)
      if (is.null(labels)) levels(res) <- if (right) breaks[-1L] else 
        add(breaks[-length(breaks)], 1, "days")
      if (attr.breaks) attr(res, "breaks") <- breaks
    }
    else
    if (valid == 2) { # weeks
      step <- step * 7
      nx <- as.numeric(as.Date(x))
      rngnx <- range(nx)
      minnx <- rngnx[1L]
      maxnx <- rngnx[2L]
      if (right) {
        web <- .webgex(ifelse(start.on.monday, 4, 3), maxnx)
        breaks <- rev(seq(web, minnx - step, by = -step))
      }
      else {
        web <- .weblex(ifelse(start.on.monday, 4, 3), minnx)
        breaks <- seq(web, maxnx + step, by = step)
      }
      
      res <- cut(nx, breaks, labels = labels)
      breaks <- mondate(as.Date(breaks, "1970-01-01"), displayFormat = dF, formatFUN = fF)
      if (is.null(labels)) levels(res) <- if (right) breaks[-1L] else 
        add(breaks[-length(breaks)], 1, "days")
      if (attr.breaks) attr(res, "breaks") <- breaks
    }
    else
    if (valid == 3) { # month
      res <- .gcut(x, step = step, startmonth = startmonth,
                   startyear = startyear,
                  include.lowest = include.lowest, right = right, 
                  labels = labels, attr.breaks = attr.breaks, dF, fF)
    }
    else
    if (valid == 4) { # year
      res <- .gcut(x, step = step * 12, startmonth = startmonth, 
                   startyear = startyear,
                   include.lowest = include.lowest, right = right, 
                  labels = labels, attr.breaks = attr.breaks, dF, fF)
    }
    else
    if (valid == 5){ # quarter
      res <- .gcut(x, step = step * 3, startmonth = startmonth, 
                   startyear = startyear,
                   include.lowest = include.lowest, right = right, 
                  labels = labels, attr.breaks = attr.breaks, dF, fF)
      }
    } # end character
  res

} # end cut.mondate

.webgex <- function(startday, maxnx) {
  maxnx%/%7 * 7 + startday - 1 + ifelse(maxnx%%7 >= startday, 7, 0)
} 
.weblex <- function(startday, minnx) {
  (minnx-1)%/%7*7 + startday - ifelse(minnx%%7 >= startday, 0, 7) - 1
}

.gcut <- function(x, step = 1, startmonth = NULL, startyear = NULL, 
                  include.lowest = TRUE, right = TRUE, labels = NULL, 
                  attr.breaks = FALSE, dF, fF) {
  if (!include.lowest) stop(
    "!include.lowest is ignored when breaks is character")
  
#  x <- as.numeric(x)
  nx <- ceiling(as.numeric(x))
  rngnx <- range(nx)
  minnx <- rngnx[1L]
  maxnx <- rngnx[2L]
  breaks <- 
    if (is.null(startyear)) {
      if (is.null(startmonth)) {
        if (right) {
          rev(seq(rngnx[2L], rngnx[1L] - step, by = -step))
        }
        else {
          seq(minnx - 1, maxnx - 1 + step, by = step)
        }
      }
      else { # null startyear, not null startmonth
        if (right) {
          # year-end boundary
          yeb <- .yebgex(startmonth, maxnx)
          # quarter-end boundary if there's one in between yeb and maxnx
          qtrb <- yeb - (yeb-maxnx)%/%step*step
          rev(seq(qtrb, minnx - step, by = -step))
        }
        else { #left
          yeb <- .yeblex(startmonth, minnx)
          qtrb <- yeb + (minnx-1-yeb)%/%step*step
          seq(qtrb, maxnx + step - 1, by = step)
        }
#        intv <- (rngnx - startmonth) %/% step * step + step + startmonth - 1 - c(step, 0)
#        seq(intv[1L], intv[2L], by = step)
      }
    }
    else { # startyear given
      if (is.null(startmonth)) 
        startmonth <- ifelse(right, (maxnx-1)%%12 + 1, maxnx%%12 + 1)
      yeb <- as.numeric(mondate.ymd(startyear, startmonth)) - 1
      # there's a max in case startyear after x
      seq(yeb, max(c(maxnx - 1, yeb)) + step, by = step)
#      if(is.null(startmonth)) startmonth <- minnx
#      yearshift.in.months <- (startyear - .mondate.year.zero) * 12
#      intv <- (rngnx - startmonth - yearshift.in.months) %/% step * step + 
#        step + startmonth - 1 + yearshift.in.months - c(step, 0)
#      seq(intv[1L], intv[2L], by = step)
    }
#return(breaks)
  res <- cut(x, breaks, labels = labels)
  breaks <- mondate(breaks, displayFormat = dF, formatFUN = fF)
  if (is.null(labels)) levels(res) <- if (right) breaks[-1L] else 
    add(breaks[-length(breaks)], 1, "days")
  #    add(breaks[-length(breaks)] - 1, 1, "days")
  if (attr.breaks) attr(res, "breaks") <- breaks
  return(res)
}

.yebgex <- function(startmonth, maxnx) ((maxnx-1)%/%12+ifelse(startmonth%%12<=maxnx%%12,1,0))*12 + 
  startmonth - 1
.yeblex <- function(startmonth, minnx) (minnx%/%12-ifelse(startmonth%%12>minnx%%12,1,0))*12 + 
  startmonth - 1

.intervalsplit <- function(res){
  lvls <- levels(res)
  nl <- length(lvls)
  z <- matrix(unlist(strsplit(lvls,",")), ncol = 2L, byrow = TRUE)
  lechar <- substr(z[, 1L], 1L, 1L)
  lenum <- as.numeric(substr(z[, 1L], 2L, nchar(z[, 1L])))
  nc <- nchar(z[, 2L])
  rechar <- substr(z[, 2L], nc, nc)
  renum <- as.numeric(substr(z[, 2L], 1L, nc - 1L))
  #    lval <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ),
  #                  upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))
  structure(cbind(lower = lenum, upper = renum), lechar = lechar, rechar = rechar)
}
.intervalsplitToChar <- function(lval)
  paste(attr(lval, "lechar"), apply(as.character(mondate(lval)), 1, paste, collapse = ","), 
        attr(lval, "rechar"), sep = "")
.year_boundary_right <- function(x, yearend.month = 12) {
  shift <- 12 - yearend.month
  (x + shift - 1) %/% 12 * 12 + 12 - shift
}
.quarter_boundary_right <- function(x, yearend.month = 12) {
  shift <- c(0, 2, 1)[yearend.month %% 3 + 1]
  (ceiling(x) + shift - 1) %/% 3 * 3 + 3 - shift
}
