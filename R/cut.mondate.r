# Find the quarter- or year-index of the mondate, return the mondate.
# I.e., find the mondate(s) of the quarter-end or year-end that x is in.
# Will be used in a sequence, so don't worry about x's mondate slots.
# cut.mondate.r
cut.mondate <- function (x, breaks, labels = NULL,
                         include.lowest = FALSE, 
                         right = TRUE,
                         start.on.monday = TRUE,
                         attr.breaks = FALSE
                         , ...) {
  # factor x with a sequence of contiguous, non-overlapping intervals 
  #   that "cover" x
  #   (see, e.g., www.wikipedia.org/wiki/Cover_(topology))
  # In conformance with cut.default, the value of x that will not be
  #   covered by default (include.lowest is FALSE) will be the 
  #   the leftmost, minimum value or rightmost, maximum value depending
  #   on right or !right, respectively. If include.lowest is TRUE, all
  #   values of x will be included in one (and only one) of the invervals.
  # All intervals will be (open, closed] or [closed, open) when 
  #   right or !right, respectively, with the exception that when
  #   include.lowest is TRUE the leftmost (when right) or rightmost 
  #   interval will be a closed on both ends.
  # The factor labels will show the endpoints of the intervals:
  #   When breaks is numeric, both endpoints will be shown.
  #   When breaks is character, only the right or !right endpoint 
  #     will be shown.
  # When breaks is numeric, the method inherits from cut.default;
  #   see ?base::cut for information on its behavior.
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
  #     
  #   
  # The closed endpoint will be the right or left endpoint depending on
  #   the argument right.
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
  #     [Note: this differs from cut.default in that the outer limits are
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
  # start.on.monday: only applicable for "weeks". See cut.Date.
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
    
    if (length(x) == 1) { # must force include.lowest = TRUE for "integral" x
                          # = 1 second of the shortest day = 1/31/3600
      if (abs(x - round(x, 0)) < 8.960573e-06 &
          !missing(include.lowest) & 
          !include.lowest) 
        stop("include.lowest = FALSE and scalar x are incompatible when x is near a month boundary")
      include.lowest <- TRUE
      }

    # if days
    if (valid == 1) { # days
      x <- as.numeric(as.Date(x))
      int <- step
      rngx <- range(x)
      breaks <- seq(from = ceiling(rngx[1] / int) * int - ifelse(include.lowest, int, 0),
                    to =   ceiling(rngx[2] / int) * int, 
                    by = int)
      res <- cut.default(x, breaks = breaks, 
                         labels = labels, right = TRUE, 
                         include.lowest = FALSE, ...)
      breaks <- mondate(as.Date(breaks, "1970-01-01"), 
                      displayFormat = dF, formatFUN = fF)
      # label appropriately
      if (is.null(labels)) levels(res) <- if (right) breaks[-1]
                                          else head(breaks, -1)
      if (attr.breaks) {
        attr(res, "breaks") <- breaks
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
      # 1970-01-01 begins Thu, Fri, Sat, Sun, which mod 7 is
      #                    0    1    2    3
      breaks <- seq(from = ((rngx[1] - 3) %/% int) * int + 3 - ifelse(include.lowest, int, 0) - 
                      ifelse(start.on.monday, 0, 1),
                    to =   ceiling((rngx[2] - 3) / int) * int + 3   , 
                    by = int)
      res <- cut.default(x, breaks = breaks, 
                         labels = labels, right = TRUE, 
                         include.lowest = FALSE, ...)
      breaks <- mondate(as.Date(breaks, "1970-01-01"), 
                      displayFormat = dF, formatFUN = fF)
      # label appropriately
      if (is.null(labels)) levels(res) <- if (right) breaks[-1]
                                          else head(breaks, -1)
      if (attr.breaks) {
        attr(res, "breaks") <- breaks
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
    else { # 3: month, 4: year, 5: quarter
      valid <- valid - 2
      int <- c(1, 12, 3)[valid] * step
      rngx <- range(x)
      breaks <- seq(from = floor(rngx[1] / int) * int - ifelse(include.lowest, int, 0),
                    to =   ceiling(rngx[2] / int) * int, 
                    by = breaks)
      res <- cut.default(as.numeric(x), breaks = as.numeric(breaks), 
                         labels = labels, right = TRUE, 
                         include.lowest = FALSE, ...)
      # label appropriately
      if (is.null(labels)) levels(res) <- if (right) as.character(breaks[-1])
                                          else as.character(head(breaks, -1))
      if (attr.breaks) {
        attr(res, "breaks") <- breaks
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
      } # end valid's
    } # end character
  res
  } # end cut.mondate

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