# This is the code from Gabor's email.

# I'm not exactly sure what result he was looking for.
# Could try running using version of mondate on CRAN,
# then run again using current github version.

library(mondate)
#(x <- mondate.ymd(2000:2003, 6, 15)) # data

#(from <- mondate(cut(min(x), breaks = "month", right = FALSE)))
#(to <- mondate(as.Date(max(x)) - 1) + 1) # - 1 day + 1 month

#mondate(seq(as.Date(from), as.Date(to), by = "month"))

# So see better what's going on, make 'x' only 2 datapts 1 yr apart
(x <- mondate.ymd(2000:2001, 6, 15)) # data

(from <- mondate(cut(min(x), breaks = "month", right = FALSE)))
(to <- mondate(as.Date(max(x)) - 1) + 1) # - 1 day + 1 month

mondate(seq(as.Date(from), as.Date(to), by = "month"))

# To see the half-open/half-closed monthly intervals covering the data,
# set attr.breaks = TRUE and re-cut.
(cx <- cut.mondate(x, breaks = "month", attr.breaks = TRUE))
cx_2 <- cut(x, breaks = "month", attr.breaks = TRUE)
stopifnot(identical(cx, cx_2))
(brks <- attr(cx, "breaks"))
class(brks)
(cx2 <- cut.mondate(x, breaks = brks))
levels(cx2)
# Hmm. Why is first interval left-closed?
# Answer: b/c the CRAN version has include.lowest = TRUE by default
(cx3 <- cut.mondate(x, breaks = brks, include.lowest = FALSE))
levels(cx3) # first interval is left-open
nx <- as.numeric(x)
nb <- as.numeric(brks)
cut(nx, nb, include.lowest = TRUE) # first interval is left-closed



# Do the same thing with two "Date's"
(d <- as.Date(x))
(cd <- cut.mondate(d, breaks = "month", attr.breaks = TRUE)) # Error!
#Error in cut.mondate(d, breaks = "month", attr.breaks = TRUE) : 
#  'x' must be a mondate

# This generates the breaks using brute force -- seq
from <- mondate.ymd(year(min(x)), month(min(x))) - 1
to <- mondate.ymd(year(max(x)), month(max(x)))
(brks.seq <- seq(from, to, by = 1))
cut(x, breaks = brks.seq)
# Voila! the first interval is left-closed!

# Hmm. First interval is not half-closed with cut.default 
cut(c(.5, 12.5), breaks = 0:13)
