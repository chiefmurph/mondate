x <- as.Date(mondate.ymd(2008, 1, 1:5))
cut(x, "days", right = TRUE, include.lowest = TRUE)
#[1] 2008-01-01 2008-01-01 2008-01-02 2008-01-03 2008-01-04
#Levels: 2008-01-01 2008-01-02 2008-01-03 2008-01-04 2008-01-05
y <- as.numeric(x)
y
breaks <- seq(min(y) - 1, max(y), by = 1)
z <- cut(y, breaks, right = TRUE, include.lowest = TRUE)
z
lval <- .intervalsplit(z)[, "upper"]
lval
levels(z) <- as.Date(lval, "1970-01-01")
z

#vs same 
breaks <- seq(max(y), min(y) - 1, by = -1)
z <- cut(y, breaks, right = TRUE, include.lowest = TRUE)
z
lval <- .intervalsplit(z)[, "upper"]
lval
levels(z) <- as.Date(lval, "1970-01-01")
z

cut(x, "days", right = FALSE, include.lowest = TRUE)
#[1] 2008-01-01 2008-01-02 2008-01-03 2008-01-04 2008-01-05
#Levels: 2008-01-01 2008-01-02 2008-01-03 2008-01-04 2008-01-05
y <- as.numeric(x)
y
breaks <- seq(min(y), max(y) + 1, by = 1)
breaks
z <- cut(y, breaks, right = FALSE, include.lowest = TRUE)
z
lval <- .intervalsplit(z)[, "lower"]
lval
levels(z) <- as.Date(lval, "1970-01-01")
z

cut(x, "days", right = TRUE, include.lowest = FALSE)
#[1] <NA>       2008-01-01 2008-01-02 2008-01-03 2008-01-04
#Levels: 2008-01-01 2008-01-02 2008-01-03 2008-01-04 2008-01-05
# bzzt s/b
#[1] <NA>       2008-01-02 2008-01-03 2008-01-04 2008-01-05
#Levels: 2008-01-02 2008-01-03 2008-01-04 2008-01-05
y <- as.numeric(x)
breaks <- seq(min(y), max(y), by = 1)
z <- cut(y, breaks, right = TRUE, include.lowest = FALSE)
z
lval <- .intervalsplit(z)[, "upper"]
lval
levels(z) <- as.Date(lval, "1970-01-01")
z

cut(x, "days", right = FALSE, include.lowest = FALSE)
#[1] 2008-01-01 2008-01-02 2008-01-03 2008-01-04 2008-01-05
#Levels: 2008-01-01 2008-01-02 2008-01-03 2008-01-04 2008-01-05
# bzzt s/b
#[1] 2008-01-01 2008-01-02 2008-01-03 2008-01-04 <NA>
#Levels: 2008-01-01 2008-01-02 2008-01-03 2008-01-04

y <- as.numeric(x)
breaks <- seq(min(y), max(y), by = 1)
z <- cut(y, breaks, right = FALSE, include.lowest = FALSE)
z
lval <- .intervalsplit(z)[, "lower"]
lval
levels(z) <- as.Date(lval, "1970-01-01")
z


cut(x, "2 days", right = TRUE, include.lowest = TRUE)
# should be:
# if right, max(x) s/b right endpoint of rightmost interval
# if !right, min(s) s/b left endpoint of leftmost interval
#[1] 2008-01-01 2008-01-03 2008-01-03 2008-01-05 2008-01-05
#Levels: 2008-01-01 2008-01-03 2008-01-05
y <- as.numeric(x)
y
breaks <- seq(max(y), min(y) - 2, by = -2)
z <- cut(y, breaks, right = TRUE, include.lowest = TRUE)
z
lval <- .intervalsplit(z)[, "upper"]
lval
levels(z) <- as.Date(lval, "1970-01-01")
z

cut(x, "2 days", right = FALSE, include.lowest = TRUE)
#[1] 2008-01-01 2008-01-01 2008-01-03 2008-01-03 2008-01-05
#Levels: 2008-01-01 2008-01-03 2008-01-05
y <- as.numeric(x)
y
breaks <- seq(min(y), max(y) + 2, by = 2)
breaks
z <- cut(y, breaks, right = FALSE, include.lowest = TRUE)
z
lval <- .intervalsplit(z)[, "lower"]
lval
levels(z) <- as.Date(lval, "1970-01-01")
z

cut(x, "2 days", right = TRUE, include.lowest = FALSE)
#[1] <NA>       2008-01-03 2008-01-03 2008-01-05 2008-01-05
#Levels: 2008-01-03 2008-01-05
y <- as.numeric(x)
#breaks <- seq(min(y), max(y), by = 2)
breaks <- seq(max(y), min(y), by = -2)
z <- cut(y, breaks, right = TRUE, include.lowest = FALSE)
z
lval <- .intervalsplit(z)[, "upper"]
lval
levels(z) <- as.Date(lval, "1970-01-01")
z

cut(x, "2 days", right = FALSE, include.lowest = FALSE)
#[1] 2008-01-01 2008-01-01 2008-01-03 2008-01-03 <NA>
#Levels: 2008-01-01 2008-01-03
y <- as.numeric(x)
y
breaks <- seq(min(y), max(y), by = 2)
breaks
z <- cut(y, breaks, right = FALSE, include.lowest = FALSE)
z
lval <- .intervalsplit(z)[, "lower"]
lval
levels(z) <- as.Date(lval, "1970-01-01")
z

# weeks
z <- as.Date(mondate.ymd(1970, 1, 1:7))
format(z, "%A")
as.numeric(z)
# so Sunday's = 3 mod 7
w <- z[4] + 7*(1:6)
as.numeric(w)
as.numeric(w) %% 7

# first sunday <= x
y <- as.numeric(x)
b <- y[1] %/% 7 * 7 + 3
d0 <- as.Date(b, "1970-01-01")
d0
format(d0, "%A")


x <- as.Date(mondate.ymd(2015, 10, 1:31))
x
format(x, "%A")
# start.on.monday = TRUE
cut(x, "weeks", right = TRUE, include.lowest = TRUE)
cut(x, "weeks", right = FALSE, include.lowest = TRUE)
cut(x, "weeks", right = TRUE, include.lowest = FALSE)
cut(x, "weeks", right = FALSE, include.lowest = FALSE)

# if right, end on Sat or Sun