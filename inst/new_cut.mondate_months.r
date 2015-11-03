(x <- mondate(seq(0, 12, by = .5)))
(y <- as.numeric(x))
ceiling(max(y))
b <- rev(seq(ceiling(max(y)), min(y) - 1, by = -1))
cut(y, b)
(n <- length(y))
(rngy <- range(y))

(b <- max(y) %/% 12 * 12)

x <- mondate.ymd(2008, 6)
x
#(y <- as.numeric(x))

(z <- ymd(range(x)))

# right
step = 3
b <- rev(seq(mondate.ymd(z[2, "year"], z[2, "month"]),
         mondate.ymd(z[1, "year"], z[1, "month"]) - step,
         by = -step))
b
cut(x, b, right = TRUE, include.lowest = FALSE)

# !right
b <- seq(mondate.ymd(z[1, "year"], z[1, "month"]) - 1,
         mondate.ymd(z[2, "year"], z[2, "month"]) + step - 1,
                    by = step)
b
res <- cut(x, b, right = TRUE, include.lowest = FALSE)
res

cut(as.Date(x), breaks = as.Date(c("2008-06-01", "2008-07-01")))

(y <- as.numeric(x))
(b <- min(y))
seq(b + .032, max(y) + .032, by = 1)

checkTrue(!is.na(y))
checkEquals(levels(y), "06/30/2008")
y <- cut(x, "month", right = FALSE)
checkTrue(!is.na(y))
checkEquals(levels(y), "05/31/2008")
