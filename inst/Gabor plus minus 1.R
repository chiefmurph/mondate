x <- mondate.ymd(2000:2003,6,15)
from <- mondate(cut(min(x), "month", right = FALSE))
to <- mondate(as.Date(max(x)) - 1) + 1 # - 1 DAY + 1 MONTH
mondate(seq(as.Date(from), as.Date(to), by = "month"))

# I think this is what he wanted
as.Date(seq(from, to, by = "month"))


# On 7/2/13 I proposed
from <- mondate(cut(min(x), "month", right = TRUE)) - 1
to <- mondate(cut(max(x), breaks = "month", right = TRUE))
(s <- seq(from, to, by = 1))
(s <- s + as.difftime(1, units = "days"))
# which gave ...
#  [1] 06/01/2000 07/01/2000 08/01/2000 09/01/2000 10/01/2000 11/01/2000 12/01/2000
# [8] 01/01/2001 02/01/2001 03/01/2001 04/01/2001 05/01/2001 06/01/2001 07/01/2001
# [15] 08/01/2001 09/01/2001 10/01/2001 11/01/2001 12/01/2001 01/01/2002 02/01/2002
# [22] 03/01/2002 04/01/2002 05/01/2002 06/01/2002 07/01/2002 08/01/2002 09/01/2002
# [29] 10/01/2002 11/01/2002 12/01/2002 01/01/2003 02/01/2003 03/01/2003 04/01/2003
# [36] 05/01/2003 06/01/2003 07/01/2003
# And he agreed that's what he wanted
# Then he could
(d <- as.Date(s))
cut(as.Date(x), breaks = d)
# which gives ...
# [1] 2000-06-01 2001-06-01 2002-06-01 2003-06-01
# 37 Levels: 2000-06-01 2000-07-01 2000-08-01 2000-09-01 2000-10-01 ... 2003-06-01
# Can now ...
cutmondate(as.Date(x), breaks = "month")
# [1] 2000-06-01 2001-06-01 2002-06-01 2003-06-01
# 37 Levels: 2000-06-01 2000-07-01 2000-08-01 2000-09-01 2000-10-01 ... 2003-06-01