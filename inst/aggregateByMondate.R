dates <- seq(as.Date("2011-10-01"), length.out=60, by="+1 day")

set.seed(1)
dat <- data.frame(
  suburb = rep(LETTERS[24:26], times=c(100, 200, 300)),
  Date_of_Onset = c(
    sample(dates-30, 100, replace=TRUE),
    sample(dates,    200, replace=TRUE),
    sample(dates+30, 300, replace=TRUE)
  ),
  amount = rnorm(600, 1000, 200)
)

dat$m <- mondate(dat$Date_of_Onset, displayFormat = "%Y-%m")

head(dat)
aggregate(dat$amount, by = list(dat$m), FUN = sum)
aggregate(dat$amount, by = list(format(dat$Date_of_Onset, format = "%Y-%m")), FUN = sum)

library(zoo)
begin <- as.yearmon("2008-01-01", frac = 0)
end <- as.yearmon("2008-12-31", frac = 1)
end - begin
end2 <- as.yearmon("2008-12-31", frac = .5)
end-end2 # why zero?
