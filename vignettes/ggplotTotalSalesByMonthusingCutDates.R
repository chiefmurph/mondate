# ggplot2 demo of summarizing total sales by month

# This solution sues the cut method for dates

library(ggplot2)

# Generate N random dates between B = beginning date & E = ending date
B <- as.Date("2015-01-01") 
E <- as.Date("2015-12-31")
set.seed(1234) # This seed will fortuitously demonstrate months with no sales.
N = 20
# There are different ways to generate random dates between two dates. This
# way takes advantage of Dates in R being represented by integers representing
# the number of days since 1/1/1970.
Date <- as.Date(sample(as.numeric(B):as.numeric(E), N, replace = TRUE), 
                origin = as.Date("1970-01-01"))

# Generate N random sales amounts from a normal distribution
Sales <- rnorm(N, 50, 10)

# Put into a data.frame for convenience for ggplot, then plot it
df <- data.frame(Date = Date, Sales = Sales)
ggplot(data = df,
       aes(x = cut(Date, breaks = "month"), Sales)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") + 
  labs(x = "Month") + 
  scale_x_discrete(labels = function(x) {format(as.Date(x), format = "%Y-%m")})
# The above has the advantage of missing 5, 6 10

# To force empty" sales for missing months, create NA "sale" amounts
# for all the months spanned by the Date's
# The best way to do that is to "cut" the Date values, then use the
# "levels" of the factor that is returned. See ...
as.Date(levels(cut(Date, breaks = "months", include.lowest = TRUE))) # works

# So create a "filler" data.frame with those dates and zero sales
filler <- data.frame(Date = as.Date(levels(cut(Date, breaks = "months"))),
                      Sales = NA)

# Now run ggplot with "filler" appended to the original data
ggplot(data = rbind(df, filler),
       aes(x = cut(Date, breaks = "month"), Sales)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar",
               na.rm = TRUE) + 
  labs(x = "Month") + 
  scale_x_discrete(labels = function(x) {format(as.Date(x), format = "%Y-%m")})


# This extra step can be optionally executed in a function

plotBarByMonth <- function(data, x, y, all.months = TRUE) {
  cutx <- cut(data[[x]], breaks = "month")
  newdata <- data.frame(cutx = cutx, y = data[[y]])
  if (all.months) {
    levx <- levels(cutx)
    newdata <- rbind(newdata,
                     data.frame(cutx = factor(levx),
                                y = rep(as.numeric(NA), length(levx))))
  } 
  ggplot(data = newdata, aes(x = cutx, y = y)) +
    stat_summary(fun.y = sum, geom = "bar", na.rm = TRUE) +
    labs(x = "Month", y = y) +
    scale_x_discrete(labels = function(x) {format(as.Date(x), format = "%Y-%m")})
}


plotBarByMonth(df, "Date", "Sales")
plotBarByMonth(df, "Date", "Sales", all.months = FALSE)
