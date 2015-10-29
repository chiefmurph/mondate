# http://stackoverflow.com/questions/12161154/formatting-histogram-x-axis-when-working-with-dates-using-r
dates <- seq(as.Date("2011-10-01"), length.out=60, by="+1 day")

set.seed(1)
dat <- data.frame(
  suburb = rep(LETTERS[24:26], times=c(100, 200, 300)),
  Date_of_Onset = c(
    sample(dates-30, 100, replace=TRUE),
    sample(dates,    200, replace=TRUE),
    sample(dates+30, 300, replace=TRUE)
  )
)

library(scales)
library(ggplot2)
ggplot(dat, aes(x=Date_of_Onset, fill=suburb)) + 
  stat_bin(binwidth=1, position="identity") + 
  scale_x_date(breaks=date_breaks(width="1 month"))

# Note the use of position="identity" to force each bar to originate 
# on the axis, otherwise you get a stacked chart by default.

library(date)
dat.geo <- dat
hist(dat.geo$Date_of_Onset[(dat.geo$suburb=="X")], "weeks", 
     format = "%d %b %y", freq=T, col=rgb(0,0,0,1), axes=F, main="")
hist(dat.geo$Date_of_Onset[(dat.geo$suburb=="Y")], "weeks", 
     format = "%d %b %y", freq=T, main="", col=rgb(1,1,1,.6), add=T, axes=F)
axis.Date(1, at=seq(as.Date("2011-10-10"), as.Date("2012-03-19"), by="2 weeks"),
          format="%d %b %y")
axis.Date(1, at=seq(as.Date("2011-10-10"), as.Date("2012-03-19"), by="weeks"), 
          labels=F, tcl= -0.5)
ggplot(dat.geo,aes(x=Date_of_Onset, group=suburb, fill=suburb))+
  stat_bin(colour="black", binwidth=1, alpha=0.5,
           position="identity") + # theme_bw()+ # didn't work
  xlab("Date of onset of symptoms")+
  ylab("Number of cases")+
  scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%b %y"))

# 
p <- ggplot(dat.geo,aes(x=Date_of_Onset, group=suburb, fill=suburb))+
  stat_bin(colour="black", binwidth=1, alpha=0.5,
           position="identity") + # theme_bw()+ # didn't work
  xlab("Date of onset of symptoms")+
  ylab("Number of cases")
print(p)
print(
  p + scale_x_date(breaks=date_breaks(width="1 month"), labels=date_format("%b %y"))
  )
p + scale_x_date(breaks=date_breaks(width="1 month"))
p + scale_x_date(breaks=date_breaks(width="1 month"))
