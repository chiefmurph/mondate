# mondate
mondate is an R package that keeps track of dates in terms of elapsed fractional months.
mondate is motivated by Damien Laker's 2008 paper 
"Time Calculations for Annualizing Returns: the Need for Standardization" 
in The Journal of Performance Measurement. 

The zoo package has similar functionality via the 'frac=' argument of as.Date.yearmon.

## Beginning of the day or end of the day?

R's Date class identifies a calendar day with a numeric value
corresponding to the beginning of the day. 
This conforms with the experimental scientist's identification of
an experiment with the time it begins.
mondate, on the other hand,
conforms with the accounting perspective
that measures actual performance in hindsight
with words such as "as of January"
or "as of year end."

### Under the hood
The numeric value in a
`Date` or `POSIX` object
is the number of seconds since the beginning of UNIX.
The numeric value in a mondate object
is the number of elapsed months since the end of 2000.
It's easier to see that the mondate(12) corresponds
to year end 2001 than to remember when UNIX was invented.

## Date format

If you instantiate mondates using the international standard
YYYY-MM-DD,
it will assume that's how you want to see them displayed.
If you use US format MM/DD/YYYY,
it will "show" them that way.
If you don't specify the format -- 
(mondate(12)) --
mondate will attempt to determine your preference by reading something from your OS.
But that is a difficult feature for this author to test as they
have not had the opportunity to maintain mondate ouside North America.

## Arithmetic

The difference between two mondates is the number of months between those days.
That makes it easy to measure the age of an accounting object relative
to the month or year it arose.
For instance,
the distance between two year-end accounting statements is 
exactly 12 months.
And one year will always be 12 months.

### Under the hood 

mondate was originally written by an actuary
who wanted to build development triangles aged in months.

One final piece of advice,
best explained with a story.
Your son Jacob was born on January 1, 
the first baby born in his hospital in 2021.
How old will Jacob be at the end of the year?
Isn't that just 

mondate("12/31/2021") - mondate("1/1/2021")?

No, that will be one thirtyfirsts of a month short.
To measure the age from the beginning of a period
to the end of another period,
use 

mondate(end_of_period) - mondate(day(beginning_of_period))

where

day_before(Date) = Date - 1

for object of R class `Date`.
