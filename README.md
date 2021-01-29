# mondate
mondate is an R package that keeps track of dates in terms of elapsed fractional months.
mondate is motivated by Damien Laker's 2008 paper 
"Time Calculations for Annualizing Returns: the Need for Standardization" 
in The Journal of Performance Measurement. 

The zoo package has similar functionality via the 'frac=' argument of as.Date.yearmon.

R's Date class identifies a calendar day with a numeric value
corresponding to the beginning of the day. 
This conforms with the experimental scientist's identification of
an experiment with the time it begins.
Business, on the other hand,
tends to measure performance in units just passed.
mondate identifies a calendar day with a numeric value
corresponding to the end of the day,
simplifying accounting "as-of" times such as
"end of business today",
"month end", and
"year end".

Overthinking:
The numeric value in a
`Date` or `POSIX` object
is the number of seconds since the beginning of UNIX.
The numeric value in a mondate object
is the number of elapsed monts since the end of 2000.
It's easier to know that the mondate value of 24 corresponds
to the end of 2002, 
versus trying to remember when UNIX was invented.

If you instantiate mondates using international standards --
YYY-MM-DD --
it will assume that's how you want to see them displayed.
If you use the US format MM/DD/YYYY,
it will display them back to you in that format.
If you don't specify the format -- 
for example, 
mondate(12), then end of 2001 --
mondate will attempt to determine your preference by read something from your OS.
But that is difficult to test for this author 
who has not had the opportunity to maintain mondate ouside the US.

The difference between two mondates is the number of months between those days.
That makes it easy to measure the age of an accounting object relative
to the month or year it arose.
For instance,
the distance between two year-end accounting statements is 
exactly 12 months.
And one year will always be 12 months.

Overthinking:
mondate was originally written by an actuary
who wanted to build development triangles aged in months.

One final piece of advice,
best explained with a story.
Your son Jacob was born on January 1, 
the first baby born in his hospital in 2021.
How old will Jacob be at the end of the year?
Isn't that just 
$$
mondate("12/31/2021") - mondate("1/1/2021")
$$

That would be one thirtyfirsts of a month short
because Jacob's age is calculated
from the beginning of the day he was born,
not the end.
