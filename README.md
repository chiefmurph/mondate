# mondate

mondate is an R package that keeps track of dates in units of elapsed fractional months.
mondate is motivated by Damien Laker's 2008 paper 
"Time Calculations for Annualizing Returns: the Need for Standardization" 
in *The Journal of Performance Measurement*. 

`zoo` has similar functionality via the 'frac=' argument of `as.Date.yearmon`.

## Beginning of the day or end of the day?

R's `Date` class identifies a calendar day with a numeric value
corresponding to *the beginning of the day*. 
This agrees with an ISO-8601 recommendation, 
which itself
conforms with the tradition of identifying
a scientific experiment with the date it begins.
`mondate`, on the other hand,
adopts the accounting perspective
that measures actual performance *in hindsight*,
using words such as "as of January"
or "as of year end."

### Under the hood
The numeric value in a
`Date` or `POSIX` object
is the number of seconds since the beginning of UNIX.
The numeric value in a mondate object
is the number of elapsed months since the end of 1999.
It's easier to quickly see that `mondate(12)` equals
year end 2000 than to have to remember when UNIX was invented.

## Date format

If you instantiate mondates using the international standard
YYYY-MM-DD,
it will assume that's how you want to `show` them later.
If instantiated in US format MM/DD/YYYY,
it will `show` them that way.
If you don't specify the format -- 
`mondate(12)` for example --
mondate will attempt to determine your preference by reading something from your OS.
But that is a difficult feature for this author to test as they
have not yet had the opportunity to maintain mondate ouside North America.

## Arithmetic

The difference between two mondates is the number of months between those days.
That makes it easy to measure the age of an accounting object relative
to the month or year it arose.
For instance,
the distance between two year-end accounting statements is 
always a multiple of 12.
A year equals 12 months in mondate by definition.

However, specifying the beginning date for mondate's arithmetic
can trip up a user.
The reason is perhaps
best explained with a simple word problem.

Your son Jacob was born on January 1, 2021.
How old will Jacob be at the end of the year?
Isn't that just 
```
mondate("12/31/2021") - mondate("1/1/2021")
```
No, that will be one thirty-firsts of a month short because
`mondate("1/1/2021")` corresponds to the end of January 1st,
not the beginning.
To measure the age from the beginning of a period
to the end of another period,
use 
```
mondate(end_of_period) - mondate(day_before(beginning_of_period))
```
where `day_before(Date) = Date - 1`
for objects of class `Date`.

### Homework

What is the numeric value under the hood of `mondate("1/1/2021")`?
Check that your answer equals 21*12+1/31 with `as.numeric(mondate("1/1/2021"))`.
