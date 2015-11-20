# A mondate Apology

The following types of sentences can be found throughout financial services:

A. The value of ABC Company liabilities as of December 31, 2015 is $100M.
B. The age of accident year 2015 reported loss as of December 31, 2015 is 12 months.

These two sentences are difficult to model in R.
The mondate2 package was written to implement a working model of sentences A and B.

First, a definition of Accounting Date.

> Definition: Accounting Date  
The cutoff date for reflecting events and recording amounts as paid or unpaid
in a financial statement or accounting system. 
The accounting date is sometimes referred to as the “as of” 
date.^[Casualty Actuarial Society *Statement
of Principles Regarding Property and Casualty Unpaid Claims Estimates*]

### Example

ABC Ins. Co.'s actuary Ed uses data as of 12/31/2015 
to estimate ABC's 12/31/2015 IBNR.
This work occurs during the month of January 2016. 
ABC closes the books during February using Ed?s estimates. 
It is understood throughout the accounting community that 
Ed's estimates are deemed to have taken effect as of 12/31/2015. 
They did not "occur" during 2016 in the sense of having 
an impact on changes between ABC's accounting statements 
as of 12/31/2015 and as of 12/31/2016.

Although financial statements are always stated as of a date, 
say, December 31, 2015,
it would be more complete to assign a time as well.
Some say "any particular moment on the 31st" could be used.^["it 
would be more accurate to write December 31, 20XX, 11:59:59, 
or any particular moment on the 
31st." http://www.investopedia.com/university/accounting/accounting5.asp]
However, payments can potentially occur up to and including midnight
December 31, 2015,
so if a time designator is to be used, 
midnight ending the day
would be the most appropriate.

Actually, ISO has a standard way to indicate the last moment of 
December 31, 20XX: "24:00".^["Midnight ...
may be referred to as either '00:00' or '24:00'.
The notation '00:00' is used at the beginning of a calendar day and 
is the more frequently used. 
At the end of a day use '24:00'. 
'2007-04-05T24:00' is the same instant as '2007-04-06T00:00'. 
www.wikipedia.org/wiki/ISO_8601]
Indeed, the two character strings
"2015-12-31 24:00:00" and "2016-01-01 00:00:00"
represent the same instant in time.

> Definition: Age  
The "age" of event E as of AOD is the 
length of time between the time event E occurs and the as of date (AOD).

Note that units are not mentioned in the definition of age. 
This is because the use case's frame of reference should 
determine the appropriate unit. 
As sentence B indicates, a month is frequently the appropriate 
unit for actuarial analysis. 

Sentences A and B can be modeled by base R, 
but with difficulty. First the easy one.

## Sentence A

It is well known that international standards (ISO 8601) 
say that a day begins at time zero that day. 
Although ISO realizes that it is possible to define a day 
as ending at midnight 24:00:00, 
they also realize that midnight one day is the same as 
time zero 00:00:00 the following day and chose the latter 
approach for reasons that are not well documented.
Base R follows that standard and represents an instant of time 
as the number of seconds that have transpired since the beginning of 1970.

On the other hand, 
in the accounting world, 
when ABC's auditors consider all 2016 financial decisions 
impacting 12/31/2015 financial statements to have occurred "as of 12/31/2015", 
it seems natural to consider those decisions to have taken place 
at the *end of the day 12/31/2015* not the *beginning of the day 1/1/2016*, 
although those two phrases represent the same point in time.

In the base R environment the "name" for the instant in time 
separating 2015 from 2016 is `2016-01-01 00:00:00`, 
or just `2016-01-01`. 
One way to implement the phrase `as of 12/31/2015` is by the following algorithm:

* Find the day corresponding to "12/31/2015":	as.Date("2015-12-31")
* Add one day:	as.Date("2015-12-31") + 1

(I told you this one was easy! 
On the other hand, who wants to have to worry about 
having to do that calculation all the time?)

## Sentence B

It is well known that "months" are comprised of different 
numbers of days.
Although it is widely accepted that the length of time between 
the beginning and end of a month is one "month",
it is also recognized that month durations
cannot be defined consistently in units of days.
Those observations give rise to a different type of unit:
"elapsed month" or "emonth".

> Definition: Elapsed Month ("emonth")  
The length of time between two instants in time *t1* and *t2*
in units of "emonths" is  
  mondate(*t2*) - mondate(*t1*)
where
  *mondate(t)* is a real number representing
the number of months between the beginning of 1970 and
time *t*.

> Example  
mondate("1970-02-01") = 1  
mondate("1971-01-01") = 12
mondate("1970-02-15 00:00:00") = 1.5 because the beginning of 
February 15, 1970 is halfway through February 1970.
as.Date("1971-01-01") - as.Date("1971-02-01") = 10.5 emonths 

Conversely, given any real number t, 
it is straightforward to find the time ?YYYYMMDD?HH:MM:SS.zzzzz? 
that is t months away from the beginning of 19790 by 
first counting whole months, 
then counting into the next month (if necessary) the number of days and 
seconds corresponding to that month per the fractional value of t.

> Defintion: Elapsed Years  
The number of elapsed years between times *t1* and *t2*
equals the number of elapsed months divided by 12.

### Case Study:

ABC Ins. Co. started on 1/1/2010 to write earthquake insurance in California. 
As claims are made, 
ABC defines the occurrence date of a claim claim to be the date of 
the earthquake and stores that date in an ISO-8601 compliant o
bject (`POSIXt`). 
ABC has had good luck so far -- only 20 claims have been made. 
Here are their occurrence dates and their know values as of 12/31/2015:

data ?

To complete Sentence B, we need one more definition:

> Definition: The age of accident year AY as of AOD is AOD - AY-01-01.

The accident year age of the 20 claims is the vector  
	mondate(?2015-12-31?) ? year(occurrenceDate)-01-01



http://smallbusiness.chron.com/differences-dates-between-balance-sheet-income-sheet-24881.html

"Balance Sheet Date
A balance sheet often states that it is prepared as of a specific date, referred to as the balance sheet date. The balance sheet reports on a company’s financial conditions, namely the values of the company’s assets, liabilities and shareholders’ equity. Values are measured in terms of their monetary amounts at particular points in time rather than over any periods. At the end of an accounting cycle, with the accounting books closed to recording new business transactions, companies can summarize their financial conditions as of the cycle's end."

http://www.casact.org/professionalism/standards/princip/SOP-Regarding-Property-and-Casualty-Unpaid-Claims-Estimates_Final%204-22-2015.pdf


## Bibliography

Here's a pretty good paper about dates, Lubridate, Chron:
http://biostat.mc.vanderbilt.edu/wiki/pub/Main/ColeBeck/datestimes.pdf