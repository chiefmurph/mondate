test.seq.mondate <- function() {
  # by missing
  seq.mondate(from = mondate("1/31/2014"), length.out = 6)
  mondate.ymd(2014, 1:6)  
  seq.mondate(from = mondate("1/1/2014"), length.out = 6)  
  mondate.ymd(2014, 1:6, 1)  
  seq.mondate(from = mondate("2/28/2014"), length.out = 6)
  mondate.ymd(2014, 2:7)
  seq.mondate(from = mondate("2/1/2014"), length.out = 6)
  c(mondate.ymd(2014, 2, 1), mondate.ymd(2014, 3:7, 2))

  seq.mondate(from = mondate("01/31/2014"), to = mondate("04/30/2014"))
  mondate.ymd(2014, 1:4)  

  # by numeric
  seq(from = mondate("8/31/2000"), by = -2)
  mondate.ymd(2000, c(8, 6, 4, 2))
    
  seq.mondate(to = mondate("04/30/2000"), by = 2)
  mondate.ymd(2000, c(1, 3))
  
  seq.mondate(from = mondate("04/30/2014"), to = mondate("08/31/2014"), by = 2)
  mondate.ymd(2014, c(4, 6, 8))  

  # by character
  checkException(seq.mondate(from = mondate("2000-04-30"), by = "days"))
  checkException(seq.mondate(to = mondate("2000-08-31"), by = "days"))
  seq.mondate(from = mondate("04/30/2014"), to = mondate("08/31/2014"), by = "days")  
  seq.mondate(from = mondate("2000-04-30"), to = mondate("2000-08-31"), by = "weeks")  
  seq.mondate(from = mondate("2000-04-30"), to = mondate("2000-08-31"), by = "months")  
  seq.mondate(from = mondate("2000-04-30"), to = mondate("2000-08-31"), by = "years")  
  seq.mondate(from = mondate("2000-04-30"), to = mondate("2000-08-31"), by = "quarters")  
  seq.mondate(from = mondate("2000-04-30"), to = mondate("2000-08-31"), by = "3 months")  
  seq.mondate(from = mondate("2000-04-30"), to = mondate("2000-08-31"), by = "4 years")  
  seq.mondate(from = mondate("2000-04-30"), to = mondate("2000-08-31"), by = "2 quarters")  

  as.character(seq.mondate(from = mondate("04/30/2014"), to = mondate("05/9/2014"), by = "days"))
  c("04/30/2014", "05/01/2014", "05/02/2014", "05/03/2014", "05/04/2014", "05/05/2014", "05/06/2014", "05/07/2014", "05/08/2014", "05/09/2014")  
  as.character(seq.mondate(from = mondate("04/30/2014"), to = mondate("05/31/2014"), by = "weeks"))
  c("04/30/2014", "05/07/2014", "05/14/2014", "05/21/2014", "05/28/2014")
  seq.mondate(from = mondate("04/30/2014"), to = mondate("08/31/2014"), by = "months")
  mondate.ymd(2014, 4:8)  
  seq.mondate(from = mondate("04/30/2014"), to = mondate("08/31/2016"), by = "years")  
  mondate.ymd(2014:2016, 4)  
  seq.mondate(from = mondate("04/30/2014"), to = mondate("08/31/2014"), by = "3 months")  
  mondate.ymd(2014, c(4, 7))  
  seq.mondate(from = mondate("04/30/2014"), to = mondate("12/31/2014"), by = "2 quarters")  
  mondate.ymd(2014, c(4, 10))  
    

  # Date
  from <- as.Date("2014-01-01")
  seq.mondate(from, by = "months", length = 7)
  to <- as.Date("2015-01-01")
  seq.mondate(to = to, by = "months", length = 7)
  seq.mondate(from = from, to = to, by = "months")
  
  # POSIXlt
  from <- as.POSIXlt("2014-01-01")
  seq.mondate(from, by = "months", length = 13)
  to <- as.POSIXlt("2015-01-01")
  seq.mondate(to = to, by = "months", length = 6)
  seq.mondate(from = from, to = to, by = "months")

  # POSIXct
  from <- as.POSIXct("2014-01-01")
  seq.mondate(from, by = "months", length = 13)
  to <- as.POSIXct("2015-01-01")
  seq.mondate(to = to, by = "months", length = 6)
  seq.mondate(from = from, to = to, by = "months")

  }

# 10/10/2015
#seqmondate(as.Date("2015-01-31"), length = 6, by = "months") # used to not work
# used to give
# [1] "2015-01-31" "2015-03-01" "2015-03-31" "2015-05-01" "2015-05-31" "2015-07-01"

test.seqmondate <- function() {
  # from, length
  # last day of the month
  checkTrue(all(
    seqmondate(from = mondate("1/31/2014"), length.out = 6) == 
      mondate.ymd(2014, 1:6)
    ))
  # first day of the month -- not as accurate as last day of month, an integer
  checkTrue(all(
    as.character(seqmondate(from = mondate("1/1/2014"), length.out = 6)) ==
      as.character(mondate.ymd(2014, 1:6, 1))
    ))

  # from, to
  # last day of the month
  checkTrue(all(
    seqmondate(from = mondate("01/31/2014"), to = mondate("04/30/2014")) ==
      mondate.ymd(2014, 1:4)
    ))
  # first day of the month
  checkTrue(all(
    as.character(seqmondate(from = mondate("1/1/2014"), to = mondate("4/1/2014"))) ==
      as.character(mondate.ymd(2014, 1:4, 1))
  ))
  
  # from, to, by
  # last day of the month
  checkTrue(all(
    seqmondate(from = mondate("01/31/2014"), to = mondate("04/30/2014"), by = 2) ==
      mondate.ymd(2014, c(1, 3))
  ))

  # last day of the month
  checkTrue(all(
    as.character(seqmondate(from = mondate("01/1/2014"), to = mondate("11/1/2014"), by = 2)) ==
      c("01/01/2014", "03/01/2014", "05/01/2014", "07/01/2014", "09/01/2014", "11/01/2014")
  ))
  
  # 'to' alone works for mondate's as seq works for integers
  checkTrue(all(
    as.character(seqmondate(to = mondate("04/30/2000"), by = 2)) ==
      c("01/31/2000", "03/31/2000")
    ))

  # by character
  # when by = "days" uses seq.Date, so both from and to must be specifiec
  checkException(seqmondate(from = mondate("2000-04-30"), by = "days"))
  checkException(seqmondate(to = mondate("2000-08-31"), by = "days"))
  checkException(seqmondate(from = mondate("2000-04-30"), by = "days", length = 3))
  
  checkTrue(all(
    as.character(
      seqmondate(from = mondate("04/30/2014"), to = mondate("05/6/2014"), by = "days")) ==
        c("04/30/2014", "05/01/2014", "05/02/2014", "05/03/2014", "05/04/2014", 
          "05/05/2014", "05/06/2014")
    ))
  checkTrue(all(
    as.character(
      seqmondate(from = mondate("2000-04-30"), to = mondate("2000-05-31"), by = "weeks")) ==
      c("2000-04-30", "2000-05-07", "2000-05-14", "2000-05-21", "2000-05-28")
  ))
      
  m <- seqmondate(from = mondate("2000-01-31"), to = mondate("2000-06-30"), by = "months")
  checkTrue(all(m == mondate.ymd(2000, 1:6)))
  
  # only one value here
  checkTrue(length(
    seqmondate(from = mondate("2000-04-30"), to = mondate("2000-08-31"), by = "years")) == 1)
  checkTrue(all(
    as.character(
      seqmondate(from = mondate("2000-04-30"), to = mondate("2005-04-30"), by = "years")) ==
      c("2000-04-30", "2001-04-30", "2002-04-30", "2003-04-30", "2004-04-30", "2005-04-30")
  ))
  checkTrue(all(
    as.character(
      seqmondate(from = mondate("2015-03-31"), to = mondate("2015-12-31"), by = "quarters")) ==
      c("2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31")
  ))
  checkTrue(all(
    as.character(
      seqmondate(from = mondate("2015-03-31"), to = mondate("2015-12-31"), by = "3 months")) ==
      c("2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31")
  ))
  checkTrue(all(
    as.character(
      seqmondate(from = mondate("2014-12-31"), to = mondate("2015-12-31"), by = "2 quarters")) ==
      c("2014-12-31", "2015-06-30", "2015-12-31")
  ))

  # repeat above with Date's

  # from, length
  # last day of the month
  checkTrue(all(
    seqmondate(from = as.Date("2014-01-31"), length.out = 6) == 
      as.Date(mondate.ymd(2014, 1:6))
  ))
  # first day of the month -- not as accurate as last day of month, an integer
  checkTrue(all(
    seqmondate(from = as.Date("2014-01-01"), length.out = 6) ==
      as.Date(mondate.ymd(2014, 1:6, 1))
  ))
  
  # from, to -- defaults to by = "months"
  # last day of the month
  checkTrue(all(
    seqmondate(from = as.Date("2014-01-31"), to = as.Date("2014-04-30")) ==
      as.Date(mondate.ymd(2014, 1:4))
  ))
  # first day of the month
  checkTrue(all(
    seqmondate(from = as.Date("2014-01-01"), to = as.Date("2014-04-01")) ==
      as.Date(mondate.ymd(2014, 1:4, 1))
    ))
  
  # from, to, by
  # last day of the month
  checkTrue(all(
    seqmondate(from = as.Date("2014-01-31"), to = as.Date("2014-04-30"), by = 2) ==
      as.Date(mondate.ymd(2014, c(1, 3)))
  ))
  
  # last day of the month
  checkTrue(all(
    as.character(seqmondate(from = as.Date("2014-01-01"), to = as.Date("2014-11-01"), by = 2)) ==
      c("2014-01-01", "2014-03-01", "2014-05-01", "2014-07-01", "2014-09-01", "2014-11-01")
  ))
  
  # 'to' alone does not work for Date's
  checkException(as.character(seqmondate(to = as.Date("2000-04-30"), by = 2)))
  
  seqmondate(from = mondate("2014-04-30"), to = mondate("2014-08-31"), by = 2)
  mondate.ymd(2014, c(4, 6, 8))  
  
  # by character
  # when by = "days" uses seq.Date, so both from and to must be specifiec
  checkException(seqmondate(from = as.Date("2000-04-30"), by = "days"))
  checkException(seqmondate(to = as.Date("2000-08-31"), by = "days"))
  checkException(seqmondate(from = as.Date("2000-04-30"), by = "days", length = 3))
  
  # days and weeks work as with seq.Date b/c that's what eventually gets called
  checkTrue(all(
      seqmondate(from = as.Date("2014-04-30"), to = as.Date("2014-05-06"), by = "days") ==
        seq(from = as.Date("2014-04-30"), to = as.Date("2014-05-06"), by = "days")
  ))
  checkTrue(all(
    seqmondate(from = as.Date("2014-04-30"), to = as.Date("2014-12-06"), by = "weeks") ==
      seq(from = as.Date("2014-04-30"), to = as.Date("2014-12-06"), by = "weeks")
  ))
  
  checkTrue(all(
    as.character(
      seqmondate(from = as.Date("2000-01-31"), to = as.Date("2000-06-30"), by = "months")) ==
      c("2000-01-31", "2000-02-29", "2000-03-31", "2000-04-30", "2000-05-31", "2000-06-30")
  ))

  # only one value here
  checkTrue(length(
    seqmondate(from = as.Date("2000-04-30"), to = as.Date("2000-08-31"), by = "years")) == 1)
  checkTrue(all(
    as.character(
      seqmondate(from = as.Date("2000-04-30"), to = as.Date("2005-04-30"), by = "years")) ==
      c("2000-04-30", "2001-04-30", "2002-04-30", "2003-04-30", "2004-04-30", "2005-04-30")
  ))
  checkTrue(all(
    as.character(
      seqmondate(from = as.Date("2000-04-01"), to = as.Date("2005-04-01"), by = "years")) ==
      c("2000-04-01", "2001-04-01", "2002-04-01", "2003-04-01", "2004-04-01", "2005-04-01")
  ))
  checkTrue(all(
    as.character(
      seqmondate(from = as.Date("2015-03-31"), to = as.Date("2015-12-31"), by = "quarters")) ==
      c("2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31")
  ))
  checkTrue(all(
    as.character(
      seqmondate(from = as.Date("2015-03-31"), to = as.Date("2015-12-31"), by = "3 months")) ==
      c("2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31")
  ))
  checkTrue(all(
    as.character(
      seqmondate(from = as.Date("2014-12-31"), to = as.Date("2015-12-31"), by = "2 quarters")) ==
      c("2014-12-31", "2015-06-30", "2015-12-31")
  ))
  
  # POSIXlt
  from <- as.POSIXlt("2014-01-01")
  checkTrue(all(
    as.character(
      seqmondate(from, by = "months", length = 13)) == 
      c("2014-01-01", "2014-02-01", "2014-03-01", "2014-04-01", "2014-05-01", 
        "2014-06-01", "2014-07-01", "2014-08-01", "2014-09-01", "2014-10-01", 
        "2014-11-01", "2014-12-01", "2015-01-01")
  ))
  to <- as.POSIXlt("2015-01-01")
  checkException(seqmondate(to = to, by = "months", length = 6))
  checkTrue(all(
    as.character(
      seqmondate(from = from, to = to, by = "months")) ==
      c("2014-01-01", "2014-02-01", "2014-03-01", "2014-04-01", "2014-05-01", 
        "2014-06-01", "2014-07-01", "2014-08-01", "2014-09-01", "2014-10-01", 
        "2014-11-01", "2014-12-01", "2015-01-01")
  ))
  
  # POSIXct
  from <- as.POSIXct("2014-01-01")
  checkTrue(all(
    as.character(
      seqmondate(from, by = "months", length = 13)) == 
      c("2014-01-01", "2014-02-01", "2014-03-01", "2014-04-01", "2014-05-01", 
        "2014-06-01", "2014-07-01", "2014-08-01", "2014-09-01", "2014-10-01", 
        "2014-11-01", "2014-12-01", "2015-01-01")
  ))
  to <- as.POSIXct("2015-01-01")
  checkException(seqmondate(to = to, by = "months", length = 6))
  checkTrue(all(
    as.character(
      seqmondate(from = from, to = to, by = "months")) ==
      c("2014-01-01", "2014-02-01", "2014-03-01", "2014-04-01", "2014-05-01", 
        "2014-06-01", "2014-07-01", "2014-08-01", "2014-09-01", "2014-10-01", 
        "2014-11-01", "2014-12-01", "2015-01-01")
  ))
  
  }

