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

