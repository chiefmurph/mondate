test.as.numeric.mondate <- function() {
  # mondate arithmetic is not distributive.
  checkEquals(as.numeric(mondate.ymd(2014, 12, 31)), 180)
  checkEquals(as.numeric(mondate.ymd(2014, 12, 31, timeunits = "years")), 
              180)
  # get timeunits from the mondate
  checkEqualsNumeric(as.numeric(mondate.ymd(2014, 12, 31, timeunits = "years"), convert = TRUE), 
              15)
  # get timeunits from as.numeric
  checkEqualsNumeric(as.numeric(mondate.ymd(2014, 12, 31), timeunits = "years", convert = TRUE), 
                     15)
}
