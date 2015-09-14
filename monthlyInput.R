########################################################################
##
## Read monthly data
##
########################################################################
rev201507 <- read.csv("rawData\\revenue201507.csv")
rev201508 <- read.csv("rawData\\revenue201508.csv")
rev20150914 <- read.csv("rawData\\revenue20150901_0914.csv")

########################################################################
##
## Transform data type
##
########################################################################
rev201507$OrderDate <- strptime(rev201507$OrderDate, "%Y/%m/%d")
rev201507$YMD <- strptime(rev201507$YMD, "%Y/%m/%d")
rev201508$OrderDate <- strptime(rev201508$OrderDate, "%Y/%m/%d")
rev201508$YMD <- strptime(rev201508$YMD, "%Y/%m/%d")
rev20150914$OrderDate <- strptime(rev20150914$OrderDate, "%Y/%m/%d")
rev20150914$YMD <- strptime(rev20150914$YMD, "%Y/%m/%d")

########################################################################
##
## Get SMD
##
########################################################################
MonthRevenue <- function(revMonth, startMonth, EndMonth){
  #   revMonth$OrderDate <- strptime(revMonth$OrderDate, "%Y/%m/%d")
  #   revMonth$YMD <- strptime(revMonth$YMD, "%Y/%m/%d")
  monthStart <- strptime(startMonth, "%Y/%m/%d"); monthEnd <- strptime(EndMonth, "%Y/%m/%d")
  monIndex <- which(revMonth$OrderDate >= monthStart & revMonth$OrderDate <= monthEnd)
  monOrder <- revMonth[monIndex, ]
  smdOrder <- monOrder[monOrder$YMD >= monthStart & monOrder$YMD <= monthEnd, ]
  return(sum(smdOrder$US.Amt))
}
MonthRevenue(rev201508, "2015/08/01", "2015/08/31")
MonthRevenue(rev201507, "2015/07/01", "2015/07/31")
MonthRevenue(rev20150914, "2015/09/01", "2015/09/30")
