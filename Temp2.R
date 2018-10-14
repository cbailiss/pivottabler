
# changing the caption of a data group
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$renderPivot()
pt$rowGroup$childGroups[[2]]$caption <- "CC Trains"
pt$renderPivot()




# basic pivot library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$renderPivot()



# prior to v1.0, this was the only option supported.
library(dplyr)
library(lubridate)
library(pivottabler)

# get the date of each train and whether that date is a weekday or weekend
trains <- bhmtrains %>%
  mutate(GbttDateTime=if_else(is.na(GbttArrival), GbttDeparture, GbttArrival),
         DayNumber=wday(GbttDateTime),
         WeekdayOrWeekend=ifelse(DayNumber %in% c(1,7), "Weekend", "Weekday"))

# render the pivot table
pt <- PivotTable$new()
pt$addData(trains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
weekendFilter <- PivotFilters$new(pt, variableName="WeekdayOrWeekend", values="Weekend")
pt$defineCalculation(calculationName="WeekendTrains", summariseExpression="n()",
                     filters=weekendFilter, visible=FALSE)
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()", visible=FALSE)
pt$defineCalculation(calculationName="WeekendTrainsPercentage",
                     type="calculation", basedOn=c("WeekendTrains", "TotalTrains"),
                     format="%.1f %%",
                     calculationExpression="values$WeekendTrains/values$TotalTrains*100")
pt$renderPivot()


#1.0 adds the PivotFilterOverrides class...

# % of TOC Total
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName="CountTrains", summariseExpression="n()", caption="Count")
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="TOC")
pt$defineCalculation(calculationName="TOCTotalTrains", filters=filterOverrides, summariseExpression="n()", caption="TOC Total")
pt$defineCalculation(calculationName="PercentageOfTOCTrains", type="calculation", basedOn=c("CountTrains", "TOCTotalTrains"),
                     calculationExpression="values$CountTrains/values$TOCTotalTrains*100", format="%.1f %%", caption="% of TOC")
pt$renderPivot()



# Same as above, done explicitly
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName="CountTrains", summariseExpression="n()", caption="Count")
filterTotal <- PivotFilter$new(pt, variableName="TrainCategory", type="ALL")
filterOverrides <- PivotFilterOverrides$new(pt, filter=filterTotal, action="replace")
pt$defineCalculation(calculationName="TOCTotalTrains", filters=filterOverrides, summariseExpression="n()", caption="TOC Total")
pt$defineCalculation(calculationName="PercentageOfTOCTrains", type="calculation", basedOn=c("CountTrains", "TOCTotalTrains"),
                     calculationExpression="values$CountTrains/values$TOCTotalTrains*100", format="%.1f %%", caption="% of TOC")
pt$renderPivot()



# % of grand total
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName="CountTrains", summariseExpression="n()", caption="Count")
filterOverrides <- PivotFilterOverrides$new(pt, removeAllFilters=TRUE)
pt$defineCalculation(calculationName="GrandTotalTrains", filters=filterOverrides, summariseExpression="n()", caption="Grand Total")
pt$defineCalculation(calculationName="PercentageOfAllTrains", type="calculation", basedOn=c("CountTrains", "GrandTotalTrains"),
                     calculationExpression="values$CountTrains/values$GrandTotalTrains*100", format="%.1f %%", caption="% of All")
pt$renderPivot()




# Multiple of cross country express
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName="CountTrains", summariseExpression="n()", caption="Count")
filter1 <- PivotFilter$new(pt, variableName="TrainCategory", values="Express Passenger")
filter2 <- PivotFilter$new(pt, variableName="TOC", values="CrossCountry")
filterOverrides <- PivotFilterOverrides$new(pt, removeAllFilters=TRUE)
filterOverrides$add(filter1, action="replace")
filterOverrides$add(filter2, action="replace")
pt$defineCalculation(calculationName="CrossCountryExpressTrains", filters=filterOverrides, summariseExpression="n()", caption="CrossCountry Express Trains")
pt$defineCalculation(calculationName="MultipleOfCCExpressTrains", type="calculation", basedOn=c("CountTrains", "CrossCountryExpressTrains"),
                     calculationExpression="values$CountTrains/values$CrossCountryExpressTrains", format="%.2f", caption="Multiple of CC Express")
pt$renderPivot()



# % DMU
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
filterDMU <- PivotFilter$new(pt, variableName="PowerType", values="DMU")
filterOverrides <- PivotFilterOverrides$new(pt, filter=filterDMU, action="and")
pt$defineCalculation(calculationName="CountDMU", filters=filterOverrides, summariseExpression="n()", caption="DMU")
pt$defineCalculation(calculationName="CountTrains", summariseExpression="n()", caption="Count")
pt$defineCalculation(calculationName="PercentageDMU", type="calculation", basedOn=c("CountTrains", "CountDMU"),
                     calculationExpression="values$CountDMU/values$CountTrains*100", format="%.1f %%", caption="% DMU")
pt$renderPivot()


# filter override criteria can contradict and result in no value
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
filterTOC <- PivotFilter$new(pt, variableName="TOC", values="London Midland")
filterOverrides <- PivotFilterOverrides$new(pt, filter=filterTOC, action="and")
pt$defineCalculation(calculationName="CountLM", filters=filterOverrides, summariseExpression="n()", caption="CountLM")
pt$defineCalculation(calculationName="CountTrains", summariseExpression="n()", caption="Count")
pt$renderPivot()

# replace instead
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
filterTOC <- PivotFilter$new(pt, variableName="TOC", values="London Midland")
filterOverrides <- PivotFilterOverrides$new(pt, filter=filterTOC, action="replace")
pt$defineCalculation(calculationName="CountLM", filters=filterOverrides, summariseExpression="n()", caption="CountLM")
pt$defineCalculation(calculationName="CountTrains", summariseExpression="n()", caption="Count")
pt$renderPivot()

# finally, or - adding the London Midland value to other TOCs
# this is a very hypothetical example
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
filterTOC <- PivotFilter$new(pt, variableName="TOC", values="London Midland")
filterOverrides <- PivotFilterOverrides$new(pt, filter=filterTOC, action="or")
pt$defineCalculation(calculationName="CountLM", filters=filterOverrides, summariseExpression="n()", caption="CountLM")
pt$defineCalculation(calculationName="CountTrains", summariseExpression="n()", caption="Count")
pt$renderPivot()


# with context
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
filterDMU <- PivotFilter$new(pt, variableName="PowerType", values="DMU")
filterOverrides <- PivotFilterOverrides$new(pt, filter=filterDMU, action="and")
pt$defineCalculation(calculationName="CountDMU", filters=filterOverrides, summariseExpression="n()", caption="DMU")
pt$defineCalculation(calculationName="CountTrains", summariseExpression="n()", caption="Count")
pt$renderPivot(includeHeaderValues=TRUE, includeRCFilters=TRUE,
               includeCalculationFilters=TRUE, includeWorkingData=TRUE, includeEvaluationFilters=TRUE,
               includeCalculationNames=TRUE, includeRawValue=TRUE, includeTotalInfo=TRUE)



# custom override function...

trains <- bhmtrains %>%
  mutate(GbttDateTime=if_else(is.na(GbttArrival), GbttDeparture, GbttArrival),
         GbttDate=as.Date(GbttDateTime))
januaryDates <- seq(as.Date("2017-01-01"), as.Date("2017-01-31"), by="days")


# comparison to yesterday
# date filter function to return yesterday
getYesterdayDateFilter <- function(pt, filters) {
  # get the date filter
  filter <- filters$getFilter("GbttDate")
  if(is.null(filter)||(filter$type=="ALL")||(length(filter$values)>1)) {
    # there is no filter on GbttDate in this cell
    # i.e. we are in one of the total cells that covers all dates,
    # so the concept of yesterday has no meaning, so block all date
    newFilter <- PivotFilter$new(pt, variableName="GbttDate", type="NONE")
    filters$setFilter(newFilter, action="replace")
  }
  else {
    # get the date value and subtract one day
    date <- filter$values
    date <- date - 1
    filter$values <- date
  }
}
# build the pivot
library(pivottabler)
pt <- PivotTable$new()
pt$addData(trains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("GbttDate", fromData=FALSE, explicitListOfValues=as.list(januaryDates), visualTotals=TRUE)
pt$defineCalculation(calculationName="CountTrains", summariseExpression="n()", caption="Count")
filterOverrides <- PivotFilterOverrides$new(pt, overrideFunction=getYesterdayDateFilter)
pt$defineCalculation(calculationName="CountPreviousDayTrains", filters=filterOverrides, summariseExpression="n()", caption="Count Previous Day")
pt$defineCalculation(calculationName="Daily Change", type="calculation", basedOn=c("CountTrains", "CountPreviousDayTrains"),
                     calculationExpression="values$CountTrains-values$CountPreviousDayTrains", caption="Daily Change")
pt$renderPivot()


# three day rolling average
# date filter function to a three day range of dates
getThreeDayFilter <- function(pt, filters) {
  # get the date filter
  filter <- filters$getFilter("GbttDate")
  if(is.null(filter)||(filter$type=="ALL")||(length(filter$values)>1)) {
    # there is no filter on GbttDate in this cell
    # i.e. we are in one of the total cells that covers all dates,
    # so the concept of yesterday has no meaning, so block all date
    newFilter <- PivotFilter$new(pt, variableName="GbttDate", type="NONE")
    filters$setFilter(newFilter, action="replace")
  }
  else {
    # get the date value and create three day filter
    date <- filter$values
    newDates <- seq(date-1, date+1, by="days")
    filter$values <- newDates
  }
}
# build the pivot
library(pivottabler)
pt <- PivotTable$new()
pt$addData(trains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("GbttDate", fromData=FALSE, explicitListOfValues=as.list(januaryDates), visualTotals=TRUE)
pt$defineCalculation(calculationName="CountTrains", summariseExpression="n()", caption="Count")
filterOverrides <- PivotFilterOverrides$new(pt, overrideFunction=getThreeDayFilter)
pt$defineCalculation(calculationName="ThreeDayCount", filters=filterOverrides, summariseExpression="n()", caption="Three Day Total")
pt$defineCalculation(calculationName="ThreeDayAverage", type="calculation", basedOn="ThreeDayCount",
                     calculationExpression="values$ThreeDayCount/3", format="%.1f", caption="Three Day Rolling Average")
pt$renderPivot()


# cumulative sum (no need to use visual totals on this one, a filter upfront is more efficient)

trains <- bhmtrains %>%
  mutate(GbttDateTime=if_else(is.na(GbttArrival), GbttDeparture, GbttArrival),
         GbttDate=as.Date(GbttDateTime)) %>%
  filter((as.Date("2017-01-01") <= GbttDate)&(GbttDate <= as.Date("2017-01-31")))
januaryDates <- seq(as.Date("2017-01-01"), as.Date("2017-01-31"), by="days")

# date filter function to all dates since 1st jan
getCumulativeFilter <- function(pt, filters) {
  # get the date filter
  filter <- filters$getFilter("GbttDate")
  if(is.null(filter)||(filter$type=="ALL")||(length(filter$values)>1)) {
    # there is no filter on GbttDate in this cell
    # i.e. we are in one of the total cells that covers all dates,
    # so the concept of yesterday has no meaning, so block all date
    newFilter <- PivotFilter$new(pt, variableName="GbttDate", type="NONE")
    filters$setFilter(newFilter, action="replace")
  }
  else {
    # get the date value and create filter
    date <- filter$values
    newDates <- seq(as.Date("2017-01-01"), date, by="days")
    filter$values <- newDates
  }
}
# build the pivot
library(pivottabler)
pt <- PivotTable$new()
pt$addData(trains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("GbttDate", fromData=FALSE, explicitListOfValues=as.list(januaryDates))
pt$defineCalculation(calculationName="CountTrains", summariseExpression="n()", caption="Count")
filterOverrides <- PivotFilterOverrides$new(pt, overrideFunction=getCumulativeFilter)
pt$defineCalculation(calculationName="CumulativeCount", filters=filterOverrides, summariseExpression="n()", caption="Cumulative Count")
pt$renderPivot()
