library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$renderPivot()



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
                     calculationExpression="values$CountDMU/values$CountTrains*100", format="%.1f %%", caption="% of TOC")
pt$renderPivot()


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





library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$renderPivot()
pt$rowGroup$childGroups[[2]]$caption <- "CC Trains"
pt$renderPivot()
