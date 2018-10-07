data <- data.frame(SaleID=1:5, Colour=c("Red", "Red", "Green", "Green", "Green", "Green", "Red", "Green", "Red", "Green"),
                   SaleItem=c("Car", "Lorry", "Car", "Train", "Train", "Lorry", "Car", "Train", "Lorry", "Car"),
                   SaleModel=c("CA", "LA", "CB", "TA", "TB", "LB", "CB", "TC", "LD", "CE"),
                   SaleDate=as.Date(c("2018-05-15", "2018-01-23", "2018-09-03", "2017-12-25", "2018-06-28")),
                   SaleDT=as.POSIXct(as.POSIXlt(c("2018-05-15 09:12:34 UTC", "2018-01-23 13:23:54 UTC", "2018-09-03 23:59:59 UTC",
                                                  "2017-12-25 11:47:19 UTC", "2018-06-28 18:00:00 UTC", "2018-06-23 12:34:15 UTC",
                                                  "2018-03-31 17:05:23 UTC", "2018-09-01 15:54:23 UTC", "2018-02-02 10:06:16 UTC",
                                                  "2016-11-25 18:12:11 UTC"))),
                   IsNewCustomer=c(TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                   SaleQuantity=as.integer(c(1,3,2,1,5,3,1,2,3,2)),
                   SaleAmount=c(12.1,2.3,5.6,3.7,1.5,1.1,0.2,3.7,2.5,2.9),
                   stringsAsFactors=FALSE)

# Data Types: integer, numeric, character, logical, date, POSIXct
# Not supported:  Factors (convert to character), POSIXlt (use POSIXct), complex (who needs these anyway!)


# integer

library(pivottabler)
pt <- PivotTable$new()
pt$addData(data)
pt$addColumnDataGroups("Colour")
pt$addRowDataGroups("SaleItem")
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
pt$defineCalculation(calculationName="TotalSales", summariseExpression="sum(SaleAmount, na.rm=TRUE)", caption="Total Sales")
pt$defineCalculation(type="calculation", basedOn=c("VolumeSold", "TotalSales"), format="%.1f",
                     calculationName="AvgSales", calculationExpression="values$TotalSales/values$VolumeSold", caption="Avg")
pt$evaluatePivot()
pt$renderPivot()


# numeric

library(pivottabler)
pt <- PivotTable$new()
pt$addData(data)
pt$addColumnDataGroups("Colour")
pt$addRowDataGroups("SaleItem")
pt$defineCalculation(calculationName="SaleCount", summariseExpression="n()", caption="Sale Count")
pt$defineCalculation(calculationName="TotalSales", summariseExpression="sum(SaleAmount, na.rm=TRUE)", caption="Total Sales")
pt$defineCalculation(type="calculation", basedOn=c("SaleCount", "TotalSales"), format="%.1f",
                     calculationName="AvgSales", calculationExpression="values$TotalSales/values$SaleCount", caption="Avg")
pt$evaluatePivot()
pt$renderPivot()


# character

library(pivottabler)
pt <- PivotTable$new()
pt$addData(data)
pt$addColumnDataGroups("Colour")
pt$addRowDataGroups("SaleItem")
pt$defineCalculation(calculationName="SaleCount", summariseExpression="n()", caption="Sale Count")
pt$defineCalculation(calculationName="FirstModel", summariseExpression="min(SaleModel, na.rm=TRUE)", caption="First Model")
pt$defineCalculation(calculationName="LastModel", summariseExpression="max(SaleModel, na.rm=TRUE)", caption="Last Model")
pt$evaluatePivot()
pt$renderPivot()


# logical

library(pivottabler)
pt <- PivotTable$new()
pt$addData(data)
pt$addColumnDataGroups("Colour")
pt$addRowDataGroups("SaleItem")
pt$defineCalculation(calculationName="SaleCount", summariseExpression="n()", caption="Sale Count")
pt$defineCalculation(calculationName="FirstIsNewCustomer", summariseExpression="min(IsNewCustomer, na.rm=TRUE)", caption="First NC")
pt$defineCalculation(calculationName="LastIsNewCustomer", summariseExpression="max(IsNewCustomer, na.rm=TRUE)", caption="Last NC")
pt$evaluatePivot()
pt$renderPivot()


# date

library(pivottabler)
pt <- PivotTable$new()
pt$addData(data)
pt$addColumnDataGroups("Colour")
pt$addRowDataGroups("SaleItem")
pt$defineCalculation(calculationName="SaleCount", summariseExpression="n()", caption="Sale Count")
pt$defineCalculation(calculationName="FirstSale", summariseExpression="min(SaleDate, na.rm=TRUE)", caption="First Sale")
pt$defineCalculation(calculationName="LastSale", summariseExpression="max(SaleDate, na.rm=TRUE)", caption="Last Sale")
pt$evaluatePivot()
pt$renderPivot()


# POSIXct

library(pivottabler)
pt <- PivotTable$new()
pt$addData(data)
pt$addColumnDataGroups("Colour")
pt$addRowDataGroups("SaleItem")
pt$defineCalculation(calculationName="SaleCount", summariseExpression="n()", caption="Sale Count")
pt$defineCalculation(calculationName="FirstSale", summariseExpression="min(SaleDT, na.rm=TRUE)", caption="First Sale")
pt$defineCalculation(calculationName="LastSale", summariseExpression="max(SaleDT, na.rm=TRUE)", caption="Last Sale")
pt$evaluatePivot()
pt$renderPivot()



# all of the above examples use the default formatting option (of as.character()) since no format parameter is specified.
# need to:
#   - write tests that use the format parameter and check they work
#   - check export to Excel to see how that handles exporting each of the above
#   - check latex export for the same reason
#   - write automated test cases for the above
#   - document the changes, at least in the NEWS file (and also check if any vignettes say data types are currently limited)


