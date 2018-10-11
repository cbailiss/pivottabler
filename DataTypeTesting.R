dtdata <- data.frame(SaleID=1:5, SaleID2=as.character(1:5), Colour=c("Red", "Red", "Green", "Green", "Green", "Green", "Red", "Green", "Red", "Green"),
                   SaleItem=c("Car", "Lorry", "Car", "Train", "Train", "Lorry", "Car", "Train", "Lorry", "Car"),
                   SaleModel=c("CA", "LA", "CB", "TA", "TB", "LB", "CB", "TC", "LD", "CE"),
                   SaleDate=as.Date(c("2018-05-15", "2018-01-23", "2018-09-03", "2017-12-25", "2018-06-28")),
                   SaleDT=as.POSIXct(c("2018-05-15 09:12:34 UTC", "2018-01-23 13:23:54 UTC", "2018-09-03 23:59:59 UTC",
                                                  "2017-12-25 11:47:19 UTC", "2018-06-28 18:00:00 UTC", "2018-06-23 12:34:15 UTC",
                                                  "2018-03-31 17:05:23 UTC", "2018-09-01 15:54:23 UTC", "2018-02-02 10:06:16 UTC",
                                                  "2016-11-25 18:12:11 UTC")),
                   IsNewCustomer=c(TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                   SaleQuantity=as.integer(c(1,3,2,1,5,3,1,2,3,2)),
                   SaleAmount=c(12.1,2.333333333,5.6,3.7,1.5,1.1,0.2,3.7,2.5,2.9),
                   stringsAsFactors=FALSE)

# Data Types: integer, numeric, character, logical, date, POSIXct
# Not supported:  Factors (convert to character), POSIXlt (use POSIXct), complex (who needs these anyway!)

library(openxlsx)

testXLSX <- function(pt) {
  wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
  addWorksheet(wb, "Data")
  pt$writeToExcelWorksheet(wb=wb, wsName="Data",
                           topRowNumber=1, leftMostColumnNumber=1,
                           applyStyles=TRUE, mapStylesFromCSS=TRUE)
  saveWorkbook(wb, file="C:\\Users\\Chris\\Desktop\\test.xlsx", overwrite = TRUE)
}

# ROW/COLUMN DATA TYPE FORMAT TESTS:  NO FORMAT = as.character()

# integer

library(pivottabler)
pt <- PivotTable$new(processingLibrary="data.table", evaluationMode="batch")
pt$addData(dtdata)
pt$addColumnDataGroups("SaleID")
pt$addRowDataGroups("SaleQuantity")
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
pt$evaluatePivot()
pt$renderPivot()

# numeric

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("SaleID")
pt$addRowDataGroups("SaleAmount")
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
pt$evaluatePivot()
pt$renderPivot()

# logical

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("SaleID")
pt$addRowDataGroups("IsNewCustomer")
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
pt$evaluatePivot()
pt$renderPivot()

# Date

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("SaleID")
pt$addRowDataGroups("SaleDate")
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
pt$evaluatePivot()
pt$renderPivot()

# POSIXct

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("SaleID")
pt$addRowDataGroups("SaleDT")
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
pt$evaluatePivot()
pt$renderPivot()

# ROW/COLUMN DATA TYPE FORMAT TESTS:  SPRINTF()

# character - N/A
# date - N/A
# POSIXct - N/A

# integer

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("SaleID")
pt$addRowDataGroups("SaleQuantity", dataFormat="%i")
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
pt$evaluatePivot()
pt$renderPivot()

# numeric

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("SaleID")
pt$addRowDataGroups("SaleAmount", dataFormat="%.1f")
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
pt$evaluatePivot()
pt$renderPivot()

# logical

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("SaleID")
pt$addRowDataGroups("IsNewCustomer", dataFormat="%i")
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
pt$evaluatePivot()
pt$renderPivot()

# ROW/COLUMN DATA TYPE FORMAT TESTS:  FORMAT()

# integer - N/A
# character - N/A

# numeric

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("SaleID")
pt$addRowDataGroups("SaleAmount", dataFormat=list(digits=4, nsmall=2))
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
pt$evaluatePivot()
pt$renderPivot()

# logical

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("SaleID")
pt$addRowDataGroups("IsNewCustomer", dataFormat=c("Existing Customer", "New Customer"))
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
pt$evaluatePivot()
pt$renderPivot()

# Date

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("SaleID")
pt$addRowDataGroups("SaleDate", dataFormat="%d %b %Y")
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
pt$evaluatePivot()
pt$renderPivot()

# POSIXct

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("SaleID")
pt$addRowDataGroups("SaleDT", dataFormat="%d %b %Y %H:%M:%S")
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
pt$evaluatePivot()
pt$renderPivot()



# MEASURE DATA TYPE CALCULATION AND FORMAT TESTS:  NO FORMAT = as.character()

# integer

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
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
pt$addData(dtdata)
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
pt$addData(dtdata)
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
pt$addData(dtdata)
pt$addColumnDataGroups("Colour")
pt$addRowDataGroups("SaleItem")
pt$defineCalculation(calculationName="SaleCount", summariseExpression="n()", caption="Sale Count")
pt$defineCalculation(calculationName="FirstIsNewCustomer", summariseExpression="as.logical(min(IsNewCustomer, na.rm=TRUE))", caption="First NC")
pt$defineCalculation(calculationName="LastIsNewCustomer", summariseExpression="as.logical(max(IsNewCustomer, na.rm=TRUE))", caption="Last NC")
pt$evaluatePivot()
pt$renderPivot()

# date

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
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
pt$addData(dtdata)
pt$addColumnDataGroups("Colour")
pt$addRowDataGroups("SaleItem")
pt$defineCalculation(calculationName="SaleCount", summariseExpression="n()", caption="Sale Count")
pt$defineCalculation(calculationName="FirstSale", summariseExpression="min(SaleDT, na.rm=TRUE)", caption="First Sale")
pt$defineCalculation(calculationName="LastSale", summariseExpression="max(SaleDT, na.rm=TRUE)", caption="Last Sale")
pt$evaluatePivot()
pt$renderPivot()



# MEASURE DATA TYPE FORMAT TESTS:  SPRINTF()

# character - N/A
# date - N/A
# POSIXct - N/A

# integer

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("Colour")
pt$addRowDataGroups("SaleItem")
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", format="%i", caption="Volume Sold")
pt$defineCalculation(calculationName="TotalSales", summariseExpression="sum(SaleAmount, na.rm=TRUE)", caption="Total Sales")
pt$defineCalculation(type="calculation", basedOn=c("VolumeSold", "TotalSales"), format="%.1f",
                     calculationName="AvgSales", calculationExpression="values$TotalSales/values$VolumeSold", caption="Avg")
pt$evaluatePivot()
pt$renderPivot()

# numeric

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("Colour")
pt$addRowDataGroups("SaleItem")
pt$defineCalculation(calculationName="SaleCount", summariseExpression="n()", caption="Sale Count")
pt$defineCalculation(calculationName="TotalSales", summariseExpression="sum(SaleAmount, na.rm=TRUE)", format="%.1f", caption="Total Sales")
pt$defineCalculation(type="calculation", basedOn=c("SaleCount", "TotalSales"), format="%.1f",
                     calculationName="AvgSales", calculationExpression="values$TotalSales/values$SaleCount", caption="Avg")
pt$evaluatePivot()
pt$renderPivot()

# logical

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("Colour")
pt$addRowDataGroups("SaleItem")
pt$defineCalculation(calculationName="SaleCount", summariseExpression="n()", caption="Sale Count")
pt$defineCalculation(calculationName="FirstIsNewCustomer", summariseExpression="as.logical(min(IsNewCustomer, na.rm=TRUE))", format="%i", caption="First NC")
pt$defineCalculation(calculationName="LastIsNewCustomer", summariseExpression="as.logical(max(IsNewCustomer, na.rm=TRUE))", format=c("No","Yes"), caption="Last NC")
pt$evaluatePivot()
pt$renderPivot()


# MEASURE DATA TYPE FORMAT TESTS:  FORMAT()

# integer - N/A
# character - N/A

# numeric

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("Colour")
pt$addRowDataGroups("SaleItem")
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
pt$defineCalculation(calculationName="TotalSales", summariseExpression="sum(SaleAmount, na.rm=TRUE)",
                     format=list(digits=4, nsmall=2), caption="Total Sales")
pt$defineCalculation(type="calculation", basedOn=c("VolumeSold", "TotalSales"), format=list(digits=4, nsmall=2),
                     calculationName="AvgSales", calculationExpression="values$TotalSales/values$VolumeSold", caption="Avg")
pt$evaluatePivot()
pt$renderPivot()

# logical

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("Colour")
pt$addRowDataGroups("SaleItem")
pt$defineCalculation(calculationName="SaleCount", summariseExpression="n()", caption="Sale Count")
pt$defineCalculation(calculationName="FirstIsNewCustomer", summariseExpression="as.logical(min(IsNewCustomer, na.rm=TRUE))",
                     format=c("No","Yes", ""), caption="First NC")
pt$defineCalculation(calculationName="LastIsNewCustomer", summariseExpression="as.logical(max(IsNewCustomer, na.rm=TRUE))",
                     format=list(), caption="Last NC")
pt$evaluatePivot()
pt$renderPivot()

# date

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("Colour")
pt$addRowDataGroups("SaleItem")
pt$defineCalculation(calculationName="SaleCount", summariseExpression="n()", caption="Sale Count")
pt$defineCalculation(calculationName="FirstSale", summariseExpression="min(SaleDate, na.rm=TRUE)",
                     format="%d %b %Y", caption="First Sale")
pt$defineCalculation(calculationName="LastSale", summariseExpression="max(SaleDate, na.rm=TRUE)",
                     format=list("%d %b %Y"), caption="Last Sale")
pt$evaluatePivot()
pt$renderPivot()

# POSIXct

library(pivottabler)
pt <- PivotTable$new()
pt$addData(dtdata)
pt$addColumnDataGroups("Colour")
pt$addRowDataGroups("SaleItem")
pt$defineCalculation(calculationName="SaleCount", summariseExpression="n()", caption="Sale Count")
pt$defineCalculation(calculationName="FirstSale", summariseExpression="min(SaleDT, na.rm=TRUE)",
                     format="%d %b %Y %H:%M:%S", caption="First Sale")
pt$defineCalculation(calculationName="LastSale", summariseExpression="max(SaleDT, na.rm=TRUE)",
                     format=list("%d %b %Y %H:%M:%S"), caption="Last Sale")
pt$evaluatePivot()
pt$renderPivot()




