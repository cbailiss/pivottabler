dtdata <- data.frame(SaleID=1:5, Colour=c("Red", "Red", "Green", "Green", "Green", "Green", "Red", "Green", "Red", "Green"),
                     SaleItem=c("Car", "Lorry", "Car", "Train", "Train", "Lorry", "Car", "Train", "Lorry", "Car"),
                     SaleModel=c("CA", "LA", "CB", "TA", "TB", "LB", "CB", "TC", "LD", "CE"),
                     SaleDate=as.Date(c("2018-05-15", "2018-01-23", "2018-09-03", "2017-12-25", "2018-06-28")),
                     SaleDT=as.POSIXct(c("2018-05-15 09:12:34 UTC", "2018-01-23 13:23:54 UTC", "2018-09-03 23:59:59 UTC",
                                         "2017-12-25 11:47:19 UTC", "2018-06-28 18:00:00 UTC", "2018-06-23 12:34:15 UTC",
                                         "2018-03-31 17:05:23 UTC", "2018-09-01 15:54:23 UTC", "2018-02-02 10:06:16 UTC",
                                         "2016-11-25 18:12:11 UTC")),
                     IsNewCustomer=c(TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                     SaleQuantity=as.integer(c(1,3,2,1,5,3,1,2,3,2)),
                     SaleQuantityDupe=as.integer(c(1,3,2,1,5,3,1,2,3,2)), # needed due to a quirk of data.table
                     SaleAmount=c(12.1,2.333333333,5.6,3.7,1.5,1.1,0.2,3.7,2.5,2.9),
                     Propensity=c(12,35,0,45,87,NA,Inf,-Inf,NaN,100),
                     stringsAsFactors=FALSE)

library(pivottabler)
pt <- PivotTable$new() #processingLibrary=processingLibrary, evaluationMode=evaluationMode)
pt$addData(dtdata)
pt$addColumnDataGroups("SaleID")
pt$addRowDataGroups("Propensity")
pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
pt$evaluatePivot()
pt$renderPivot(exportOptions=list(skipNegInf=TRUE,skipPosInf=TRUE,skipNA=TRUE,skipNaN=TRUE))
pt$renderPivot(exportOptions=list(exportNegInfAs="-Infinity",exportPosInfAs="Infinity",exportNAAs="Nothing",exportNaNAs="Not a Number"))

library(openxlsx)
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
addWorksheet(wb, "Data")
pt$writeToExcelWorksheet(wb=wb, wsName="Data",
                         topRowNumber=1, leftMostColumnNumber=1,
                         outputValuesAs="rawValue",
                         applyStyles=TRUE, mapStylesFromCSS=TRUE,
                         exportOptions=list(exportNegInfAs="-Infinity",exportPosInfAs="Infinity",exportNAAs="Nothing",exportNaNAs="Not a Number"))
saveWorkbook(wb, file="C:\\Users\\Chris\\Desktop\\test.xlsx", overwrite = TRUE)




library(pivottabler)
pt <- PivotTable$new() #processingLibrary=processingLibrary, evaluationMode=evaluationMode)
pt$addData(dtdata)
pt$addColumnDataGroups("SaleID")
pt$addRowDataGroups("SaleDT", styleDeclarations=list("xl-value-format"="yyyy-mm-dd hh:mm"))
pt$defineCalculation(calculationName="Propensity", summariseExpression="sum(Propensity)", caption="Propensity")
pt$evaluatePivot()
#pt$renderPivot()
#pt$renderPivot(exportOptions=list(skipNegInf=TRUE,skipPosInf=TRUE,skipNA=TRUE,skipNaN=TRUE))
pt$renderPivot(exportOptions=list(exportNegInfAs="-Infinity",exportPosInfAs="Infinity",exportNAAs="Nothing",exportNaNAs="Not a Number"))

library(openxlsx)
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
addWorksheet(wb, "Data")
pt$writeToExcelWorksheet(wb=wb, wsName="Data",
                         topRowNumber=1, leftMostColumnNumber=1,
                         outputHeadingsAs="rawValue", outputValuesAs="rawValue",
                         applyStyles=TRUE, mapStylesFromCSS=TRUE,
                         exportOptions=list(exportNegInfAs="-Infinity",exportPosInfAs="Infinity",exportNAAs="Nothing",exportNaNAs="Not a Number"))
saveWorkbook(wb, file="C:\\Users\\Chris\\Desktop\\test.xlsx", overwrite = TRUE)





