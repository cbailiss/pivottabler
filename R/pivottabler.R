#' @import htmlwidgets
#' @export
pivottabler <- function(pt, width=NULL, height=NULL, styleNamePrefix=NULL,
                   includeRCFilters=FALSE, includeCalculationFilters=FALSE,
                   includeCalculationNames=FALSE, includeRawValue=FALSE) {
  settings <- list() # may need this in the future
  widgetData <- list(
    tableCss = pt$getCss(styleNamePrefix=styleNamePrefix),
    tableHtml = as.character(pt$getHtml(styleNamePrefix=styleNamePrefix,
                                        includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                        includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue)),
    settings = settings
  )
  htmlwidgets::createWidget("pivottabler", widgetData, width=width, height=height)
}

#' @export
pivottablerOutput <- function(outputId, width = "100%", height = "100%") {
  shinyWidgetOutput(outputId, "pivottabler", width, height, package = "pivottabler")
}

#' @export
renderPivottabler <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, sigmaOutput, env, quoted = TRUE)
}
