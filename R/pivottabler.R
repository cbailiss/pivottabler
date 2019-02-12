#' Render a pivot table as a HTML widget.
#'
#' The \code{pivottabler} function is primarily intended for use with Shiny web
#' applications.
#'
#' @import htmltools
#' @import htmlwidgets
#' @export
#' @param pt The pivot table to render.
#' @param width The target width.
#' @param height The target height.
#' @param styleNamePrefix A text prefix to be prepended to the CSS declarations
#'   (to ensure uniqueness).
#' @param includeRCFilters Show/hide filter detail for debugging.
#' @param includeCalculationFilters Show/hide filter detail for debugging.
#' @param includeWorkingData Show/hide working data detail for debugging.
#' @param includeEvaluationFilters Show/hide filter detail for debugging.
#' @param includeCalculationNames Show/hide filter detail for debugging.
#' @param includeRawValue Show/hide filter detail for debugging.
#' @return A HTML widget.
#' @examples
#' # See the Shiny vignette in this package for examples.
pivottabler <- function(pt, width=NULL, height=NULL, styleNamePrefix=NULL,
                   includeRCFilters=FALSE, includeCalculationFilters=FALSE,
                   includeWorkingData=FALSE,
                   includeEvaluationFilters=FALSE, includeCalculationNames=FALSE,
                   includeRawValue=FALSE) {
  settings <- list() # may need this in the future
  widgetData <- list(
    tableCss = pt$getCss(styleNamePrefix=styleNamePrefix),
    tableHtml = as.character(pt$getHtml(styleNamePrefix=styleNamePrefix,
                                        includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                        includeWorkingData=includeWorkingData,
                                        includeEvaluationFilters=includeEvaluationFilters,
                                        includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue)),
    settings = settings
  )
  htmlwidgets::createWidget("pivottabler", widgetData, width=width, height=height)
}

#' Standard function for Shiny scaffolding.
#' @export
#' @param outputId The id of the html element that will contain the htmlwidget.
#' @param width The target width of the htmlwidget.
#' @param height The target height of the htmlwidget.
pivottablerOutput <- function(outputId, width = "100%", height = "100%") {
  shinyWidgetOutput(outputId, "pivottabler", width, height, package = "pivottabler")
}

#' Standard function for Shiny scaffolding.
#' @export
#' @param expr The R expression to execute and render in the Shiny web application.
#' @param env Standard shiny argument for a render function.
#' @param quoted Standard shiny argument for a render function.
renderPivottabler <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, pivottablerOutput, env, quoted = TRUE)
}
