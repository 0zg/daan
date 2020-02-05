# oso 20190805
#snelle korte rapportage van de kolommen in een scattermatrix
#snel verbanden zien tussen kolommen
#scatterMatrix verwacht nummers en factors
#functie gejat uit library(userfriendlyscience)

print.scatterMatrix <- function(x, ...) {
  ###
  print(x$output$scatterMatrix, ...);
}

#' scatterMatrix
#'
#' scatterMatrix produced a matrix with jittered scatterplots, histograms, and
#' correlation coefficients.
#'
#'
#' @param dat A dataframe containing the items in the scale. All variables in
#' this dataframe will be used if items is NULL.
#' @param items If not NULL, this should be a character vector with the names
#' of the variables in the dataframe that represent items in the scale.
#' @param plotSize Size of the final plot in millimeters.
#' @param sizeMultiplier Allows more flexible control over the size of the plot
#' elements
#' @param axisLabels Passed to ggpairs function to set axisLabels.
#' @param powerHist Whether to use the default ggpairs histogram on the
#' diagonal of the scattermatrix, or whether to use the powerHist version.
#' @param ...  Additional arguments are passed on to powerHist.
#' @return
#'
#' An object with the input and several output variables. Most notably:
#' \item{output$scatterMatrix}{A scattermatrix with histograms on the diagonal
#' and correlation coefficients in the upper right half.}
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utilities univar

maak_geplotte_samenvatting_van_data <- function (df, plotSize = 180, sizeMultiplier = 1)
{
  library(dplyr)
  library(ggplot2)
  library(GGally)
  scatterMatrix <- function(dat, items = NULL, plotSize = 180,
                            sizeMultiplier = 1, axisLabels = "none", powerHist = TRUE,
                            ...) {
    if (is.null(items)) {
      items <- names(dat)
    }
    extractVarName <- function(x) {
      regexpr <- "[[:alnum:]]+\\[[[:alnum:]]*,[[:blank:]]*['\"]([[:alnum:]]+)['\"]\\]"
      if (grepl(regexpr, x))
        return(sub(regexpr, "\\1", x))
      else return(sub(".*\\$(.*?)[])]*$", "\\1", x))
    }
    dlvTheme <- function(base_size = 11, base_family = "",
                         ...) {
      theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(...)
    }
    powerHistf <- function(vector, histColor = "#0000CC",
                           distributionColor = "#0000CC", normalColor = "#00CC00",
                           distributionLineSize = 1, normalLineSize = 1, histAlpha = 0.25,
                           xLabel = NULL, yLabel = NULL, normalCurve = TRUE,
                           distCurve = TRUE, breaks = 30, theme = dlvTheme(),
                           rug = NULL, jitteredRug = TRUE, rugSides = "b",
                           rugAlpha = 0.2, returnPlotOnly = FALSE) {
      varName <- deparse(substitute(vector))
      vector <- na.omit(vector)
      if (!is.numeric(vector)) {
        tryCatch(vector <- as.numeric(vector), error = function(e) {
          stop("The vector you supplied is not numeric; I tried to convert it, ",
               "but my attempt failed. The error I got is:\n",
               e)
        })
      }
      res <- list(input = as.list(environment()), intermediate = list(),
                  output = list())
      res$input$sampleSize = length(vector)
      res$intermediate$distribution <- res$input$vector
      res$intermediate$tempBinWidth <- (max(res$input$vector) -
                                          min(res$input$vector))/breaks
      scalingFactor <- max(table(cut(vector, breaks = breaks)))
      if (normalCurve) {
        res$intermediate$normalX <- c(seq(min(res$input$vector),
                                          max(res$input$vector), by = (max(res$input$vector) -
                                                                         min(res$input$vector))/(res$input$sampleSize -
                                                                                                   1)))
        res$intermediate$normalY <- dnorm(res$intermediate$normalX,
                                          mean = mean(res$input$vector), sd = sd(res$input$vector))
        res$intermediate$normalY <- (1/max(res$intermediate$normalY)) *
          scalingFactor * res$intermediate$normalY
        res$dat <- data.frame(normalX = res$intermediate$normalX,
                              normalY = res$intermediate$normalY, distribution = res$intermediate$distribution)
      }
      else {
        res$dat <- data.frame(distribution = res$intermediate$distribution)
      }
      if (is.null(xLabel)) {
        xLabel <- paste0("Value of ", extractVarName(varName))
      }
      if (is.null(yLabel)) {
        yLabel <- "Frequency"
      }
      res$plot <- ggplot(data = res$dat, aes(x = distribution)) +
        xlab(xLabel) + ylab(yLabel) + geom_histogram(color = NA,
                                                     fill = histColor, na.rm = TRUE, alpha = histAlpha,
                                                     binwidth = res$intermediate$tempBinWidth)
      if (distCurve) {
        res$plot <- res$plot + geom_line(aes_q(y = bquote(..scaled.. *
                                                            .(scalingFactor))), stat = "density", color = distributionColor,
                                         size = distributionLineSize, na.rm = TRUE)
      }
      if (normalCurve) {
        res$plot <- res$plot + geom_line(aes(x = normalX,
                                             y = normalY), color = normalColor, size = normalLineSize,
                                         na.rm = TRUE)
      }
      res$plot <- res$plot + theme
      if (is.null(rug)) {
        if (nrow(res$dat) < 1000) {
          rug <- TRUE
        }
        else {
          rug <- FALSE
        }
      }
      if (rug) {
        if (jitteredRug) {
          res$plot <- res$plot + geom_rug(color = distributionColor,
                                          sides = rugSides, aes(y = 0), position = "jitter",
                                          alpha = rugAlpha, na.rm = TRUE)
        }
        else {
          res$plot <- res$plot + geom_rug(color = distributionColor,
                                          sides = rugSides, alpha = rugAlpha, na.rm = TRUE)
        }
      }
      if (!is.null(res$input$xLabel) && is.logical(res$input$xLabel) &&
          !(res$input$xLabel)) {
        res$plot <- res$plot + theme(axis.title.x = element_blank())
      }
      if (!is.null(res$input$yLabel) && is.logical(res$input$yLabel) &&
          !(res$input$yLabel)) {
        res$plot <- res$plot + theme(axis.title.y = element_blank())
      }
      if (returnPlotOnly) {
        return(res$plot)
      }
      else {
        class(res) <- "powerHist"
        return(res)
      }
    }
    res <- list(input = list(dat = dat, items = items, plotSize = plotSize,
                             sizeMultiplier = sizeMultiplier, axisLabels = axisLabels),
                intermediate = list(), output = list())
    res$intermediate$dat <- dat[complete.cases(dat[, items]),
                                items]
    res$intermediate$dat <- data.frame(lapply(res$intermediate$dat,
                                              "as.numeric"))
    res$intermediate$baseSize <- baseSize <- (sizeMultiplier *
                                                (plotSize/length(items)))/100
    res$intermediate$plotSettings <- plotSettings <- theme(axis.line = element_line(size = baseSize),
                                                           panel.grid.major = element_line(size = baseSize/2),
                                                           line = element_line(size = baseSize/2), axis.ticks = element_line(size = baseSize/2))
    res$intermediate$ggpairs.normal <- ggpairs(res$intermediate$dat,
                                               diag = list(continuous = "barDiag", discrete = "barDiag"),
                                               axisLabels = res$input$axisLabels)
    res$intermediate$ggpairs.jittered <- ggpairs(res$intermediate$dat,
                                                 diag = list(continuous = "blankDiag"), upper = list(continuous = GGally::wrap("cor")),
                                                 lower = list(continuous = GGally::wrap("points",
                                                                                        position = "jitter")), axisLabels = res$input$axisLabels)
    res$intermediate$ggpairs.combined <- res$intermediate$ggpairs.jittered
    if (powerHist) {
      res$intermediate$powerHists <- list()
      for (currentVar in 1:length(items)) {
        res$intermediate$powerHists[[items[currentVar]]] <- powerHistf(res$intermediate$dat[[items[currentVar]]],
                                                                       ...)
        res$intermediate$ggpairs.combined <- putPlot(res$intermediate$ggpairs.combined,
                                                     res$intermediate$powerHists[[items[currentVar]]]$plot,
                                                     currentVar, currentVar)
      }
    }
    else {
      for (currentVar in 1:length(items)) {
        res$intermediate$ggpairs.combined <- putPlot(res$intermediate$ggpairs.combined,
                                                     getPlot(res$intermediate$ggpairs.normal, currentVar,
                                                             currentVar), currentVar, currentVar)
      }
    }
    res$output$scatterMatrix <- res$intermediate$ggpairs.combined
    for (currentRowFromTop in 1:length(items)) {
      for (currentColumnFromLeft in 1:length(items)) {
        res$output$scatterMatrix <- putPlot(res$output$scatterMatrix,
                                            getPlot(res$output$scatterMatrix, currentRowFromTop,
                                                    currentColumnFromLeft) + plotSettings, currentRowFromTop,
                                            currentColumnFromLeft)
      }
    }
    class(res) <- "scatterMatrix"
    return(res)
  }
  output <- scatterMatrix(dplyr::mutate_if(dplyr::mutate_if(dplyr::select(df,
                                                                          dplyr::everything()), is.character, as.factor), is.integer,
                                           as.numeric), plotSize = plotSize, sizeMultiplier = sizeMultiplier)
  return(output)
}
