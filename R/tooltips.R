# This file is part of ggtips (R package to display tooltips on svg ggplot)
#
# @author Pawel Piatkowski
#
# Copyright 2018 Genentech, Inc.
#
# Permission is hereby granted, free of charge, to any person obtaining
# copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#' Get tooltips
#'
#' Returns a list of tooltips for a ggplot, grouped by plot geometries
#' and layers.
#' Each element of the list is a data frame with HTML-formatted tooltip
#' contents (column `tooltip`) and coordinates relative to the size of
#' the image (`coordX` and `coordY`).
#'
#' @param plot A \link{ggplot} object.
#' @param g A gtable object compiled from the plot (see \link{arrangeGrob}).
#' @param varDict Variable dictionary in the following format:
#' \code{list(<variable> = <label>, ...)},
#' where \code{<variable>} is a valid name of a variable mapped
#' with \link{aes}, and \code{<label>} is a character string.
#' It defines the composition of information displayed in tooltips.
#' @param plotScales A list with two fields: x and y. Defines axis
#' scales (transformations) for the purpose of displaying original
#' values in tooltips. If NULL (default), values are displayed "as is".
#' @param callback Callback function for adding custom content to the tooltips
#' (see the example app).
#' @param addAttributes Logical parameter determinig whether extra geom
#' attributes should be add to tooltip object.
#'
getTooltips <- function(plot,
                        varDict,
                        plotScales,
                        g,
                        callback,
                        addAttributes = FALSE,
                        wrap = FALSE) {
  gb <- ggplot2::ggplot_build(plot)
  tooltipData <- getTooltipData(
    plot,
    built = gb,
    varDict = varDict,
    plotScales = plotScales,
    callback = callback
  )
  layoutNames <- assignLayoutNamesToPanels(g)

  totalPlotSize <- getGrobSize(g)
  plotWidth <- totalPlotSize$width
  plotHeight <- totalPlotSize$height
  colWidths <- sapply(
    1:length(g$widths),
    function(i) { gridColWidth(g, i) }
  )
  colWidths <- correctMargins(colWidths, plotWidth)
  rowHeights <- sapply(
    1:length(g$heights),
    function(i) { gridRowHeight(g, i) }
  )
  rowHeights <- correctMargins(rowHeights, plotHeight)
  geomList <- sapply(
    layoutNames,
    function(layoutName) { getGeomsFromGrob(nameToGrob(g, layoutName)) },
    simplify = FALSE,
    USE.NAMES = TRUE
  )
  geomNames <- lapply(plot$layers, getLayerGeom)
  uniqueGeomNames <- unlist(unique(geomNames))
  tooltips <- mapply(
    function(layer, df, geom) {
      if (is.null(geom)) {
        # Geometry not supported
        return(NULL)
      }
      tooltipContents <- tooltipDataToText(df, wrap = wrap)
      coords <- lapply(layoutNames, function(layoutName) {
        getGeomCoordsForGrob(
          g,
          layoutName = layoutName,
          geomList = geomList,
          geom = geom,
          colWidths = colWidths,
          rowHeights = rowHeights
        )
      })
      coords <- do.call(rbind, coords)
      if (is.null(coords)) {
        NULL
      } else {
        coords$coordX <- coords$coordX / plotWidth
        coords$coordY <- 1 - coords$coordY / plotHeight
        cbind(tooltip = tooltipContents, coords)
      }
    },
    plot$layers,
    tooltipData,
    geomNames,
    SIMPLIFY = FALSE
  )
  # Group tooltip tables by geometries
  res <- sapply(
    uniqueGeomNames,
    function(geomName) {
      tooltips[which(geomNames == geomName)]
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  if (addAttributes) {
    attr(res, "colWidths") <- colWidths
    attr(res, "rowHeights") <- rowHeights
  }

  res
}

#' Save ggplot and get tooltips
#'
#' Wrapper for \link{ggsave}; after saving a plot, returns an HTML-formatted
#' list of tooltip data (see \link{getTooltips}).
#'
#' @param plot ggplot object or customGrob, see "getSvgAndTooltipdata" for
#' more details.
#' @param g A gtable object compiled from the plot (see \link{arrangeGrob}).
#' @param varDict Variable dictionary in the following format:
#' \code{list(<variable> = <label>, ...)},
#' where \code{<variable>} is a valid name of a variable mapped
#' with \link{aes}, and \code{<label>} is a character string.
#' It defines the composition of information displayed in tooltips.
#' @param plotScales A list with two fields: x and y. Defines axis
#' scales (transformations) for the purpose of displaying original
#' values in tooltips. If NULL (default), values are displayed "as is".
#' @param ggPlotObj optional, used if plot is a customGrob.
#' @param callback Callback function for adding custom content to the tooltips
#' (see the example app).
#' @param addAttributes Logical parameter determinig whether extra geom
#' attributes should be add to tooltip object.
#'
#' @return A list.
#' @export
saveAndGetTooltips <- ggplot2::ggsave

formals(saveAndGetTooltips) <- c(
  formals(saveAndGetTooltips),
  alist(
    varDict = ,
    plotScales = ,
    g = ,
    ggPlotObj = NULL,
    callback = NULL,
    addAttributes = FALSE,
    wrap = FALSE
  )
)
body(saveAndGetTooltips)[[length(body(saveAndGetTooltips))]] <- quote(
  ggtips:::getTooltips(
    plot = `if`(is.null(ggPlotObj), plot, ggPlotObj),
    varDict = varDict,
    plotScales = plotScales,
    g = g,
    callback = callback,
    addAttributes = addAttributes,
    wrap = wrap
  )
)
