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


#' Render plot with tooltips
#'
#' @param plot A \link{ggplot} object.
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
#' @param point.size Point size for calibrating hovering accuracy (optional).
#' @param dpi DPI value (optional).
#' @param width Plot width (in inches; optional).
#' @param height Plot height (in inches; optional).
#' @param customGrob optional grob object. It allows to pass original plot with
#' grob manipulations.
#' @param ... Additional parameters passed to \link{ggsave}.
#'
#' @note Non-numeric variables are only back-transformed to their original
#' values if they are passed to \link{ggplot} as factors.
#'
#' @export
renderWithTooltips <- function(plot,
                               varDict,
                               plotScales = NULL,
                               callback = NULL,
                               point.size = 10,
                               dpi = 72,
                               width = NA,
                               height = NA,
                               customGrob = NULL,
                               ...) {
  if (!requireNamespace("shiny")) {
    stop("renderWithTooltips() requires Shiny")
  }

  shiny::installExprFunction({
    res <- ggtips::getSvgAndTooltipdata(
      plot = plot,
      varDict = varDict,
      plotScales = plotScales,
      callback = callback,
      point.size = point.size,
      dpi = dpi,
      width = width,
      height = height,
      customGrob = customGrob,
      ...
    )
    ggtips::htmlWithGivenTooltips(
      svg = res$svg,
      data = res$data,
      height = height,
      width = width,
      point.size = point.size
    )
  }, name = "func", eval.env = parent.frame(), quoted = FALSE)

  shiny::createRenderFunction(func, function(result, shinysession, name, ...) {
    if (is.null(result) || length(result) == 0) return(NULL)
    shiny:::processDeps(result, shinysession)
  }, shiny::uiOutput, list())
}


#' Render plot with given tooltips data
#'
#' @param svg A SVG plot object.
#' @param data List with tooltip data.
#' @param point.size Point size for calibrating hovering accuracy (optional).
#' @param width Plot width (in inches; optional).
#' @param height Plot height (in inches; optional).
#'
#' @export
htmlWithGivenTooltips <- function(svg,
                                  data,
                                  height = NA,
                                  width = NA,
                                  point.size = 10) {
  if (length(data) == 0) {
    return(shiny::HTML(svg))
  }
  data <- list(
    data = data,
    width = width,
    height = height,
    size = point.size
  )
  id <- as.numeric(Sys.time())*1000

  script <- paste0(
    "<script>",
    "$('[data-id=\"%s\"]').closest('.shiny-html-output').ggtips(%s);",
    "</script>"
  )
  shiny::tagList(
    shiny::HTML(svg),
    getDependencies(),
    htmltools::tags$div(`data-id` = id, class = "ggtips-tooltip"),
    shiny::HTML(
      sprintf(script, id, jsonlite::toJSON(data, auto_unbox = TRUE))
    )
  )
}

#' Render SVG object and return it with tooltip data
#'
#' @param plot A \link{ggplot} object.
#' @param varDict Variable dictionary in the following format:
#' \code{list(<variable> = <label>, ...)},
#' where \code{<variable>} is a valid name of a variable mapped
#' with \link{aes}, and \code{<label>} is a character string.
#' It defines the composition of information displayed in tooltips.
#' if argument is NULL it will not generate tooltip data, that may speed up
#' generating of plots.
#' @param plotScales A list with two fields: x and y. Defines axis
#' scales (transformations) for the purpose of displaying original
#' values in tooltips. If NULL (default), values are displayed "as is".
#' @param callback Callback function for adding custom content to the tooltips
#' (see the example app).
#' @param point.size Point size for calibrating hovering accuracy (optional).
#' @param dpi DPI value (optional).
#' @param width Plot width (in inches; optional).
#' @param height Plot height (in inches; optional).
#' @param customGrob optional grob object. It allows to pass original plot with
#' grob manipulations.
#' @param ... Additional parameters passed to \link{ggsave}.
#'
#' @note Non-numeric variables are only back-transformed to their original
#' values if they are passed to \link{ggplot} as factors.
#'
#' @export
getSvgAndTooltipdata <- function(plot,
                                 varDict,
                                 plotScales = NULL,
                                 callback = NULL,
                                 point.size = 10,
                                 dpi = 72,
                                 width = NA,
                                 height = NA,
                                 customGrob = NULL,
                                 addAttributes = FALSE,
                                 wrap = FALSE,
                                 ...) {
  outfile <- tempfile(fileext = ".svg")

  # avoid lazy eval to use correct directory
  plot <- plot
  customGrob <- customGrob

  currentDir <- getwd()
  setwd(tempdir())
  # arrangeGrob produces Rplots.pdf which may cause permission issue when run on shiny server
  # to be more precise, ggplot2:::ggplot_gtable.ggplot_built is the origin of the issue
  grob <- gridExtra::arrangeGrob(`if`(is.null(customGrob), plot, customGrob))
  setwd(currentDir)

  data <- if (is.null(varDict)) {
    ggplot2::ggsave(
      plot = grob,
      filename = outfile,
      dpi = dpi,
      width = width,
      height = height,
      limitsize = FALSE,
      ...
    )
    NULL
  } else {
    saveAndGetTooltips(
      plot = grob,
      ggPlotObj = plot,
      g = grob[[1]][[1]],
      callback = callback,
      filename = outfile,
      varDict = varDict,
      plotScales = plotScales,
      dpi = dpi,
      width = width,
      height = height,
      limitsize = FALSE,
      addAttributes = addAttributes,
      wrap = wrap,
      ...
    )
  }
  svg <- readSvgAndRemoveTextLength(outfile)
  Encoding(svg) <- "UTF-8"

  list(
    svg = svg,
    data = data
  )
}

#' Run a demo app for ggtips
#'
#' Launches a Shiny app demonstrating the capabilities of ggtips.
#'
#' @export
demo <- function(host = "0.0.0.0", port = 3838L) {
  if (!require("shiny")) {
    stop("Shiny is required to run the demo")
  }
  shiny::runApp(
    appDir = system.file("example", package = "ggtips"),
    host = host,
    port = port
  )
}

#' Helper function that can be used to render GGPlot with ggtips in renderUI
#'
#' @export
plotWithTooltips <- function(plot,
                             varDict,
                             plotScales = NULL,
                             callback = NULL,
                             point.size = 10,
                             dpi = 72,
                             width = NA,
                             height = NA,
                             customGrob = NULL,
                             wrap = FALSE,
                             ...) {
  res <- ggtips::getSvgAndTooltipdata(
    plot = plot,
    varDict = varDict,
    plotScales = plotScales,
    callback = callback,
    point.size = point.size,
    dpi = dpi,
    width = width,
    height = height,
    customGrob = customGrob,
    wrap = wrap,
    ...
  )
  ggtips::htmlWithGivenTooltips(
    svg = res$svg,
    data = res$data,
    height = height,
    width = width,
    point.size = point.size
  )
}
