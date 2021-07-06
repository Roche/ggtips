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

#' Parse plot/layer mapping
#'
#' Converts ggplot mapping to a character vector.
#'
#' @param mapping ggplot mapping.
#'
#' @return Character.
parseMapping <- function(mapping) {
  if (is.symbol(mapping)) {
    as.character(mapping)
  } else if (rlang::is_quosure(mapping)) {
    rlang::quo_name(mapping)
  } else {
    as.character(mapping)
  }
}

#' Get layer aesthetics
#'
#' Returns a list of aesthetics for each layer in a ggplot.
#'
#' @param layer A ggplot object.
#'
#' @return A list.
#' @export
getLayerAesthetics <- function(plot) {
  layers <- plot$layers
  plotMapping <- plot$mapping
  lapply(layers, function(layer) {
    layerMapping <- if (layer$inherit.aes) {
      plotMapping
    } else {
      ggplot2::aes()
    }
    
    if (length(layer$mapping) > 0) {
      layerMapping[names(layer$mapping)] <- layer$mapping
    }
    
    lapply(layerMapping, parseMapping)
  })
}

#' Get layer geometries
#'
#' Returns grid-compatible geometry names for a ggplot layer.
#'
#' @param layer A ggplot layer object.
#'
#' @return A character vector.
#' @export
getLayerGeom <- function(layer) {
  geomDict <- list(
    #TODO complete the list
    # GeomLine = "polyline",
    # GeomPath = "polyline",
    GeomPoint = "points"
  )
  classes <- class(layer$geom)
  unique(unlist(geomDict[classes]))
}

#' Unmap factors
#'
unmapFactors <- function(df, origin) {
  # Order factor levels in the original data frame
  origin <- freezeFactorLevels(origin)
  # Include only matching rows
  origin <- origin[rownames(df),]
  # Select only factor variables
  factors <- Filter(
    function(name) { is.factor(origin[[name]]) },
    names(origin)
  )
  for (name in factors) {
    origColumn <- droplevels(origin[[name]])
    if (name %in% names(df)) {
      # Map values in the column to the original values
      column <- df[[name]]
      asFactor <- factor(column, levels = unique(column))
      df[[name]] <- levels(origColumn)[asFactor]
    } else {
      if (length(origColumn) == nrow(df)) {
        # Simply add the column from the original data frame
        df[[name]] <- origColumn
      }
    }
  }
  df
}

#' Unmap aesthetics
#'
unmapAes <- function(data, mapping, plot) {
  plotLayersData <- getPlotLayerData(plot)

  unmapped <- mapply(
    function(df, map, plotData) {
      mapNames <- names(map)
      names(df) <- sapply(names(df), function(name) {
        if (name %in% mapNames) { map[[name]] } else { name }
      })
      unmapFactors(df, origin = plotData)
    },
    data,
    mapping,
    plotLayersData,
    SIMPLIFY = FALSE
  )
  orderByPanels(unmapped)
}

#' Order by panels
#' 
#' Orders each data frame in a list by column \code{PANEL} if it exists.
#' 
#' @param dfList A list of data frames.
#' 
#' @return A list of data frames.
orderByPanels <- function(dfList) {
  lapply(dfList, function(df) {
    if (!"PANEL" %in% names(df)) {
      df
    } else {
      df[order(df[["PANEL"]]), ]
    }
  })
}

#' Get plot layer data
#' 
#' Returns list of data elements from plot layers. If plot layer data element is 
#' ggplot2 waiver then plot's data element is used as default. 
#' 
getPlotLayerData <- function(plot) {
  lapply(
    plot$layers, 
    function(l) { if (is(l$data, "waiver")) plot$data else l$data }
  )
}

#' Remove out of range data
#'
#' If plot has data that was filtered when specific geom was added 
#' it should be filtered out of data.
#'
removeOutOfRangeData <- function(data, plot, built) {
  lapply(data, function(d) {
    range <- getRanges(plot, built)

    if (is(plot$coordinates, "CoordFlip") && isGgplot2()) {
      d <- d[d$x >= min(range$y) & d$x <= max(range$y), ]
      d <- d[d$y >= min(range$x) & d$y <= max(range$x), ]
    } else {
      d <- d[d$x >= min(range$x) & d$x <= max(range$x), ]
      d <- d[d$y >= min(range$y) & d$y <= max(range$y), ]
    }
    
    d
  })
}

#' Get range data
#'
#' Depends on ggplot2 version
#'
getRanges <- function(plot, built) {
  if (isGgplot2()) {
    xRanges <- sapply(built$layout$panel_ranges, function(x) x[["x.range"]])
    yRanges <- sapply(built$layout$panel_ranges, function(x) x[["y.range"]])
  } else {
    xRanges <- sapply(built$layout$panel_scales_x, function(scale) {
      ggplot2:::expand_limits_scale(
        scale = scale,
        expand = ggplot2:::default_expansion(scale), 
        coord_limits = built$layout$coord$limits$x 
      )
    })
    yRanges <- sapply(built$layout$panel_scales_y, function(scale) {
      ggplot2:::expand_limits_scale(
        scale = scale,
        expand = ggplot2:::default_expansion(scale), 
        coord_limits = built$layout$coord$limits$y 
      )
    })
  }
  
  list(
    x = c(min(xRanges[1, ]), max(xRanges[2, ])),
    y = c(min(yRanges[1, ]), max(yRanges[2, ]))
  )
}

#' Add custom contents to the tooltips
#'
#' For each row of the plot data, applies a callback function that returns
#' an HTML character string to be appended to the contents.
#'
addCustomContents <- function(data, callback) {
  if (is.null(callback)) {
    data
  } else {
    fun <- function(x) { c(.custom = callback(x)) }
    lapply(data, function(df) {
      plyr::adply(df, .margins = 1L, .fun = fun)
    })
  }
}

#' Use columns defined in variable dictionary
#'
getNamesFromVarDict <- function(df, varDict, mapping) {
  dfNames <- names(df)
  # If varDict is NULL, use all mapped columns
  if (is.null(varDict)) {
    dictNames <- setdiff(unlist(mapping), ".custom")
    varDict <- structure(as.list(dictNames), names = dictNames)
  }
  customColumn <- if (".custom" %in% dfNames) {
    df[[".custom"]]
  }
  validNames <- intersect(names(varDict), dfNames)
  if (length(validNames) == 0 && is.null(customColumn)) {
    return(NULL)
  }
  varDict <- varDict[validNames]
  df <- df[names(varDict)]
  names(df) <- varDict

  if (!is.null(customColumn)) {
    cbind(".custom" = customColumn, df)
  } else {
    df
  }
}

#' As trans
#' 
#' Gets a proper trans object from scales package. Original function 
#' scales::as.trans() is not working properly when scales are in Imports
#' 
#' @param x character string, the scale name
#' 
#' @return scale object
as_trans <- function(x){
  trans <- get(paste0(x, "_trans"), asNamespace("scales"))
  trans()
}

#' Untransform scales
#'
untransformScales <- function(data, plotScales) {
  lapply(data, function(df) {
    if (!is.null(plotScales$x) && "x" %in% names(df)) {
      tr <- as_trans(plotScales$x)
      df[["x"]] <- tr$inverse(df[["x"]])
    }
    if (!is.null(plotScales$y) && "y" %in% names(df)) {
      tr <- as_trans(plotScales$y)
      df[["y"]] <- tr$inverse(df[["y"]])
    }
    df
  })
}

#' Get decimal places per each element in x
#'
#' @author Michal Jakubczak
decimalPlaces <- function(x) {
  sapply(
    X = strsplit(
      x = sub(
        pattern = "0+$",
        replacement = "",
        x = format( # uses options("digits") to return significant digits
          x = x,
          trim = TRUE,
          scientific = FALSE
        )
      ),
      split = ".",
      fixed = TRUE
    ),
    FUN = function(x) {
      if (is.na(x[2])) {
        0
      } else {
        nchar(x[2])
      }
    }
  )
}

#' Round data column values
#'
#' @author Michal Jakubczak
roundColumn <- function(column, maxDecimals = 3) {
  if (all(is.double(column))) {
    digits <- decimalPlaces(column)

    nDigits <- min(
      maxDecimals,
      max(digits, na.rm = TRUE)
    )
    column <- sprintf(
      paste0("%.", nDigits, "f"),
      round(column, digits = nDigits)
    )
  }
}

#' Round values
#'
#' @author Michal Jakubczak
roundValues <- function(data) {
  lapply(data, function(df) {
    if (nrow(df) > 0 && "x" %in% names(df)) {
      df[["x"]] <- roundColumn(df[["x"]])        
    }
    if (nrow(df) > 0 && "y" %in% names(df)) {
      df[["y"]] <- roundColumn(df[["y"]]) 
    }

    df
  })
}

#' Remove rows with NA for required aes
#' 
removeRowsWithNA <- function(data, layers, originalData) {
  mapply(
    FUN = function(df, layer, origData){
      origData[["row_index"]] <- seq_len(nrow(origData))
      # don't inform twice about data removal (Removed n rows containing missing values (geom_point))
      origData <- suppressWarnings(layer$geom$handle_na(origData, layer$geom_params))
      
      df[origData$row_index, ]
    },
    data,
    layers,
    originalData,
    SIMPLIFY = FALSE
  )
}

#'  Get data for tooltip contents
#'
getTooltipData <- function(plot, built, varDict, plotScales, callback) {
  mapping <- getLayerAesthetics(plot)
  data <- built$data
  data <- removeOutOfRangeData(data = data, plot = plot, built = built)
  data <- untransformScales(data, plotScales = plotScales)
  data <- roundValues(data)
  originalData <- data
  data <- unmapAes(data, mapping = mapping, plot = plot)
  data <- addCustomContents(data, callback = callback)
  data <- removeRowsWithNA(data, plot$layers, originalData) # must be executed after addCustomContents
  lapply(data, getNamesFromVarDict, varDict = varDict, mapping = mapping)
}

#' Convert tooltip data to character strings
#'
tooltipDataToText <- function(df, wrap = FALSE, width = 50) {
  df <- sapply(names(df), function(varName) {
    text <- if (varName == ".custom") {
      df[[varName]]
    } else {
      wrapped <- if (isTRUE(wrap)) {
        wrap(df[[varName]], width = width)
      } else {
        df[[varName]]
      }
      paste(varName, wrapped, sep = ": ")
    }
    paste0("<li>", text, "</li>")
  })
  if (is.vector(df)) {
    df <- t(df)
  }
  sprintf("<ul>%s</ul>", apply(df, 1, paste, collapse = ""))
}
