## 0.4.0
### Bugs
* add addAttributes parameter to getSvgAndTooltipdata to fix unused argument errors
* Fix the ordering of plot data for some trellises
* Optimize the workflow
* Bump up minimal required versions of R and ggplot2

## 0.3.5
### Features
* Allow to set transparent plots
### Bugs
* fix error when tooltip function return empty list
* Workaround for unfilled shapes

## 0.3.4
### Features
* NULL `varDict` turns off rendering tooltips

### Bugs
* Handling NA values
* Flipped coords
* Plots with custom ranges
* Multi-faceted trellises

## 0.3.3
* Updated MIT license

## 0.3.2
### Bugs
* fix tooltip offset with scrollbar on content
* fix issue on Firefox [#36](https://github.com/Roche/ggtips/issues/36)
* add missing height parameter
* enable polygon points for tooltips

## 0.3.1
### Bugs
* argument height

## 0.3.0
### Features
* improved API `ggtips::plotWithTooltips`

## 0.2.1
### Bugs
* fix hiding tooltip when hover out the point

## 0.2.0
### Features
* customGrob option
### Bugs
* fix arrow in FireFox [#15](https://github.com/Roche/ggtips/issues/15)
* compatibility fix for GGPlot 2 and 3 [#17](https://github.com/Roche/ggtips/pull/17)
* fix error with multiple panels and removed middle one [#18](https://github.com/Roche/ggtips/pull/18)
* fix right aligned tooltip [#19](https://github.com/Roche/ggtips/pull/19)

## 0.1.0
* Initial version
