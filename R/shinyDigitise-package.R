#' @keywords internal
"_PACKAGE"

# The app itself lives in inst/shinyDigitise and calls these packages with pkg::fun.
# R CMD check only looks at the R/ folder, so one function from each package is
# imported here to record that the app needs them.
#' @importFrom bslib bs_theme
#' @importFrom dplyr group_by
#' @importFrom DT datatable
#' @importFrom graphics points
#' @importFrom magick image_read
#' @importFrom metaDigitise getExtracted
#' @importFrom scales alpha
#' @importFrom shinyalert shinyalert
#' @importFrom shinyFiles shinyFileChoose
#' @importFrom shinyjqui jqui_draggable
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyscreenshot screenshot
#' @importFrom shinyWidgets switchInput
#' @importFrom stats lm
#' @importFrom tippy tippy_this
#' @importFrom utils write.csv
NULL
