#' Extract data from figures with the shinyDigitise app
#'
#' @description
#' Opens the shinyDigitise app, a point-and-click interface to the
#' [metaDigitise](https://github.com/daniel1noble/metaDigitise) package for
#' extracting data from figures in published papers, for example for a
#' meta-analysis. Put the figures (`.png`, `.jpg`, `.tiff` or `.pdf`) in one
#' folder, open it in the app, and digitise them one at a time or as a batch.
#'
#' For each figure the app walks you through orientation, axis calibration,
#' and clicking the points, groups and error bars. It works with box plots,
#' mean and error plots, scatter plots and histograms. Extractions are redrawn
#' over the figure so they can be checked, and each figure's calibration and
#' clicks are saved in a `caldat/` folder inside the image folder, so it can be
#' reopened, edited or checked later in shinyDigitise or metaDigitise. Figures
#' already digitised are imported and combined with new ones.
#'
#' @param dir Optional path to the folder containing the figures to digitise.
#'   If `NULL` (the default), choose the folder in the app.
#' @return When the app is closed, a data frame of the extracted values (the
#'   same summary metaDigitise gives: for example group means, standard
#'   deviations and sample sizes), also saved as `ExtractedData.csv` in the
#'   image folder. `NULL` if nothing was digitised.
#' @seealso [metaDigitise::metaDigitise()], which works with the same saved
#'   files.
#' @examples
#' \dontrun{
#' # Choose the folder of figures in the app
#' data <- shinyDigitise()
#'
#' # Or open a folder of figures directly
#' data <- shinyDigitise(dir = "~/meta-analysis/figures")
#' head(data)
#'
#' # To try the app without your own figures, make four example plots
#' # (one of each type) in a temporary folder and open that
#' tmp_dir <- file.path(tempdir(), "figures")
#' dir.create(tmp_dir)
#' set.seed(103)
#' x <- rnorm(20)
#' y <- rnorm(20)
#' means <- c(mean(x), mean(y))
#' ses <- c(sd(x), sd(y)) / sqrt(20) * 1.96
#'
#' png(file.path(tmp_dir, "mean_error.png"), width = 480, height = 480)
#' plot(means, ylim = range(means - ses, means + ses) + c(-0.1, 0.1),
#'      xlim = c(0.5, 2.5), xaxt = "n", pch = 19, cex = 2,
#'      ylab = "Variable +/- SE", xlab = "Treatment", main = "Mean Error")
#' arrows(1:2, means + ses, 1:2, means - ses, code = 3, angle = 90, length = 0.1)
#' invisible(dev.off())
#'
#' png(file.path(tmp_dir, "boxplot.png"), width = 480, height = 480)
#' boxplot(x, y, main = "Boxplot")
#' invisible(dev.off())
#'
#' png(file.path(tmp_dir, "histogram.png"), width = 480, height = 480)
#' hist(c(x, y), xlab = "variable", main = "Histogram")
#' invisible(dev.off())
#'
#' png(file.path(tmp_dir, "scatterplot.png"), width = 480, height = 480)
#' plot(x, y, main = "Scatterplot")
#' invisible(dev.off())
#'
#' data <- shinyDigitise(dir = tmp_dir)
#' }
#' @export


shinyDigitise <- function(dir=NULL){
#if dir is missing then label the dir object as missing (important for SD initation).
#the folder is passed to the app as an option (restored on exit) rather than being
#written into the user's global environment.
  if(missing(dir) || is.null(dir)){
    dir <- "Missing"
  } else {
    if(!is.character(dir) || length(dir) != 1 || is.na(dir)) stop("dir must be a single folder path", call. = FALSE)
    if(!dir.exists(dir)) stop("The folder '", dir, "' does not exist", call. = FALSE)
  }
  old <- options(shinyDigitise.dir = dir)
  on.exit(options(old), add = TRUE)

appDir <- system.file("shinyDigitise", package = "shinyDigitise")
shiny::runApp(appDir, display.mode = "normal")

  }

