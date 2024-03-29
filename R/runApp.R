#' @title shinyDigitise
#' @description shinyDigitise is the graphical user interface to the metaDigitise data extraction package. Allows for single or batch processing of figures with .png, .jpg, .tiff, .pdf extensions within a set directory. The metaDigitise() package then consolidates the data and exports the data for each image and image type. It can also summarise the data, provide the raw data (if scatterplots) and automatically imports previously finished data and merges it with newly digitised data. metaDigitise() also allows users to check their calibration along with editing previous digitisations.
#' @param dir the path name to the directory / folder where the files are located /can be left blank
#' @return A data frame or list containing the raw digitised data or the processed, summary statistics from the digitised data
#' @examples
#' \donttest{
#' # temporary directory (taken from metaDigitise)
#' tmp_dir <- tempdir()
#' 
#' # Simulate data
#' set.seed(103)
#' x <- rnorm(20,0,1)
#' y <- rnorm(20,0,1)
#' means <- c(mean(x),mean(y))
#' ses <- c(sd(x)/sqrt(length(x))*1.96, sd(y)/sqrt(length(y))*1.96)
#' 
#' #Generate mock figures
#' png(filename = paste0(tmp_dir,"/mean_error.png"), width = 480, height = 480)
#' plot(means, ylim = c(min(means-ses)-0.1,max(means+ses)+0.1), xlim=c(0.5,2.5), 
#' xaxt="n", pch=19, cex=2, ylab="Variable +/- SE", xlab="Treatment", main="Mean Error")
#' arrows(1:length(means),means+ses, 1:length(means), means-ses, code=3, angle=90, length=0.1)
#' axis(1,1:length(means),names(means))
#' dev.off()
#' png(filename = paste0(tmp_dir, "/boxplot.png"), width = 480, height = 480)
#' boxplot(x,y, main="Boxplot")
#' dev.off()
#' png(filename = paste0(tmp_dir, "/histogram.png"),width = 480, height = 480)
#' hist(c(x,y), xlab= "variable", main="Histogram")
#' dev.off()
#' png(filename = paste0(tmp_dir, "/scatterplot.png"), width = 480, height = 480)
#' plot(x,y, main="Scatterplot")
#' dev.off()
#' 
#' #shinyDigitise figures
#' \dontrun{
#' data <- shinyDigitise(dir = tmp_dir)
#' }
#' }
#' @export


shinyDigitise <- function(dir=NULL){
#if dir is missing then label the dir object as missing (important for SD initation).
#load into the environment the dir object and assign the UIs and Server into the env.
  if(missing(dir)){
  dir <- "Missing"
shiny_env <- 1
envir = as.environment(shiny_env)
assign("dir", dir, envir = envir)
  } else {
#else if dir is not missing then label the dir object as the provided datapath (important for SD initation). 
#otherwise same as previous
dir <- dir
shiny_env <- 1
envir = as.environment(shiny_env)
assign("dir", dir, envir = envir)
  }

appDir <- system.file("shinyDigitise", package = "shinyDigitise")
shiny::runApp(appDir, display.mode = "normal")

  }

