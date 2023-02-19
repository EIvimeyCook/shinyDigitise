#' @title shinyDigitise
#' @description Single or batch processing of figures with .png, .jpg, .tiff, .pdf extensions within a set directory. metaDigitise() consolidates the data and exports the data for each image and image type. It can also summarise the data, provide the raw data (if scatterplots) and automatically imports previously finished data and merges it with newly digitised data. metaDigitise() also allows users to check their calibration along with editing previous digitisations.
#' @param dir the path name to the directory / folder where the files are located /can be left blank
#' @export


shinyDigitise <- function(dir=NULL){
#if dir is missing then label the dir object as missing (important for SD initation).
#load into the environment the dir object and assign the UIs and Server into the env.
  if(missing(dir)){
  dir <- "Missing"
  assign("dir",dir,.GlobalEnv)
  } else {
#else if dir is not missing then label the dir object as the provided datapath (important for SD initation). 
#otherwise same as previous
   dir <- dir
   assign("dir",dir,.GlobalEnv)
  }

appDir <- system.file("shinyDigitise", package = "shinyDigitise")
shiny::runApp(appDir, display.mode = "normal")

  }
