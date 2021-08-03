
#load packages
library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(magick)
library(metaDigitise)
library(fresh)
library(shinythemes)
library(bslib)

if(Sys.info()["user"]=="joelpick"){
  dir <- "/Users/joelpick/Desktop/images"
}else{
  dir <- "~/Downloads/Image"
}


if( (substring(dir, nchar(dir)) == "/") == FALSE){
    dir <- paste0(dir, "/")
  }
  setup_calibration_dir(dir)
#  done_details <- dir_details(dir)
# details <- get_notDone_file_details(dir)

details <- dir_details(dir)

counter_total <- length(details$paths)
# filename(details$paths[1])
