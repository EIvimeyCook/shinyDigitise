
#load packages
library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(magick)
library(metaDigitise)
library(fresh)
library(shinythemes)
library(bslib)
library(tibble)
library(DT)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(scales)
library(shinyalert)
library(emojifont)
load.emojifont('OpenSansEmoji.ttf')


# if(Sys.info()["user"]=="joelpick"){
#   dir <- "/Users/joelpick/Desktop/images"
# }else{
#   dir <- "~/Downloads/Image"
# }

# if(Sys.info()["user"]=="edwardivimey-cook"){
#   dir <- "/Volumes/GoogleDrive/My Drive/Postdoc/Other Projects/R Packages/ShinyDigitise/Image"
# }else{
#   dir <- "~/Downloads/Image"
# }


# if( (substring(dir, nchar(dir)) == "/") == FALSE){
#     dir <- paste0(dir, "/")
#   }
#   setup_calibration_dir(dir)
# #  done_details <- dir_details(dir)
# # details <- get_notDone_file_details(dir)

# details <- dir_details(dir)

# counter_total <- length(details$paths)
# # filename(details$paths[1])

# n <- 20
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

# col=sample(col_vector, n)

