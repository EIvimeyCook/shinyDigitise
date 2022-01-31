
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

showNotification2 <- function (ui, action = NULL, duration = 5, closeButton = TRUE, 
                               id = NULL, type = c("default", "message", "warning", "error"), 
                               session = shiny:::getDefaultReactiveDomain()) {
  if (is.null(id)) 
    id <- shiny:::createUniqueId(8)
  res <- shiny:::processDeps(HTML(ui), session)
  actionRes <- shiny:::processDeps(action, session)
  session$sendNotification("show", list(html = res$html, action = actionRes$html, 
                                        deps = c(res$deps, actionRes$deps), duration = if (!is.null(duration)) duration * 
                                          1000, closeButton = closeButton, id = id, type = match.arg(type)))
  id
}


