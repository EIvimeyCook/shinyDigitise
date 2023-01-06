rm(list=ls())
library(metaDigitise)
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
devtools::load_all("R")
dat<-shinyDigitise(here::here("Image"))

write.csv(dat, "dat.csv", row.names = F)


rm(list=ls())
devtools::install_github("EIvimeyCook/ShinyDigitise", force = T)
library(shinyDigitise)
dat<-shinyDigitise(here::here("Image"))




