
rm(list=ls())
devtools::install_github(repo = "EIvimeyCook/ShinyDigitise@InDev", force = TRUE)
library(shinyDigitise)
dat<-shinyDigitise(dir = "~/Desktop")

devtools::load_all("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/AllFiles/Other Projects/R Packages/ShinyDigitise/R")
library(shinyDigitise)
dat<-shinyDigitise(dir =  "~/Desktop") 
