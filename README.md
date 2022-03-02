![alt text](https://github.com/EIvimeyCook/ShinyDigitise/blob/master/inst/logos/shiny.png)

# ShinyDigitise

Shiny interface to the metaDigitise package (https://github.com/daniel1noble/metaDigitise)

## To install:

devtools::install_github("EIvimeyCook/ShinyDigitise")

library(shinyDigitise)

## To use the shiny app:

df<-shinyDigitise(Folder where your images are located)

*To view the extracted data just simply call the object you've created or view the resulting .csv file (which will be saved into the same folder as the images).*
