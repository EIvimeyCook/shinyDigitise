![alt text](https://github.com/EIvimeyCook/ShinyDigitise/blob/master/inst/logos/shiny.png)

# shinyDigitise

shinyDigtise builds upon the popular metaDigtise package and provides users with an interactive UI to help extraction of data from (currently) three different plot types.
The main functions used in shinyDigitise are called from the [metaDigitise package](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13118) which allows users to extract descriptive statistics from various plot types. 

Importantly, both packages allows for replotting and checking of data extraction from graphs - which all contibute to increased reproducibility. 

## To install:

devtools::install_github("EIvimeyCook/ShinyDigitise")

library(shinyDigitise)

## To use the shiny app:

df<-shinyDigitise(Folder where your images are located)

*To view the extracted data just simply call the object you've created or view the resulting .csv file (which will be saved into the same folder as the images).*
