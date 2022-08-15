![alt text](https://github.com/EIvimeyCook/ShinyDigitise/blob/master/inst/logos/shiny.png)

# shinyDigitise

shinyDigitise builds upon the popular metaDigitise package and provides users with an interactive UI to help extraction of data from (currently) three different plot types.
The main functions used in shinyDigitise are called from the [metaDigitise package](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13118) which allows users to extract descriptive statistics from various plot types. 

Importantly, both packages allows for replotting and checking of data extraction from graphs - which all contribute to increased reproducibility. 

## To install:

devtools::install_github("EIvimeyCook/ShinyDigitise")

library(shinyDigitise)

## To use the shiny app:

df<-shinyDigitise(Folder where your images are located)

## Basic workflow (the app will walk you through each stage):

NB. you can adjust point size and group name positions on the fly.

1. Choose plot type - currently mean/error + boxplot + xy mean/error
2. Orientate figure - flip or rotate the graph (rotation angle is shown on screen)
3. Calibrate axes - click calibrate mode on, and, depending on the plot type shown, click on the axes in the relevant order and add the variable name+values. These will then appear on the plot (the size will depend on the point size slider).
4. Add groups - Clicking add group will cause a pop up to appear to enter data (name + sample size). This will then appear in the table. Clicking the group will allow you to either click points on the graph (you have to press the click points button first) or delete the group. Lastly, you can select the type of error show (if mean/error graph)
5. Comments - add a comment to your data.
6. Finished! The app will close when you've reached the end of your graphs.



*To view the extracted data just simply call the object you've created or view the resulting .csv file (which will be saved into the same folder as the images).*

