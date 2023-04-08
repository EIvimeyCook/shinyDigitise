<p align="center">
  <img src="https://github.com/EIvimeyCook/shinyDigitise/blob/master/inst/shinyDigitise/www/img/shinyDigitise.png" width = "200"/>
</p>

<div align="center">
 <h1>shinyDigitise</h1>
</div>

shinyDigitise builds upon the popular metaDigitise package and provides users with an interactive UI to help extraction of data from five different plot types.
The main functions used in shinyDigitise are called from the [metaDigitise package](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13118) which allows users to extract descriptive statistics from various plot types. 

Importantly, both packages allows for replotting and checking of data extraction from graphs - which all contribute to increased reproducibility. 

Introduced in this new preprint: [A framework for improving the reproducibility of data extraction for meta-analysis. EcoEvoRxiv (2022)](https://ecoevorxiv.org/repository/view/4814/).

## To install:

devtools::install_github("EIvimeyCook/ShinyDigitise")

<br>

**If you plan on using shinyDigitise to extract data from xy mean graphs, you need the dev version of metaDigitise.**

devtools::install_github("joelpick/metaDigitise")

<br>
There is a known bug with some versions of R Studio (https://github.com/rstudio/rstudio/issues/12649). Be sure to update R Studio to the latest version.

## To use the shiny app:

Video tutorial [here](https://www.youtube.com/watch?v=b9KvRsO8SPY)

<br>

library(shinyDigitise)

df<-shinyDigitise(Folder where your images are located)

or

df<-shinyDigitise() - where you specify the folder through a menu system within SD.

## Basic workflow (the app will walk you through each stage):

If a directory is provided, you'll skip straight to the file selection phase. If not you'll be asked to select a file from the image folder. 

NB. you can adjust point size and group name positions on the fly aswell as zoom in on the image by clicking and dragging a box over the desired area.

1. Choose plot type - mean/error + boxplot + xy mean/error + histogram + scatterplot
2. Orientate figure - flip or rotate the graph (rotation angle is shown on screen)
3. Calibrate axes - click calibrate mode on, and, depending on the plot type shown, doublick click on the axes in the relevant order and add the variable name+values. These will then appear on the plot (the size will depend on the point size slider).
4. Add groups - Clicking add group will cause a pop up to appear to enter data (name + sample size). This will then appear in the table. Clicking the group will allow you to either doubleclick points on the graph (you have to press the click points button first) or delete the group. Lastly, you can select the type of error show (if mean/error graph)
5. Comments - add a comment to your data.
6. Finished! The app will close when you've reached the end of your graphs.



*To view the extracted data just simply call the object you've created or view the resulting .csv file (which will be saved into the same folder as the images).*
