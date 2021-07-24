#load packages
library(shiny)
library(shinyWidgets)
library(shinyalert)
library(shinyjs)
library(shinyFiles)
library(magick)
library(metaDigitise)
library(fresh)
library(shinythemes)

dir <- "~/Downloads/Image"

if( (substring(dir, nchar(dir)) == "/") == FALSE){
		dir <- paste0(dir, "/")
	}
  setup_calibration_dir(dir)
 done_details <- dir_details(dir)
details <- get_notDone_file_details(dir)

ui <- fluidPage(
    useShinyalert(),
    useShinyjs(),
    theme = shinytheme("sandstone"),
    titlePanel(title=div(img(src="shiny.jpg", height = 100)), windowTitle = "shinyDigitise"),

    fluidRow(
        column(3,
          # shinyDirButton('dir', 'Select a folder', 'Please select a folder', FALSE),
          # verbatimTextOutput("dir", placeholder = TRUE),
          htmlOutput("progress")
        ),
        column(3,
          awesomeRadio(
              inputId = "ShowOnlyNew",
              label = "Show Only Unprocessed Images:",
              choices = c("Yes", "No"),
              status = "warning",
              inline=TRUE
          )
        ),
        column(3,
          textInput(inputId="general_cex",label="Point Size",value=1)
        )

    ),

    # Sidebar with a slider input for number of bins
    fluidRow(

      column(3,
        wellPanel(
            # checkboxGroupButtons(
            #     inputId = "Next",
            #     choices = c("Next", "Previous"),
            #     status = "danger"
            # )
            actionButton(inputId = "Previous",
                         label = "Previous"),
            actionButton(inputId = "Next",
                         label = "Next"),

          ),

        ## I've changed this because flip and rotate are different processes, so need two buttons
        wellPanel(
            # checkboxGroupButtons(
            #     inputId = "Orient",
            #     label = h3("Orientation:"),
            #     choices = c("Flip", "Rotate"),
            #     status = "danger"
            # )
            actionButton(inputId = "flip",
                         label = "Flip"),
            actionButton(inputId = "rotate",
                         label = "Rotate"),
          ),

          wellPanel(
            awesomeRadio(
                inputId = "PlotType",
                label = h3("Plot type:"),
                choices = c("Mean/error", "Scatterplot", "Histogram", "Boxplot"),
                status = "primary",
                inline=TRUE)
              ),

          wellPanel(
            conditionalPanel(
                 condition = "input.PlotType == 'Histogram'",
                textInput(inputId = "xvar",
                      label = "X Axis Name"),
                br(),
                actionButton(inputId = "calib",
                         label = "Calibrate"),
                br(),
                textInput(inputId = "y1",
                          label = "Y1 Value"),
                textInput(inputId = "y2",
                          label = "Y2 Value"),
                br(),
                textInput(inputId = "x1",
                          label = "X1 Value"),
                textInput(inputId = "x2",
                        label = "X2 Value"),
                br(),
                textInput(inputId = "nsamp",
                        label = "Known sample size"),

                       ),
               conditionalPanel(
                 condition = "input.PlotType == 'Mean/error'",
                 textInput(inputId = "xvar",
                              label = "X Axis Name"),
                              br(),
                              actionButton(inputId = "calib",
                                label = "Calibrate"),
                             br(),
                            br(),
                            textInput(inputId = "y1",
                                label = "Y1 Value"),
                            textInput(inputId = "y2",
                                label = "Y2 Value"),
                            br(),
	                         awesomeRadio(
	                             inputId = "errortype",
	                             label = h3("Type of error"),
	                             choices = c("SE", "95%CI", "SD"),
	                             status = "warning",
	                             inline=TRUE
	                         )


                       ),
               conditionalPanel(
                 condition = "input.PlotType == 'Scatterplot'",
                 textInput(inputId = "xvar",
                              label = "X Axis Name"),
                              textInput(inputId = "yvar",
                                           label = "Y Axis Name"),
                              br(),
                              actionButton(inputId = "calib",
                           label = "Calibrate"),
                           br(),
                             br(),
                           textInput(inputId = "y1",
                                label = "Y1 Value"),
	                        textInput(inputId = "y2",
	                             label = "Y2 Value"),
                             br(),
                             textInput(inputId = "x1",
                              label = "X1 Value"),
                              textInput(inputId = "x2",
                               label = "X2 Value"),
                           br(),
                           awesomeCheckbox(
                             inputId = "Id005",
                             label = "Logged values?",
                             value = TRUE,
                             status = "danger"),
                             br(),
                           textInput(inputId = "nsamp",
                                        label = "Known sample size"),

                       ),
               conditionalPanel(
                 condition = "input.PlotType == 'Boxplot'",
                 textInput(inputId = "yvar",
                      label = "Y Axis Name"),
                      br(),
                      actionButton(inputId = "calib",
                                       label = "Calibrate"),
	                   br(),
                       textInput(inputId = "y1",
		                    label = "Y1 Value"),
                        textInput(inputId = "y2",
                             label = "Y2 Value"),
                     br(),
                 awesomeCheckbox(
                   inputId = "Id005",
                   label = "Logged values?",
                   value = TRUE,
                   status = "danger"),



            )
          ),
          wellPanel(
              checkboxGroupButtons(
                  inputId = "grouping",
                  label = h3("Groups"),
                  choices = c("Add", "Delete"),
                  status = "royal"
              )
            )

        ),
        column(6,wellPanel(
        uiOutput("metaPlot")
    ))
      )
    )




server <- function(input, output, session) {

#dirchoose function
  # shinyDirChoose(
  #   input,
  #   'dir',
  #   roots = c(home = '~'),
  #   filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  # )

#create reactive object of dir
  # dir <- reactive(input$dir)

#render the file path in the outut
 # output$dir <- renderText({  # use renderText instead of renderPrint
 #   parseDirPath(c(home = '~'), dir())
 # })


# # done files
# done_details$names
#
# dir<-"/Users/joelpick/Desktop/images/"
# metaDigitise(dir)
# setup_calibration_dir(dir)
#setup caldat file
     values <- reactiveValues(images = NULL, images_paths = NULL)

# observeEvent(input$dir, {
  # values$images <-  list.files( parseDirPath( c(home = '~'), dir()))


   # Add directory of static resources to Shiny's web server
   # addResourcePath(prefix = "imgResources", directoryPath = dir)
   #  values$images <-  paste0("imgResources/",list.files( dir)
   # <- paste0("imgResources/myplot", seq_len(3), ".png")


# setup_calibration_dir(paste0(parseDirPath(c(home = '~'), dir())))
# done_details <- dir_details(paste0(parseDirPath(c(home = '~'), dir())))
# details <- get_notDone_file_details(paste0(parseDirPath(c(home = '~'), dir())))
# all_paths <- dir_details(dir)$paths
# not_done_paths <- get_notDone_file_details(dir)$paths
# counter_total <-
# if(input$ShowOnlyNew == "yes"){
#   length(not_done_paths)
# }
# else{
#   length(all_paths)
# }
# })

addResourcePath(prefix = "imgResources", directoryPath = dir)
 images <-  paste0("imgResources/",list.files( dir))


counter <- reactiveValues(countervalue = 1)

output$metaPlot <- renderUI({
  img(src=paste(images[counter$countervalue]))

})

observeEvent(input$Next, {
    counter$countervalue <- counter$countervalue + 1
})

observeEvent(input$Previous, {
    counter$countervalue <- counter$countervalue - 1
})

#alert for trying to go backward below 1
observeEvent(counter$countervalue, {
    if(counter$countervalue == 0) {
      counter$countervalue <- counter$countervalue + 1
}
})

# output$metaPlot <- renderImage({
#   if(input$ShowOnlyNew == "yes"){
#     not_done_paths[counter]
#   }
#   else{
#     all_paths[counter]
#   }
#   })



output$progress <- renderText({
    percent = round(counter$countervalue*100, 0)
    paste0("<font color=\"#ff3333\"><b>",percent,"%", " ", "screened","</b></font>")

})


}





shinyApp(ui = ui, server = server)
