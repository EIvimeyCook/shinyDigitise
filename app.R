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
 done_details <- dir_details(dir)
details <- get_notDone_file_details(dir)

counter_total <- length(details$paths)

ui <- fluidPage(
    tags$head(
      tags$style(".buttonagency .bttn-primary{background-color: darkblue;}"),
      tags$style(type = 'text/css',".myclass1 {background-color: #EFF8CD;}"),
      tags$style(type = 'text/css',".myclass2 {background-color: #EFF8CD;}"),
      tags$style(type = 'text/css',".myclass3 {background-color: #EFF8CD;}"),
      tags$style(type = 'text/css',".myclass4 {background-color: #EFF8CD;}"),
      tags$style(type = 'text/css',".myclass5 {background-color: #c9d7e8;}")
    ),
    theme = bs_theme(fg = "#3F1010", primary = "#332C50", base_font = "Arial", 
                                     font_scale = NULL, `enable-shadows` = TRUE, spacer = "0.8rem", 
                                     bootswatch = "flatly", bg = "#FFFFFF"),
    fluidRow(
    titlePanel(title=div(img(src="shiny.jpg", height = 80),"shinyDigitise"), windowTitle = "shinyDigitise"),
        column(2, wellPanel(
          # shinyDirButton('dir', 'Select a folder', 'Please select a folder', FALSE),
          # verbatimTextOutput("dir", placeholder = TRUE),
          progressBar(id = "progress", value = 0, status = "danger", striped = TRUE, display_pct = T)
        )),
    column(3,wellPanel(
      # checkboxGroupButtons(
      #     inputId = "Next",
      #     choices = c("Next", "Previous"),
      #     status = "danger"
      # )
      div(class = "buttonagency",
      actionBttn(
        inputId = "Previous",
        label = "Previous", 
        style = "material-flat",
        color = "primary",
      ),
      actionBttn(
        inputId = "Next",
        label = "Next", 
        style = "material-flat",
        color = "primary"
      ))
      
    )),
    
        column(2, wellPanel(
          strong("Show only unprocessed images:"), class = "myclass1", id = "myid1",
          prettyToggle(
            inputId = "ShowOnlyNew",
            label_on = "Yes", 
            icon_on = icon("check"),
            status_on = "info",
            status_off = "warning", 
            label_off = "No",
            icon_off = icon("remove")
          )
        )),
    
        column(2, wellPanel(
               sliderTextInput(
                 inputId = "general_cex",
                 label = strong("Point Size:"), 
                 choices = c(seq(from = 0.1, to = 3, by = 0.1)),
                 grid = F)
        ))
    ),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel( width = 4,
        id = "tPanel",style = "overflow-y:scroll; max-height: 600px; position:relative;",

        ## I've changed this because flip and rotate are different processes, so need two buttons
        wellPanel(
          strong("Image adjust:"), class = "myclass2", id = "myid1",
          br(),
            # checkboxGroupButtons(
            #     inputId = "Orient",
            #     label = h3("Orientation:"),
            #     choices = c("Flip", "Rotate"),
            #     status = "danger"
            # )
          div(class = "buttonagency",
          actionBttn(
            inputId = "Flip",
            label = "Flip",
            style = "material-flat", 
            color = "primary"
          ),
          actionBttn(
            inputId = "Rotate",
            label = "Rotate",
            style = "material-flat", 
            color = "primary"
          ))
          ),

          wellPanel(
            pickerInput(
                inputId = "PlotType",
                label = h6(strong("Plot type:")),
                choices = c("Mean/error", "Scatterplot", "Histogram", "Boxplot"),
                
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
	                             label = h6(strong("Type of error")),
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
                   value = F,
                   status = "danger"),



            )
          ),
          wellPanel(
            strong("Groups:"), class = "myclass3", id = "myid1",
            br(),
            div(class = "buttonagency",
            actionBttn(
              inputId = "Add",
              label = "Add", 
              style = "material-flat",
              color = "primary"
            ),
            actionBttn(
              inputId = "delete",
              label = "Delete", 
              style = "material-flat",
              color = "primary"
            ))
              )
          

        ),
mainPanel(
        plotOutput("metaPlot", 
                   click="plot_click",
                   dblclick = "plot_dblclick",
                   hover = "plot_hover",
                   brush = "plot_brush"),
        verbatimTextOutput("info")
    )
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
#   length(details$paths)
# }
# else{
#   length(details$paths)
# }
# # })

# addResourcePath(prefix = "imgResources", directoryPath = dir)
 images <-  details$paths#paste0(dir,list.files( dir))

counter <- reactiveValues(countervalue = 1)

# output$metaPlot <- renderUI({
#
#   img(src=paste(images[counter$countervalue]))
#
# })
output$metaPlot <- renderPlot({

  image <- image_read(images[counter$countervalue])
	plot(image)
}

	# list(src = image, contentType = "image/png")
# } ,
#height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0))
)


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
}})

observeEvent(counter$countervalue, {
	if(counter$countervalue > counter_total) {
		counter$countervalue <- counter_total
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
    percent = round(counter$countervalue/counter_total*100, 0)
    paste0("<font color=\"#ff3333\"><b>",percent,"%", " ", "extracted","</b></font>")

})

output$info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
  }
  xy_range_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
           " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
  }
  
  paste0(
    "click: ", xy_str(input$plot_click),
    "dblclick: ", xy_str(input$plot_dblclick),
    "hover: ", xy_str(input$plot_hover),
    "brush: ", xy_range_str(input$plot_brush)
  )
})


}





shinyApp(ui = ui, server = server)
