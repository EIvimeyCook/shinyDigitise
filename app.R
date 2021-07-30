#load packages
library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(magick)
library(metaDigitise)
library(fresh)
library(shinythemes)
library(bslib)
library(metaDigitise)

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

textInput3<-function (inputId, label, value = "",...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,...))
}

ui <- fluidPage(
    tags$head(
      tags$style(".buttonagency .bttn-primary{background-color: black; color: white;}"),
      tags$style(type = 'text/css',".myclass1"),
      tags$style(type = 'text/css',".myclass2}"),
      tags$style(type = 'text/css',".myclass3"),
      tags$style(type = 'text/css',".myclass4"),
      tags$style(type = 'text/css',".myclass5")
    ),
    theme = bs_theme(fg = "#3F1010", primary = "#332C50", base_font = "Arial", 
                                     font_scale = NULL, `enable-shadows` = TRUE, spacer = "0.8rem", 
                                     bootswatch = "flatly", bg = "#FFFFFF"),
    fluidRow(
    titlePanel(title=div(img(src="shiny.jpg", height = 80),"shinyDigitise"), windowTitle = "shinyDigitise"),
    column(width = 3, offset = 2, 
           wellPanel(
      # checkboxGroupButtons(
      #     inputId = "Next",
      #     choices = c("Next", "Previous"),
      #     status = "danger"
      # )
      div(class = "buttonagency",
          fluidRow(
            column(width = 8,
      actionButton(
        inputId = "Previous",
        label = "Previous", 
        #style = "float",
        #color = "primary",
      )),
      column(width = 2,
      actionButton(
        inputId = "Next",
        label = "Next", 
        #style = "float",
        #color = "primary"
      )),
      )
    )
    )
    ),
    
        column(2, wellPanel(
          strong("Show processed images:"), class = "myclass1", id = "myid1",
          checkboxInput(
            inputId = "ShowOnlyNew",
            label = NULL,
            value = F,
            #label_on = "Yes", 
            #icon_on = icon("check"),
            #status_on = "info",
            #status_off = "warning", 
            #label_off = "No",
            #icon_off = icon("remove")
          )
        )),
    
        column(2, wellPanel(
               sliderInput(
                 inputId = "general_cex",
                 label = NULL, 
                 value = 1,
                 min = 0.1,
                 max = 3, 
                 ticks  = F)
        ))
    ),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel( width = 4,
        id = "tPanel",style = "overflow-y:scroll; max-height: 600px; position:relative;",
        
        h5(htmlOutput("progress")),
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
          actionButton(
            inputId = "Flip",
            label = "Flip",
            #style = "float", 
            #color = "primary"
          ),
          actionButton(
            inputId = "Rotate",
            label = "Rotate",
            #style = "float", 
            #color = "primary"
          ))
          ),

          wellPanel(
            radioButtons(
                inputId = "PlotType",
                label = strong("Plot type:"),
                choices = c("Mean/error", "Scatterplot", "Histogram", "Boxplot"),
                inline = T,
                #checkIcon = list(
                  #yes = tags$i(class = "fa fa-check-square", 
                   #            style = "color: white"),
                  #no = tags$i(class = "fa fa-square-o", 
                              #style = "color: white"))
            )
              ),

          wellPanel(
            conditionalPanel(
                 condition = "input.PlotType == 'Histogram'",
                 actionButton(inputId = "calib",
                              label = "Calibrate"),
                 br(),
                 br(),
                textInput(inputId = "xvar",
                          label = NULL,
                          placeholder = "X Axis Name"),
                    splitLayout(
                textInput(inputId = "y1",
                          label = NULL,
                          placeholder = "Y1 Value" ),
                textInput(inputId = "y2",
                          label = NULL,
                          placeholder = "Y2 Value" ),
                textInput(inputId = "x1",
                          label = NULL,
                          placeholder = "X1 Value" ),
                textInput(inputId = "x2",
                          label = NULL,
                          placeholder = "X2 Value" )
                    ),
                textInput(inputId = "nsamp",
                          placeholder = "Known sample size",
                          label = NULL),

                       ),
               conditionalPanel(
                 condition = "input.PlotType == 'Mean/error'",
                 actionButton(inputId = "calib",
                              label = "Calibrate"),
                 br(),
                 br(),
                 textInput(inputId = "xvar",
                           label = NULL,
                           placeholder = "X Axis Name"),
                 splitLayout(
                 textInput(inputId = "y1",
                           label = NULL,
                           placeholder = "Y1 Value" ),
                 textInput(inputId = "y2",
                           label = NULL,
                           placeholder = "Y2 Value" )
                 ),
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
                 actionButton(inputId = "calib",
                              label = "Calibrate"),
                 br(),
                 br(),
                 splitLayout(
                   textInput(inputId = "xvar",
                             label = NULL,
                             placeholder = "X Axis Name"),
                   textInput(inputId = "yvar",
                             label = NULL,
                             placeholder = "Y Axis Name")),
                 splitLayout(
                   textInput(inputId = "y1",
                             label = NULL,
                             placeholder = "Y1 Value" ),
                   textInput(inputId = "y2",
                             label = NULL,
                             placeholder = "Y2 Value" ),
                   textInput(inputId = "x1",
                             label = NULL,
                             placeholder = "X1 Value" ),
                   textInput(inputId = "x2",
                             label = NULL,
                             placeholder = "X2 Value" )
                   
                 ),
                           checkboxInput(
                             inputId = "log",
                             label = "Logged values?",
                             value = FALSE),
                             #status = "info"),
                           textInput(inputId = "nsamp",
                                        placeholder = "Known sample size",
                                     label = NULL),

                       ),
               conditionalPanel(
                 condition = "input.PlotType == 'Boxplot'",
                 actionButton(inputId = "calib",
                              label = "Calibrate"),
                 br(),
                 br(),
                 textInput(inputId = "yvar",
                           label = NULL,
                           placeholder = "Y Axis Name"),
                 splitLayout(
                   textInput(inputId = "y1",
                             label = NULL,
                             placeholder = "Y1 Value" ),
                   textInput(inputId = "y2",
                             label = NULL,
                             placeholder = "Y2 Value" )
                 ),
                 checkboxInput(
                   inputId = "log",
                   label = "Logged values?",
                   value = FALSE),
                   #status = "info"),
            )
          ),
          wellPanel(
            strong("Groups:"), class = "myclass3", id = "myid1",
            br(),
            div(class = "buttonagency",
            actionButton(
              inputId = "Add",
              label = "Add", 
              #style = "float",
              #color = "primary"
            ),
            actionButton(
              inputId = "delete",
              label = "Delete", 
              #style = "float",
              #color = "primary"
            ))
              )
          

        ),
mainPanel(
        plotOutput("metaPlot", 
                   click="plot_click",
                   dblclick = "plot_dblclick",
                   hover = "plot_hover",
                   brush = "plot_brush",
                   height = "100%", width = "100%"),
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
  paste0("<font color=\"#ff3333\"><b>",counter$countervalue, "/", counter_total,"</b></font>")
  
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

session$onSessionEnded(function() {
  stopApp()
})


}

shinyApp(ui = ui, server = server)
