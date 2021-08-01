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


dir_details <- function(dir){
  detail_list <- list()
# file_pattern <- "[.][pjt][dnip][fpg]*$"
  file_pattern <- "(?i)[.][pjt][dnip][efpg]*$"
  detail_list$images <- list.files(dir, pattern = file_pattern)
  detail_list$name <- gsub(file_pattern, "", detail_list$images)
  detail_list$paths <- paste0(dir, detail_list$images)
  detail_list$cal_dir <- paste0(dir, "caldat/")
  detail_list$calibrations <- list.files(paste0(dir, "caldat/"))
  detail_list$doneCalFiles <- if(length(detail_list$calibrations)==0) { 
    vector(mode="character") 
  } else{ 
    paste0(detail_list$cal_dir, detail_list$calibrations) 
  }

  return(detail_list)
}




if( (substring(dir, nchar(dir)) == "/") == FALSE){
    dir <- paste0(dir, "/")
  }
  setup_calibration_dir(dir)
#  done_details <- dir_details(dir)
# details <- get_notDone_file_details(dir)

details <- dir_details(dir)

counter_total <- length(details$paths)
# filename(details$paths[1])

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
     column(width = 4,
      titlePanel(title=div(img(src="shiny.jpg", height = 60),"shinyDigitise"), windowTitle = "shinyDigitise"),
      ),
      # column(width = 1,
      #   br(),
      # ),
      column(width = 8, 
        br(),
        div(style="display: inline-block;vertical-align:top; width: 10%; font-size:x-large;",
          p(htmlOutput("progress", inline=TRUE)),
        ),
        # div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
        div(style="display: inline-block;vertical-align:top; width: 20%; ",
          actionButton(
            inputId = "Previous",
            label = "Previous", 
            style='padding:4px'
            #style = "float",
            #color = "primary",
          ),  
          actionButton(
            inputId = "Next",
            label = "Next", 
            style='padding:4px'
            #style = "float",
            #color = "primary"
          )
        ),
        # div(style="display: inline-block;vertical-align:top; width: 5%;",HTML("<br>")),

      # ),
      # column(3,
      #   br(), 
        div(style="display: inline-block;vertical-align:top; width: 20% ",strong("Show processed images:")),
        div(style="display: inline-block;vertical-align:top; width: 5%; ",checkboxInput(
          inputId = "ShowOnlyNew",
          label = NULL,
          value = F,
          #label_on = "Yes", 
          #icon_on = icon("check"),
          #status_on = "info",
          #status_off = "warning", 
          #label_off = "No",
          #icon_off = icon("remove")
        )),
        
      # ), 
      # # column(1, align="left",
      # #   #class = "myclass1", id = "myid1",
      # #   br(),
        
      # # ),
      # column(3, 
      #   br(), 
        # div(style="display: inline-block;vertical-align:top; width: 10%;",HTML("<br>")),

        div(style="display: inline-block;vertical-align:top; width: 10%;",strong("point size:")),
        div(style="display: inline-block;vertical-align:top;  width: 20%;",
          sliderInput(
          inputId = "cex",
          label = NULL, 
          value = 1,
          min = 0.1,
          max = 3, 
          ticks  = FALSE))
      )
      # column(2, 
      #   br(),
        
      # )
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
                inputId = "plot_type",
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
                 condition = "input.plot_type == 'Histogram'",
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
                 condition = "input.plot_type == 'Mean/error'",
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
                 condition = "input.plot_type == 'Scatterplot'",
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
                 condition = "input.plot_type == 'Boxplot'",
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
        verbatimTextOutput("image_name"),
        plotOutput("metaPlot", 
                   click="plot_click",
                   dblclick = "plot_dblclick",
                   hover = "plot_hover",
                   brush = "plot_brush",
                   height = "600px", width = "100%"),
        verbatimTextOutput("info")
    )
      )
    )

server <- function(input, output, session) {
  
  ################################################
  #    Counter and previous/next buttons
  ################################################
  
  # start counter at 1
  counter <- reactiveValues(countervalue = 1)

  # when next is pressed up the counter and check that its within total
  observeEvent(input$Next, {
    cv <- counter$countervalue + 1
    if(cv > counter_total) {
      counter$countervalue <- counter_total
    }else{
      counter$countervalue <- cv
    }
  })

  # when next is pressed up the counter and check that its above 0
  observeEvent(input$Previous, {
    cv <- counter$countervalue - 1
    if(cv == 0) {
      counter$countervalue <- 1
    }else{
      counter$countervalue <- cv
    }
  })
  
  observeEvent(counter$countervalue, {
    counter$caldat <- paste0(details$cal_dir,details$name[counter$countervalue]) 
    # if(file.exists(counter$caldat)){
    #   # plot_values <- readRDS(values$caldat)
    #   values <- do.call("reactiveValues",readRDS(counter$caldat))
    # }else{
    #   # plot_values <- reactiveValuesToList(values)
    #       # updateRadioButtons(session, "plot_type", selected=)

    #   values <- reactiveValues(
    #     image_name = details$name[counter$countervalue],
    #     image_file = details$paths[counter$countervalue]
    #     # cex = input$cex,
    #     # plot_type = input$plot_type
    #   )
    # }
  })

## when counter changes
## values - save if past certain point? some raw_data?
## values - empty
## check caldat
## if no caldat fill in some stuff
##  if caldat load caldat and fill in some stuff
    # updateRadioButtons(session, "plot_type", selected=)
    # updateRadioButtons(session, "plot_type", selected=)
## #updateSelectInput




  ################################################
  #   Digitisation and plotting
  ################################################

  # work out image 
  # observe( values$image_name <- details$name[counter$countervalue] )

  # observe({
  #   if(file.exists(paste0(details$cal_dir,details$name[counter$countervalue]))){

  #     values <- do.call("reactiveValues",readRDS(paste0(details$cal_dir,details$name[counter$countervalue]))))
  #   }else{
  #     values$image_name <- details$name[counter$countervalue] 
  #     values$image_file <- details$paths[counter$countervalue] 
  #     values$cex <- input$cex 
  #     values$caldat <- paste0(details$cal_dir,values$image_name) 
  #     values$plot_type <- input$plot_type 
  #   }

  # })

  values <- reactiveValues()

  observe( values$image_name <- details$name[counter$countervalue] )
  observe( values$image_file <- details$paths[counter$countervalue] )
  observe( values$cex <- input$cex )
  observe( values$plot_type <- input$plot_type )


  output$image_name <- renderText({values$image_name})

  output$metaPlot <- renderPlot({
    # image <- image_read(values$image_file)
    # par(mar=c(0,0,0,0))
    # plot(image)
    par(mar=c(0,0,0,0))

    if(file.exists(counter$caldat)){
      plot_values <- readRDS(counter$caldat)
    }else{
      plot_values <- reactiveValuesToList(values)
    }
    # plot_values <- reactiveValuesToList(values)

    do.call(internal_redraw,plot_values)

    
  })



  output$progress <- renderText({
    paste0("<font color=\"#ff3333\"><b>",counter$countervalue, "/", counter_total,"</b></font>")
    
  }) 


# output$metaPlot <- renderImage({
#   if(input$ShowOnlyNew == "yes"){
#     not_done_paths[counter]
#   }
#   else{
#     all_paths[counter]
#   }
#   })


   

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

names(readRDS(paste0(details$cal_dir,details$name[1])))

shinyApp(ui = ui, server = server)