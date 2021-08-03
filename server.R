
shinyServer(function(input, output, session) {
  
  ################################################
  #    Counter and previous/next buttons
  ################################################
  # start counter at 1
  counter <- reactiveValues(countervalue = 1)
  values <- reactiveValues()
  

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
  
  output$progress <- renderText({
    paste0("<font color=\"#ff3333\"><b>",counter$countervalue, "/", counter_total,"</b></font>")
    
  }) 


  ################################################
  #    when counter changes
  ################################################
  
## when counter changes
## values - save if past certain point? some raw_data?
## values - empty
## check caldat
## if no caldat fill in some stuff
##  if caldat load caldat and fill in some stuff
    # updateRadioButtons(session, "plot_type", selected=)
    # updateRadioButtons(session, "plot_type", selected=)
## #updateSelectInput


  observeEvent(counter$countervalue, {
    counter$caldat <- paste0(details$cal_dir,details$name[counter$countervalue]) 
    if(file.exists(counter$caldat)){
      # plot_values <- readRDS(values$caldat)
      values <<- do.call("reactiveValues",readRDS(counter$caldat))
    }else{
      # plot_values <- reactiveValuesToList(values)
          # updateRadioButtons(session, "plot_type", selected=)

      values <<- reactiveValues(
        image_name = details$name[counter$countervalue],
        image_file = details$paths[counter$countervalue],
        flip = FALSE
        # cex = input$cex,
        # plot_type = input$plot_type
      )
    }

    output$image_name <- renderText({values$image_name})

    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })

        output$info <- renderText({
      "**** NEW PLOT ****
mean_error and boxplots should be vertically orientated. 
If they are not then chose flip to correct this. 
If figures are wonky, chose rotate."
})
  })


  ################################################
  #   Flip
  ################################################
  observeEvent(input$flip, {
    values$flip <<- input$flip

output$info <- renderText({
      "**** ROTATE ****
Click left hand then right hand side of x axis\n"
})

    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })

  })

  ################################################
  #   Rotate
  ################################################
  
  # plot_click_slow <- debounce(reactive(input$plot_click), 300)

  observeEvent(input$rotate, {
      
    # x.dist <- rot_angle$x[2] - rot_angle$x[1]
    # y.dist <- rot_angle$y[2] - rot_angle$y[1]
    
    # f <- atan2(y.dist, x.dist) * 180/pi
    # values$rotate <<- rotate + f


  })

output$rotation <- renderText({

  
  })
  

  ################################################
  #   Plot type
  ################################################

  observe( values$plot_type <- input$plot_type )


  ################################################
  #   Calibrate
  ################################################


  ################################################
  #   Digitisation
  ################################################

  observe( values$cex <- input$cex )


   

  # output$info <- renderText({
  #   xy_str <- function(e) {
  #     if(is.null(e)) return("NULL\n")
  #     paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
  #   }
  #   xy_range_str <- function(e) {
  #     if(is.null(e)) return("NULL\n")
  #     paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
  #            " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
  #   }
    
  #   paste0(
  #     "click: ", xy_str(input$plot_click),
  #     "dblclick: ", xy_str(input$plot_dblclick),
  #     "hover: ", xy_str(input$plot_hover),
  #     "brush: ", xy_range_str(input$plot_brush)
  #   )
  # })


  ################################################
  #   What happens when you quit
  ################################################

  session$onSessionEnded(function() {
    stopApp()
  })


})