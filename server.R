
shinyServer(function(input, output, session) {
  ################################################
  #    Counter and previous/next buttons
  ################################################
  # start counter at 1
  counter <- reactiveValues(countervalue = 1)
  values <- reactiveValues()


  # when next is pressed up the counter and check that its within total
  observeEvent(input$continue, {
    plot_values <- reactiveValuesToList(values)
    saveRDS(plot_values, paste0(details$cal_dir,details$name[counter$countervalue]))
    updateSwitchInput(
      session = session,
      inputId = "calib_mode",
      value = FALSE
    )
    clickcounter$clickcount <- 1
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
      clickcounter$clickcount <- 0
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

      updateSliderInput(session, "cex", value=values$cex)

      updatePrettyRadioButtons(session, "plot_type", selected=values$plot_type)

      #  updateTextInput(session,inputId = "yvar",value=values$variable["y"])
      #  updateTextInput(session,inputId = "xvar",vvalue=alues$variable["x"])
      # updateTextInput(session,inputId = "x1",value=values$point_vals["x1"])
      #  updateTextInput(session,inputId = "x2",value=values$point_vals["x2"])
      # updateTextInput(session,inputId = "y1",value=values$point_vals["y1"])
      #  updateTextInput(session,inputId = "y2",value=values$point_vals["y2"])

      if(values$plot_type=="mean_error")       updatePrettyRadioButtons(session, "errortype", selected=values$error_type)
      # update

    }else{
      # plot_values <- reactiveValuesToList(values)
      # updateRadioButtons(session, "plot_type", selected=)

      values <<- reactiveValues(
        image_name = details$name[counter$countervalue],
        image_file = details$paths[counter$countervalue],
        flip = FALSE,
        rotate=0,
        calpoints=NULL,
        variable=NULL,
        point_vals=NULL
        # rotate_mode=FALSE,
        # cex = input$cex,
        # plot_type = input$plot_type
      )
    }

    updateSwitchInput(session,"flip",value=values$flip)
    updateSwitchInput(session,"rotate_mode",value=FALSE)
    values$rotate_mode <- FALSE

    updateSliderInput(session,"rotate",value=FALSE,values$rotate)

    output$rotation <- renderText({paste("rotation angle:", values$rotate)})

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

dat<-readRDS("Image/caldat/Screenshot 2021-12-13 at 11.57.16")
dat

  ################################################
  #   Flip
  ################################################
  observeEvent(input$flip, {
    values$flip <<- input$flip

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

  observeEvent(input$rotate_mode, {

    if(input$rotate_mode){
      output$info <- renderText({
        "**** ROTATE ****\nUse the slider to change the rotation angle\n"
      })
      show("togslide")
    }else{
      hide("togslide")
      output$info <- renderText({ " "})
    }

    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      values$rotate_mode <- input$rotate_mode
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })

  })

  observeEvent(input$rotate, {
    if(input$rotate_mode){
      values$rotate <<- input$rotate
      output$rotation <- renderText({paste("rotation angle:", values$rotate)})
    }
  })

  ################################################
  #   Plot type
  ################################################

  observe( values$plot_type <<- input$plot_type )


  ################################################
  #   Calibrate
  ################################################

  #create mepty click counter
  clickcounter <- reactiveValues(clickcount = 0)

  #add to calpoints and scrtore click locations
  observeEvent(input$calib_mode, {

    if (input$calib_mode) {

    calpoints <- reactiveValues(x = NULL, y = NULL)
    
    clickcounter$clickcount <- 0
    values$calpoints <<- NULL

    observeEvent(input$plot_click2,{
      if (input$calib_mode) {
        clicktot <- clickcounter$clickcount + 1
        if(input$plot_type %in% c("scatterplot", "histogram")){
          if (clicktot <= 4) {
            clickcounter$clickcount <- clicktot
            calpoints$x <- c(calpoints$x, input$plot_click2$x)
            calpoints$y <- c(calpoints$y, input$plot_click2$y)
          } else {
            clickcounter$clickcount <- 0
          }
        }else{
          if (clicktot <= 2) {
            clickcounter$clickcount <- clicktot
            calpoints$x <- c(calpoints$x, input$plot_click2$x)
            calpoints$y <- c(calpoints$y, input$plot_click2$y)
          }
          
          else {
            clickcounter$clickcount <- 0
          }
        }

        values$calpoints <<- as.data.frame(reactiveValuesToList(calpoints))
      }
    })

    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })
    #output$metaPlot <- renderPlot({
      #par(mar=c(0,0,0,0))
      #plot_values <- reactiveValuesToList(values)
     # do.call(internal_redraw,plot_values)
    #})


    #add click help

      if(input$plot_type == "scatterplot"|input$plot_type == "histogram"){
        output$info <- renderText({
          "   Calibrate ---> Click on known values on axes in this order:
  |
  2
  |
  |
  1
  |___3___________4_____
  "
        })
      }
      else{ output$info <- renderText({
          "   Calibrate ---> Click on known values on axes in this order:
  |
  2
  |
  |
  1
  |_____________________
  "
        })
      }

      #add click locationsin text
      output$clickinfo <- renderText({
        paste0("x = ", calpoints$x, ", y = ", calpoints$y, "\n")
      })

    }
    else {
      output$clickinfo <- renderText({
        " "
      })
      output$info <- renderText({
        "**** NEW PLOT ****
mean_error and boxplots should be vertically orientated.
If they are not then chose flip to correct this.
If figures are wonky, chose rotate."
      })
    }


  })
  

  # observeEvent(input$plot_click2, {

  #   if (input$calib_mode) {
  #     clicktot <- clickcounter$clickcount + 1
  #     if(input$plot_type == "scatterplot"|input$plot_type == "histogram"){
  #       if (clicktot >= 4) {
  #         # updateSwitchInput(
  #         #   session = session,
  #         #   inputId = "calib_mode",
  #         #   value = FALSE
  #         # )
  #         clickcounter$clickcount <- 0
  #       } else {
  #         clickcounter$clickcount <- clicktot
  #       }
  #     }else {
  #       if (clicktot >= 2) {
  #       # updateSwitchInput(
  #       #   session = session,
  #       #   inputId = "calib_mode",
  #       #   value = FALSE
  #       # )
  #       clickcounter$clickcount <- 0
  #     } else {
  #       clickcounter$clickcount <- clicktot
  #     }}
  #   }
  # })

  ################################################
  #    calib data
  ################################################

  observeEvent(input$yvar, {
    
    req(input$yvar)
    
    var_name <- reactiveValues(yvar = NULL)
    var_name$yvar <- input$yvar
    values$variable <<- var_name$yvar
    
    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })
  })
  
  observeEvent(c(input$y1, input$y2, input$x1, input$x2), {
    
    var_num <- reactiveValues(y1 = NULL, y2 = NULL, x1 = NULL, x2 = NULL)
    var_num$y1 <- as.numeric(input$y1)
    var_num$y1 <- as.numeric(input$y2)
    var_num$x1 <- as.numeric(input$x1)
    var_num$x2 <- as.numeric(input$x2)
    values$point_vals <<- as.vector(reactiveValuesToList(var_num))

    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })
  })
  
  observeEvent(input$xvar, {
    
    var_name2 <- reactiveValues(xvar = NULL)
    var_name2$xvar <- input$xvar
    values$variable <<- var_name2$xvar
    
    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })
  })
  
  

  ################################################
  #   Show hide panels
  ################################################

  observeEvent(input$calib_mode, {
    shinyjs::toggle(id= "calib_data")
  })

  ################################################
  #   Digitisation
  ################################################

  observe( values$cex <<- input$cex )


  ###############################################
  # Group Table
  ##############################################
  mod_df <- reactiveValues(x = basic)

  output$group_table<- DT::renderDT({
    DT::datatable(
      isolate(mod_df$x),
      editable = list(target = 'column', disable = list(columns = 3)),
      options = list(lengthChange = TRUE, dom = 't')) %>%
      formatStyle("Point_Colour", backgroundColor = styleEqual(mod_df$x$Point_Colour, mod_df$x$Point_Colour)) %>%
      formatStyle(columns = c(1:NCOL(mod_df$x)))
  })

  observe({
    updatePickerInput(session = session, inputId = "delete_row",
                      choices = 1:nrow(mod_df$x))
  })


  observeEvent(input$add, {
    mod_df$x <- mod_df$x %>%
      dplyr::bind_rows(
        dplyr::tibble(Group_Name = "Insert group name",
                      Sample_Size = "Insert sample size",
                      Point_Colour = sample(col, 1, F))
      )

  })


  observeEvent(input$delete, {

    mod_df$x <- mod_df$x[-as.integer(input$delete_row), ]

  })

  proxy <- DT::dataTableProxy('group_table')

  observe({
    DT::replaceData(proxy, mod_df$x)

  })


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
