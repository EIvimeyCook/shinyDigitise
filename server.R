shinyServer(function(input, output, session) {
  
  ################################################
  # Counter and initial plots
  ################################################
  
  # start counter at 1.
  counter <<- reactiveValues(countervalue = 1)
  
  # when counter value is changed (either conitnue or previous is pressed) then read in new data.
  # if extracted/calibrated data already exists then plot/read in that data.
  # if not then create a new values object that we can store data in.
  # also keeps flip/rotate and image details.
  # Provides info for new plotting as text.
  observeEvent(counter$countervalue, {
    counter$caldat <- paste0(details$cal_dir, details$name[counter$countervalue])
    
    if (file.exists(counter$caldat)) {
      # plot_values <- readRDS(values$caldat)
      values <<- do.call("reactiveValues", readRDS(counter$caldat))
      updateSliderInput(session, "cex", value = values$cex)
      updatePrettyRadioButtons(session, "plot_type", selected = values$plot_type)
      
      if (values$plot_type == "mean_error") {
        updatePrettyRadioButtons(session, "errortype", selected = values$error_type)
      }
      # update
    } else {
      # plot_values <- reactiveValuesToList(values)
      # updateRadioButtons(session, "plot_type", selected=)
      
      values <<- reactiveValues(
        image_name = details$name[counter$countervalue],
        image_file = details$paths[counter$countervalue],
        flip = FALSE,
        rotate = 0,
        calpoints = NULL,
        variable = NULL,
        point_vals = NULL,
        raw_data = NULL,
        # rotate_mode=FALSE,
        cex = input$cex,
        plot_type = input$plot_type,
        pos="right",
        comment=NULL
      )
    }
    
    updateSwitchInput(session, "flip", value = values$flip)
    updateSwitchInput(session, "rotate_mode", value = FALSE)
    values$rotate_mode <- FALSE
    updateSliderInput(session, "rotate", value = FALSE, values$rotate)
    output$rotation <- renderText({
      paste("rotation angle:", values$rotate)
    })
    output$image_name <- renderText({
      values$image_name
    })
    
    updateSwitchInput(
      session = session,
      inputId = "calib_mode",
      value = FALSE
    )
    
    updateSwitchInput(
      session = session,
      inputId = "extract_mode",
      value = FALSE
    )

    output$metaPlot <- renderPlot({
      par(mar = c(0, 0, 0, 0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw, plot_values)
    })
    
    output$orientation_check <- renderImage({
      list(src ="www/tick.jpg", height = 30)
    },deleteFile=FALSE)

    output$calibrate_check <- renderImage({
      list(src ="www/cross.jpg", height = 30)
    },deleteFile=FALSE)

    output$extract_check <- renderImage({
      list(src ="www/cross.jpg", height = 30)
    },deleteFile=FALSE)

    output$info <- renderText({
      "**** NEW PLOT ****
mean_error and boxplots should be vertically orientated.
If they are not then chose flip to correct this.
If figures are wonky, chose rotate."
    })
  })
  
  ################################################
  # Plot type and cex
  ################################################
  
  # record the plot type for the data file - influences clicking etc.
  observe(values$plot_type <<- input$plot_type)
  
  #record cex used (adjusted with slider)
  observe(values$cex <<- input$cex)

  observe(values$pos <<- input$pos)
  
  ################################################
  # Flip
  ################################################
  
  # record whether we flip the image or not
  observeEvent(input$flip, {
    values$flip <<- input$flip
    
    output$metaPlot <- renderPlot({
      par(mar = c(0, 0, 0, 0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw, plot_values)
    })
  })
  
  ################################################
  # Rotate
  ################################################
  
  # rotate the image using a slider. Text + slider gets displayed as you click rotate mode.
  observeEvent(input$rotate_mode, {
    if (input$rotate_mode) {
      
      # if we are in rotate mode - toggle extract mode and calib mode off.
      updateSwitchInput(
        session = session,
        inputId = "calib_mode",
        value = FALSE
      )
      updateSwitchInput(
        session = session,
        inputId = "extract_mode",
        value = FALSE
      )
      
      output$info <- renderText({
        "**** ROTATE ****\nUse the slider to change the rotation angle\n"
      })
      show("togslide")
    } else {
      hide("togslide")
      output$info <- renderText({
        " "
      })
    }
    
    output$metaPlot <- renderPlot({
      par(mar = c(0, 0, 0, 0))
      values$rotate_mode <- input$rotate_mode
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw, plot_values)
    })
  })
  
  observeEvent(input$rotate, {
    if (input$rotate_mode) {
      values$rotate <<- input$rotate
      output$rotation <- renderText({
        paste("rotation angle:", values$rotate)
      })
    }
  })
  
  
  ################################################
  # Calibrate
  ################################################
  
  # create empty clikc counter for plotclicks.
  clickcounter <- reactiveValues(clickcount = 0)
  
  # create a container for calibration points.
  calpoints <- reactiveValues(x = NULL, y = NULL)

  # if calib mode button pressed
  observeEvent(input$calib_mode, {

    # resent click counter
    clickcounter$clickcount <- 0
    
    # show the calib_data object (where we enter names and values for axis).
    # shinyjs::toggle(id = "calib_data")
    
    # clears any previously entered text
    shinyjs::reset("y_var_input")
    shinyjs::reset("x_var_input")
    shinyjs::reset("y_coord_input")
    shinyjs::reset("y_coord_input")

    # toggle extract mode and rotate mode off.
    updateSwitchInput(
      session = session,
      inputId = "extract_mode",
      value = FALSE
    )
    updateSwitchInput(
      session = session,
      inputId = "rotate_mode",
      value = FALSE
    )

    if (input$calib_mode) {
     # if in calib mode 

      ### show relevant input boxes
      if(input$plot_type %in% c("mean_error","boxplot","scatterplot")){
        show("y_var_input")
      }
      
      if(input$plot_type %in% c("histogram","scatterplot")){
        show("x_var_input")
        show("x_coord_input")
      }
      
      show("y_coord_input")
      
      # delete all previous data
      values$calpoints <<- NULL
      values$variable <<- NULL
      values$point_vals <<- NULL
      calpoints$x <- NULL
      calpoints$y <- NULL

      # plot-specific calibration help
      if (input$plot_type == "scatterplot" | input$plot_type == "histogram") {
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
      } else {
        output$info <- renderText({
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
      
      # Also shows click locations (probably don't need this in future).
      # and gives calibration placement help.  
      output$clickinfo <- renderText({
        paste0("x = ", calpoints$x, ", y = ", calpoints$y, "\n")
      })
    } else {
    ## what happens when calibrate mode is switched off

      # hide variable and coord inputs      
      hide("y_var_input")
      hide("x_var_input")
      hide("y_coord_input")
      hide("x_coord_input")

      if( is.null(values$calpoints) || is.null(values$variable) || is.null(values$point_vals) ){
        output$calibrate_check <- renderImage({
          list(src ="www/cross.jpg", height = 30)
        },deleteFile=FALSE)
      }else{
        output$calibrate_check <- renderImage({
          list(src ="www/tick.jpg", height = 30)
        },deleteFile=FALSE)
      }

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
    
  # when the plot is clicked in calibrate mode
  observeEvent(input$plot_click2, {
    if (input$calib_mode) {

      # increase clickcounter (becomes clicktot object).
      clicktot <- clickcounter$clickcount + 1

       # if the plot type is sp or hist and the clicktotal is four or less, then store the calibration points and update the clickcounter. 
      if (input$plot_type %in% c("scatterplot", "histogram")) {
        if (clicktot <= 4) {
          clickcounter$clickcount <- clicktot
          calpoints$x <- c(calpoints$x, input$plot_click2$x)
          calpoints$y <- c(calpoints$y, input$plot_click2$y)
        } 
      } else {
      # if the plot type is me or bp and the clicktotal is 2 or less, store the calibration points and update the clickcounter.
        if (clicktot <= 2) {
          clickcounter$clickcount <- clicktot
          calpoints$x <- c(calpoints$x, input$plot_click2$x)
          calpoints$y <- c(calpoints$y, input$plot_click2$y)
        } 
      }
      
      # Then convert these to a dataframe and plot
      values$calpoints <<- as.data.frame(reactiveValuesToList(calpoints))
      output$metaPlot <- renderPlot({
        par(mar = c(0, 0, 0, 0))
        plot_values <- reactiveValuesToList(values)
        do.call(internal_redraw, plot_values)
      })
    }
  })

  ################################################
  # Calibrate labeling
  ################################################
  
  # take the inputs from the y axis/y1 and y2 and add to the values object
  # other plots to be finished

  observeEvent(c(input$y_var,input$x_var), {
    if (input$calib_mode) {
      if(input$plot_type %in% c("mean_error","boxplot")){
        values$variable <<- input$y_var
      }else if(input$plot_type %in% c("histogram")){
        values$variable <<- input$x_var
      }else{
        values$variable <<- c(y=input$y_var,y=input$x_var)
      }
      
      output$metaPlot <- renderPlot({
        par(mar = c(0, 0, 0, 0))
        plot_values <- reactiveValuesToList(values)
        do.call(internal_redraw, plot_values)
      })
    }
  })
  
  observeEvent(c(input$y1, input$y2,input$x1, input$x2), {
    if (input$calib_mode) {
      if(input$plot_type %in% c("mean_error","boxplot")){
        values$point_vals <<- c(input$y1,input$y2)
      }else{
        values$point_vals <<- c(input$y1,input$y2,input$x1, input$x2)
      }
      
      output$metaPlot <- renderPlot({
        par(mar = c(0, 0, 0, 0))
        plot_values <- reactiveValuesToList(values)
        do.call(internal_redraw, plot_values)
      })
    }
  })






  
  ################################################
  # Extraction
  ################################################

    
  # for row count, 
  row_count <- reactiveValues(x = NULL)
  
  # dataframe containing group name etc
  mod_df <- reactiveValues(x = NULL)

  # create empty click counter
  plotcounter <- reactiveValues(plotclicks = NULL)
  
  #container for which rows and cell are selected 
  selected <- reactiveValues(row = NULL, cell=NULL)
  
  #container for for plotting values and
  valpoints <- reactiveValues(x = NULL, y = NULL, id = NULL, n = NULL)
  
  # container for add T/F.
  add_mode <- reactiveValues(add = FALSE)
  

  observeEvent(input$extract_mode, {
  
    # if we are in extract mode
    if (input$extract_mode) {
  
      #toggle calibrate mode and rotate mode off.
      updateSwitchInput(
        session = session,
        inputId = "calib_mode",
        value = FALSE
      )
      updateSwitchInput(
        session = session,
        inputId = "rotate_mode",
        value = FALSE
      )
      
      #show the group_data ovject (the table for clicking/groups and sample sizes)
      show("group_data")

      # show the error type select input (ofr mean_error).
      if (input$plot_type == "mean_error") {
        show("error_type_select")
      }
    # show help text
      output$info <- renderText({
        "**** EXTRACTING DATA ****
1. Group names and sample size should be entered into the table on the sidebar before points are added.
2. To add points to a group, first click the group on the sidebar then click 'Add Points'.
3. To delete a group, click on the desired group in the table on the sidebar then press 'Delete Group'."
      })


      ################################################
      # Group Name and Sample Size Table
      ################################################
      
      if (is.null(values$raw_data)) {
      # if values raw data is null/empty then create new data to show in the table.
        basic <- tibble(
          Group_Name = NA,
          Sample_Size = NA
        )
        ## ??? what does this do
        mod_df$x <- basic[-nrow(basic),]
        ## ???

        row_count$x <- 0

      } else {
      # otherwise read in the data that already exists from the raw data.
        raw_dat <- as.data.frame(values$raw_data)
        raw_dat_sum <- aggregate(n ~ id, raw_dat, unique)
        names(raw_dat_sum) <- c("Group_Name", "Sample_Size")
        mod_df$x <- raw_dat_sum
        row_count$x <- nrow(raw_dat_sum)
        valpoints <- values$raw_data
      }

      # this is then rendered in a DT table.
      output$group_table <- DT::renderDT({
        DT::datatable(
          mod_df$x,
          editable = list(target = "cell"),
          options = list(lengthChange = TRUE, dom = "t")
        )
      })

    }else{
    
    ## hide 
      hide("group_data")
      hide("error_type_select")
      
      if( is.null(values$raw_data) ){
        output$extract_check <- renderImage({
          list(src ="www/cross.jpg", height = 30)
        },deleteFile=FALSE)
      }else{
        output$extract_check <- renderImage({
          list(src ="www/tick.jpg", height = 30)
        },deleteFile=FALSE)
      }

    # help text to show when extract mode is false.
      output$info <- renderText({
        ""
      })
    }
  
  })
  
  
  ################################################
  #  Adding points
  ################################################
  
  
  # when you click add group a popup appears which asks you to add group and sample size.
  # this is then added onto the raw data.
  # the row count increases and another row is then added after.
  observeEvent(input$add_group, {
    shinyalert(html = TRUE, text = tagList(
      textInput("group", "Group Name", ""),
      numericInput("sample_size", "Sample Size", ""),
    ))
    
    row_count$x <- row_count$x + 1
    mod_df$x <- mod_df$x %>%
      dplyr::bind_rows(
        dplyr::tibble(
          Group_Name = NA, 
          Sample_Size = NA
        )
      )
  })

  # what happens when you click a cell on the group table. 
  # Useful for deleting groups and labeling points. Highlights the cell.
  observeEvent(input$group_table_cell_clicked, {
    selected$cell <- input$group_table_cell_clicked$value
  })
  
  # what happens when you click on a cell/row on the group table. 
  # Useful for deleting groups and labeling points. Highlights the row.
  observeEvent(input$group_table_rows_selected, {
    selected$row <- input$group_table_rows_selected
    # print(selected$row)
  })
  

  observeEvent(input$click_group, {
    # plotcounter becomes 0.
    plotcounter$plotclicks <- 0
    
    # add mode becomes T
    add_mode$add <- TRUE
    
    # if you click group without any row selected - return an error.
    if(is.null(selected$cell)){
      shinyalert(
        title = "Select a group to plot",
        text = "No group has been selected",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    } else {
      
      if (add_mode$add) {
        # similar to above, if you click add points and add mode is T and there is data present for that selected cell then remove this selected group from the plot and plotcounter becomes zero after replotting.

        # if(as.data.frame(reactiveValuesToList(mod_df$x[selected$row,"Group_Name"]) %in% values$raw_data$id)){
        if(length(stringr::str_detect(values$raw_data$id, as.character(selected$cell))) != 0){
          values$raw_data <<- as.data.frame(reactiveValuesToList(valpoints))
          values$raw_data <<- values$raw_data %>%
            filter(!stringr::str_detect(id, as.character(selected$cell)))
          valpoints$x <- values$raw_data$x
          valpoints$y <- values$raw_data$y
          valpoints$id <- values$raw_data$id
          valpoints$n <- values$raw_data$n
        }
      }
      ## ?? this seems problematic
      values$raw_data <<- as.data.frame(reactiveValuesToList(valpoints))
    }
  })
    
  # when you click on the plot and if add mode is true, increase plotcount by 1.
  # if the graph is mean_error and the plotcount is 2 or less then add data to valpoints object.
  # if its boxplot and five or less, then do the same.
  # otherwise add mode becomes false and it switches off.
  # valpoints is then converted to a dataframe and plotted.
  # location of x and y valpoints are given in text.
  #  when you click to add points

  observeEvent(input$plot_click2, {
    if (add_mode$add) {
      plotcounter$plotclicks <- plotcounter$plotclicks + 1
      
      dat_mod <- as.data.frame(reactiveValuesToList(mod_df))
      print(dat_mod)

      if (input$plot_type == "mean_error") {
        if (plotcounter$plotclicks <= 2) {
          # isolate({
            valpoints$x <- c(valpoints$x, input$plot_click2$x)
            valpoints$y <- c(valpoints$y, input$plot_click2$y)
            valpoints$id <- c(valpoints$id, dat_mod[selected$row, 1])
            valpoints$n <- c(valpoints$n, dat_mod[selected$row, 2])
          # })
          print(valpoints$x)
          print(valpoints$y)
        } else {
          add_mode$add <- FALSE
        }
      }
      if (input$plot_type == "boxplot") {
        if (plotcounter$plotclicks <= 5) {
          # isolate({
            valpoints$x <- c(valpoints$x, input$plot_click2$x)
            valpoints$y <- c(valpoints$y, input$plot_click2$y)
            valpoints$id <- c(valpoints$id, dat_mod[selected$row, 1])
            valpoints$n <- c(valpoints$n, dat_mod[selected$row, 2])
          # })
        } else {
          add_mode$add <- FALSE 
        }
      }
      # if(any(duplicated(valpoints$x))){
      #   dat_mod <- as.data.frame(reactiveValuesToList(mod_df))
      #   valpoints$x <- valpoints$x[-2]
      #   valpoints$y <- valpoints$y[-2]
      #   valpoints$id <- valpoints$id[-2]
      #   valpoints$n <- valpoints$n[-2]
        
      #   plotcounter$plotclicks <- plotcounter$plotclicks - 1
        
      # } else{ 
        values$raw_data <<- as.data.frame(reactiveValuesToList(valpoints))
      # }

      output$clickinfo <- renderText({
        # print(any(duplicated(valpoints$x)))
        paste0("x = ", valpoints$x, ", y = ", valpoints$y, "\n")
      })

       output$metaPlot <- renderPlot({
        par(mar = c(0, 0, 0, 0))
        plot_values <- reactiveValuesToList(values)
        print(plot_values$raw_data)
        do.call(internal_redraw, plot_values)
      })   
    }
  })


  ################################################
  # Deleting extracted points
  ################################################
  
    # what happens when you press the delete group.
  # if no cell is selected to delete, then show a modal alert.
  # however if one is, then search the data fro that cell and remove it from the df.
  # this will also cause the plot and raw data to update and remove anything with this group.
  observeEvent(input$del_group, {
    if (input$del_group) {
      if(is.null(selected$cell)){
        shinyalert(
          title = "Select a group to delete",
          text = "No group has been selected",
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      } else{
        row_count$x <- row_count$x - 1
        remove_dat <- which(mod_df$x == selected$cell)[1]
        mod_df$x <- mod_df$x[-remove_dat, ]
        values$raw_data <<- as.data.frame(reactiveValuesToList(valpoints))
        values$raw_data <<- values$raw_data %>%
          filter(!stringr::str_detect(id, as.character(selected$cell)))
        
        valpoints$x <- values$raw_data$x
        valpoints$y <- values$raw_data$y
        valpoints$id <- values$raw_data$id
        valpoints$n <- values$raw_data$n
      }}
    output$metaPlot <- renderPlot({
      par(mar = c(0, 0, 0, 0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw, plot_values)
    })
  })
  
  # this is necessary for DT to work.
  proxy <- DT::dataTableProxy("group_table")
  
  # edit the data table with data from the modal poopup.
  observeEvent(input$group, {
    mod_df$x[row_count$x,1] <<- input$group
  })
  
  # edit the data table with data from the modal popup.
  observeEvent(input$sample_size, {
    mod_df$x[row_count$x,2] <<- input$sample_size
  })
  
  
  ################################################
  # Comments
  ################################################
  
  # record comments
  observeEvent(input$comment, {
    values$comment <<- input$comment
  }) 
  
  ################################################
  # Previous/next buttons
  ################################################
  
  # when next is pressed up the counter and check that its within total.
  # if data exists - convert that data right into plot values.
  # save all calibrated and extracted data.
  # switch calib and etxract mode to false.
  # modal box pops up when you've finished.
  
  observeEvent(input$continue, {
    plot_values <- reactiveValuesToList(values)
    saveRDS(plot_values, paste0(details$cal_dir, details$name[counter$countervalue]))
    
    cv <- counter$countervalue + 1
    
    if (cv > counter_total) {
      counter$countervalue <- counter_total
      shinyalert(
        title = "Congratulations!",
        text = "You've finished digitising!",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    } else {
      counter$countervalue <- cv
    }
  })
  
  # when next is pressed up the counter and check that its above 0
  observeEvent(input$previous, {
    cv <- counter$countervalue - 1
    
    if (cv == 0) {
      counter$countervalue <- 1
      # clickcounter$clickcount <- 0
    } else {
      counter$countervalue <- cv
    }
  })
  
  #output the progress text
  output$progress <- renderText({
    paste0("<font color=\"#ff3333\"><b>", counter$countervalue, "/", counter_total, "</b></font>")
  })
  
  
  
  
  ################################################
  # What happens when you quit
  ################################################
  
  #the app stops when you exit - not sure what this does.
  session$onSessionEnded(function() {
    stopApp()
  })
})