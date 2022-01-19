shinyServer(function(input, output, session) {
################################################
  # Counter and previous/next buttons
################################################
  
  # start counter at 1.
  counter <<- reactiveValues(countervalue = 1)
  
  #create reactive container for plotting and data.
  values <<- reactiveValues()

  # when next is pressed up the counter and check that its within total.
  # if data exists - convert that data right into plot values.
  # save all calibrated and extracted data.
  # switch calib and etxract mode to false.
  # modal box pops up when you've finished.
  
  observeEvent(input$continue, {
    plot_values <- reactiveValuesToList(values)
    saveRDS(plot_values, paste0(details$cal_dir, details$name[counter$countervalue]))

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

    # clickcounter$clickcount <- 0

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
  # Counter and initial plots
################################################

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
        raw_data = NULL
        # rotate_mode=FALSE,
        # cex = input$cex,
        # plot_type = input$plot_type
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

    output$metaPlot <- renderPlot({
      par(mar = c(0, 0, 0, 0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw, plot_values)
    })

    output$info <- renderText({
      "**** NEW PLOT ****
mean_error and boxplots should be vertically orientated.
If they are not then chose flip to correct this.
If figures are wonky, chose rotate."
    })
  })

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
  # Plot type and cex
################################################

  # record the plot type for the data file - influences clicking etc.
  observe(values$plot_type <<- input$plot_type)
  
  #record cex used (adjusted with slider)
  observe(values$cex <<- input$cex)


################################################
  # Calibrate plotting
################################################

  # create empty clikc counter for plotclicks.
  clickcounter <<- reactiveValues(clickcount = 0)

  # when in calibrate mode - create a container for calibration points.
  # the clickcounter is creater which starts at 0.
  # values$calpoints starts as NULL.
  
  # when the plot is clicked - increase clickcounter (becomes clicktot object).
  # if the plot type is sp or hist and the clicktotal is four or less then store the calibration points.
  # if it is 2 or less (and therefore bp/me) also store the calibration points.
  # Lastly, convert these to a dataframe for plotting.
  
  # Also shows click locations (probably don't need this in future).
  # and gives calibration placement help.
  observeEvent(input$calib_mode, {
    if (input$calib_mode) {
      calpoints <- reactiveValues(x = NULL, y = NULL)

      clickcounter$clickcount <- 0
      values$calpoints <<- NULL

      observeEvent(input$plot_click2, {
        if (input$calib_mode) {
          clicktot <- clickcounter$clickcount + 1
          if (input$plot_type %in% c("scatterplot", "histogram")) {
            if (clicktot <= 4) {
              clickcounter$clickcount <- clicktot
              calpoints$x <- c(calpoints$x, input$plot_click2$x)
              calpoints$y <- c(calpoints$y, input$plot_click2$y)
            } else {
              # clickcounter$clickcount <- 0
            }
          } else {
            if (clicktot <= 2) {
              clickcounter$clickcount <- clicktot
              calpoints$x <- c(calpoints$x, input$plot_click2$x)
              calpoints$y <- c(calpoints$y, input$plot_click2$y)
            } else {
              # clickcounter$clickcount <- 0
            }
          }

          values$calpoints <<- as.data.frame(reactiveValuesToList(calpoints))
        }
      })

      output$metaPlot <- renderPlot({
        par(mar = c(0, 0, 0, 0))
        plot_values <- reactiveValuesToList(values)
        do.call(internal_redraw, plot_values)
      })


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
      
      output$clickinfo <- renderText({
        paste0("x = ", calpoints$x, ", y = ", calpoints$y, "\n")
      })
    } else {
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

################################################
  # Calibrate labeling
################################################

  # take the inputs from the y axis/y1 and y2 and add to the values object
  # other plots to be finished
  
  observeEvent(input$yvar_me, {
    if (input$calib_mode) {
      values$variable <<- input$yvar_me

      output$metaPlot <- renderPlot({
        par(mar = c(0, 0, 0, 0))
        plot_values <- reactiveValuesToList(values)
        do.call(internal_redraw, plot_values)
      })
    }
  })


  observeEvent(c(input$y1_me, input$y2_me), {
    if (input$calib_mode) {
      values$point_vals <<- c(input$y1_me,input$y2_me)

      output$metaPlot <- renderPlot({
        par(mar = c(0, 0, 0, 0))
        plot_values <- reactiveValuesToList(values)
        do.call(internal_redraw, plot_values)
      })
    }
  })

  # boxplot
  # observeEvent(input$yvar_bp, {
  #   var_name$yvar <- input$yvar_bp
  #   values$variable <<- var_name$yvar

  #   output$metaPlot <- renderPlot({
  #     par(mar = c(0, 0, 0, 0))
  #     plot_values <- reactiveValuesToList(values)
  #     do.call(internal_redraw, plot_values)
  #   })
  # })

  # observeEvent(c(input$y1_bp, input$y2_bp), {
  #   var_num$y1 <- input$y1_bp
  #   var_num$y2 <- input$y2_bp
  #   values$point_vals <<- as.vector(reactiveValuesToList(var_num))

  #   output$metaPlot <- renderPlot({
  #     par(mar = c(0, 0, 0, 0))
  #     plot_values <- reactiveValuesToList(values)
  #     do.call(internal_redraw, plot_values)
  #   })
  # })


  # # scatterplot
  # observeEvent(c(input$yvar_sp, input$xvar_sp), {
  #   var_name$yvar <- input$yvar_sp
  #   var_name$xvar <- input$xvar_sp
  #   values$variable <<- c(var_name$yvar, var_name$xvar)

  #   output$metaPlot <- renderPlot({
  #     par(mar = c(0, 0, 0, 0))
  #     plot_values <- reactiveValuesToList(values)
  #     do.call(internal_redraw, plot_values)
  #   })
  # })

  # observeEvent(c(input$y1_sp, input$y2_sp, input$x1_sp, input$x2_sp), {
  #   var_num$y1 <- input$y1_sp
  #   var_num$y2 <- input$y2_sp
  #   var_num$x1 <- input$x1_sp
  #   var_num$x2 <- input$x2_sp
  #   values$point_vals <<- as.vector(reactiveValuesToList(var_num))

  #   output$metaPlot <- renderPlot({
  #     par(mar = c(0, 0, 0, 0))
  #     plot_values <- reactiveValuesToList(values)
  #     do.call(internal_redraw, plot_values)
  #   })
  # })


  # # histogram
  # observeEvent(input$xvar_hist, {
  #   var_name$xvar <- input$xvar_hist
  #   values$variable <<- var_name$xvar

  #   output$metaPlot <- renderPlot({
  #     par(mar = c(0, 0, 0, 0))
  #     plot_values <- reactiveValuesToList(values)
  #     do.call(internal_redraw, plot_values)
  #   })
  # })

  # observeEvent(c(input$y1_hist, input$y2_hist, input$x1_hist, input$x2_hist), {
  #   var_num$y1 <- input$y1_hist
  #   var_num$y2 <- input$y2_hist
  #   var_num$x1 <- input$x1_hist
  #   var_num$x2 <- input$x2_hist
  #   values$point_vals <<- as.vector(reactiveValuesToList(var_num))

  #   output$metaPlot <- renderPlot({
  #     par(mar = c(0, 0, 0, 0))
  #     plot_values <- reactiveValuesToList(values)
  #     do.call(internal_redraw, plot_values)
  #   })
  # })


################################################
  # Show/hide panels/swtitches
################################################
  
  # if we are in calib mode - toggle extract mode and rotate mode off.
  observeEvent(input$calib_mode, {
    if (input$calib_mode) {
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
    }
  })

  # if we are in extract mode - toggle calibrate mode and rotate mode off.
  observeEvent(input$extract_mode, {
    if (input$extract_mode) {
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
    }
  })

  # if we are in rotate mode - toggle extract mode and calib mode off.
  observeEvent(input$rotate_mode, {
    if (input$rotate_mode) {
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
    }
  })

  # if we are in calib mode - show the calib_data object (where we enter names and values for axis).
  observeEvent(input$calib_mode, {
    shinyjs::toggle(id = "calib_data")
  })
  
  # if we are in extract mode - show the error type select input (ofr mean_error).
  observeEvent(input$extract_mode, {
    if (input$plot_type == "mean_error") {
      shinyjs::toggle(id = "error_type_select")
    }
  })
  
  # if we are in extract mode - show the group_data ovject (the table for clicking/groups and sample sizes)
  observeEvent(input$extract_mode, {
    shinyjs::toggle(id = "group_data")

################################################
    # Group Name and Sample Size Table
################################################

  # create multiple reactive objects.
  # for row count, for the dataframe.
  row_count <<- reactiveValues(x = NULL)
  mod_df <<- reactiveValues(x = NULL)

  # help text to show when extract mode is on.
    if (input$extract_mode) {
      output$info <- renderText({
        "**** EXTRACTING DATA ****
1. Group names and sample size should be entered into the table on the sidebar before points are added.
2. To add points to a group, first click the group on the sidebar then click 'Add Points'.
3. To delete a group, click on the desired group in the table on the sidebar then press 'Delete Group'."
      })
    }
  
  # help text to show when extract mode is false.
    if (input$extract_mode == F) {
      output$info <- renderText({
        "**** NEW PLOT ****
mean_error and boxplots should be vertically orientated.
If they are not then chose flip to correct this.
If figures are wonky, chose rotate."
      })
    }

  # if values raw data is null/empty then create new data to show in the table.
  # otherwise read in the data that already exists from the raw data.
  # this is then rendered in a DT table.
    if (is.null(values$raw_data)) {
      basic <- tibble(
        Group_Name = NA,
        Sample_Size = NA
      )

      row_count$x <- 0
      mod_df$x <- basic[-nrow(basic),]
    } else {
      raw_dat <- as.data.frame(values$raw_data)
      raw_dat_sum <- aggregate(n ~ id, raw_dat, unique)
      names(raw_dat_sum) <- c("Group_Name", "Sample_Size")
      mod_df$x <- raw_dat_sum
      row_count$x <- nrow(raw_dat_sum)
      valpoints <- values$raw_data
    }
    output$group_table <- DT::renderDT({
      DT::datatable(
        mod_df$x,
        editable = list(target = "cell"),
        options = list(lengthChange = TRUE, dom = "t")
      )
    })
  })

  # what happens when you press the delete group.
  # if no cell is selected to delete, then show a modal alert.
  # however if one is, then search the data fro that cell and remove it from the df.
  # this will also cause the plot and raw data to update and remove anything with this group.
  observeEvent(input$del_group, {
    if (input$del_group) {
      if(is.null(selected_cell$x)){
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
      remove_dat <- which(mod_df$x == selected_cell$x)[1]
      mod_df$x <- mod_df$x[-remove_dat, ]
      values$raw_data <<- as.data.frame(reactiveValuesToList(valpoints))
      values$raw_data <<- values$raw_data %>%
        filter(!stringr::str_detect(id, as.character(selected_cell$x)))

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
  
################################################
  #  Adding points
################################################
  
  # create empty click counter and which cell/row is selected.
  # for plotting values and for add T/F.
  plotcounter <<- reactiveValues(plotclicks = NULL)
  selected_cell <<- reactiveValues(x = NULL)
  selected_row <<- reactiveValues(x = NULL)
  valpoints <<- reactiveValues(x = NULL, y = NULL, id = NULL, n = NULL, plot_n)
  add_mode <<- reactiveValues(add = NULL)
  
  # what happens when you click a cell on the group table. 
  # Useful for deleting groups and labeling points. Highlights the cell.
  observeEvent(input$group_table_cell_clicked, {
    selected_cell$x <- input$group_table_cell_clicked$value
  })

  # what happens when you click on a cell/row on the group table. 
  # Useful for deleting groups and labeling points. Highlights the row.
  observeEvent(input$group_table_rows_selected, {
    selected_row$x <- input$group_table_rows_selected
  })

  # what happens when you click to add points.
  # plotcounter becomes 0.
  # add mode becomes T, which means we can add points.
  # if you click group without any row selected - return an error.
  # when you click on the plot and if add mode is true, increase plotcount by 1.
  # if the graph is mean_error and the plotcount is 2 or less then add data to valpoints object.
  # if its boxplot and five or less, then do the same.
  # otherwise add mode becomes false and it switches off.
  # valpoints is then converted to a dataframe and plotted.
  # location of x and y valpoints are given in text.
  observeEvent(input$click_group, {
    plotcounter$plotclicks <- 0
    add_mode$add=TRUE
    
    if(is.null(selected_cell$x)){
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
    } else {if(is.null(selected_cell$x) == F){
    observeEvent(input$plot_click2, {
      if (add_mode$add) {
        plotcounter$plotclicks <- plotcounter$plotclicks + 1

        if (input$plot_type == "mean_error") {
          if (add_mode$add) {
            if (plotcounter$plotclicks <= 2) {
              isolate({
              dat_mod <- as.data.frame(reactiveValuesToList(mod_df))
              valpoints$x <- c(valpoints$x, input$plot_click2$x)
              valpoints$y <- c(valpoints$y, input$plot_click2$y)
              
              if(valpoints$x)
              valpoints$id <- c(valpoints$id, dat_mod[selected_row$x, 1])
              valpoints$n <- c(valpoints$n, dat_mod[selected_row$x, 2])
              valpoints$plot_n <- c(valpoints$plot_n, plotcounter$plotclicks)
              })
              
              print(valpoints$x)
              print(valpoints$y)
              print(valpoints$plot_n)
            } else {
              add_mode$add <- FALSE
              
            }
          }
        }
        if (input$plot_type == "boxplot") {
          if (add_mode$add) {
            if (plotcounter$plotclicks <= 5) {
              isolate({
              dat_mod <- as.data.frame(reactiveValuesToList(mod_df))
              valpoints$x <- c(valpoints$x, input$plot_click2$x)
              valpoints$y <- c(valpoints$y, input$plot_click2$y)
              valpoints$id <- c(valpoints$id, dat_mod[selected_row$x, 1])
              valpoints$n <- c(valpoints$n, dat_mod[selected_row$x, 2])
              })
            } else {
              add_mode$add <- FALSE

            }
          }
        }
        values$raw_data <<- as.data.frame(reactiveValuesToList(valpoints))
      }
    })
    output$metaPlot <- renderPlot({
      par(mar = c(0, 0, 0, 0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw, plot_values)
    })
    output$clickinfo <- renderText({
      paste0("x = ", valpoints$x, ", y = ", valpoints$y, "\n")
    })
    }
      }
  })
  
  # similar to above, if you click add points and add mode is T and there is data present for that selected cell.
  # then remove this selected group from the plot and plotcounter becomes zero after replotting.

  

################################################
  # What happens when you quit
################################################

  #the app stops when you exit - not sure what this does.
  session$onSessionEnded(function() {
    stopApp()
  })
})
