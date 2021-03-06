
shinyDigitise_server <- function(input, output, session){

  output$shinylogo <- shiny::renderImage({
    list(src =base::system.file("logos/shinyDigitise.png", package="shinyDigitise"), height = 60)
  },deleteFile=FALSE)

  output$shinytext <- shiny::renderText({
    "shinyDigitise"
  })

  ################################################
  # Counter and initial plots
  ################################################

  # start counter at 1.
  counter <<- shiny::reactiveValues(countervalue = 1)

  # when counter value is changed (either conitnue or previous is pressed):
  shiny::observeEvent(counter$countervalue, {

    shinyjs::hide("orient_well")
    shinyjs::hide("extract_well")
    shinyjs::hide("calib_well")
    shinyjs::hide("comm_well")
    shinyjs::show("plot_well")

    # find previously extracted data:
    counter$caldat <- paste0(details$cal_dir, details$name[counter$countervalue])

    # if extracted/calibrated data already exists:
    if (file.exists(counter$caldat)) {
      # plot_values <- readRDS(values$caldat)

      #read in that data.
      values <<- do.call("reactiveValues", readRDS(counter$caldat))

      # update
      shiny::updateSliderInput(session, "cex", value = values$cex)
      shinyWidgets::updatePrettyRadioButtons(session, "pos", selected = values$pos)
      shinyWidgets::updatePrettyRadioButtons(session, "plot_type", selected = values$plot_type)
      shiny::updateTextInput(session, "comment", value = paste(values$comment))

      if (values$plot_type %in% c("mean_error","xy_mean_error")) {
        shinyWidgets::updatePrettyRadioButtons(session, "errortype", selected = values$error_type)
      }
      # update


    } else {
      # if not then create a new values object that we can store data in.
      #  keeps flip/rotate and image details.

      values <<- shiny::reactiveValues(
        image_name = details$name[counter$countervalue],
        image_file = details$paths[counter$countervalue],
        plot_type = NULL,
        flip = FALSE,
        rotate = 0,
        calpoints = NULL,
        variable = NULL,
        point_vals = NULL,
        log_axes = c(axes="n"),
        raw_data = NULL,
        # rotate_mode=FALSE,
        error_type = NULL,
        comment = NULL,
        cex = input$cex,
        pos = "right"
      )
    shinyWidgets::updatePrettyRadioButtons(session, "plot_type", selected = character(0))
    shinyWidgets::updatePrettyRadioButtons(session, "errortype", selected = character(0))
    }
    

    shinyWidgets::updateSwitchInput(
      session = session,
      inputId = "flip",
      value = values$flip
    )

    shinyWidgets::updateSwitchInput(
      session = session,
      inputId = "rotate_mode",
      value = FALSE
    )

    values$zoom_coords <- NULL

    values$rotate_mode <- FALSE

    shiny::updateSliderInput(
      session = session,
      inputId = "rotate",
      value = FALSE,
      values$rotate
    )
    output$rotation <- shiny::renderText({
      paste("rotation angle:", values$rotate)
    })
    output$image_name <- shiny::renderText({
      values$image_name
    })

    shinyWidgets::updateSwitchInput(
      session = session,
      inputId = "calib_mode",
      value = FALSE
    )

    shinyWidgets::updateSwitchInput(
      session = session,
      inputId = "extract_mode",
      value = FALSE
    )

    #### update tick boxes
    output$plottype_check_text <- shiny::renderText({
      if(check_plottype(shiny::reactiveValuesToList(values))){
         emojifont::emoji('white_check_mark')
      }else{
        emojifont::emoji('warning')
      }
    })
    output$orientation_check_text <- shiny::renderText({
      if(check_orientation(shiny::reactiveValuesToList(values))){
         emojifont::emoji('white_check_mark')
      }else{
        emojifont::emoji('warning')
      }
    })
    output$calibrate_check_text <- shiny::renderText({
      if(check_calibrate(shiny::reactiveValuesToList(values))){
         emojifont::emoji('white_check_mark')
      }else{
        emojifont::emoji('warning')
      }
    })
    output$extract_check_text <- shiny::renderText({
      if(check_extract(shiny::reactiveValuesToList(values))){
         emojifont::emoji('white_check_mark')
      }else{
        emojifont::emoji('warning')
      }
    })

    output$metaPlot <- shiny::renderPlot({
      graphics::par(mar = c(0, 0, 0, 0))
      plot_values <- shiny::reactiveValuesToList(values)
      do.call(internal_redraw, c(plot_values, shiny=TRUE))
    })
  })


  ################################################
  # Plot type and cex
  ################################################

  # record the plot type for the data file - influences clicking etc.

  shiny::observeEvent(input$plot_type,{
    values$plot_type <<- input$plot_type
    
    #### update tick boxes
    output$plottype_check_text <- shiny::renderText({
      if(check_plottype(shiny::reactiveValuesToList(values))){
         emojifont::emoji('white_check_mark')
      }else{
        emojifont::emoji('warning')
      }
    })
    output$calibrate_check_text <- shiny::renderText({
      if(check_calibrate(shiny::reactiveValuesToList(values))){
         emojifont::emoji('white_check_mark')
      }else{
        emojifont::emoji('warning')
      }
    })
  })

  #record cex used (adjusted with slider)
  shiny::observe(values$cex <<- input$cex)

  shiny::observe(values$pos <<- input$pos)

  shiny::observe(values$error_type <<- input$errortype)

  # shiny::observeEvent(input$errortype,{
  #   values$error_type <<- input$errortype
  #   })

  ################################################
  # zoom
  ################################################
  
  # shiny::observeEvent(input$plot_brush,{
  #   # print(input$plot_brush)
  #   values$zoom_coords <<- c(input$plot_brush$xmin,input$plot_brush$xmax,input$plot_brush$ymin,input$plot_brush$ymax)
  #   output$metaPlot <- shiny::renderPlot({
  #     par(mar = c(0, 0, 0, 0))
  #     plot_values <- shiny::reactiveValuesToList(values)
  #     do.call(internal_redraw, c(plot_values, shiny=TRUE))
  #   })
  #   # session$resetBrush("plot_brush")
  #   # runjs("document.getElementById('plot_brush').remove()")
  # })

  # shiny::observeEvent(input$plot_dblclick,{
  #   session$resetBrush("plot_brush")

  #   values$zoom_coords <<- NULL
  #   output$metaPlot <- shiny::renderPlot({
  #     par(mar = c(0, 0, 0, 0))
  #     plot_values <- shiny::reactiveValuesToList(values)
  #     do.call(internal_redraw, c(plot_values, shiny=TRUE))
  #   })
  # })

  ################################################
  # Flip
  ################################################

  # record whether we flip the image or not
  shiny::observeEvent(input$flip, {
    values$flip <<- input$flip

    output$metaPlot <- shiny::renderPlot({
      graphics::par(mar = c(0, 0, 0, 0))
      plot_values <- shiny::reactiveValuesToList(values)
      do.call(internal_redraw, c(plot_values, shiny=TRUE))
    })
  })

  ################################################
  # Rotate
  ################################################

  # rotate the image using a slider. Text + slider gets displayed as you click rotate mode.
  shiny::observeEvent(input$rotate_mode, {
    if (input$rotate_mode) {

      # if we are in rotate mode - toggle extract mode and calib mode off.
      shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "calib_mode",
        value = FALSE
      )
      shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "extract_mode",
        value = FALSE
      )
      shinyjs::show("togslide")
    } else {
      shinyjs::hide("togslide")
    }

    output$metaPlot <- shiny::renderPlot({
      graphics::par(mar = c(0, 0, 0, 0))
      values$rotate_mode <- input$rotate_mode
      plot_values <- shiny::reactiveValuesToList(values)
      do.call(internal_redraw, c(plot_values, shiny=TRUE))
    })
  })

  shiny::observeEvent(input$rotate, {
    if (input$rotate_mode) {
      values$rotate <<- input$rotate
      output$rotation <- shiny::renderText({
        paste("rotation angle:", values$rotate)
      })
    }
  })


  ################################################
  # Calibrate
  ################################################

  # create empty clikc counter for plotclicks.
  clickcounter <- shiny::reactiveValues(clickcount = 0)

  # create a container for calibration points.
  calpoints <- shiny::reactiveValues(x = NULL, y = NULL)

  # if calib mode button pressed
  shiny::observeEvent(input$calib_mode, {
    # resent click counter
    clickcounter$clickcount <- 0

    # show the calib_data object (where we enter names and values for axis).
    # shinyjs::toggle(id = "calib_data")

    # clears any previously entered text
    shinyjs::reset("y_var_input")
    shinyjs::reset("x_var_input")
    shinyjs::reset("y_coord_input")
    shinyjs::reset("y_coord_input")

    if (input$calib_mode) {
      if(is.null(values$plot_type)){
        shinyalert::shinyalert(
          title = "No plot type selected",
          text = "Please select a plot type before continuing with extraction",
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
        shinyWidgets::updateSwitchInput(
            session = session,
            inputId = "calib_mode",
            value = FALSE
        )
      }else{
        output$calib_info <- shiny::renderUI({ shiny::HTML(paste0(
        " <b> Click on known values on axes in this order: <br/>
            | <br/>
            2 <br/>
            | <br/>
            | <br/>
            1 <br/> <b/>",
            if(!input$plot_type %in% c("mean_error","boxplot")){
              "<b> |___3___________4_____ <br/> <b/>"
            }else{
              "<b> |_____________________ <br/> <b/>"
            },
           "<br/>",
         "<b> Then fill in info: <b/>"
          ))})
        
        # toggle extract mode and rotate mode off.
        shinyWidgets::updateSwitchInput(
          session = session,
          inputId = "extract_mode",
          value = FALSE
        )
        shinyWidgets::updateSwitchInput(
          session = session,
          inputId = "rotate_mode",
          value = FALSE
        )

        # if in calib mode

        ### show relevant input boxes
        if(!input$plot_type %in% c("histogram")){
          shinyjs::show("y_var_input")
        }

        if(!input$plot_type %in% c("mean_error","boxplot")){
          shinyjs::show("x_var_input")
          shinyjs::show("x_coord_input")
        }

        shinyjs::show("y_coord_input")

        # delete all previous data
        values$calpoints <<- NULL
        values$variable <<- NULL
        values$point_vals <<- NULL
        calpoints$x <- NULL
        calpoints$y <- NULL
      }
    } else {
      ## what happens when calibrate mode is switched off

      # hide variable and coord inputs
      shinyjs::hide("y_var_input")
      shinyjs::hide("x_var_input")
      shinyjs::hide("y_coord_input")
      shinyjs::hide("x_coord_input")
  output$calib_info <- shiny::renderUI({shiny::HTML("")})
    output$calibrate_check_text <- shiny::renderText({
      if(check_calibrate(shiny::reactiveValuesToList(values))){
         emojifont::emoji('white_check_mark')
      }else{
        emojifont::emoji('warning')
      }
    })
    }
  })

  # when the plot is clicked in calibrate mode
  shiny::observeEvent(input$plot_click2, {
    if (input$calib_mode) {

      # increase clickcounter (becomes clicktot object).
      clicktot <- clickcounter$clickcount + 1

      # if the plot type is me or bp then stoe two clicks, otherwise store 4 clicks and update the clickcounter.
      max_cal_clicks <- ifelse(input$plot_type %in% c("mean_error","boxplot"),2,4)
      if (clicktot <= max_cal_clicks) {
        clickcounter$clickcount <- clicktot
        calpoints$x <- c(calpoints$x, input$plot_click2$x)
        calpoints$y <- c(calpoints$y, input$plot_click2$y)
      }

      # Then convert these to a dataframe and plot
      values$calpoints <<- as.data.frame(shiny::reactiveValuesToList(calpoints))
      output$metaPlot <- shiny::renderPlot({
        graphics::par(mar = c(0, 0, 0, 0))
        plot_values <- shiny::reactiveValuesToList(values)
        do.call(internal_redraw, c(plot_values, shiny=TRUE))
      })
    }
  })

  ################################################
  # Calibrate labeling
  ################################################

  # take the inputs from the y axis/y1 and y2 and add to the values object
  # other plots to be finished

  shiny::observeEvent(c(input$y_var,input$x_var), {
    if (input$calib_mode) {
      if(input$plot_type %in% c("mean_error","boxplot")){
        values$variable <<- input$y_var
      }else if(input$plot_type %in% c("histogram")){
        values$variable <<- input$x_var
      }else{
        values$variable <<- c(y=input$y_var,y=input$x_var)
      }

      output$metaPlot <- shiny::renderPlot({
        graphics::par(mar = c(0, 0, 0, 0))
        plot_values <- shiny::reactiveValuesToList(values)
        do.call(internal_redraw, c(plot_values, shiny=TRUE))
      })
    }
  })

  shiny::observeEvent(c(input$y1, input$y2,input$x1, input$x2), {
    if (input$calib_mode) {
      if(input$plot_type %in% c("mean_error","boxplot")){
        values$point_vals <<- c(input$y1,input$y2)
      }else{
        values$point_vals <<- c(input$y1,input$y2,input$x1, input$x2)
      }

      output$metaPlot <- shiny::renderPlot({
        graphics::par(mar = c(0, 0, 0, 0))
        plot_values <- shiny::reactiveValuesToList(values)
        do.call(internal_redraw, c(plot_values, shiny=TRUE))
      })
    }
  })


  ################################################
  # Extraction
  ################################################
   #hint for extraction step
  shiny::observeEvent(input$plot_type, {
    
      if(input$plot_type == "mean_error"){
        output$plothintmean <- shiny::renderUI({ 
          shiny::strong("Click on Error Bar, followed by the Mean")
        })
        shinyjs::hide("plothintxy")
        shinyjs::hide("plothintbox")
        shinyjs::show("plothintmean")
      }
      if(input$plot_type == "xy_mean_error"){
        output$plothintxy <- shiny::renderUI({ 
        shiny::strong("Click on Y Error Bar, followed by the Mean, followed by the X Error Bar")
        })
        shinyjs::hide("plothintmean")
        shinyjs::hide("plothintbox")
        shinyjs::show("plothintxy")
      }
    
      if(input$plot_type == "boxplot"){
        output$plothintbox <- shiny::renderUI({ 
        shiny::strong("Click on Max, Upper Q, Median, Lower Q, and Minimum in that order")
      })
        shinyjs::hide("plothintxy")
        shinyjs::hide("plothintmean")
        shinyjs::show("plothintbox")
      }
    
    })

  # for row count,
  row_count <- shiny::reactiveValues(x = NULL)

  # dataframe containing group name etc
  mod_df <- shiny::reactiveValues(x = NULL)

  # create empty click counter
  plotcounter <- shiny::reactiveValues(plotclicks = NULL)

  #container for which rows and cell are selected
  selected <- shiny::reactiveValues(row = NULL, cell=NULL)
  
  #container for which row are clicked
  clicked<- shiny::reactiveValues(row = NULL)

  #container for for plotting values and
  valpoints <- shiny::reactiveValues(x = NULL, y = NULL, id = NULL, n = NULL)

  # container for extract T/F.
  extract_mode <- shiny::reactiveValues(extract = FALSE)
  # container for add T/F.
  add_mode <- shiny::reactiveValues(add = FALSE)


  shiny::observeEvent(extract_mode$extract, {

    # if we are in extract mode
    if (extract_mode$extract) {
        #toggle calibrate mode and rotate mode off.
      shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "calib_mode",
        value = FALSE
      )
      shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "rotate_mode",
        value = FALSE
      )
      #show the group_data ovject (the table for clicking/groups and sample sizes)
      shinyjs::show("group_data")

      # show the error type select input (ofr mean_error).
      if (input$plot_type %in% c("mean_error","xy_mean_error")) {
        shinyjs::show("error_type_select")
      }
      ################################################
      # Group Name and Sample Size Table
      ################################################

      if (is.null(values$raw_data)) {
        # if values raw data is null/empty then create new data to show in the table.
        basic <- data.frame(
          Group_Name = NA,
          Sample_Size = NA
        )
        #this is so it doesnt immediately plot a new row.
        mod_df$x <- basic[-nrow(basic),]

        row_count$x <- 0

        valpoints$x <- NULL
        valpoints$y <- NULL
        valpoints$id <- NULL
        valpoints$n <- NULL

      } else {
        # otherwise read in the data that already exists from the raw data.
        raw_dat <- as.data.frame(values$raw_data)
        raw_dat_sum <- stats::aggregate(n ~ id, raw_dat, unique)
        names(raw_dat_sum) <- c("Group_Name", "Sample_Size")
        mod_df$x <- raw_dat_sum
        row_count$x <- nrow(raw_dat_sum)
        valpoints$x <- values$raw_data$x
        valpoints$y <- values$raw_data$y
        valpoints$id <- values$raw_data$id
        valpoints$n <- values$raw_data$n
      }

      # this is then rendered in a DT table.
      output$group_table <- DT::renderDT({
        DT::datatable(
          mod_df$x,
          editable = list(target = "cell", disable = list(columns = c(1))),
          selection = "single",
          options = list(lengthChange = TRUE, dom = "t", pageLength = 100)
        )
      })
    
    }else{

      ## hide
      shinyjs::hide("group_data")
      shinyjs::hide("error_type_select")

      output$extract_check_text <- shiny::renderText({
        if(check_extract(shiny::reactiveValuesToList(values))){
           emojifont::emoji('white_check_mark')
        }else{
          emojifont::emoji('warning')
        }
      })
    }

  })


  ################################################
  #  Adding points
  ################################################
  # Create modal
  popupModal <- function(failed = FALSE) {
    shiny::modalDialog(
      shiny::textInput("group", "Group Name", ""),
      shiny::numericInput("sample_size", "Sample Size", ""),
      if (failed)
        shiny::div(tags$b("No group name or duplicated group name detected", style = "color: red;")),
      footer = shiny::tagList(
        shiny::actionButton("cancel", "Cancel"),
        shiny::actionButton("ok", "OK")
      )
    )
  }

  # when you click add group a popup appears which asks you to add group and sample size.
  # this is then added onto the raw data.
  # the row count increases and another row is then added after.
  shiny::observeEvent(input$add_group, {
    shiny::showModal(popupModal())
    row_count$x <- row_count$x + 1
    mod_df$x <- rbind(mod_df$x,
      data.frame(
         Group_Name = NA,
         Sample_Size = NA
       )
    )
  })

  shiny::observeEvent(input$ok, {

    if (!is.null(input$group) && nzchar(input$group)){
      shiny::removeModal()
    } else {
      shiny::showModal(popupModal(failed = TRUE))
    }
  })

  shiny::observeEvent(input$cancel, {
      shiny::removeModal()
    row_count$x <- row_count$x - 1
    mod_df$x <- mod_df$x[-nrow(mod_df$x), ]
  })

  # what happens when you click a cell on the group table.
  # Useful for deleting groups and labeling points. Highlights the cell.
  shiny::observeEvent(input$group_table_cell_clicked, {
    selected$cell <- input$group_table_cell_clicked$value
  })

  # what happens when you click on a cell/row on the group table.
  # Useful for deleting groups and labeling points. Highlights the row.
  shiny::observeEvent(input$group_table_rows_selected, {
    selected$row <- input$group_table_rows_selected
  })

  shiny::observeEvent(counter$countervalue,{
    if(is.null(input$group_table_rows_selected)){
      shinyjs::disable("del_group")
    }
  })


  shiny::observeEvent(counter$countervalue,{
    if(is.null(input$group_table_rows_selected)){
      shinyjs::disable("click_group")
    }
  })

  shiny::observeEvent(input$group_table_rows_selected,{
    shinyjs::enable("click_group")
    shinyjs::enable("del_group")
  })


  shiny::observeEvent(input$click_group, {

    plotcounter$plotclicks <- 0

    # add mode becomes T
    add_mode$add <- TRUE

    # if you click group without any row selected - return an error.
    if(length(selected$row) == 0){
      shinyalert::shinyalert(
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
      add_mode$add <- FALSE
    } else {

      if (add_mode$add) {

        # similar to above, if you click add points and add mode is T and there is data present for that selected cell then remove this selected group from the plot and plotcounter becomes zero after replotting.
        # if(as.data.frame(shiny::reactiveValuesToList(mod_df$x[selected$row,"Group_Name"]) %in% values$raw_data$id)){
        if(length(stringr::str_detect(values$raw_data$id, as.character(mod_df$x[selected$row,1]))) != 0){
          values$raw_data <<- as.data.frame(shiny::reactiveValuesToList(valpoints))
          remove_string <-  as.character(mod_df$x[selected$row,1])
          values$raw_data <<- values$raw_data[!grepl(remove_string, values$raw_data$id),]
          valpoints$x <- values$raw_data$x
          valpoints$y <- values$raw_data$y
          valpoints$id <- values$raw_data$id
          valpoints$n <- values$raw_data$n
        }else{
          values$raw_data <<-NULL
        }
      }
      ## ?? this seems problematic
      # values$raw_data <<- as.data.frame(shiny::reactiveValuesToList(valpoints))
    }
  })

  # when you click on the plot and if add mode is true, increase plotcount by 1.
  # if the graph is mean_error and the plotcount is 2 or less then add data to valpoints object.
  # if its boxplot and five or less, then do the same.
  # otherwise add mode becomes false and it switches off.
  # valpoints is then converted to a dataframe and plotted.
  # location of x and y valpoints are given in text.
  #  when you click to add points

  shiny::observeEvent(input$plot_click2, {
    if (add_mode$add) {
      plotcounter$plotclicks <- plotcounter$plotclicks + 1
      dat_mod <- as.data.frame(shiny::reactiveValuesToList(mod_df))
      max_clicks <-
        ifelse(input$plot_type == "mean_error",2,
        ifelse(input$plot_type == "xy_mean_error",3,
        ifelse(input$plot_type == "boxplot",5,
           NA)))
      if (plotcounter$plotclicks <= max_clicks) {
        valpoints$x <- c(valpoints$x, input$plot_click2$x)
        valpoints$y <- c(valpoints$y, input$plot_click2$y)
        valpoints$id <- c(valpoints$id, dat_mod[selected$row, 1])
        valpoints$n <- c(valpoints$n, dat_mod[selected$row, 2])
      }
      if (plotcounter$plotclicks == max_clicks) {
        add_mode$add <- FALSE
        selected$row <<- NULL
        DT::selectRows(proxy, selected = NULL)
      }

      values$raw_data <<- as.data.frame(shiny::reactiveValuesToList(valpoints))

      output$metaPlot <- shiny::renderPlot({
        graphics::par(mar = c(0, 0, 0, 0))
        plot_values <- shiny::reactiveValuesToList(values)
        do.call(internal_redraw, c(plot_values, shiny=TRUE))
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
  shiny::observeEvent(input$del_group, {
    # if (input$del_group) {
      if(is.null(selected$row)){
        shinyalert::shinyalert(
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
        values$raw_data <<- as.data.frame(shiny::reactiveValuesToList(valpoints))
        remove_string <-  as.character(mod_df$x[selected$row,1])
        values$raw_data <<- values$raw_data[!grepl(remove_string, values$raw_data$id),]
        valpoints$x <- values$raw_data$x
        valpoints$y <- values$raw_data$y
        valpoints$id <- values$raw_data$id
        valpoints$n <- values$raw_data$n
        mod_df$x <- mod_df$x[-selected$row, ]
      }
    # }

    output$metaPlot <- shiny::renderPlot({
      graphics::par(mar = c(0, 0, 0, 0))
      plot_values <- shiny::reactiveValuesToList(values)
      do.call(internal_redraw, c(plot_values, shiny=TRUE))
    })
  })

  # this is necessary for DT to work.
  proxy <- DT::dataTableProxy("group_table")

  # edit the data table with data from the modal popup.
  shiny::observeEvent(input$sample_size, {
    mod_df$x[row_count$x,2] <<- input$sample_size
  })

  # edit the data table with data from the modal poopup.
  shiny::observeEvent(input$group, {
    mod_df$x[row_count$x,1] <<- input$group
  })

  #edit data with DT input
  shiny::observeEvent(input$group_table_cell_edit, {
    change_string <-  as.character(mod_df$x[selected$row,1])
    mod_df$x <-  DT::editData(mod_df$x, input$group_table_cell_edit)
    print(change_string)
    print(mod_df$x)
    if(is.null(values$raw_data)){
    } else {values$raw_data[grepl(change_string, values$raw_data[[2]]),][[3]] <- input$group_table_cell_edit$value
}
  })
  
  
  shiny::observeEvent(input$group_table_row_last_clicked, {
    clicked$row <- c(clicked$row,input$group_table_row_last_clicked)
    print(values$raw_data)
    if(any(duplicated(clicked$row))){
      clicked$row <<- NULL
      #selected$row <<- NULL
      DT::selectRows(proxy, selected = NULL)
    }
  })

  shiny::observeEvent(input$ok, {
    if(any(duplicated(mod_df$x[,1]))){
      shinyalert::shinyalert(
        title = "Duplicated group name detected",
        text = "This group has been deleted",
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
      row_count$x <- row_count$x - 1
      mod_df$x <- mod_df$x[-nrow(mod_df$x), ]
    }
  })



  ################################################
  # Comments
  ################################################

  # record comments
  shiny::observeEvent(input$comment,{
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

  shiny::observeEvent(input$continue, {
    plot_values <- shiny::reactiveValuesToList(values)
    if(check_plottype(plot_values) & check_calibrate(plot_values) & check_extract(plot_values)){
      plot_values$processed_data <- process_data(plot_values)
      class(plot_values) <- 'metaDigitise'
      saveRDS(plot_values, paste0(details$cal_dir, details$name[counter$countervalue]))

      cv <- counter$countervalue + 1

      if (cv > counter_total) {
        counter$countervalue <- counter_total
        shinyalert::shinyalert(
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
          animation = TRUE,
          inputId = "exit",
          callbackJS = "function(x){
                 setTimeout(function(){window.close();}, 500);
               }")
        # metaDigitise::getExtracted(dir)
        # shiny::stopApp()
        
      } else {
        counter$countervalue <- cv
      }
    }else{
      shinyalert::shinyalert(
        title = "Warning!",
        text = "You haven't completed this image.",
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
    }
  })
  
  shiny::observeEvent(input$exit, {
    shiny::stopApp(returnValue=metaDigitise::getExtracted(dir))
    utils::write.csv(metaDigitise::getExtracted(dir), paste0(dir,"ExtractedData.csv"))

  })

  # when next is pressed up the counter and check that its above 0
  # shiny::observeEvent(input$previous, {
  #   cv <- counter$countervalue - 1

  #   if (cv == 0) {
  #     counter$countervalue <- 1
  #     # clickcounter$clickcount <- 0
  #   } else {
  #     counter$countervalue <- cv
  #   }
  # })

  #output the progress text
  output$progress <- shiny::renderText({
    paste0("<font color=\"#ff3333\"><b>", counter$countervalue, "/", counter_total, "</b></font>")
  })

  ################################################
  # Previous/next step buttons
  ################################################
  
  # shiny::observeEvent(counter$countervalue, {

  # })
  
  shiny::observeEvent(input$plot_step, {
    if(is.null(values$plot_type)){
      shinyalert::shinyalert(
        title = "No plot type selected",
        text = "Please select a plot type before continuing with extraction",
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
    }else{
      shinyjs::show("orient_well")
      shinyjs::hide("plot_well")
    }
  })
  
  shiny::observeEvent(input$orient_step, {
    shinyjs::show("calib_well")
    shinyjs::hide("orient_well")
  })
  
  shiny::observeEvent(input$calib_step, {
    shinyjs::show("extract_well")
    extract_mode$extract <<- TRUE
    shinyjs::hide("calib_well")
  })
  
  shiny::observeEvent(input$extract_step, {
    shinyjs::show("comm_well")
    shinyjs::hide("extract_well")
    extract_mode$extract <<- FALSE
  })
  
  shiny::observeEvent(input$orient_back, {
    shinyjs::show("plot_well")
    shinyjs::hide("orient_well")
  })
  
  shiny::observeEvent(input$calib_back, {
    shinyjs::show("orient_well")
    shinyjs::hide("calib_well")
  })
  
  shiny::observeEvent(input$extract_back, {
    shinyjs::show("calib_well")
    shinyjs::hide("extract_well")
    extract_mode$extract <<- FALSE
  })
  
  shiny::observeEvent(input$comm_back, {
    shinyjs::show("extract_well")
    extract_mode$extract <<- TRUE
    shinyjs::hide("comm_well")
  })
  
  # shiny::observeEvent(input$continue, {
    
  # })
  


  ################################################
  # What happens when you quit
  ################################################

  #the app stops when you exit - not sure what this does.
  session$onSessionEnded(function() {
    shiny::stopApp(returnValue=metaDigitise::getExtracted(dir))
    utils::write.csv(metaDigitise::getExtracted(dir), paste0(dir,"ExtractedData.csv"))

  })
}
