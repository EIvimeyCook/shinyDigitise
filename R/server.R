
shinyDigitise_server <- function(input, output, session){

  output$shinylogo <- renderImage({
    list(src ="www/shiny.jpg", height = 60)
  },deleteFile=FALSE)

  output$shinytext <- renderText({
    "shinyDigitise"
  })

  ################################################
  # Counter and initial plots
  ################################################

  # start counter at 1.
  counter <<- reactiveValues(countervalue = 1)

  # when counter value is changed (either conitnue or previous is pressed):
  observeEvent(counter$countervalue, {
    # find previously extracted data:
    counter$caldat <- paste0(details$cal_dir, details$name[counter$countervalue])

    # if extracted/calibrated data already exists:
    if (file.exists(counter$caldat)) {
      # plot_values <- readRDS(values$caldat)

      #read in that data.
      values <<- do.call("reactiveValues", readRDS(counter$caldat))

      # update
      updateSliderInput(session, "cex", value = values$cex)
      updatePrettyRadioButtons(session, "pos", selected = values$pos)
      updatePrettyRadioButtons(session, "plot_type", selected = values$plot_type)
      updateTextInput(session, "comment", value = paste(values$comment))

      if (values$plot_type == "mean_error") {
        updatePrettyRadioButtons(session, "errortype", selected = values$error_type)
      }
      # update


    } else {
      # if not then create a new values object that we can store data in.
      #  keeps flip/rotate and image details.

      values <<- reactiveValues(
        image_name = details$name[counter$countervalue],
        image_file = details$paths[counter$countervalue],
        flip = FALSE,
        rotate = 0,
        calpoints = NULL,
        variable = NULL,
        point_vals = NULL,
        log_axes=c(axes="n"),
        raw_data = NULL,
        # rotate_mode=FALSE,
        cex = input$cex,
        plot_type = NULL,
        pos="right",
        comment=NULL
      )
    updatePrettyRadioButtons(session, "plot_type", selected = character(0))
    }
    

    updateSwitchInput(
      session = session,
      inputId = "flip",
      value = values$flip
    )

    updateSwitchInput(
      session = session,
      inputId = "rotate_mode",
      value = FALSE
    )

    values$zoom_coords <- NULL

    values$rotate_mode <- FALSE

    updateSliderInput(
      session = session,
      inputId = "rotate",
      value = FALSE,
      values$rotate
    )
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

    #### update tick boxes
    output$plottype_check_text <- renderText({
      if(check_plottype(reactiveValuesToList(values))){
         emoji('white_check_mark')
      }else{
        emoji('warning')
      }
    })
    output$orientation_check_text <- renderText({
      if(check_orientation(reactiveValuesToList(values))){
         emoji('white_check_mark')
      }else{
        emoji('warning')
      }
    })
    output$calibrate_check_text <- renderText({
      if(check_calibrate(reactiveValuesToList(values))){
         emoji('white_check_mark')
      }else{
        emoji('warning')
      }
    })
    output$extract_check_text <- renderText({
      if(check_extract(reactiveValuesToList(values))){
         emoji('white_check_mark')
      }else{
        emoji('warning')
      }
    })

    output$metaPlot <- renderPlot({
      par(mar = c(0, 0, 0, 0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw, c(plot_values, shiny=TRUE))
    })

    # Provides for new plotting as text.
    showNotification2(ui = 
"1. mean_error and boxplots should be vertically orientated. <br/> 
 2. If they are not then chose flip to correct this. <br/>
 3. If figures are wonky, chose rotate.", id = "orient_notif", duration = NULL, type = "default"
    )
    removeNotification(id = "group_notif")
    removeNotification(id = "calib_notif")
    removeNotification(id = "rotate_notif")
    
  })


  ################################################
  # Plot type and cex
  ################################################

  # record the plot type for the data file - influences clicking etc.

  observeEvent(input$plot_type,{
    values$plot_type <<- input$plot_type
    
    #### update tick boxes
    output$plottype_check_text <- renderText({
      if(check_plottype(reactiveValuesToList(values))){
         emoji('white_check_mark')
      }else{
        emoji('warning')
      }
    })
    output$calibrate_check_text <- renderText({
      if(check_calibrate(reactiveValuesToList(values))){
         emoji('white_check_mark')
      }else{
        emoji('warning')
      }
    })
  })

  #record cex used (adjusted with slider)
  observe(values$cex <<- input$cex)

  observe(values$pos <<- input$pos)

  observe(values$error_type <<- input$error_type_select)


  ################################################
  # zoom
  ################################################
  
  observeEvent(input$plot_brush,{
    # print(input$plot_brush)
    values$zoom_coords <<- c(input$plot_brush$xmin,input$plot_brush$xmax,input$plot_brush$ymin,input$plot_brush$ymax)
    output$metaPlot <- renderPlot({
      par(mar = c(0, 0, 0, 0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw, c(plot_values, shiny=TRUE))
    })
    # session$resetBrush("plot_brush")
    # runjs("document.getElementById('plot_brush').remove()")
  })

  observeEvent(input$plot_dblclick,{
    session$resetBrush("plot_brush")

    values$zoom_coords <<- NULL
    output$metaPlot <- renderPlot({
      par(mar = c(0, 0, 0, 0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw, c(plot_values, shiny=TRUE))
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
      do.call(internal_redraw, c(plot_values, shiny=TRUE))
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
      showNotification2(ui = "Use the slider to change the rotation angle", id = "rotate_notif", duration = NULL, type = "message"
      )
      removeNotification(id = "orient_notif")
      removeNotification(id = "calib_notif")
      removeNotification(id = "group_notif")
      show("togslide")
    } else {
      hide("togslide")
    }

    output$metaPlot <- renderPlot({
      par(mar = c(0, 0, 0, 0))
      values$rotate_mode <- input$rotate_mode
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw, c(plot_values, shiny=TRUE))
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

    if (input$calib_mode) {
      if(is.null(values$plot_type)){
        shinyalert(
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
        updateSwitchInput(
            session = session,
            inputId = "calib_mode",
            value = FALSE
        )
      }else{

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

        # if in calib mode

        ### show relevant input boxes
        if(!input$plot_type %in% c("histogram")){
          show("y_var_input")
        }

        if(!input$plot_type %in% c("mean_error","boxplot")){
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
        if (!input$plot_type %in% c("mean_error","boxplot")) {
          showNotification2(ui = "
          Click on known values on axes in this order: <br/>
          | <br/>
          2 <br/>
          | <br/>
          | <br/>
          1 <br/>
          |___3_________4___" ,
          id = "calib_notif", duration = NULL, type = "default")
          removeNotification(id = "orient_notif")
          removeNotification(id = "group_notif")
          removeNotification(id = "rotate_notif")
        } else {
          showNotification2(ui = "
          Click on known values on axes in this order: <br/> 
          | <br/>
          2 <br/>
          | <br/>
          | <br/>
          1 <br/>
          |_________________" ,
                            id = "calib_notif", duration = NULL, type = "default")          
  removeNotification(id = "orient_notif")
  removeNotification(id = "group_notif")
  removeNotification(id = "rotate_notif")
        }

        # Also shows click locations (probably don't need this in future).
        # and gives calibration placement help.
      }
    } else {
      ## what happens when calibrate mode is switched off

      # hide variable and coord inputs
      hide("y_var_input")
      hide("x_var_input")
      hide("y_coord_input")
      hide("x_coord_input")

    output$calibrate_check_text <- renderText({
      if(check_calibrate(reactiveValuesToList(values))){
         emoji('white_check_mark')
      }else{
        emoji('warning')
      }
    })
    }
  })

  # when the plot is clicked in calibrate mode
  observeEvent(input$plot_click2, {
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
      values$calpoints <<- as.data.frame(reactiveValuesToList(calpoints))
      output$metaPlot <- renderPlot({
        par(mar = c(0, 0, 0, 0))
        plot_values <- reactiveValuesToList(values)
        do.call(internal_redraw, c(plot_values, shiny=TRUE))
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
        do.call(internal_redraw, c(plot_values, shiny=TRUE))
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
        do.call(internal_redraw, c(plot_values, shiny=TRUE))
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
  
  #container for which row are clicked
  clicked<- reactiveValues(row = NULL)

  #container for for plotting values and
  valpoints <- reactiveValues(x = NULL, y = NULL, id = NULL, n = NULL)

  # container for add T/F.
  add_mode <- reactiveValues(add = FALSE)


  observeEvent(input$extract_mode, {

    # if we are in extract mode
    if (input$extract_mode) {
      if(is.null(values$plot_type)){
        shinyalert(
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
        updateSwitchInput(
            session = session,
            inputId = "extract_mode",
            value = FALSE
        )
      }else{
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
        showNotification2(ui =
         "1. Group names and sample size should be entered into the table on the sidebar before points are added. <br/>
          2. To add points to a group, first click the group on the sidebar then click 'Add Points'. <br/>
          3. To delete a group, click on the desired group in the table on the sidebar then press 'Delete Group'."
, duration = NULL, id = "group_notif", type = "default")
        
        removeNotification(id = "orient_notif")
        removeNotification(id = "calib_notif")
        removeNotification(id = "rotate_notif")
        


        ################################################
        # Group Name and Sample Size Table
        ################################################

        if (is.null(values$raw_data)) {
          # if values raw data is null/empty then create new data to show in the table.
          basic <- tibble(
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
          raw_dat_sum <- aggregate(n ~ id, raw_dat, unique)
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
            options = list(lengthChange = TRUE, dom = "t")
          )
        })
      }
    }else{

      ## hide
      hide("group_data")
      hide("error_type_select")

      output$extract_check_text <- renderText({
        if(check_extract(reactiveValuesToList(values))){
           emoji('white_check_mark')
        }else{
          emoji('warning')
        }
      })
    }

  })


  ################################################
  #  Adding points
  ################################################
  # Create modal
  popupModal <- function(failed = FALSE) {
    modalDialog(
      textInput("group", "Group Name", ""),
      numericInput("sample_size", "Sample Size", ""),
      if (failed)
        div(tags$b("No group name or duplicated group name detected", style = "color: red;")),
      footer = tagList(
        actionButton("cancel", "Cancel"),
        actionButton("ok", "OK")
      )
    )
  }

  # when you click add group a popup appears which asks you to add group and sample size.
  # this is then added onto the raw data.
  # the row count increases and another row is then added after.
  observeEvent(input$add_group, {
    showModal(popupModal())
    row_count$x <- row_count$x + 1
    mod_df$x <- mod_df$x %>%
      dplyr::bind_rows(
        dplyr::tibble(
          Group_Name = NA,
          Sample_Size = NA
        )
      )
  })

  observeEvent(input$ok, {

    if (!is.null(input$group) && nzchar(input$group)){
      removeModal()
    } else {
      showModal(popupModal(failed = TRUE))
    }
  })

  observeEvent(input$cancel, {
      removeModal()
    row_count$x <- row_count$x - 1
    mod_df$x <- mod_df$x[-nrow(mod_df$x), ]
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
  })
  
    observeEvent(input$group_table_row_last_clicked, {
    clicked$row <- c(clicked$row,input$group_table_row_last_clicked)
    if(any(duplicated(clicked$row))){
      clicked$row <<- NULL
      selected$row <<- NULL
      selectRows(proxy, selected = NULL)
    }
  })

  observeEvent(counter$countervalue,{
    if(is.null(input$group_table_rows_selected)){
      disable("del_group")
    }
  })


  observeEvent(counter$countervalue,{
    if(is.null(input$group_table_rows_selected)){
      disable("click_group")
    }
  })

  observeEvent(input$group_table_rows_selected,{
    enable("click_group")
    enable("del_group")
  })


  observeEvent(input$click_group, {

    plotcounter$plotclicks <- 0

    # add mode becomes T
    add_mode$add <- TRUE

    # if you click group without any row selected - return an error.
    if(length(selected$row) == 0){
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
      add_mode$add <- FALSE
    } else {

      if (add_mode$add) {

        # similar to above, if you click add points and add mode is T and there is data present for that selected cell then remove this selected group from the plot and plotcounter becomes zero after replotting.
        # if(as.data.frame(reactiveValuesToList(mod_df$x[selected$row,"Group_Name"]) %in% values$raw_data$id)){
        if(length(stringr::str_detect(values$raw_data$id, as.character(mod_df$x[selected$row,1]))) != 0){
          values$raw_data <<- as.data.frame(reactiveValuesToList(valpoints))
          remove_string <-  as.character(mod_df$x[selected$row,1])
          values$raw_data <<- values$raw_data[!grepl(remove_string, values$raw_data$id),]
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
        selectRows(proxy, selected = NULL)
      }

      values$raw_data <<- as.data.frame(reactiveValuesToList(valpoints))

      output$metaPlot <- renderPlot({
        par(mar = c(0, 0, 0, 0))
        plot_values <- reactiveValuesToList(values)
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
  observeEvent(input$del_group, {
    # if (input$del_group) {
      if(is.null(selected$row)){
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
        values$raw_data <<- as.data.frame(reactiveValuesToList(valpoints))
        remove_string <-  as.character(mod_df$x[selected$row,1])
        values$raw_data <<- values$raw_data[!grepl(remove_string, values$raw_data$id),]
        valpoints$x <- values$raw_data$x
        valpoints$y <- values$raw_data$y
        valpoints$id <- values$raw_data$id
        valpoints$n <- values$raw_data$n
        mod_df$x <- mod_df$x[-selected$row, ]
      }
    # }

    output$metaPlot <- renderPlot({
      par(mar = c(0, 0, 0, 0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw, c(plot_values, shiny=TRUE))
    })
  })

  # this is necessary for DT to work.
  proxy <- DT::dataTableProxy("group_table")

  # edit the data table with data from the modal popup.
  observeEvent(input$sample_size, {
    mod_df$x[row_count$x,2] <<- input$sample_size
  })

  # edit the data table with data from the modal poopup.
  observeEvent(input$group, {
    mod_df$x[row_count$x,1] <<- input$group
  })

  #edit data with DT input
  observeEvent(input$group_table_cell_edit, {
    change_string <-  as.character(mod_df$x[selected$row,1])
    values$raw_data[grepl(change_string, values$raw_data$id),]$n <- input$group_table_cell_edit$value
    mod_df$x <-  editData(mod_df$x, input$group_table_cell_edit)
  })

  observeEvent(input$ok, {
    if(any(duplicated(mod_df$x[,1]))){
      shinyalert(
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
      values$raw_data <<- as.data.frame(reactiveValuesToList(valpoints))
      remove_string <-  as.character(mod_df$x[nrow(mod_df$x),1])
      values$raw_data <<- values$raw_data[!grepl(remove_string, values$raw_data$id),]
      valpoints$x <- values$raw_data$x
      valpoints$y <- values$raw_data$y
      valpoints$id <- values$raw_data$id
      valpoints$n <- values$raw_data$n
      mod_df$x <- mod_df$x[-nrow(mod_df$x), ]
    }
  })



  ################################################
  # Comments
  ################################################

  # record comments
  observeEvent(input$comment,{
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
    if(check_plottype(plot_values) & check_calibrate(plot_values) & check_extract(plot_values)){
      plot_values$processed_data <- process_data(plot_values)
      class(plot_values) <- 'metaDigitise'
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
    }else{
      shinyalert(
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

  # when next is pressed up the counter and check that its above 0
  # observeEvent(input$previous, {
  #   cv <- counter$countervalue - 1

  #   if (cv == 0) {
  #     counter$countervalue <- 1
  #     # clickcounter$clickcount <- 0
  #   } else {
  #     counter$countervalue <- cv
  #   }
  # })

  #output the progress text
  output$progress <- renderText({
    paste0("<font color=\"#ff3333\"><b>", counter$countervalue, "/", counter_total, "</b></font>")
  })

  ################################################
  # Previous/next step buttons
  ################################################
  
  observeEvent(counter$countervalue, {
    hide("orient_well")
    hide("extract_well")
    hide("calib_well")
    hide("comm_well")
  })
  
  observeEvent(input$plot_step, {
    show("orient_well")
    hide("plot_well")
  })
  
  observeEvent(input$orient_step, {
    show("calib_well")
    hide("orient_well")
  })
  
  observeEvent(input$calib_step, {
    show("extract_well")
    hide("calib_well")
  })
  
  observeEvent(input$extract_step, {
    show("comm_well")
    hide("extract_well")
  })
  
  observeEvent(input$orient_back, {
    show("plot_well")
    hide("orient_well")
  })
  
  observeEvent(input$calib_back, {
    show("orient_well")
    hide("calib_well")
  })
  
  observeEvent(input$extract_back, {
    show("calib_well")
    hide("extract_well")
  })
  
  observeEvent(input$comm_back, {
    show("extract_well")
    hide("comm_well")
  })
  
  observeEvent(input$continue, {
    show("plot_well")
  })
  


  ################################################
  # What happens when you quit
  ################################################

  #the app stops when you exit - not sure what this does.
  session$onSessionEnded(function() {
    stopApp()
  })
}
