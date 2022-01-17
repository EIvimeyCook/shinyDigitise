
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

    clickcounter$clickcount <- 0

    cv<-counter$countervalue + 1

    if(cv > counter_total) {
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
    }else{

      counter$countervalue <- cv

   }

  })

  # when next is pressed up the counter and check that its above 0
  observeEvent(input$previous, {

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


  observeEvent(counter$countervalue, {
    counter$caldat <- paste0(details$cal_dir,details$name[counter$countervalue])

    if(file.exists(counter$caldat)){
      # plot_values <- readRDS(values$caldat)
      values <<- do.call("reactiveValues",readRDS(counter$caldat))
      updateSliderInput(session, "cex", value=values$cex)
      updatePrettyRadioButtons(session, "plot_type", selected=values$plot_type)

      if(values$plot_type=="mean_error")
        updatePrettyRadioButtons(session, "errortype", selected=values$error_type)
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
        point_vals=NULL,
        raw_data=NULL
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

  ################################################
  #    calib data
  ################################################

  #mean error####
  observeEvent(input$yvar_me, {

    var_name <- reactiveValues(yvar = NULL)
    var_name$yvar <- input$yvar_me
    values$variable <<- var_name$yvar

    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })
  })


  observeEvent(c(input$y1_me, input$y2_me), {

    var_num <- reactiveValues(y1 = NULL, y2 = NULL, x1 = NULL, x2 = NULL)
    var_num$y1 <- input$y1_me
    var_num$y2 <- input$y2_me
    values$point_vals <<- as.vector(reactiveValuesToList(var_num))

    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })
  })

  #boxplot ######
  observeEvent(input$yvar_bp, {

    var_name <- reactiveValues(yvar = NULL)
    var_name$yvar <- input$yvar_bp
    values$variable <<- var_name$yvar

    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })
  })

  observeEvent(c(input$y1_bp, input$y2_bp), {

    var_num <- reactiveValues(y1 = NULL, y2 = NULL, x1 = NULL, x2 = NULL)
    var_num$y1 <- input$y1_bp
    var_num$y2 <- input$y2_bp
    values$point_vals <<- as.vector(reactiveValuesToList(var_num))

    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })
  })


  #scatterplot ######
  observeEvent(c(input$yvar_sp, input$xvar_sp), {

    var_name <- reactiveValues(yvar = NULL, xvar = NULL)
    var_name$yvar <- input$yvar_sp
    var_name$xvar <- input$xvar_sp
    values$variable <<- c(var_name$yvar, var_name$xvar)

    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })
  })

  observeEvent(c(input$y1_sp, input$y2_sp, input$x1_sp, input$x2_sp), {

    var_num <- reactiveValues(y1 = NULL, y2 = NULL, x1 = NULL, x2 = NULL)
    var_num$y1 <- input$y1_sp
    var_num$y2 <- input$y2_sp
    var_num$x1 <- input$x1_sp
    var_num$x2 <- input$x2_sp
    values$point_vals <<- as.vector(reactiveValuesToList(var_num))

    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })
  })


  #histogram #######
  observeEvent(input$xvar_hist, {

    var_name <- reactiveValues(xvar = NULL)
    var_name$xvar <- input$xvar_hist
    values$variable <<- var_name$xvar

    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })
  })

  observeEvent(c(input$y1_hist, input$y2_hist, input$x1_hist, input$x2_hist), {

    var_num <- reactiveValues(y1 = NULL, y2 = NULL, x1 = NULL, x2 = NULL)
    var_num$y1 <- input$y1_hist
    var_num$y2 <- input$y2_hist
    var_num$x1 <- input$x1_hist
    var_num$x2 <- input$x2_hist
    values$point_vals <<- as.vector(reactiveValuesToList(var_num))

    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })
  })




  ################################################
  #   Show hide panels
  ################################################
#need to only do one thing at a time
observeEvent(input$calib_mode, {
  if(input$calib_mode){
  updateSwitchInput(
  session = session,
  inputId = "extract_mode",
  value = FALSE)
  updateSwitchInput(
  session = session,
  inputId = "rotate_mode",
  value = FALSE)}
})

observeEvent(input$extract_mode, {
  if(input$extract_mode){
  updateSwitchInput(
  session = session,
  inputId = "calib_mode",
  value = FALSE)
  updateSwitchInput(
  session = session,
  inputId = "rotate_mode",
  value = FALSE)}
})

observeEvent(input$rotate_mode, {
  if(input$rotate_mode){
  updateSwitchInput(
  session = session,
  inputId = "calib_mode",
  value = FALSE)
  updateSwitchInput(
  session = session,
  inputId = "extract_mode",
  value = FALSE)}
})


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
observeEvent(input$extract_mode, {
  if(input$plot_type == "mean_error"){
  shinyjs::toggle(id= "error_type_select")
}
})

row_count <- reactiveValues(x = NULL)
mod_df <- reactiveValues(x = NULL)
valpoints<-reactiveValues(x = NULL, y = NULL, id = NULL, n = NULL)

  observeEvent(input$extract_mode, {
    shinyjs::toggle(id= "group_data")
    
    output$info <- renderText({
      "**** EXTRACTING DATA ****
Group names and sample size should be entered into the table on the sidebar before points are added.
      To delete a group, click on the desired group in the table on the sidebar then press delete"
    })


    if(is.null(values$raw_data)){
      basic <- tibble(
      Group_Name = "Insert group name",
      Sample_Size = "Insert sample size")

      row_count$x <- 1
      mod_df$x <-  basic

    }else{
      raw_dat<- readRDS(counter$caldat)$raw_data
      raw_dat_sum <- aggregate(n~id,raw_dat,unique)
      names(raw_dat_sum) <- c("Group_Name","Sample_Size")
      mod_df$x <- raw_dat_sum
      row_count$x <- nrow(raw_dat_sum)
      valpoints <- values$raw_data
    }
    output$group_table<- DT::renderDT({
      DT::datatable(
        mod_df$x,
        editable = list(target = 'cell'),
        options = list(lengthChange = TRUE, dom = 't'))

  })
})

  observeEvent(input$del_group, {
    if(input$del_group){
    row_count$x <- row_count$x - 1
    print(which(mod_df$x == selected_cell$x, arr.ind = TRUE))
    remove_dat <- which(mod_df$x == selected_cell$x)[1]
    mod_df$x <- mod_df$x[-remove_dat,]
    print(paste("Remove this - ", remove_dat))
    values$raw_data <<- as.data.frame(reactiveValuesToList(valpoints))
    values$raw_data <<- values$raw_data %>%
      filter(!stringr::str_detect(id, as.character(selected_cell$x)))
    
    valpoints$x <- values$raw_data$x
    valpoints$y<- values$raw_data$y
    valpoints$id<- values$raw_data$id
    valpoints$n<- values$raw_data$n
    
    }
    output$metaPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot_values <- reactiveValuesToList(values)
      do.call(internal_redraw,plot_values)
    })
  })

  proxy <- DT::dataTableProxy('group_table')


  ################################################
  #   Add/delete points
  ################################################
  #create mepty click counter
  plotcounter <- reactiveValues(plotclicks = 0)

## updates with names/sample sizes
  observeEvent(input$group_table_cell_edit, {
    mod_df$x <-  editData(mod_df$x, input$group_table_cell_edit)
  })


  #add to valpoints and scrtore click locations
  observeEvent(input$add_group, {
    row_count$x <- row_count$x + 1
    mod_df$x <- mod_df$x %>%
      dplyr::bind_rows(
        dplyr::tibble(Group_Name = "Insert group name",
                      Sample_Size = "Insert sample size")
      )
})
  
  selected_cell <- reactiveValues(x=NULL)
  
  observeEvent(input$group_table_cell_clicked, {
    selected_cell$x <- input$group_table_cell_clicked$value
    print(selected_cell$x)
    #https://stackoverflow.com/questions/37985461/shiny-dt-row-last-clicked
  })
  

selected_row <- reactiveValues(x=NULL)

observeEvent(input$group_table_rows_selected, {
  selected_row$x <- input$group_table_rows_selected
  print(selected_row$x)
  #https://stackoverflow.com/questions/37985461/shiny-dt-row-last-clicked
})

observeEvent(input$click_group, {

  plotcounter$plotclicks <- 0
  add_mode<-reactiveValues(add=TRUE)

  observeEvent(input$plot_click2,{
    if (add_mode$add) {
      plotcounter$plotclicks<-plotcounter$plotclicks + 1

    if(input$plot_type == "mean_error"){
      if (add_mode$add) {
        if (plotcounter$plotclicks <= 2) {
          dat_mod <- as.data.frame(reactiveValuesToList(mod_df))
          valpoints$x <- c(valpoints$x, input$plot_click2$x)
          valpoints$y <- c(valpoints$y, input$plot_click2$y)
          valpoints$id <- c(valpoints$id, dat_mod[selected_row$x,1])
          valpoints$n <- c(valpoints$n, dat_mod[selected_row$x,2])
          
          print(valpoints$x)
          print(valpoints$id)
          print(valpoints$y)
          print(valpoints$n)
          
        } else {
          add_mode$add<-FALSE

          plotcounter$plotclicks <- 0
        }}}
    if(input$plot_type == "boxplot"){
      if (add_mode$add) {
        if (plotcounter$plotclicks <= 5) {
          dat_mod <- as.data.frame(reactiveValuesToList(mod_df))
          valpoints$x <- c(valpoints$x, input$plot_click2$x)
          valpoints$y <- c(valpoints$y, input$plot_click2$y)
          valpoints$id <- c(valpoints$id, dat_mod[selected_row$x,1])
          valpoints$n <- c(valpoints$n, dat_mod[selected_row$x,2])
        } else {
          plotcounter$plotclicks <- 0
          add_mode$add<-FALSE
        }}}

  print(values$raw_data)
  print(plotcounter$plotclicks)
  values$raw_data <<- as.data.frame(reactiveValuesToList(valpoints))

    }
    })
  output$metaPlot <- renderPlot({
    par(mar=c(0,0,0,0))
    plot_values <- reactiveValuesToList(values)
    do.call(internal_redraw,plot_values)
  })
  output$clickinfo <- renderText({
    paste0("x = ", valpoints$x, ", y = ", valpoints$y, "\n")
  })
})


  ################################################
  #   What happens when you quit
  ################################################

  session$onSessionEnded(function() {
    stopApp()
  })


})
