#shinyDigitise UI function

server <- function(input, output, session){

  ################################################
  # Initial Startup
  ################################################

#reactive objects and counters.

   # start counter at 1 - helps if this is before things
  counter <- shiny::reactiveValues(countervalue = 0, next_count = 0)

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

  # container for extract T/F.
  extract_mode <- shiny::reactiveValues(extract = FALSE)

  # container for add T/F.
  add_mode <- shiny::reactiveValues(add = FALSE)

  #image import container
  image_import <- shiny::reactiveValues(select = FALSE, multiple = FALSE, extract_rev = NULL)

  # create empty clikc counter for plotclicks.
  clickcounter <- shiny::reactiveValues(clickcount = 0)

  # create a container for calibration points.
  calpoints <- shiny::reactiveValues(x = NULL, y = NULL)

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

  # container for extract T/F.
  extract_mode <- shiny::reactiveValues(extract = FALSE)

  # container for add T/F.
  add_mode <- shiny::reactiveValues(add = FALSE)

   # container for add T/F.
  scatterChoice <- shiny::reactiveValues(col_list = NULL, pch_list = NULL)

  #praising action button + logo leads to citations
  shiny::observeEvent(input$citeme, {
    shinyalert::shinyalert(
      title = "shinyDigitise",
      text = paste(shiny::tags$h5("Made by Ed Ivimey-Cook and Joel Pick"),
                   shiny::tags$br(),
                   "<b> To cite, please reference both this package:</b>",
                   shiny::tags$br(),
                   shiny::tags$br(),
                   "<p><a href= https://ecoevorxiv.org/repository/view/4814/> Ivimey-Cook, E. R., Noble, D. W., Nakagawa, S., Lajeunesse, M. J., & Pick, J. L. (2022). A framework for improving the reproducibility of data extraction for meta-analysis.</a></p>",
                   shiny::tags$br(),
                   "<b> and metaDigitise:</b>", 
                   shiny::tags$br(),
                   shiny::tags$br(),
                   "<p><a href= https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13118 >Pick, J. L., Nakagawa, S., & Noble, D. W. (2019). Reproducible, flexible and high-throughput data extraction from primary literature: The metaDigitise r package. Methods in Ecology and Evolution, 10(3), 426-431.</a></p>"),
      size = "l", 
      closeOnClickOutside = FALSE,
      html = TRUE,
      type = "",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      animation = TRUE,
      imageUrl =  "img/shinyDigitise.png",
      imageHeight = "88",
      imageWidth = "80"
      )
  })

#take data quiery on startup in a popup modal including directory. This wont show if you've already selected a dir
#walks you through the selection from selecting, to whether you want to review or extract
#then it asks if you want to extract all or unfinished
#or you can select a specific one
#then click a button to start
  data_modal <- shiny::modalDialog(
    title = shiny::div(shiny::tags$img(src = "img/shinyDigitise.png", height = "88px", width = "80px")),
    shiny::div(style = "text-align: center", offset = 0, 
    shinyFiles::shinyFilesButton("choosefolder", "Please select an image file from within the folder of images", title = NULL, multiple = FALSE,
        filetype = list(picture = c("jpg", "png", "jpeg", "tiff")))),
    shiny::tags$br(),
    shinyjs::hidden(shiny::div(id = "data_import_title1",
                               shiny::tags$h5("Please select whether you want to review or extract:", align = "center"))),
    shiny::tags$br(),
    shinyjs::hidden(shiny::div(id = "data_rev_or_extract",
     style = "text-align: center", offset = -6,
      shinyWidgets::switchInput(
   inputId = "rev_or_extractbut",
    size = "large",
    value = FALSE,
    inline = TRUE,
    onLabel = "Review Mode",
    offLabel = "Extract Mode",
    onStatus = "primary",
    offStatus = "warning"))),
   shiny::tags$br(),
    shinyjs::hidden(shiny::div(id = "data_import_title2",
     shiny::tags$h5("Please select which graphs you'd like to import:", align = "center"), 
     shiny::tags$h6("Note if only 'All' is showing, there are no further images to extract!", align = "center"))),
   shiny::tags$br(),
    shinyjs::hidden(shiny::div(id = "data_all_or_unfin",
     style = "text-align: center", offset = 0,
                shiny::radioButtons(
           inputId = "all_or_unfin_but",
           label = NULL,
           choices = c("All", "Finished", "Unfinished"),
           selected = character(0),
           inline = TRUE, 
))),
shiny::tags$br(),
shinyjs::hidden(shiny::div(id = "data_import_title3",
  shiny::tags$h5("Alternatively, please select a specific graph from your chosen directory:", align = "center"))), 
shiny::tags$br(),
shinyjs::hidden(shiny::div(id = "data_select_image",style = "text-align: center", offset = 0,
  shinyWidgets::pickerInput(
   inputId = "select_image_pick",
    choices = c(""),
    select = "",
   options = list(
      style = "btn-primary"),
   inline = TRUE,
   width = "auto"
))), 
shiny::tags$br(),
shinyjs::hidden(shiny::div(id = "data_import_extract",
                           shiny::actionButton("extract_but", "Get Extracting!"))),
    shinyjs::hidden(shiny::div(id = "data_import_review", 
                               shiny::actionButton("review_but", "Get Reviewing!"))),
                                   easyClose = FALSE,
                               footer = NULL,
                               size = "xl",
                               fade = TRUE
)


  # Create modal
  popupModal1 <- function(failed = FALSE) {
    shinyjqui::jqui_draggable(shiny::modalDialog(
      shiny::textInput("group", "Group Name", ""),
      shiny::numericInput("sample_size", "Sample Size", ""),
      shiny::tags$br(),
      shiny::tags$br(),
      shiny::actionButton("cancel", "Cancel"),
     shiny::actionButton("ok", "OK"),
      if (failed)
        shiny::div(shiny::tags$b("No group name or duplicated group name detected", style = "color: red;")),
      footer = shiny::tagList(
        shiny::tags$em("Note - This window is draggable")
      )
    )
    )
  }

#starting choice for scatterplots colours and shapes
  scatterDatShape <-  data.frame(
    shape_name = c(
      "Circle", 
      "Square",
      "Triangle" ,
      "Diamond" ,
      "Circle Cross",
      "Cross",
      "Asterisk" ,
      "Open Circle",
      "Open Square"))

   scatterDatCol <- data.frame(
    colour_name = c(
      "Orange",
      "Purple",
      "Blue",
      "Green",
      "Black", 
      "Grey", 
      "Red",
      "Brown",
      "DarkGreen"))


#dyanmic reactive dataframes
scatterChoiceShape <- shiny::reactive({
 if(!is.null(input$pch)){
  scatterChoice$pch_list <- c(scatterChoice$pch_list, input$pch)
    scatterDatShape <- subset(scatterDatShape, !(shape_name %in% scatterChoice$pch_list))
    } else {
      scatterDatShape <- scatterDatShape
    }
})

scatterChoiceCol <- shiny::reactive({
 if(!is.null(input$col)){
  scatterChoice$col_list <- c(scatterChoice$col_list, input$col)
    scatterDatCol<- subset(scatterDatCol, !(colour_name %in% scatterChoice$col_list))
    } else {
      scatterDatCol <- scatterDatCol
    }
})


#popup for scatterlot
  popupModal2 <- function(failed = FALSE) {
    shinyjqui::jqui_draggable(shiny::modalDialog(
      shiny::textInput("group", "Group Name", ""),
      shiny::numericInput("sample_size", "Sample Size", ""),
      shiny::selectInput("pch", "Shape", scatterChoiceShape()$shape_name[1], choices = scatterChoiceShape()$shape_name),
      shiny::selectInput("col", "Colour", scatterChoiceCol()$colour_name[1], choices = scatterChoiceCol()$colour_name),
      shiny::tags$br(),
      shiny::tags$br(),
      shiny::actionButton("cancel", "Cancel"),
     shiny::actionButton("ok", "OK"),
      if (failed)
        shiny::div(shiny::tags$b("No group name or duplicated group name detected", style = "color: red;")),
      footer = shiny::tagList(
        shiny::tags$em("Note - This window is draggable")
      )
    )
    )
  }

#hide all panels and call the modal on startup hide inital entry to stop spurious inputs
  shinyjs::hide("metaPlot")
  shinyjs::hide("tPanel")
  shinyjs::hide("top_well")
  shinyjs::hide("citeme")
  shiny::showModal(data_modal)
 

#importing data - if directory is missing take the user to a folderchoice, if it is provided then use that instead. Makes sure there is a / on the end
importDatapath <-  shiny::reactive({
   if(dir == "Missing"){
     shinyFiles::shinyFileChoose(input,'choosefolder',roots = c(home = '~'), session = session)
     shiny::req(input$choosefolder)
   if (is.null(input$choosefolder))
      return(NULL) 
      return(paste0(sub("/[^/]+$", "", shinyFiles::parseFilePaths(c(home = '~'), input$choosefolder)$datapath), "/"))
      } 
  else if(dir != "Missing"){
  if((substring(dir, nchar(dir)) == "/") == FALSE){
  return(paste0(dir, "/"))
} else {
  return(dir)
}
}

})

#when the datapath is not null or simply a slash, then show the other options. Allows for the starting options to be shown. Also setups folders for metaDigitise to work. Creates a temporary dettails files for the dropdown.
shiny::observe({
if(!is.null(importDatapath()) & as.character(importDatapath()) != "/"){

  image_import$extract_rev <<- TRUE

#setup the metadigitise folder and import the inital details from the datapath
metaDigitise::setup_calibration_dir(importDatapath())  
details <<- metaDigitise::dir_details(importDatapath())


#hide unfinished if none left to finish
  if(length(details$images)==length(details$doneCalFiles)){
  shiny::updateRadioButtons(session = session, inputId = "all_or_unfin_but", choices = c("All", "Finished"), selected = character(0), inline = T)
} else{}

  if(length(details$calibrations) == 0){
shiny::updateRadioButtons(session = session, inputId = "all_or_unfin_but", choices = c("All", "Unfinished"), selected = character(0), inline = T)
  }

#if length = 0, show a warning
if(length(details$image)==0){
            shinyalert::shinyalert(
          title = "Warning",
          text = "The folder you've selected has no images, please restart and provide an appropriate folder or leave this argument blank",
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
          animation = TRUE)
        shiny::stopApp()
}

  
  #hide and show relevant parts of the modal dialog
  shinyjs::hide("choosefolder")
  shinyjs::show("data_import_title2")
  shinyjs::show("data_import_title1")
  shinyjs::show("data_import_title3")
  shinyjs::show("data_all_or_unfin")
  shinyjs::show("data_select_image")
  shinyjs::show("data_rev_or_extract")
  shinyjs::show("data_import_extract")

#update the image with names of images.
  shiny::div(style = "text-align: center", offset = 0,
   shinyWidgets::updatePickerInput(
    session = session,
   inputId = "select_image_pick",
    choices = c("",details$name),
    select = "",
   clearOptions = TRUE
   ))
}
})


#show or hide different buttons based on extraction/review choice then show or hide various panels + update the switch
shiny::observeEvent(input$rev_or_extractbut, {

  shiny::req(importDatapath())

  if(input$rev_or_extractbut == FALSE){
  shinyjs::show("data_import_extract")
  shinyjs::show("extract_but")
    shinyjs::hide("review_but")
    shinyjs::hide("data_import_review")
            shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "rev_mode",
       value = FALSE
      )

  } else if(input$rev_or_extractbut == TRUE){
    shinyjs::show("review_but")
    shinyjs::show("data_import_review")
  shinyjs::hide("data_import_extract")
  shinyjs::hide("extract_but")
   shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "rev_mode",
        value = TRUE
      )
  }
  })

#obsrve the import mode selection. Either All or unfinished. Changes the metadigitise import and updates the picker 
shiny::observeEvent(input$all_or_unfin_but, {

  shiny::req(importDatapath())

  if(input$all_or_unfin_but == "All"){
  details <<- metaDigitise::dir_details(importDatapath())
  counter_total <<- length(details$paths)
  image_import$multiple <<- TRUE
  image_import$select <<- FALSE
  
  #set counters and next values at 1
  counter$countervalue <<- 1
  counter$next_count <<- 1

 shiny::div(style = "text-align: center", offset = 0,
   shinyWidgets::updatePickerInput(
    session = session,
   inputId = "select_image_pick",
    choices = c("",details$name),
    select = "",
   clearOptions = TRUE
))
  } 
    else if(input$all_or_unfin_but == "Finished"){
  details <<- metaDigitise::dir_details(importDatapath())
  done_figures <<- details$name %in% details$calibrations

    # Remove the files that are already done.
  details$images <<- details$images[done_figures]
  details$name <<- details$name[done_figures]
  details$paths <<- details$paths[done_figures]

  counter_total <<- length(details$paths)
  image_import$multiple <<- TRUE
  image_import$select <<- FALSE
  
  #set counters and next values at 1
  counter$countervalue <<- 1
  counter$next_count <<- 1

 shiny::div(style = "text-align: center", offset = 0,
   shinyWidgets::updatePickerInput(
    session = session,
   inputId = "select_image_pick",
    choices = c("",details$name),
    select = "",
   clearOptions = TRUE
))
  } 

  else if(input$all_or_unfin_but == "Unfinished"){
    details <<- metaDigitise::get_notDone_file_details(importDatapath())
    counter_total <<- length(details$paths)
    image_import$multiple <<- TRUE
    image_import$select <<- FALSE
    #set counters and next values at 1
    counter$countervalue <<- 1
    counter$next_count <<- 1

 shiny::div(style = "text-align: center", offset = 0,
   shinyWidgets::updatePickerInput(
    session = session,
   inputId = "select_image_pick",
    choices = c("",details$name),
    select = "",
   clearOptions = TRUE
))
       } else {
        image_import$multiple <<- FALSE
       }
       })


#observe event for specific image selection. If it doesnt equal nothing then import that specific photo. Also reset the choices buttons.
shiny::observeEvent(input$select_image_pick, {

shiny::req(importDatapath())

  if(input$select_image_pick != ""){
    image_import$select <<- TRUE
    details <<- metaDigitise::dir_details(importDatapath())
    image_name <<- input$select_image_pick
    details$paths <<- details$paths[match(image_name,details$name)]
    details$name <<- image_name
    counter_total <<- length(details$paths)
    
    #set counters and next values at 1
    counter$countervalue <<- 1
    counter$next_count <<- 1
    
  shiny::div(style = "text-align: center", offset = 0,
   shiny::updateRadioButtons(
   inputId = "all_or_unfin_but",
   label = NULL,
   choices = c("All", "Finished", "Unfinished"),
   selected = character(0),
   inline = TRUE
))
  image_import$multiple <<- FALSE
} else {
  image_import$select <<- FALSE
}
})

#if you click review or extract ok, update the counter, show the panels and remove the modal or show an error if you havent selected all or unfinished
shiny::observeEvent(input$extract_but|input$review_but, {

  shiny::req(importDatapath())
  shiny::req(input$extract_but|input$review_but)

  if(image_import$multiple | image_import$select){

  #output the progress text
  output$progress <- shiny::renderText({
    paste0("<font color=\"#ff3333\"><b>", counter$countervalue, "/", counter_total, "</b></font>")
  })

 shiny::removeModal() 
   shinyjs::show("tPanel")
   shinyjs::show("metaPlot")
   shinyjs::show("top_well1")
   shinyjs::show("top_well2")
   shinyjs::show("top_well3")
   shinyjs::show("top_well4")
   shinyjs::show("top_well5")
   shinyjs::show("top_well6")
   shinyjs::show("top_well7")
   shinyjs::show("citeme")
   
   if(input$rev_or_extractbut == FALSE){
     shinyjs::show("plot_well")
   } else {shinyjs::hide("plot_well")}
   

  } else {
          shinyalert::shinyalert(
          title = "Warning",
          text = "Please select graph(s) to import",
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
          animation = TRUE)
  }   
 })

  ################################################
  # Counter and initial plots
  ################################################

  # when counter value is changed changes - this changes when images are imported or all/unfinished are selected
  shiny::observeEvent(counter$countervalue, {
    
    shiny::req(importDatapath())
    shiny::req(input$all_or_unfin_but)

    # find previously extracted data:
    counter$caldat <- paste0(details$cal_dir, details$name[counter$countervalue])

    # if extracted/calibrated data already exists:
    if (file.exists(counter$caldat)) {

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
    }
     else {
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
  # Reviewing mode
  ################################################
     #what happens if you only want to review
 shiny::observeEvent(input$rev_mode|input$next_review|input$prev_review, {

   extract_mode$extract <<- FALSE
   
   shiny::req(importDatapath())

  if(input$rev_mode){
    shinyjs::hide("orient_well")
    shinyjs::hide("extract_well")
    shinyjs::hide("calib_well")
    shinyjs::hide("comm_well")
    shinyjs::hide("plot_well")
    shinyjs::show("rev_well")
    shinyjs::enable("rev_well")

if(counter$countervalue == 1){
  shinyjs::disable("prev_review")
}
    } else {
    shinyjs::hide("orient_well")
    shinyjs::hide("extract_well")
    shinyjs::hide("calib_well")
    shinyjs::hide("comm_well")
    shinyjs::show("plot_well")
    shinyjs::hide("rev_well")
}

})


  ################################################
  # Plot type and hints
  ################################################

  # record the plot type for the data file - influences clicking and hints, resets all data if plto type changes

  shiny::observeEvent(input$plot_type,{
    
    shiny::req(importDatapath())

    if (!file.exists(counter$caldat)|input$plot_type != values$plot_type && !is.null(values$plot_type)) {

    if(input$plot_type == "mean_error"){
      #container for for plotting values
      valpoints <<- shiny::reactiveValues(x = NULL, y = NULL, id = NULL, n = NULL)
    }

    if(input$plot_type == "xy_mean_error"){
      valpoints <<- shiny::reactiveValues(x = NULL, y = NULL, id = NULL, n = NULL)
    }
  
    if(input$plot_type == "boxplot"){
      valpoints <<- shiny::reactiveValues(x = NULL, y = NULL, id = NULL, n = NULL)

    }
    
    if(input$plot_type == "scatterplot"){
      valpoints <<- shiny::reactiveValues(x = NULL, y = NULL, id = NULL, n = NULL, pch=NULL, col=NULL)

    }

    if(input$plot_type == "histogram"){
      valpoints <<- shiny::reactiveValues(x = NULL, y = NULL, id = NULL, n = NULL, bar=NULL)

    }

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

        if(input$plot_type=="histogram"){
          # basic$Bar <- NA
          valpoints$bar <- NULL
        }

         if(input$plot_type=="scatterplot"){
          basic$Shape <- NA
          basic$Colour <- NA

          valpoints$pch <- NULL
          valpoints$col <- NULL
        }

        values$plot_type <<- input$plot_type


       output$metaPlot <- shiny::renderPlot({
        graphics::par(mar = c(0, 0, 0, 0))
        plot_values <- shiny::reactiveValuesToList(values)
        do.call(internal_redraw, c(plot_values, shiny=TRUE))
      })
       
       } else{
        # otherwise read in the data that already exists from the raw data but dont allow it to aggregate if youve deleted a row.
          if(values$plot_type=="scatterplot"){
                        valpoints <<- shiny::reactiveValues(x = NULL, y = NULL, id = NULL, n = NULL, pch=NULL, col=NULL)
           raw_dat <- as.data.frame(values$raw_data)
                   if(nrow(raw_dat)>0){
          raw_dat_sum <- raw_dat |>
                        dplyr::group_by(id, pch, col) |>
                        dplyr::summarize(n = unique(n))
          names(raw_dat_sum) <- c("Group_Name", "Sample_Size", "Shape", "Colour")
        mod_df$x <- raw_dat_sum
        row_count$x <- nrow(raw_dat_sum)
        valpoints$x <- values$raw_data$x
        valpoints$y <- values$raw_data$y
        valpoints$id <- values$raw_data$id
        valpoints$n <- values$raw_data$n
         valpoints$pch <- values$raw_data$pch
          valpoints$col <- values$raw_data$col   
      }
      } 
      if(values$plot_type=="histogram"){
              valpoints <<- shiny::reactiveValues(x = NULL, y = NULL, id = NULL, n = NULL, bar=NULL)
        raw_dat <- as.data.frame(values$raw_data)
        if(nrow(raw_dat)>0){
                raw_dat_sum <- raw_dat |>
                        dplyr::group_by(id) |>
                        dplyr::summarize(n = unique(n))
        names(raw_dat_sum) <- c("Group_Name", "Sample_Size")
        mod_df$x <- raw_dat_sum
        row_count$x <- nrow(raw_dat_sum)
        valpoints$x <- values$raw_data$x
        valpoints$y <- values$raw_data$y
        valpoints$id <- values$raw_data$id
        valpoints$n <- values$raw_data$n
          valpoints$bar <- values$raw_data$bar

}
}  else {
        raw_dat <- as.data.frame(values$raw_data)
        valpoints <<- shiny::reactiveValues(x = NULL, y = NULL, id = NULL, n = NULL)
        if(nrow(raw_dat)>0){
        raw_dat_sum <- raw_dat |>
                        dplyr::group_by(id) |>
                        dplyr::summarize(n = unique(n))
        names(raw_dat_sum) <- c("Group_Name", "Sample_Size")
        mod_df$x <- raw_dat_sum
        row_count$x <- nrow(raw_dat_sum)
        valpoints$x <- values$raw_data$x
        valpoints$y <- values$raw_data$y
        valpoints$id <- values$raw_data$id
        valpoints$n <- values$raw_data$n
        }
    }
  }

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

shiny::observe({
if(!is.null(importDatapath()) & as.character(importDatapath()) != "/" & counter$countervalue >=1){
  #record cex used (adjusted with slider)

  shiny::observe(values$cex <<- input$cex)

  shiny::observe(values$pos <<- input$pos)

  shiny::observe(values$error_type <<- input$errortype)
}
})

  ################################################
  # Zoom
  ################################################
  
  shiny::observeEvent(input$zoom,{

    shiny::req(importDatapath())
    shiny::req(input$all_or_unfin_but)
    
    if(input$zoom){
  values$zoom_coords <<- c(input$plot_brush$xmin,input$plot_brush$xmax,input$plot_brush$ymin,input$plot_brush$ymax)
   output$metaPlot <- shiny::renderPlot({
     graphics::par(mar = c(0, 0, 0, 0))
  plot_values <- shiny::reactiveValuesToList(values)
  do.call(internal_redraw, c(plot_values, shiny = TRUE))
  })
 session$resetBrush("plot_brush")
  shinyjs::runjs("document.getElementById('plot_brush').remove()")
    }
    })

  ################################################
  # Flip
  ################################################

  # record whether we flip the image or not
  shiny::observeEvent(input$flip, {

    shiny::req(importDatapath())
    shiny::req(input$all_or_unfin_but)

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
     
    shiny::req(importDatapath())
    shiny::req(input$all_or_unfin_but)

    if (input$rotate_mode) {

      # if we are in rotate mode - toggle extract mode and calib mode off.
      shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "calib_mode",
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

  # if calib mode button pressed
  shiny::observeEvent(input$calib_mode, {
     
    shiny::req(importDatapath())
    shiny::req(input$all_or_unfin_but)

    # resent click counter
    clickcounter$clickcount <- 0
    
    # show the calib_data object (where we enter names and values for axis).
    # shinyjs::toggle(id = "calib_data")

    shiny::updateTextInput(session,"y_var", value="")
    shiny::updateTextInput(session,"x_var", value="")
    shiny::updateNumericInput(session,"x1", value="")
    shiny::updateNumericInput(session,"x2", value="")
    shiny::updateNumericInput(session,"y1", value="")
    shiny::updateNumericInput(session,"y2", value="")


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
        " <b> <i>Double</i> click on known values on axes in this order: <br/>
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

      # if the plot type is me or bp then store two clicks, otherwise store 4 clicks and update the clickcounter.
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
        values$variable <<- c(y=input$y_var,x=input$x_var)
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
  # Extraction - using input
  ################################################
   #hint for extraction step

#update switches
  shiny::observeEvent(extract_mode$extract, {
 
    shiny::req(importDatapath())
    shiny::req(input$all_or_unfin_but)

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
        shinyjs::enable("add_group")
        #this is so it doesnt immediately plot a new row.
        mod_df$x <- basic[-nrow(basic),]

        row_count$x <- 0

        valpoints$x <- NULL
        valpoints$y <- NULL
        valpoints$id <- NULL
        valpoints$n <- NULL

        if(input$plot_type=="histogram"){
          # basic$Bar <- NA
          valpoints$bar <- NULL
          shinyjs::disable("add_group")
          row_count$x <- row_count$x + 1
          mod_df$x <- rbind(mod_df$x,
                            data.frame(
                              Group_Name = "Group1",
                              Sample_Size = NA
                            )
          )
        }

        if(input$plot_type=="scatterplot"){
          shinyjs::enable("add_group")
          basic$Shape <- NA
          basic$Colour <- NA

          valpoints$pch <- NULL
          valpoints$col <- NULL
        }

      } else {
        # otherwise read in the data that already exists from the raw data but dont allow it to aggregate if youve deleted a row.
        if(input$plot_type=="scatterplot"){
           shinyjs::enable("add_group")
          raw_dat <- as.data.frame(values$raw_data)
          if(nrow(raw_dat)>0){
            raw_dat_sum <- raw_dat |>
                        dplyr::group_by(id, pch, col) |>
                        dplyr::summarize(n = unique(n))
            names(raw_dat_sum) <- c("Group_Name", "Sample_Size", "Shape", "Colour")
            mod_df$x <- raw_dat_sum
            row_count$x <- nrow(raw_dat_sum)
            valpoints$x <- values$raw_data$x
            valpoints$y <- values$raw_data$y
            valpoints$id <- values$raw_data$id
            valpoints$n <- values$raw_data$n
            valpoints$pch <- values$raw_data$pch
            valpoints$col <- values$raw_data$col   
          } 
        } 
        if(input$plot_type=="histogram"){
          shinyjs::disable("add_group")

          raw_dat <- as.data.frame(values$raw_data)
          if(nrow(raw_dat)>0){
              raw_dat_sum <- raw_dat |>
                  dplyr::group_by(id) |>
                  dplyr::summarize(n = unique(n))
            names(raw_dat_sum) <- c("Group_Name", "Sample_Size")
            mod_df$x <- raw_dat_sum
            row_count$x <- nrow(raw_dat_sum)
            valpoints$x <- values$raw_data$x
            valpoints$y <- values$raw_data$y
            valpoints$id <- values$raw_data$id
            valpoints$n <- values$raw_data$n
            valpoints$bar <- values$raw_data$bar
          }
        }else{
          shinyjs::enable("add_group")
          raw_dat <- as.data.frame(values$raw_data)
          if(nrow(raw_dat)>0){
            raw_dat_sum <- raw_dat |>
                            dplyr::group_by(id) |>
                            dplyr::summarize(n = unique(n))
            names(raw_dat_sum) <- c("Group_Name", "Sample_Size")
            mod_df$x <- raw_dat_sum
            row_count$x <- nrow(raw_dat_sum)
            valpoints$x <- values$raw_data$x
            valpoints$y <- values$raw_data$y
            valpoints$id <- values$raw_data$id
            valpoints$n <- values$raw_data$n
          }
        }
      }

      # this is then rendered in a DT table.
      if(input$plot_type=="histogram"){
        output$group_table <- DT::renderDT({
          DT::datatable(
            mod_df$x,
            editable = list(target = "cell", disable = list(columns = c(1,2,3))),
            selection = list(mode = 'single', selected = c(1)),
            options = list(scrollX = TRUE,lengthChange = TRUE, dom = "t", pageLength = 100)
          )
        })
                output$group_table2 <- DT::renderDT({
          DT::datatable(
            mod_df$x,
            editable = list(target = "cell"),
            selection = list(mode = 'single', selected = c(1)),
            options = list(scrollX = TRUE,lengthChange = TRUE, dom = "t", pageLength = 100)
          )
        })
      }else{
        output$group_table <- DT::renderDT({
          DT::datatable(
            mod_df$x,
            editable = list(target = "cell", disable = list(columns = c(1,2,3))),
            selection = "single",
            options = list(scrollX = TRUE,lengthChange = TRUE, dom = "t", pageLength = 100)
          )
        })
                output$group_table2 <- DT::renderDT({
          DT::datatable(
            mod_df$x,
            editable = list(target = "cell"),
            selection = "single",
            options = list(scrollX = TRUE,lengthChange = TRUE, dom = "t", pageLength = 100)
          )
        })
      }
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

  # when you click add group a popup appears which asks you to add group and sample size.
  # this is then added onto the raw data.
  # the row count increases and another row is then added after.
  shiny::observeEvent(input$add_group, {
    
    if(input$plot_type == "scatterplot"){
      shiny::showModal(popupModal2())
      row_count$x <- row_count$x + 1
      mod_df$x <- rbind(mod_df$x,
        data.frame(
         Group_Name = NA,
         Sample_Size = NA,
         Shape = 19,
         Colour = "Orange"
       )
        )
    } 
    else{
      shiny::showModal(popupModal1()) 
      row_count$x <- row_count$x + 1
      mod_df$x <- rbind(mod_df$x,
        data.frame(
           Group_Name = NA,
           Sample_Size = NA
         )
      )
    }
    })
    

#what happens when you press ok
  shiny::observeEvent(input$ok, {

    if (!is.null(input$group) && nzchar(input$group)){
      shiny::removeModal()
    } else {
      if(input$plot_type == "scatterplot"){
        shiny::showModal(popupModal2(failed = TRUE))
      }else{
        shiny::showModal(popupModal1(failed = TRUE))      
      }
      
    }
  })

#what happens when you prss cancel
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
    if(is.null(selected$row)|length(selected$row) == 0 | nrow(mod_df$x) == 0 ){
        shinyjs::disable("del_group")
        shinyjs::disable("click_group")
        shinyjs::disable("edit_group")
      shinyalert::shinyalert(
        title = "Select a group to plot",
        text = "No group has been selected or created",
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

          if(input$plot_type=="scatterplot"){
            valpoints$pch <- values$raw_data$pch
            valpoints$col <- values$raw_data$col
          }

          if(input$plot_type=="histogram"){
            valpoints$bar <- values$raw_data$bar
          }

        }else{
          values$raw_data <<-NULL
        }
      }
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
    shiny::req(importDatapath())
    shiny::req(input$all_or_unfin_but)

    if (add_mode$add & extract_mode$extract & !is.null(selected$row)) {
      plotcounter$plotclicks <- plotcounter$plotclicks + 1
      dat_mod <- as.data.frame(shiny::reactiveValuesToList(mod_df))
      max_clicks <-
        ifelse(input$plot_type == "mean_error",2,
        ifelse(input$plot_type == "xy_mean_error",3,
        ifelse(input$plot_type == "boxplot",5,
        ifelse(input$plot_type == "scatterplot",1000,
        ifelse(input$plot_type == "histogram",1000,
           NA)))))
      if (plotcounter$plotclicks <= max_clicks) {
        valpoints$x <- c(valpoints$x, input$plot_click2$x)
        valpoints$y <- c(valpoints$y, input$plot_click2$y)
        valpoints$id <- c(valpoints$id, dat_mod[selected$row, 1])
        valpoints$n <- c(valpoints$n, dat_mod[selected$row, 2])
        if(input$plot_type=="scatterplot"){
          valpoints$pch <- c(valpoints$pch, dat_mod[selected$row, 3])
          valpoints$col <- c(valpoints$col, dat_mod[selected$row, 4])
        }
        if(input$plot_type=="histogram"){
          valpoints$bar <- c(valpoints$bar, ceiling((plotcounter$plotclicks)/2))
        }

      }
      if (plotcounter$plotclicks == max_clicks) {
        add_mode$add <- FALSE
        selected$row <<- NULL
        DT::selectRows(proxy, selected = NULL)
      }

      values$raw_data <<- as.data.frame(shiny::reactiveValuesToList(valpoints))
      if(!input$plot_type=="histogram" | is.even(plotcounter$plotclicks)){


       output$metaPlot <- shiny::renderPlot({
        graphics::par(mar = c(0, 0, 0, 0))
        plot_values <- shiny::reactiveValuesToList(values)
        do.call(internal_redraw, c(plot_values, shiny=TRUE))
      })
      }


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
    shiny::req(importDatapath())
    shiny::req(input$all_or_unfin_but)
      if(is.null(selected$row)|length(selected$row) == 0 | nrow(mod_df$x) == 0 ){
        shinyjs::disable("del_group")
        shinyjs::disable("click_group")

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
        if(input$plot_type=="scatterplot"){
          valpoints$pch <- values$raw_data$pch
          valpoints$col <- values$raw_data$col
        }
        if(input$plot_type=="histogram"){
          valpoints$bar <- values$raw_data$bar
            shinyjs::enable("add_group")
        }
        mod_df$x <- mod_df$x[-selected$row, ]
            if(nrow(mod_df$x) == 0 ){
        shinyjs::disable("del_group")
        shinyjs::disable("click_group")
      }
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

  shiny::observeEvent(input$pch, {
    if(input$pch == "Circle"){
      mod_df$x[row_count$x,3] <<- 19
    } else if(input$pch == "Square"){
      mod_df$x[row_count$x,3] <<- 15
    } else if(input$pch == "Triangle"){
      mod_df$x[row_count$x,3] <<- 17
    } else if(input$pch == "Diamond"){
      mod_df$x[row_count$x,3] <<- 18
    } else if(input$pch == "Circle Cross"){
      mod_df$x[row_count$x,3] <<- 13
    } else if(input$pch == "Cross"){
      mod_df$x[row_count$x,3] <<- 4
    } else if(input$pch == "Asterisk"){
      mod_df$x[row_count$x,3] <<- 8
    } else if(input$pch == "Open Circle"){
      mod_df$x[row_count$x,3] <<- 1
    } else if(input$pch == "Open Square"){
      mod_df$x[row_count$x,3] <<- 0
    }
})

  shiny::observeEvent(input$col, {
    mod_df$x[row_count$x,4] <<- input$col
  })



  #edit data with DT input (not using)
  shiny::observeEvent(input$group_table_cell_edit, {
    change_string <-  as.character(mod_df$x[selected$row,1])
    mod_df$x <-  DT::editData(mod_df$x, input$group_table_cell_edit)
    if(is.null(values$raw_data)){
    } else {values$raw_data[grepl(change_string, values$raw_data[[2]]),][[3]] <- input$group_table_cell_edit$value
}
  })
  
  shiny::observeEvent(input$group_table_row_last_clicked, {
    clicked$row <- c(clicked$row,input$group_table_row_last_clicked)
    if(any(duplicated(clicked$row))){
      clicked$row <<- NULL
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
  # Editing group info
  ################################################

    popupModal3 <- function(failed = FALSE) {
    shinyjqui::jqui_draggable(shiny::modalDialog(
     DT::DTOutput("group_table2"),
      shiny::tags$br(),
      shiny::tags$br(),
     shiny::actionButton("close", "Close"),
      footer = shiny::tagList(
        shiny::tags$em("Note - This window is draggable")
      )
    )
    )
  }
    shiny::observeEvent(input$edit_group, {
      shiny::showModal(popupModal3())
    })

    shiny::observeEvent(input$close, {
    values$raw_data <<- as.data.frame(shiny::reactiveValuesToList(valpoints))
      output$metaPlot <- shiny::renderPlot({
      graphics::par(mar = c(0, 0, 0, 0))
      plot_values <- shiny::reactiveValuesToList(values)
      do.call(internal_redraw, c(plot_values, shiny=TRUE))
    })
      shiny::removeModal()
    })

    shiny::observeEvent(input$group_table2_cell_edit, {
      info <- input$group_table2_cell_edit
      i <- info$row
      j <- info$col
      v <- info$value
     mod_df$x[i, j] <<- DT::coerceValue(v, mod_df$x [i, j])
  #update mod_df data with edited info
      dat_mod <- as.data.frame(shiny::reactiveValuesToList(mod_df))
        valpoints$id <- dat_mod[i, 1]
        valpoints$n <- dat_mod[i, 2]
     if(input$plot_type=="scatterplot"){
          valpoints$pch <- dat_mod[i, 3]
          valpoints$col <- dat_mod[i, 4]
        }
DT::replaceData(proxy, mod_df$x)
    })

  ################################################
  # Comments
  ################################################

  # record comments
  shiny::observeEvent(input$comment,{
    shiny::req(importDatapath())
    shiny::req(input$all_or_unfin_but)
    values$comment <<- input$comment
  })
  
  ################################################
  # Record extraction file
  ################################################
  shiny::observeEvent(input$take_screenshot, {
    shinyscreenshot::screenshot(selector = "plot",
                                filename = paste0("Extraction_imgrecord: ", details$name[counter$countervalue]),
                                id = "metaPlot", scale = 2)
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
        shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "rev_mode",
        value = FALSE
      )

    shinyjs::hide("comm_well")
    shinyjs::show("plot_well")
    shinyjs::reset("y_var_input")
    shinyjs::reset("x_var_input")
    shinyjs::reset("y_coord_input")
    shinyjs::reset("y_coord_input")
    shiny::updateTextInput(session,"y_var_input", value="")
    shiny::updateTextInput(session,"x_var_input", value="")
    shiny::updateTextInput(session,"comment", value="")


    shiny::req(importDatapath())
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

    shiny::observeEvent(input$next_review, {
         shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "rev_mode",
        value = TRUE
      )
  
    plot_values <- shiny::reactiveValuesToList(values)
      cv <- counter$countervalue + 1
      counter$next_count <- counter$next_count + 1

      if (cv > counter_total) {
        counter$countervalue <- counter_total
        shinyalert::shinyalert(
          title = "Congratulations!",
          text = "You've finished reviewing!",
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
          animation = TRUE)

        shinyjs::disable("next_review")
        
      } else {
        counter$countervalue <- cv
      }
  })

     shiny::observeEvent(input$prev_review, {
         shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "rev_mode",
        value = TRUE
      )
       
    plot_values <- shiny::reactiveValuesToList(values)
      cv <- counter$countervalue - 1
      counter$next_count <- counter$next_count + 1

      if (cv <1) {
        counter$countervalue <- 1
      } else {
        counter$countervalue <- cv
      }

  })


  ################################################
  # Previous/next step buttons
  ################################################

  shiny::observeEvent(input$plot_step, {
    shiny::req(importDatapath())
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
    shinyWidgets::updateSwitchInput(session, "calib_mode", value = FALSE)
              if(input$plot_type == "mean_error"){
      output$plothintmean <- shiny::renderUI({ 
        shiny::tags$strong("Double-Click on the Error Bar, followed by the Mean")
      })
      shinyjs::hide("hint_xy")
      shinyjs::hide("hint_box")
      shinyjs::hide("hint_scatter")
      shinyjs::show("hint_mean")
      shinyjs::hide("hint_hist")
    }
    
    if(input$plot_type == "xy_mean_error"){
      output$plothintxy <- shiny::renderUI({ 
        shiny::tags$strong("Double-Click on the Y Error Bar, followed by the Mean, followed by the X Error Bar")
      })
      shinyjs::hide("hint_mean")
      shinyjs::hide("hint_box")
      shinyjs::hide("hint_scatter")
      shinyjs::show("hint_xy")
      shinyjs::hide("hint_hist")
    }
    
    if(input$plot_type == "boxplot"){
      output$plothintbox <- shiny::renderUI({ 
        shiny::tags$strong("Double-Click on the Max, Upper Q, Median, Lower Q, and Minimum in that order")
      })
      shinyjs::hide("hint_xy")
      shinyjs::hide("hint_scatter")
      shinyjs::hide("hint_mean")
      shinyjs::show("hint_box")
      shinyjs::hide("hint_hist")
    }
    
    if(input$plot_type == "scatterplot"){
      output$plothintscatter <- shiny::renderUI({ 
        shiny::tags$strong("Double-Click on points you want to add")
      })
      shinyjs::hide("hint_xy")
      shinyjs::show("hint_scatter")
      shinyjs::hide("hint_mean")
      shinyjs::hide("hint_box")
      shinyjs::hide("hint_hist")
    }
    
    if(input$plot_type == "histogram"){
      output$plothinthist <- shiny::renderUI({ 
        shiny::tags$strong("Double-Click on the left followed by the right upper corners of each bar")
      })
      shinyjs::hide("hint_xy")
      shinyjs::hide("hint_scatter")
      shinyjs::hide("hint_mean")
      shinyjs::hide("hint_box")
      shinyjs::show("hint_hist")
    }
  })
  
  shiny::observeEvent(input$extract_step, {
    shinyjs::show("comm_well")
    shinyjs::hide("extract_well")
    extract_mode$extract <<- FALSE
    selected$row <<- NULL
    shinyjs::hide("hint_xy")
    shinyjs::hide("hint_scatter")
    shinyjs::hide("hint_mean")
    shinyjs::hide("hint_box")
    shinyjs::hide("hint_hist")
  })
  
  shiny::observeEvent(input$orient_back, {
    shinyjs::show("plot_well")
    shinyjs::hide("orient_well")
  })
  
  shiny::observeEvent(input$calib_back, {
    shinyjs::show("orient_well")
    shinyjs::hide("calib_well")
    shinyWidgets::updateSwitchInput(session, "calib_mode", value = FALSE)
  })
  
  shiny::observeEvent(input$extract_back, {
    shinyjs::show("calib_well")
    shinyjs::hide("extract_well")
    extract_mode$extract <<- FALSE
    selected$row <<- NULL
    shinyjs::hide("hint_xy")
    shinyjs::hide("hint_scatter")
    shinyjs::hide("hint_mean")
    shinyjs::hide("hint_box")
    shinyjs::hide("hint_hist")
  })
  
  shiny::observeEvent(input$comm_back, {
    shinyjs::show("extract_well")
    extract_mode$extract <<- TRUE
    shinyjs::hide("comm_well")
              if(input$plot_type == "mean_error"){
      output$plothintmean <- shiny::renderUI({ 
        shiny::tags$strong("Double-Click on the Error Bar, followed by the Mean")
      })
      shinyjs::hide("hint_xy")
      shinyjs::hide("hint_box")
      shinyjs::hide("hint_scatter")
      shinyjs::show("hint_mean")
      shinyjs::hide("hint_hist")
    }
    
    if(input$plot_type == "xy_mean_error"){
      output$plothintxy <- shiny::renderUI({ 
        shiny::tags$strong("Double-Click on the Y Error Bar, followed by the Mean, followed by the X Error Bar")
      })
      shinyjs::hide("hint_mean")
      shinyjs::hide("hint_box")
      shinyjs::hide("hint_scatter")
      shinyjs::show("hint_xy")
      shinyjs::hide("hint_hist")
    }
    
    if(input$plot_type == "boxplot"){
      output$plothintbox <- shiny::renderUI({ 
        shiny::tags$strong("Double-Click on the Max, Upper Q, Median, Lower Q, and Minimum in that order")
      })
      shinyjs::hide("hint_xy")
      shinyjs::hide("hint_scatter")
      shinyjs::hide("hint_mean")
      shinyjs::show("hint_box")
      shinyjs::hide("hint_hist")
    }
    
    if(input$plot_type == "scatterplot"){
      output$plothintscatter <- shiny::renderUI({ 
        shiny::tags$strong("Double-Click on points you want to add")
      })
      shinyjs::hide("hint_xy")
      shinyjs::show("hint_scatter")
      shinyjs::hide("hint_mean")
      shinyjs::hide("hint_box")
      shinyjs::hide("hint_hist")
    }
    
    if(input$plot_type == "histogram"){
      output$plothinthist <- shiny::renderUI({ 
        shiny::tags$strong("Double-Click on the left followed by the right upper corners of each bar")
      })
      shinyjs::hide("hint_xy")
      shinyjs::hide("hint_scatter")
      shinyjs::hide("hint_mean")
      shinyjs::hide("hint_box")
      shinyjs::show("hint_hist")
    }
  })
    

  ################################################
  # What happens when you quit
  ################################################

  #the app stops when you exit - not sure what this does.
  session$onSessionEnded(function() {
    shiny::isolate(shiny::stopApp(returnValue=metaDigitise::getExtracted(importDatapath())))
    shiny::isolate(utils::write.csv(metaDigitise::getExtracted(importDatapath()), paste0(importDatapath(),"ExtractedData.csv")))

  })


}
