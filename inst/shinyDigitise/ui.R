
ui <- function(){
  #various starting functions with theme and fonts
  shiny::fluidPage(
    shinyjs::useShinyjs(),
    theme = bslib::bs_theme(
      primary = "#fe9807", secondary = "#7393B3", 
      info = "#E51C23", font_scale = NULL, bootswatch = "materia",
      base_font = bslib::font_collection(bslib::font_google("Atkinson Hyperlegible"), "Arial Narrow Bold", "sans-serif"),
      code_font = bslib::font_collection(bslib::font_google("Atkinson Hyperlegible"), "Arial Narrow Bold", "sans-serif")),
    
    #header pnael, with SD logo as an action button that activates the citing function button
    #contains a switch for review or extract
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::titlePanel(
          title=shiny::splitLayout(cellWidths = c("40%","50%"),
                                   tippy::with_tippy(shiny::actionButton(
                                     inputId = "citeme", 
                                     style="color: white; background-color: white; border-color: white; box-shadow: 0px 0px 0px 0px white;",
                                     label =  shiny::tags$img(src = "img/shinyDigitise.png", height = "88px", width = "80px")),"Click me!"),

    #contains a switch for review or extract
          shinyjs::hidden(shiny::div(id = "top_well7",
            tippy::with_tippy(shinyWidgets::switchInput(
   inputId = "rev_mode",
    labelWidth = "80px",
    value = FALSE,
    onLabel = "Review Mode",
    offLabel = "Extract Mode"
), "Click here to either review or extract data")
            ))
        ),
        windowTitle = "shinyDigitise")
      ),

#progress number based on counter and counter total
      shiny::column(width = 8,
                    shiny::tags$br(),
        shiny::div(style="display: inline-block;vertical-align:top; width: 7%; font-size:x-large;",
           shiny::p(shiny::htmlOutput("progress", inline=TRUE)),
        ),

        #point size for plotting
        shinyjs::hidden(shiny::div(id = "top_well4", style="display: inline-block;vertical-align:top; width: 10%;",shiny::strong("Point size:"))),
        shinyjs::hidden(shiny::div(id = "top_well5", style="display: inline-block;vertical-align:top; width: 15%;",
        shiny::sliderInput(
           inputId = "cex",
           label = NULL,
           value = 1,
           min = 0.1,
           max = 3,
           ticks  = FALSE))),  

           #position of the label group name      
        shinyjs::hidden(shiny::div(id = "top_well1", style="display: inline-block;vertical-align:top; width: 10% ")),
        shinyjs::hidden(shiny::div(id = "top_well2", style="display: inline-block;vertical-align:top; width: 15%;",shiny::strong("Group Name Position:"))),
        shinyjs::hidden(shiny::div(id = "top_well3", style="display: inline-block;vertical-align:top;  width: 15%;",
           shinyWidgets::prettyRadioButtons(
             inputId = "pos",
             label = NULL,
             choiceNames = c("right", "top"),
             choiceValues = c("right", "top"),
             inline = T,
             icon = shiny::icon("check"),
             bigger = TRUE,
             status = "info",
             animation = "jelly"
           )
        )),

        #zoom button, allows for zooming
        shinyjs::hidden(shiny::div(id = "top_well6",style="display: inline-block;vertical-align:top;  width: 20%;",
        shiny::actionButton(
        inputId = "zoom",
         label = "Zoom"),
        tippy::tippy_this("zoom", "Drag area to zoom then press here! To Reset, click here when no box is present")
      )),
      )
    ),
    
    # Name of the sidebar
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        id = "tPanel",
        style = "overflow-y:scroll; max-height: 800px; position:relative;",
        

        #image name
        shiny::verbatimTextOutput("image_name"),

        ####------------------ 
        ### review panel
        ####------------------
        #take a screenshot, previous or next review.
          shiny::wellPanel(
          shinyjs::hidden(
            shiny::div(id = "rev_well",
            shiny::actionButton("take_screenshot", "Download Extraction Figure"),
            shiny::tags$br(),
            shiny::tags$br(),
            shiny::splitLayout(
              shiny::actionButton("prev_review", "Previous"),
              shiny::actionButton("next_review", "Next"))
        )
          )
        ),
        ####------------------ 
        ### Plot Type Panel
        ####------------------
        #The various plot types
        shiny::wellPanel(
          shiny::splitLayout(
            cellWidths = c("80%","20%"),
          
          shiny::strong("1. Choose Plot type:"),
          shiny::textOutput("plottype_check_text"),
          shiny::tags$head(shiny::tags$style("#plottype_check_text{font-size: 20px;}"))

          ),
          shinyjs::hidden(shiny::div(id = 'plot_well',
          shinyWidgets::prettyRadioButtons(
            inputId = "plot_type",
            label = NULL,
            choiceNames = c("Mean/error", "Boxplot", "XY Mean/Error", "Scatterplot", "Histogram"),
            choiceValues = c("mean_error", "boxplot", "xy_mean_error", "scatterplot", "histogram"),
            selected = character(0),
            inline = T,
            icon = shiny::icon("check"),
            bigger = TRUE,
            status = "info",
            animation = "jelly"
          ),
      
          shiny::actionButton(
            inputId = "plot_step",
            label = "Next step",
            style = "padding:4px"
          ),))
        ),
        
        ####------------------ 
        ### Orientation Panel
        ####------------------
        #Zoom or roation panels. 
        shiny::wellPanel(
          shiny::splitLayout(
                cellWidths = c("80%","20%"),
          shiny::strong("2. Orientate Figure:"),
          shiny::textOutput("orientation_check_text"),
          shiny::tags$head(shiny::tags$style("#orientation_check_text{font-size: 20px;}"))
          ),
          shinyjs::hidden(shiny::div(id = "orient_well",class = "buttonagency",
                shiny::tags$br(),
                shiny::strong("All graph types should be vertically orientated."), shiny::tags$br(),
                shiny::strong("If they are not then chose flip to correct this."), shiny::tags$br(), 
                shiny::strong("If figures are wonky, chose rotate."),shiny::tags$br(),
                shiny::tags$br(),
                shiny::strong("Otherwise press next."),shiny::tags$br(),
              shiny::tags$br(),
              shiny::splitLayout(  
              shinyWidgets::switchInput(
                  inputId = "flip",
                  label = shiny::strong("Flip"),
                  labelWidth = "100px",
                  onLabel = "Yes",
                  offLabel = "No"
                ),
              shinyWidgets::switchInput(
                inputId = "rotate_mode",
                label = shiny::strong("Rotate"),
                labelWidth = "100px",
                onLabel = "Yes",
                offLabel = "No"
              )),
              shiny::splitLayout(shiny::tags$br(),
              shiny::textOutput("rotation", inline=TRUE),
              ),
              shiny::tags$br(),
              shinyjs::hidden(
                shiny::div(id="togslide",
                    shinyWidgets::sliderTextInput(
                      inputId = "rotate",
                      label = NULL,
                      choices = seq(from = -45, to = 45, by = 5),
                      selected = 0,
                      grid = T
                    )
                )
              ),
              shiny::splitLayout(
              shiny::actionButton(
                inputId = "orient_back",
                label = "Previous step",
                style = "padding:4px"
              ),
              shiny::actionButton(
                inputId = "orient_step",
                label = "Next step",
                style = "padding:4px"
              )
              )
          )
        )),
        
        ####------------------ 
        ### Calibrate Panel
        ####------------------
        #calibration panel with specific inputs based on plot type selected. 
        shiny::wellPanel(
          shiny::splitLayout(
            cellWidths = c("80%","20%"),
            shiny::strong("3. Calibrate Axes:"),
            shiny::textOutput("calibrate_check_text"),
            shiny::tags$head(shiny::tags$style("#calibrate_check_text{font-size: 20px;}"))
          ),
          shinyjs::hidden(
            shiny::div(id = "calib_well",
           shinyWidgets::switchInput(
              inputId = "calib_mode",
              label = shiny::strong("Calibrate mode"),
              labelWidth = "100px",
              onLabel = "Yes",
              offLabel = "No",
              onStatus = "primary",
            ),
           shiny::htmlOutput("calib_info"),
          shinyjs::hidden(
            shiny::div(id="y_var_input",
                shiny::textInput(inputId = "y_var",
                          label = NULL,
                          placeholder = "Y Variable")
                
            )
          ),
          shinyjs::hidden(
            shiny::div(id="x_var_input",
                shiny::textInput(inputId = "x_var",
                          label = NULL,
                          placeholder = "X Variable")
                
            )
          ),
          shinyjs::hidden(
            shiny::div(id="y_coord_input",
                shiny::splitLayout(
                  cellWidths = c("15%","35%","15%","35%"),
                  "Y1",
                  shiny::numericInput(inputId = "y1",
                               label = NULL,
                               value= NA),
                  "Y2",
                  shiny::numericInput(inputId = "y2",
                               label= NULL,
                               value= NA )
                )
            )
          ),
          shinyjs::hidden(
            shiny::div(id="x_coord_input",
                shiny::splitLayout(
                  cellWidths = c("15%","35%","15%","35%"),
                  "X1",
                  shiny::numericInput(inputId = "x1",
                               label = NULL,
                               value= NA),
                  "X2",
                  shiny::numericInput(inputId = "x2",
                               label= NULL,
                               value= NA )
                )
            )
          ),
          shinyjs::hidden(
            shiny::div(id="log_input",
               shinyWidgets::prettyCheckbox(
                  inputId = "log_sp",
                  label = "Logged values?",
                  value = FALSE,
                  status = "info"),
                shiny::textInput(
                  inputId = "nsamp_sp",
                  placeholder = "Known sample size",
                  label = NULL)
            )
          ),
          shiny::splitLayout(
            shiny::actionButton(
              inputId = "calib_back",
              label = "Previous step",
              style = "padding:4px"
            ),
            shiny::actionButton(
              inputId = "calib_step",
              label = "Next step",
              style = "padding:4px"
            )
          )
        ))
        ),
        
        ####------------------ 
        ### Extraction Panel
        ####------------------
         #  1. Group names and sample size should be entered into the table on the sidebar before points are added.
         #  2. To add points to a group, first click the group on the sidebar then click 'Add Points'.
         #  3. To delete a group, click on the desired group in the table on the sidebar then press 'Delete Group'."
         #  4. With appropriate error type for each model. 

        shiny::wellPanel(
          shiny::splitLayout(
            cellWidths = c("80%","20%"),
            shiny::strong("4. Extract Data:"),
            shiny::textOutput("extract_check_text"),
            shiny::tags$head(shiny::tags$style("#extract_check_text{font-size: 20px;}"))
          ),
          shinyjs::hidden(
            shiny::div(id = "extract_well",
           shinyjs::hidden(
            shiny::div(id = "group_data",
                       shiny::tags$br(),
        shiny::strong("1. Click add groups to enter group names and sample size before adding points."), shiny::tags$br(),
        shiny::strong("2. To add points, click the group in the table then press 'Click Points' and then proceed to double-Click points."), shiny::tags$br(),
        shiny::strong("3. To delete a group, click on the desired group in the table and then press 'Delete Group'."), shiny::tags$br(),
        shiny::tags$br(),
              shiny::splitLayout(
                shiny::div(class = "buttonagency",
                    shiny::actionButton(
                      inputId = "add_group",
                      label = "Add Group"
                    ),
                    shiny::actionButton(
                      inputId = "click_group",
                      label = "Click Points"
                    ),
                    shiny::actionButton(
                      inputId = "del_group",
                      label = "Delete Group"
                    )
                  )
                ),
        shiny::tags$br(),
                DT::DTOutput("group_table")
           )),
        shiny::tags$br(),
          shinyjs::hidden(
            shiny::div(id = "error_type_select",
              shinyWidgets::prettyRadioButtons(
                inputId = "errortype",
                label = shiny::strong("Type of error:"),
                choiceNames = c("SE", "95%CI", "SD"),
                choiceValues = c("se","CI95","sd"),
                inline = T,
                icon = shiny::icon("check"),
                bigger = TRUE,
                status = "danger",
                animation = "jelly"
              )
            )
          ),
          shiny::splitLayout(
            shiny::actionButton(
              inputId = "extract_back",
              label = "Previous step",
              style = "padding:4px;"
            ),
            shiny::actionButton(
              inputId = "extract_step",
              label = "Next step",
              style = "padding:4px"
            )
          )
          )
          )
        ),
        
        ####------------------ 
        ### comment panel
        ####------------------
        #Comment panel w/ previous and conitnue buttons.
        shiny::wellPanel(
          shiny::strong("5. Comments:"),
          shinyjs::hidden(
            shiny::div(id = "comm_well",
          shiny::textInput(
            inputId = "comment",
            label = NULL,
            value=NULL
          ),
          shiny::splitLayout(
            shiny::actionButton(
              inputId = "comm_back",
              label = "Previous step",
              style = "padding:4px"
            ),
            shiny::actionButton(
              inputId = "continue",
              label = "Continue",
              style = "padding:4px"
            )
          )
        )))
        ),
      
      ####------------------ 
      ### Plot panel
      ####------------------
      #click and brush have specific actions. Brush allows for zooming, click allows for clicking.
      shiny::mainPanel(
                   shinyjs::hidden(
            shiny::div(id = "hint_mean",shiny::uiOutput("plothintmean"))),
                   shinyjs::hidden(
            shiny::div(id = "hint_xy",shiny::uiOutput("plothintxy"))),
                   shinyjs::hidden(
            shiny::div(id = "hint_box",shiny::uiOutput("plothintbox"))),
                   shinyjs::hidden(
            shiny::div(id = "hint_scatter",shiny::uiOutput("plothintscatter"))),
                    shinyjs::hidden(
            shiny::div(id = "hint_hist",shiny::uiOutput("plothinthist"))),
            shiny::tags$br(),
        shiny::plotOutput(
          "metaPlot",
          dblclick = "plot_click2",
          brush = shiny::brushOpts(
            "plot_brush",
            resetOnNew=TRUE,
          ),
          height = "600px", 
          width = "100%"
        )
      )
    )
  )
}
