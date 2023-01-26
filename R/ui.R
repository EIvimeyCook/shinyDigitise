
shinyDigitise_UI <- function(){
  shiny::fluidPage(
    shinyjs::useShinyjs(),
    theme = bslib::bs_theme(
      primary = "#66947A", secondary = "#66947A", 
      info = "#E51C23", font_scale = NULL, bootswatch = "materia",
      base_font = bslib::font_collection(bslib::font_google("Atkinson Hyperlegible"), "Arial Narrow Bold", "sans-serif"),
      code_font = bslib::font_collection(bslib::font_google("Atkinson Hyperlegible"), "Arial Narrow Bold", "sans-serif")),
    
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::titlePanel(
          title=shiny::splitLayout(cellWidths = c("20%","50%"),
                                   shiny::actionButton(
                                     inputId = "citeme", 
                                     style="color: white; background-color: white; border-color: white; box-shadow: 0px 0px 0px 0px white;",
                                     label =  img(src = "inst/logos/shinyDigitise.png", height = 60)),
          shinyjs::hidden(shiny::div(id = "top_well7",shinyWidgets::switchInput(
   inputId = "rev_mode",
    labelWidth = "80px",
    value = FALSE,
    onLabel = "Review Mode",
    offLabel = "Extract Mode"
)))
        ),
        windowTitle = "shinyDigitise")
      ),

      shiny::column(width = 8,
        shiny::br(),
        shiny::div(style="display: inline-block;vertical-align:top; width: 10%; font-size:x-large;",
           shiny::p(shiny::htmlOutput("progress", inline=TRUE)),
        ),
        shinyjs::hidden(shiny::div(id = "top_well4", style="display: inline-block;vertical-align:top; width: 10%;",shiny::strong("Point size:"))),
        shinyjs::hidden(shiny::div(id = "top_well5", style="display: inline-block;vertical-align:top;  width: 20%;",
        shiny::sliderInput(
           inputId = "cex",
           label = NULL,
           value = 1,
           min = 0.1,
           max = 3,
           ticks  = FALSE))),        
        shinyjs::hidden(shiny::div(id = "top_well1", style="display: inline-block;vertical-align:top; width: 10% ")),
        shinyjs::hidden(shiny::div(id = "top_well2", style="display: inline-block;vertical-align:top; width: 20%;",shiny::strong("Group Name Position:"))),
        shinyjs::hidden(shiny::div(id = "top_well3", style="display: inline-block;vertical-align:top;  width: 20%;",
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
        shinyjs::hidden(shiny::div(id = "top_well6",style="display: inline-block;vertical-align:top;  width: 2%;",
actionButton("zoom", "Zoom")
                   ))
      )
    ),
    
    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        id = "tPanel",
        style = "overflow-y:scroll; max-height: 800px; position:relative;",
        
        shiny::verbatimTextOutput("image_name"),

        ####------------------ 
        ### review panel
        ####------------------
          shiny::wellPanel(
          shinyjs::hidden(
            shiny::div(id = "rev_well",
            shiny::actionButton("take_screenshot", "Download Extraction Figure"),
            br(),
            br(),
            splitLayout(
              shiny::actionButton("prev_review", "Previous"),
              shiny::actionButton("next_review", "Next"))
        )
          )
        ),
        ####------------------ 
        ### Plot Type Panel
        ####------------------
        shiny::wellPanel(
          shiny::splitLayout(
            cellWidths = c("80%","20%"),
          
          shiny::strong("1. Choose Plot type:"),
          shiny::textOutput("plottype_check_text"),
          tags$head(tags$style("#plottype_check_text{font-size: 20px;}"))

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
        shiny::wellPanel(
          shiny::splitLayout(
                cellWidths = c("80%","20%"),
          shiny::strong("2. Orientate Figure:"),
          shiny::textOutput("orientation_check_text"),
          tags$head(tags$style("#orientation_check_text{font-size: 20px;}"))
          ),
          shinyjs::hidden(shiny::div(id = "orient_well",class = "buttonagency",
                tags$br(),
                shiny::strong("All graph types should be vertically orientated."), tags$br(),
                shiny::strong("If they are not then chose flip to correct this."), tags$br(), 
                shiny::strong("If figures are wonky, chose rotate."),tags$br(),
                tags$br(),
                shiny::strong("Otherwise press next."),tags$br(),
              shiny::br(),
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
              shiny::splitLayout(shiny::br(),
              shiny::textOutput("rotation", inline=TRUE),
              ),
              shiny::br(),
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
        shiny::wellPanel(
          shiny::splitLayout(
            cellWidths = c("80%","20%"),
            shiny::strong("3. Calibrate Axes:"),
            shiny::textOutput("calibrate_check_text"),
            tags$head(tags$style("#calibrate_check_text{font-size: 20px;}"))
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
         # "1. Group names and sample size should be entered into the table on the sidebar before points are added. <br/>
         #  2. To add points to a group, first click the group on the sidebar then click 'Add Points'. <br/>
         #  3. To delete a group, click on the desired group in the table on the sidebar then press 'Delete Group'."

        shiny::wellPanel(
          shiny::splitLayout(
            cellWidths = c("80%","20%"),
            shiny::strong("4. Extract Data:"),
            shiny::textOutput("extract_check_text"),
            tags$head(tags$style("#extract_check_text{font-size: 20px;}"))
          ),
          shinyjs::hidden(
            shiny::div(id = "extract_well",
           shinyjs::hidden(
            shiny::div(id = "group_data",
                tags$br(),
        shiny::strong("1. Click add groups to enter group names and sample size before adding points."), tags$br(),
        shiny::strong("2. To add points click the group on the sidebar then click 'Click Points' and click points."), tags$br(),
        shiny::strong("3. To delete a group, click on the desired group in the table on the sidebar then press 'Delete Group'."), tags$br(),
        tags$br(),
        shiny::uiOutput("plothintmean"),
        shiny::uiOutput("plothintxy"),
        shiny::uiOutput("plothintbox"),
        shiny::uiOutput("plothintscatter"),
        shiny::uiOutput("plothinthist"),
          tags$br(),
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
        tags$br(),
                DT::DTOutput("group_table")
           )),
        tags$br(),
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
              style = "padding:4px; color: #fff; background-color: #337ab7; border-color: #2e6da4"
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
      shiny::mainPanel(
        shiny::plotOutput(
          "metaPlot",
          click = "plot_click2",
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
