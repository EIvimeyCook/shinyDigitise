shinyDigitise_UI <- function(){
  fluidPage(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             position:fixed;
             bottom: calc(1%);
             left: calc(1%);
             margin-left: auto;
             margin-right: auto;
             width: 100%;
             max-width: 450px;
             }
             "
        )
      )
    ),
    theme = bs_theme(
      primary = "#66947A", secondary = "#66947A", 
      info = "#E51C23", font_scale = NULL, bootswatch = "materia",
      base_font = font_collection(font_google("Atkinson Hyperlegible"), "Arial Narrow Bold", "sans-serif"),
      code_font = font_collection(font_google("Atkinson Hyperlegible"), "Arial Narrow Bold", "sans-serif")),
    
    fluidRow(
      column(
        width = 4,
        titlePanel(
          title=splitLayout(cellWidths = c("15%","85%"),
          imageOutput(
            "images/shinylogo",
            height = "60px"
          ),
          textOutput("shinytext")
        ),
        windowTitle = "shinyDigitise")
      ),

      column(width = 8,
        br(),
        div(style="display: inline-block;vertical-align:top; width: 10%; font-size:x-large;",
           p(htmlOutput("progress", inline=TRUE)),
        ),
        div(style="display: inline-block;vertical-align:top; width: 10%;",strong("Point size:")),
        div(style="display: inline-block;vertical-align:top;  width: 20%;",
         sliderInput(
           inputId = "cex",
           label = NULL,
           value = 1,
           min = 0.1,
           max = 3,
           ticks  = FALSE)),
        div(style="display: inline-block;vertical-align:top; width: 10% "),
        div(style="display: inline-block;vertical-align:top; width: 20%;",strong("Group Name Position:")),
        div(style="display: inline-block;vertical-align:top;  width: 20%;",
           prettyRadioButtons(
             inputId = "pos",
             label = NULL,
             choiceNames = c("right", "top"),
             choiceValues = c("right", "top"),
             inline = T,
             icon = icon("check"),
             bigger = TRUE,
             status = "info",
             animation = "jelly"
             #checkIcon = list(
             #yes = tags$i(class = "fa fa-check-square",
             #            style = "color: white"),
             #no = tags$i(class = "fa fa-square-o",
             #style = "color: white"))
           )
        )
      )
      # column(2,
      #   br(),
      
      # )
    ),
    
    
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel( 
        width = 4,
        id = "tPanel",
        style = "overflow-y:scroll; max-height: 800px; position:relative;",
        
        verbatimTextOutput("image_name"),
        ####------------------ 
        ### Plot Type Panel
        ####------------------
        wellPanel(
          splitLayout(
            cellWidths = c("80%","20%"),
          
          strong("1. Choose Plot type:"),
          textOutput("plottype_check_text"),
          tags$head(tags$style("#plottype_check_text{font-size: 20px;}"))

          ),
          hidden(div(id = 'plot_well',
          prettyRadioButtons(
            inputId = "plot_type",
            label = NULL,
            choiceNames = c("Mean/error", "Boxplot", "XY Mean/Error"),
            choiceValues = c("mean_error", "boxplot", "xy_mean_error"),
            # choiceNames = c("Mean/error", "Scatterplot", "Histogram", "Boxplot"),
            # choiceValues = c("mean_error", "scatterplot", "histogram", "boxplot"),
            selected = character(0),
            inline = T,
            icon = icon("check"),
            bigger = TRUE,
            status = "info",
            animation = "jelly"
            #checkIcon = list(
            #yes = tags$i(class = "fa fa-check-square",
            #            style = "color: white"),
            #no = tags$i(class = "fa fa-square-o",
            #style = "color: white"))
          ),
          # imageOutput(
            #   "plottype_check",
            #   height = "30px"
            # )

          
          actionButton(
            inputId = "plot_step",
            label = "Next step",
            style = "padding:4px"
          ),))
        ),
        
        ####------------------ 
        ### Orientation Panel
        ####------------------
        wellPanel(
          splitLayout(
                cellWidths = c("80%","20%"),
          strong("2. Orientate Figure:"),
          textOutput("orientation_check_text"),
          tags$head(tags$style("#orientation_check_text{font-size: 20px;}"))
          ),
          hidden(div(id = "orient_well",class = "buttonagency",
              # splitLayout(cellWidths = c(150, 200, 200),
                "mean_error and boxplots should be vertically orientated. \n If they are not then chose flip to correct this. \n If figures are wonky, chose rotate.",
                switchInput(
                  inputId = "flip",
                  label = strong("Flip"),
                  labelWidth = "100px",
                  onLabel = "Yes",
                  offLabel = "No"
                  #style = "float",
                  #color = "primary"
                ),
                # imageOutput(
                #   "orientation_check",
                #   height = "30px"
                # )
                # textOutput("orientation_check_text"),
                # tags$head(tags$style("#orientation_check_text{font-size: 20px;}"))
                
              # cellArgs = list(style = "padding: 1px"),
              textOutput("rotation", inline=TRUE),
              switchInput(
                inputId = "rotate_mode",
                label = strong("Rotate mode"),
                labelWidth = "100px",
                onLabel = "Yes",
                offLabel = "No"
                #style = "float",
                #color = "primary"
                # )
              ),
              hidden(
                div(id="togslide",
                    sliderTextInput(
                      inputId = "rotate",
                      label = NULL,
                      choices = seq(from = -45, to = 45, by = 5),
                      selected = 0,
                      grid = T
                    )
                )
              ),
              splitLayout(
              actionButton(
                inputId = "orient_back",
                label = "Previous step",
                style = "padding:4px"
              ),
              actionButton(
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
        wellPanel(
          splitLayout(
            cellWidths = c("80%","20%"),
            strong("3. Calibrate Axes:"),


            # imageOutput(
            #   "calibrate_check",
            #   height = "30px"
            # )
            textOutput("calibrate_check_text"),
            tags$head(tags$style("#calibrate_check_text{font-size: 20px;}"))
          ),
          hidden(
            div(id = "calib_well",
           switchInput(
              inputId = "calib_mode",
              label = strong("Calibrate mode"),
              labelWidth = "100px",
              onLabel = "Yes",
              offLabel = "No",
              onStatus = "primary",
            ),
           htmlOutput("calib_info"),
           
          hidden(
            div(id="y_var_input",
                textInput(inputId = "y_var",
                          label = NULL,
                          placeholder = "Y Variable")
                
            )
          ),
          hidden(
            div(id="x_var_input",
                textInput(inputId = "x_var",
                          label = NULL,
                          placeholder = "X Variable")
                
            )
          ),
          hidden(
            div(id="y_coord_input",
                splitLayout(
                  cellWidths = c("15%","35%","15%","35%"),
                  "Y1",
                  numericInput(inputId = "y1",
                               label = NULL,
                               value= NA),
                  "Y2",
                  numericInput(inputId = "y2",
                               label= NULL,
                               value= NA )
                )
            )
          ),
          hidden(
            div(id="x_coord_input",
                splitLayout(
                  cellWidths = c("15%","35%","15%","35%"),
                  "X1",
                  numericInput(inputId = "x1",
                               label = NULL,
                               value= NA),
                  "X2",
                  numericInput(inputId = "x2",
                               label= NULL,
                               value= NA )
                )
            )
          ),
          hidden(
            div(id="log_input",
                prettyCheckbox(
                  inputId = "log_sp",
                  label = "Logged values?",
                  value = FALSE,
                  status = "info"),
                textInput(
                  inputId = "nsamp_sp",
                  placeholder = "Known sample size",
                  label = NULL)
            )
          ),
          splitLayout(
            actionButton(
              inputId = "calib_back",
              label = "Previous step",
              style = "padding:4px"
            ),
            actionButton(
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

        wellPanel(
          splitLayout(
            cellWidths = c("80%","20%"),
            strong("Extract Data:"),
            textOutput("extract_check_text"),
            tags$head(tags$style("#extract_check_text{font-size: 20px;}"))
          ),
          hidden(
            div(id = "extract_well",
            switchInput(
              inputId = "extract_mode",
              label = strong("Extract mode"),
              labelWidth = "100px",
              onLabel = "Yes",
              offLabel = "No",
              onStatus = "primary"
            ),
            # imageOutput(
            #   "extract_check",
            #   height = "30px"
            # )
            
          
           hidden(
            div(id = "group_data",
        "1. Click add groups to enter group names and sample size before adding points. \n
        2. To add points click the group on the sidebar then click 'Click Points' and click points. \n
         3. To delete a group, click on the desired group in the table on the sidebar then press 'Delete Group'.",
              splitLayout(
                div(class = "buttonagency",
                    actionButton(
                      inputId = "add_group",
                      label = "Add Group",
                      #style = "float",
                      #color = "primary"
                    ),
                    actionButton(
                      inputId = "click_group",
                      label = "Click Points",
                      #style = "float",
                      #color = "primary"
                    ),
                    actionButton(
                      inputId = "del_group",
                      label = "Delete Group",
                      #style = "float",
                      #color = "primary"
                    )
                  )
                ),
                DTOutput("group_table")
           )),
          hidden(
            div(id = "error_type_select",
              prettyRadioButtons(
                inputId = "errortype",
                label = strong("Type of error:"),
                choiceNames = c("SE", "95%CI", "SD"),
                choiceValues = c("se","CI95","sd"),
                inline = T,
                icon = icon("check"),
                bigger = TRUE,
                status = "danger",
                animation = "jelly"
              )
            )
          ),
          splitLayout(
            actionButton(
              inputId = "extract_back",
              label = "Previous step",
              style = "padding:4px"
            ),
            actionButton(
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
        wellPanel(
          strong("5. Comments:"),
          hidden(
            div(id = "comm_well",
          textInput(
            inputId = "comment",
            label = NULL,
            value=NULL
          ),
          splitLayout(
            actionButton(
              inputId = "comm_back",
              label = "Previous step",
              style = "padding:4px"
            ),
            actionButton(
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
      mainPanel(
        #verbatimTextOutput("info"),
        plotOutput(
          "metaPlot",
          click = "plot_click2",
          dblclick = "plot_dblclick",
          brush = brushOpts(
            "plot_brush",
            resetOnNew=TRUE,
            delayType="debounce"
          ),
          height = "600px", 
          width = "100%"
        ),  
        verbatimTextOutput("clickinfo"),
        br(),
        br()
        
      )
    )
  )
}
