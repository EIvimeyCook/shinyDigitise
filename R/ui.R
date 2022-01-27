shinyDigitise_UI <- function(){
  fluidPage(
    useShinyjs(),
    useShinyalert(),
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
            "shinylogo",
            height = "60px"
          ),
          textOutput("shinytext")
        ),
        windowTitle = "shinyDigitise")
      ),
    # column(width = 1,
      #   br(),
      # ),
      column(width = 8,
        br(),
        div(style="display: inline-block;vertical-align:top; width: 10%; font-size:x-large;",
           p(htmlOutput("progress", inline=TRUE)),
        ),
        # div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
        # div(style="display: inline-block;vertical-align:top; width: 5%;",HTML("<br>")),

        # ),
        # column(3,
        #   br(),
        div(style="display: inline-block;vertical-align:top; width: 15% ",strong("Show processed images:")),
        div(style="display: inline-block;vertical-align:top; width: 10%; ",prettyCheckbox(
         value = F,
         icon = icon("check"),
         status = "danger",
         animation = "jelly",
         inputId = "ShowOnlyNew",
         label = NULL,
         #label_on = "Yes",
         #icon_on = icon("check"),
         #status_on = "info",
         #status_off = "warning",
         #label_off = "No",
         #icon_off = icon("remove")
        )),

        # ),
        # # column(1, align="left"
        # #
        # #   br(),

        # # ),
        # column(3,
        #   br(),
        # div(style="display: inline-block;vertical-align:top; width: 10%;",HTML("<br>")),

        div(style="display: inline-block;vertical-align:top; width: 10%;",strong("Point size:")),
        div(style="display: inline-block;vertical-align:top;  width: 15%;",
         sliderInput(
           inputId = "cex",
           label = NULL,
           value = 1,
           min = 0.1,
           max = 3,
           ticks  = FALSE)),
        div(style="display: inline-block;vertical-align:top; width: 10% "),
        div(style="display: inline-block;vertical-align:top;  width: 15%;",
           prettyRadioButtons(
             inputId = "pos",
             label = "Group Name Position",
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
        
        ####------------------ 
        ### Plot Type Panel
        ####------------------
        wellPanel(
          "Choose Plot type:",
          splitLayout(
            cellWidths = c("80%","20%"),
          prettyRadioButtons(
            inputId = "plot_type",
            label = NULL,
            choiceNames = c("Mean/error", "Boxplot", "XY Mean/Error"),
            choiceValues = c("mean_error", "boxplot", "xy_mean_error"),
            # choiceNames = c("Mean/error", "Scatterplot", "Histogram", "Boxplot"),
            # choiceValues = c("mean_error", "scatterplot", "histogram", "boxplot"),
            inline = T,
            icon = icon("check"),
            bigger = TRUE,
            status = "info",
            animation = "jelly",
            selected = character(0)
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
          textOutput("plottype_check_text")
          )
        ),
        
        ####------------------ 
        ### Orientation Panel
        ####------------------
        wellPanel(
          "Orientate Figure:",
          
          div(class = "buttonagency",
              # splitLayout(cellWidths = c(150, 200, 200),
              splitLayout(
                cellWidths = c("80%","20%"),
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
                textOutput("orientation_check_text")
              ),  
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
              )
              
          )
        ),
        
        ####------------------ 
        ### Calibrate Panel
        ####------------------
        wellPanel(
          "Calibrate Axes:",
          splitLayout(
            cellWidths = c("80%","20%"),
            switchInput(
              inputId = "calib_mode",
              label = strong("Calibrate mode"),
              labelWidth = "100px",
              onLabel = "Yes",
              offLabel = "No",
              onStatus = "primary",
            ),
            # imageOutput(
            #   "calibrate_check",
            #   height = "30px"
            # )
            textOutput("calibrate_check_text")
          ),
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
          )
        ),
        
        ####------------------ 
        ### Extraction Panel
        ####------------------
        wellPanel(
          "Extract Data:",
          splitLayout(
            cellWidths = c("80%","20%"),
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
            textOutput("extract_check_text")
          ),
          hidden(
            div(id = "group_data",
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
            )
          ),
          hidden(
            div(id = "error_type_select",
              prettyRadioButtons(
                inputId = "errortype",
                label = h6(strong("Type of error:")),
                choiceNames = c("SE", "95%CI", "SD"),
                choiceValues = c("se","CI95","sd"),
                inline = T,
                icon = icon("check"),
                bigger = TRUE,
                status = "danger",
                animation = "jelly"
              )
            )
          )
        ),
        
        ####------------------ 
        ### comment panel
        ####------------------
        wellPanel(
          "Comments:",
          textInput(
            inputId = "comment",
            label = NULL,
            value=NULL
          )
        ),
        
        ####------------------ 
        ### next previous panel
        ####------------------
        wellPanel(
          actionButton(
            inputId = "previous",
            label = "Previous",
            style = "padding:4px"
            # style = "float",
            # color = "primary",
            
          ),
          actionButton(
            inputId = "continue",
            label = "Continue",
            style = "padding:4px"
            # style = "float",
            # color = "primary",
            
          )
          
        )
      ),
      
      ####------------------ 
      ### Plot panel
      ####------------------
      mainPanel(
        verbatimTextOutput("image_name"),
        plotOutput(
          "metaPlot",
          click = "plot_click2",
          dblclick = "plot_dblclick",
          hover = "plot_hover",
          brush = "plot_brush",
          height = "600px", 
          width = "100%"
        ),
        verbatimTextOutput("info"),
        verbatimTextOutput("clickinfo"),
        br(),
        br()
        
      )
    )
  )
}
