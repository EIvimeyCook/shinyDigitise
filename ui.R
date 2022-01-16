
shinyUI(fluidPage(
    useShinyjs(),
    useShinyalert(),
  theme = bs_theme(
    primary = "#66947A", secondary = "#66947A", 
    info = "#E51C23", font_scale = NULL, bootswatch = "materia",
    base_font = font_collection(font_google("Atkinson Hyperlegible"), "Arial Narrow Bold", "sans-serif"),
    code_font = font_collection(font_google("Atkinson Hyperlegible"), "Arial Narrow Bold", "sans-serif")),

  fluidRow(
     column(width = 4,
      titlePanel(title=div(img(src="shiny.jpg", height = 60),"shinyDigitise"), windowTitle = "shinyDigitise"),
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
          ticks  = FALSE))
      )
      # column(2,
      #   br(),

      # )
    ),



    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel( width = 4,
        id = "tPanel",style = "overflow-y:scroll; max-height: 600px; position:relative;",


        ## I've changed this because flip and rotate are different processes, so need two buttons
        wellPanel(
            # checkboxGroupButtons(
            #     inputId = "Orient",
            #     label = h3("Orientation:"),
            #     choices = c("Flip", "Rotate"),
            #     status = "danger"
            # )
          div(class = "buttonagency",
              # splitLayout(cellWidths = c(150, 200, 200),
              switchInput(
                inputId = "flip",
                label = strong("Flip"),
                labelWidth = "60px",
                onLabel = "Yes",
                offLabel = "No"
              #style = "float",
              #color = "primary"
            ),
              # cellArgs = list(style = "padding: 1px"),
              textOutput("rotation", inline=TRUE),
              switchInput(
                inputId = "rotate_mode",
                label = strong("Rotate mode"),
                labelWidth = "60px",
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
              )))

          )),

          wellPanel(
            prettyRadioButtons(
                inputId = "plot_type",
                label = NULL,
                choiceNames = c("Mean/error", "Scatterplot", "Histogram", "Boxplot"),
                choiceValues = c("mean_error", "scatterplot", "histogram", "boxplot"),
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
            switchInput(
              inputId = "calib_mode",
              label = strong("Calibrate mode"),
              labelWidth = "60px",
              onLabel = "Yes",
              offLabel = "No",
              onStatus = "primary",
            )
              ),
        

        wellPanel(id = "calib_data",
            conditionalPanel(
                 condition = "input.plot_type == 'histogram'",
                textInput(inputId = "xvar_hist",
                          label = NULL,
                          placeholder = "X Variable"),
                    splitLayout(
                textInput(inputId = "y1_hist",
                          label = NULL,
                          placeholder = "Y1 Value" ),
                textInput(inputId = "y2_hist",
                          label = NULL,
                          placeholder = "Y2 Value" ),
                textInput(inputId = "x1_hist",
                          label = NULL,
                          placeholder = "X1 Value" ),
                textInput(inputId = "x2_hist",
                          label = NULL,
                          placeholder = "X2 Value" )
                    ),
                textInput(inputId = "nsamp_hist",
                          placeholder = "Known sample size",
                          label = NULL),

                       ),
               conditionalPanel(
                 condition = "input.plot_type == 'mean_error'",
                 textInput(inputId = "yvar_me",
                           label = NULL,
                           placeholder = "Y Variable"),
                 splitLayout(
                 textInput(inputId = "y1_me",
                           label = NULL,
                           placeholder= "Y1 Value"),
                 textInput(inputId = "y2_me",
                           label= NULL,
                           placeholder= "Y2 Value" )
                 ),

                       ),
               conditionalPanel(
                 condition = "input.plot_type == 'scatterplot'",
                 splitLayout(
                   textInput(inputId = "yvar_sp",
                             label = NULL,
                             placeholder = "Y Variable"),
                   textInput(inputId = "xvar_sp",
                             label = NULL,
                             placeholder = "X Variable")),

                 splitLayout(
                   textInput(inputId = "y1_sp",
                             label = NULL,
                             placeholder = "Y1 Value" ),
                   textInput(inputId = "y2_sp",
                             label = NULL,
                             placeholder = "Y2 Value" ),
                   textInput(inputId = "x1_sp",
                             label = NULL,
                             placeholder = "X1 Value" ),
                   textInput(inputId = "x2_sp",
                             label = NULL,
                             placeholder = "X2 Value" )

                 ),
                 prettyCheckbox(
                             inputId = "log_sp",
                             label = "Logged values?",
                             value = FALSE,
                             status = "info"),
                 textInput(inputId = "nsamp_sp",
                              placeholder = "Known sample size",
                           label = NULL),

                ),
               conditionalPanel(
                 condition = "input.plot_type == 'boxplot'",
                 textInput(inputId = "yvar_bp",
                           label = NULL,
                           placeholder = "Y Variable"),
                 splitLayout(
                   textInput(inputId = "y1_bp",
                             label = NULL,
                             placeholder = "Y1 Value" ),
                   textInput(inputId = "y2_bp",
                             label = NULL,
                             placeholder = "Y2 Value" )
                 ),
                 prettyCheckbox(
                   inputId = "log_bp",
                   label = "Logged values?",
                   value = FALSE,
                   status = "info"),
            )
          ),
          wellPanel(switchInput(
                      inputId = "extract_mode",
                      label = strong("Extract mode"),
                      labelWidth = "60px",
                      onLabel = "Yes",
                      offLabel = "No",
                      onStatus = "primary")),
                  wellPanel(id = "error_type_select",
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
                  )),
        wellPanel(id = "group_data",
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
                ))),
          DTOutput("group_table")
        )
        ),
mainPanel(
        verbatimTextOutput("image_name"),
        plotOutput("metaPlot",
                   click = "plot_click2",
                   dblclick = "plot_dblclick",
                   hover = "plot_hover",
                   brush = "plot_brush",
                   height = "600px", width = "100%"
        ),
        verbatimTextOutput("info"),
        verbatimTextOutput("clickinfo"),
        br(),
        br(),
          actionButton(
            inputId = "continue",
            label = "Continue",
            style = "padding:4px"
            # style = "float",
            # color = "primary",
          
        ),
        actionButton(
          inputId = "previous",
          label = "Previous",
          style = "padding:4px"
          # style = "float",
          # color = "primary",
          
        )
    )
      )
    )
)
