
shinyUI(fluidPage(
  theme = bs_theme(base_font = font_collection(font_google("News Cycle"),
                                               "Arial Narrow Bold", "sans-serif"), code_font = font_collection(font_google("News Cycle"),
                                                "Arial Narrow Bold", "sans-serif"), font_scale = NULL, `enable-gradients` = TRUE,
                                                 `enable-shadows` = TRUE, bootswatch = "journal"),

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
        div(style="display: inline-block;vertical-align:top; width: 20%; ",
          actionButton(
            inputId = "Previous",
            label = "Previous",
            style='padding:4px'
            #style = "float",
            #color = "primary",
          ),
          actionButton(
            inputId = "Next",
            label = "Next",
            style='padding:4px'
            #style = "float",
            #color = "primary"
          )
        ),
        # div(style="display: inline-block;vertical-align:top; width: 5%;",HTML("<br>")),

      # ),
      # column(3,
      #   br(),
        div(style="display: inline-block;vertical-align:top; width: 20% ",strong("Show processed images:")),
        div(style="display: inline-block;vertical-align:top; width: 5%; ",prettyCheckbox(
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
          strong("Image adjust:"),
          br(),
            # checkboxGroupButtons(
            #     inputId = "Orient",
            #     label = h3("Orientation:"),
            #     choices = c("Flip", "Rotate"),
            #     status = "danger"
            # )
          div(class = "buttonagency",
              splitLayout(cellWidths = c(150, 100, 100),
              cellArgs = list(style = "padding: 1px"),
              switchInput(
                inputId = "flip",
                label = strong("Flip"),
                labelWidth = "60px",
                onLabel = "Yes",
                offLabel = "No"
            #style = "float",
            #color = "primary"
          ),
          actionButton(
            inputId = "rotate",
            label = "Rotate",
            #style = "float",
            #color = "primary"
          ),
          textOutput("rotation", inline=TRUE)
          ))),

          wellPanel(
            prettyRadioButtons(
                inputId = "plot_type",
                label = strong("Plot type:"),
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
            )
              ),

          wellPanel(
            conditionalPanel(
                 condition = "input.plot_type == 'histogram'",
                 actionButton(inputId = "calib",
                              label = "Calibrate"),
                 br(),
                 br(),
                textInput(inputId = "xvar",
                          label = NULL,
                          placeholder = "X Variable"),
                    splitLayout(
                textInput(inputId = "y1",
                          label = NULL,
                          placeholder = "Y1 Value" ),
                textInput(inputId = "y2",
                          label = NULL,
                          placeholder = "Y2 Value" ),
                textInput(inputId = "x1",
                          label = NULL,
                          placeholder = "X1 Value" ),
                textInput(inputId = "x2",
                          label = NULL,
                          placeholder = "X2 Value" )
                    ),
                textInput(inputId = "nsamp",
                          placeholder = "Known sample size",
                          label = NULL),

                       ),
               conditionalPanel(
                 condition = "input.plot_type == 'mean_error",
                 actionButton(inputId = "calib",
                              label = "Calibrate"),
                 br(),
                 br(),
                 textInput(inputId = "yvar",
                           label = NULL,
                           placeholder = "Y Variable"),
                 splitLayout(
                 textInput(inputId = "y1",
                           label = NULL,
                           placeholder = "Y1 Value" ),
                 textInput(inputId = "y2",
                           label = NULL,
                           placeholder = "Y2 Value" )
                 ),
                 prettyRadioButtons(
                               inputId = "errortype",
                               label = h6(strong("Type of error")),
                               choiceNames = c("SE", "95%CI", "SD"),
                               choiceValues = c("se","CI95","sd"),
                               inline = T,
                               icon = icon("check"),
                               bigger = TRUE,
                               status = "danger",
                               animation = "jelly"
                           )


                       ),
               conditionalPanel(
                 condition = "input.plot_type == 'scatterplot'",
                 actionButton(inputId = "calib",
                              label = "Calibrate"),
                 br(),
                 br(),
                 splitLayout(
                   textInput(inputId = "yvar",
                             label = NULL,
                             placeholder = "Y Variable"),
                   textInput(inputId = "xvar",
                             label = NULL,
                             placeholder = "X Variable")),

                 splitLayout(
                   textInput(inputId = "y1",
                             label = NULL,
                             placeholder = "Y1 Value" ),
                   textInput(inputId = "y2",
                             label = NULL,
                             placeholder = "Y2 Value" ),
                   textInput(inputId = "x1",
                             label = NULL,
                             placeholder = "X1 Value" ),
                   textInput(inputId = "x2",
                             label = NULL,
                             placeholder = "X2 Value" )

                 ),
                 prettyCheckbox(
                             inputId = "log",
                             label = "Logged values?",
                             value = FALSE,
                             status = "info"),
                 textInput(inputId = "nsamp",
                              placeholder = "Known sample size",
                           label = NULL),

                ),
               conditionalPanel(
                 condition = "input.plot_type == 'boxplot'",
                 actionButton(inputId = "calib",
                              label = "Calibrate"),
                 br(),
                 br(),
                 textInput(inputId = "yvar",
                           label = NULL,
                           placeholder = "Y Variable"),
                 splitLayout(
                   textInput(inputId = "y1",
                             label = NULL,
                             placeholder = "Y1 Value" ),
                   textInput(inputId = "y2",
                             label = NULL,
                             placeholder = "Y2 Value" )
                 ),
                 prettyCheckbox(
                   inputId = "log",
                   label = "Logged values?",
                   value = FALSE,
                   status = "info"),
            )
          ),
          wellPanel(
            strong("Groups:"),
            br(),
            splitLayout(
              div(class = "buttonagency",
                  actionButton(
                    inputId = "add",
                    label = "New Group",
                    #style = "float",
                    #color = "primary"
                  ),
                  actionButton(
                    inputId = "delete",
                    label = "Delete Group",
                    #style = "float",
                    #color = "primary"
                  ),
                  actionButton(
                    inputId = "newpoints",
                    label = "New Points",
                    #style = "float",
                    #color = "primary"
                  ))),
              pickerInput(inputId = "delete_row", label = NULL,
                          choices = NULL)
          ),
        wellPanel(
          DTOutput("group_table")
        )


        ),
mainPanel(
        verbatimTextOutput("image_name"),
        plotOutput("metaPlot",
                   click="plot_click",
                   dblclick = "plot_dblclick",
                   hover = "plot_hover",
                   brush = "plot_brush",
                   height = "600px", width = "100%"),
        verbatimTextOutput("info")
    )
      )
    )
)
