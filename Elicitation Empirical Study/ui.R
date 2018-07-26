
ui <- shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("name","ID"),
      # creates name input box / in later fase number id box
      selectInput("Var","Sales results",c("Sales 1",
                                                  "Sales 2")),
      numericInput("Aantal.verkopers",
                   "Number of sales",
                   value = NA, step = 1),
      numericInput("Minimum",
                   "Mimimum sales value",
                   value = NA, step = 1),
      # minimum value of interrest box
      numericInput("Maximum",
                   "Maximum sales value",
                   value = NA, step = 1),
      # maximum value of interrest box
      actionButton("reset", "Reset drawing"),
      # reset button
      actionButton("Undo", "Undo"),
      # undo button
      downloadButton('downloadData', 'Submit'),
      #submit button
      p(),
      #blank line
      verbatimTextOutput("count"),
    verbatimTextOutput("mean"),
    verbatimTextOutput("total")),
      # count of amount of "stickers"
    mainPanel(
      # Some custom CSS for a smaller font for preformatted text
      tags$head(
        tags$style(HTML("
                        pre, table.table {
                        font-size: smaller;
                        }
                        "))
        ),
      
      fluidRow(
        ## first column containing the input frame where stickers can be added
        column(width = 12,
               plotOutput("fitdistr",width = 720, height = 350),
        column(width = 12,offset = 1,
               # In a imageOutput, passing values for click, dblclick, hover, or brush
               # will enable those interactions.
               imageOutput("image1",width=600, height = 300,
                           # Equivalent to: click = clickOpts(id = "image_click")
                           click = "image_click" ))
        
        # second column containing the fitted distribution plot
        )
      ) ) ) ) )

