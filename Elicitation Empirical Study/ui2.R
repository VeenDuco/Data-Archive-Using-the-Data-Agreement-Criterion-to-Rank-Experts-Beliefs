
ui <- shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("name","ID"),
      # creates name input box / in later fase number id box
      selectInput("Var","Sales results",c("Sales 1",
                                          "Sales 2")),
      numericInput("Gemiddelde","Total",value=NA,step=1),
       numericInput("Minimum",
                   "Reasonable lowerbound",
                   value = NA, step = 1),
      # minimum value of interrest box
      numericInput("Maximum",
                   "Reasonable upperbound",
                   value = NA, step = 1),
      # maximum value of interrest box

      #numericInput("Staart","Onzekerheid",value=2,step=1),
      downloadButton('downloadData', 'Submit')),
      # count of amount of "stickers"
    mainPanel(
      
               plotOutput("fitdistr",width = 720, height = 350)
        # second column containing the fitted distribution plot
     ) ) ))

