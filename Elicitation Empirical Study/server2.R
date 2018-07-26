server <- function(input, output, session) {
 
  
#param <- reactiveValues(
#  min <- NULL,
#  max <- NULL,
#  mean <- NULL
#  )
  
 # observeEvent(input$Submit, {
  #    param$min[1] <- input$Minimum
   #   param$max[1] <- input$Maximum
    #  param$mean[1] <- input$Gemiddelde
     # })
    
  output$fitdistr <- renderPlot({
    #min = rep(input$Minimum,input$Staart)
    min = rep(input$Minimum,10)
    #max = rep(input$Maximum,input$Staart)
    max = rep(input$Maximum,10)
    
    #mean.min <- rep(input$Gemiddelde-1,100)
    mean = rep(input$Gemiddelde,100)
    #mean.max <- rep(input$Gemiddelde+1,100)
    
    data <- c(min,mean,max)
    
    out2 <- snormFit(data)
        
    xas <- seq(min[1]-(mean[1]/10),max[1]+(mean[1]/10),length.out=600)
    #yas <- dsnorm(xas,out2$par[1],out2$par[2],out2$par[3])
    yas <- dsnorm(xas,input$Gemiddelde,out2$par[2],out2$par[3])
    plot(xas,yas, ylab = " ", type = "l",yaxt="n", xlab = "")
    
    })

 
  datasetInput <- reactive({
    #min = rep(input$Minimum,input$Staart)
    min = rep(input$Minimum,10)
    #max = rep(input$Maximum,input$Staart)
    max = rep(input$Maximum,10)
    #mean.min <- rep(input$Gemiddelde-1,100)
    mean = rep(input$Gemiddelde,100)
    #mean.max <- rep(input$Gemiddelde+1,100)
    
    data <- c(min,mean,max)
    
    out2 <- snormFit(data)
    out <- c(input$Gemiddelde,out2$par[2],out2$par[3])
    return(out)
    
    })
  
  ####################################################################################
  # Download handler for the download button
  ####################################################################################
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$name, Sys.Date(), '.txt', sep='') 
      # returns the matrix with parameters of interrest
      # names the file as name input + date
    },
    content = function(file) {
      write.table(datasetInput(), file)
    },
    contentType = "text/csv"
  )
  
}