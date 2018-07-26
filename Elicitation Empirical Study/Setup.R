# set working directory that includes this file and both files that are sourced
library(png)
library(shiny)
library(MASS)
library(fGarch)

### Obtaining means
source("ui.r")
source("server.r")
shinyApp(ui, server)

### Obtaining hyperparameters
source("ui2.r")
source("server2.r")
shinyApp(ui, server)
