# R-script by Gunjan Gautam 

# Set working directory
setwd("C:/CIT/R/")

# Read the data in the dataframe format
data.csv <- read.csv("process_sim.csv")

#-------------------------------------------------------------------------------
# Fit a linear regression model to the 
# generated  scatterplots. The chart includes the fitted line and 
# a table with the slope and intercept should be present within the Shiny dashboard.
#-------------------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(tidyverse)

data <- read.csv("process_sim.csv")

shinyUI <- fluidPage(theme = shinytheme("readable"),
                     titlePanel("Fitting a linear regression model to the scatterplots"),
                     
                     sidebarLayout(
                       position = "right",
                       sidebarPanel(
                         
                         radioButtons("plot", "Plot Type",c("Scatterplot"="scat")),
                         
                         selectInput("x", "X variable", names(data), selected= names(data)[[8]]),
                         selectInput("y", "y variable", names(data), selected= names(data)[[9]])
                       ),
                       
                       mainPanel(
                         tabsetPanel(type  = "tabs",
                                     tabPanel("Scatter Plot", plotOutput("plotxy")),
                                     tabPanel("Coefficients Table ", tableOutput("table"))
                                     
                         )
                       )
                     )
)


shinySERVER<- function(input, output){
  
  output$plotxy <- renderPlot({
    if(input$plot=="scat"){
      reg_model<-lm(data[input$y][[1]]~data[input$x][[1]])
      coeff=coefficients(reg_model)
      eq = paste("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
      plot(data[input$x][[1]],data[input$y][[1]],
           abline(reg_model,col="red"),
           col="dark blue",
           xlab=input$x, ylab=input$y,
           main = paste("Scatterplot of", input$y, "vs", input$x)
      )
      
      output$table <- renderTable({
        matrix(c(coeff[2], coeff[1]), nrow = 1, ncol = 2, byrow = TRUE,
               dimnames = list(c("row1"),c("Slope","y-intercept"))
               #dimnames = list(colnames(c("Slope" = "V1","y-intercept" = "V2")))
        )
      }
      
      )
      
      
    }
    
    
  })
}
shinyApp(shinyUI, shinySERVER)
