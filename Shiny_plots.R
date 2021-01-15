# R-script by Gunjan Gautam for Assignement 2
setwd("C:/R/")

#-------------------------------------------------------------------------------
# Build a Shiny app or dashboard allowing a scatterplot for any 
# combination of variables to be displayed. It should 
# also generate histograms, boxplots etc. of your data in this app.
#-------------------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(ggplot2)

# Read the data in the dataframe format
data <- read.csv("process_sim.csv")

shinyUI <- fluidPage(theme = shinytheme("darkly"), # Functions for creating fluid page layouts. To create a fixed page use the fixeunlike fluidPage(), fixed pages cannot make use of higher-level layout functions like sidebarLayout, rather, all layout must be done with fixedRow and column.
                     titlePanel("Drawing Different Plots Dynamically"), #Create a panel containing an application title.
                     
                     sidebarLayout( #Create a layout with a sidebar and main area. The sidebar typically contains input controls. The main area occupies 2/3 of the horizontal width and typically contains outputs.
                       
                       sidebarPanel(
                         
                         radioButtons("plot", "Choose Plot Type",
                                      c("Scatterplot"="scat",
                                        "Boxplot"="box",
                                        "Histogram"="his",
                                        "Density plot"="dp")),
                         
                         selectInput("x", "X variable", names(data), selected= # Create a select list to choose a single or multiple items from a listm
                                       names(data)[[8]]), # inputId,label,choices,initially selected value and defaults is first value
                         selectInput("y", "y variable", names(data), selected= # [i] returns data.frame [[i]] returns numeric so  significant differences between the two methods are the class of the objects they return when used for extraction
                                       names(data)[[9]])
                       ),
                       
                       mainPanel(
                         tabsetPanel(type  = "tabs", #type = c("tabs", "pills", "hidden"),
                                     tabPanel("Plot", plotOutput("plotxy")) # (title, value = title) plotxy is outputId
                                     #tabPanel("Summary", verbatimTextOutput("summary"))
                                     
                         )
                       )
                     )
)
shinySERVER<- function(input, output){
  
  output$plotxy <- renderPlot({ #Renders a reactive plot that is suitable for assigning to an output slot.
    if(input$plot=="scat"){
      plot(data[input$x][[1]],data[input$y][[1]], #An expression that generates a plot.
           col="blue",
           xlab=input$x, ylab=input$y,
           main = paste("Scatterplot of", input$y, "vs", input$x),
      )
    }
    
    if(input$plot=="box"){
      boxplot(
        as.formula(paste(input$y, "~", input$x)), 
        data=data,
        col="green",
        xlab=input$x, ylab=input$y,
        main = paste("Boxplot of", input$y, "vs", input$x))
    }
    
    if(input$plot=="dp"){
      den<-density(data[input$x][[1]])
      plot(den, main="Density plot of selected X-variable", input$x)
      polygon(den, col="red", border="blue")
      xlab = input$x
    }
    
    if(input$plot=="his"){
      temp<-ggplot(data, aes(x=data[input$x][[1]]))+
        geom_histogram(color="darkblue", fill="lightblue")
      temp + labs(title = "Histogram of the selected X-variable", x = input$x, y="Frequency")
    }
    
    
  })
  
  output$summary <- renderPrint({
    summary(data[input$x][[1]])
    
  })
  
}
shinyApp(shinyUI, shinySERVER)
