#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Calculate nth number in Fibonacci sequence
fib <- function(n) ifelse(n<3, 1, fib(n-1)+fib(n-2))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 25,
                     value = 5),
         shiny::checkboxInput("individualObs", "Show rug?", value = FALSE),
         sliderInput("obs", "Number of observations:",
                     min = 0, max = 1000, value = 500),
         actionButton("goButton", "Go!")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plotOut"),
         tableOutput("tableOut"),
         textOutput("nthValue"),
         textOutput("nthValueInv"),
         plotOutput("distPlot")
      )
   )
)

server <- function(input, output) {
  output$plotOut <- renderPlot({
    hist(faithful$eruptions, breaks = as.numeric(input$bins))
    if (input$individualObs)
      rug(faithful$eruptions)
  })
  
  output$tableOut <- renderTable({
    if (input$individualObs)
      faithful
    else
      NULL
  })
  
  currentFib <- reactive({ fib(as.numeric(input$bins)) })
  
  output$nthValue    <- renderText({ currentFib() })
  output$nthValueInv <- renderText({ 1 / currentFib() })
  
  output$distPlot <- renderPlot({
    
    # # Take a dependency on input$goButton
    # input$goButton
    
    # Prevent plot from being draw before goButton is pressed
    if (input$goButton == 0) return()
    
    # Use isolate() to avoid dependency on input$obs
    dist <- isolate(rnorm(input$obs))
    hist(dist)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

