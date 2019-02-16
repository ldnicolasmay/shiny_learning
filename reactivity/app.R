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
         actionButton("goButton", "Go!"),
         sliderInput("a",
                     label = "Select an input to display",
                     min = 0, max = 100, value = 50)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plotOut"),
         tableOutput("tableOut"),
         
         textOutput("nthValue"),
         textOutput("nthValueInv"),
         
         plotOutput("distPlot"),
         
         h1(textOutput("text")),
         
         h3("URL components"),
         verbatimTextOutput("urlText"),
         h3("Parse query string"),
         verbatimTextOutput("queryText")
      )
   )
)

server <- function(input, output, session) {
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
  
  output$text <- renderText({
    print(input$a)
  })
  
  output$urlText <- renderText({
    paste(sep = "",
          "protocol:", session$clientData$url_protocol, "\n",
          "hostname: ", session$clientData$url_hostname, "\n",
          "pathname: ", session$clientData$url_pathname, "\n",
          "port: ",     session$clientData$url_port,     "\n",
          "search: ",   session$clientData$url_search,   "\n"
          )
  })
  
  output$queryText <- renderText({
    query <- parseQueryString(session$clientData$url_search)
    paste(names(query), query, sep = "=", collapse = ", ")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

