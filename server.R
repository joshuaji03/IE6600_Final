library(shiny)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    x <- faithful$waiting
    bins <- seq(min(x),max(x),length.out = input$bins +1)
    
    hist(x, breaks = bins, col ="#75AADB",bordre = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "histogram of waiting times")
  })
}