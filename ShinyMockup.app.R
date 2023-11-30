library(shiny)
ui<-fluidPage(
  # Application title
  titlePanel("Practitioner inputs"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Resource type:",
                  choices = c("Grassland","Forest","Freshwater","Coastal fishery","Mangrove")),
      hr(),
      sliderInput("freq",
                  "Economic benefit",
                  min = 1,  max = 200, value = 50),
      sliderInput("freq",
                  "Ease of adoption",
                  min = 1,  max = 5, value = 4),
      sliderInput("max",
                  "Starting costs",
                  min = 1,  max = 200,  value = 75),
      sliderInput("max",
                  "Observability of benefits",
                  min = 1,  max = 5,  value = 4)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)

server<-function(input, output) {
  
  output$main_plot <- renderPlot({
    
    hist(faithful$eruptions,
         probability = TRUE,
         breaks = as.numeric(input$n_breaks),
         xlab = "Duration (minutes)",
         main = "Geyser eruption duration")
    
    if (input$individual_obs) {
      rug(faithful$eruptions)
    }
    
    if (input$density) {
      dens <- density(faithful$eruptions,
                      adjust = input$bw_adjust)
      lines(dens, col = "blue")
    }
    
  })
}
shinyApp(ui = ui, server = server)
