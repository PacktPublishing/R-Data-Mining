

ui <- fluidPage(
  numericInput("num", label = h3("Numeric input"), value = 100),
  plotOutput("plot")
)

server <- function(input, output, session) {

  output$plot <- renderPlot({ 
    rnorm(input$num) %>% hist() })
}

shinyApp(ui, server)
