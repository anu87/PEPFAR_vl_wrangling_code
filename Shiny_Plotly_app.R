library(shiny)
load("Newdata1.rda")

# Define UI for application that draws a histogram
ui <- fluidPage(
  plotlyOutput("plot1"),
  tableOutput("table1")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plot1 <- renderPlotly({
      plot_ly(Newdata1, x = ~nat_var, y = ~Valuex, mode = "markers", type = "scatter", 
              source = "first") %>%
        layout(title = "TX_CURR data",
               dragmode =  "select",
               plot_bgcolor = "6A446F")     
  })  
    
    
  output$table1 <- renderTable({
pl <- event_data("plotly_selected", source = "first")
idx <- pl$pointNumber + 1
Newdata1[idx,]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

