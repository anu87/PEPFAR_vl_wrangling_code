library(shiny)
library(plotly)
library(ggplot2)
library(tidyr)
library(scales)

load("Newdata1.rda")
# Newdata1$pSNU <- with(Newdata1, paste(PSNU, comma(Valuex), sep=" | "))

# Define UI for application that draws a histogram
ui <- fluidPage(
  plotlyOutput("plot1"),
  plotlyOutput("plot2"),
  tableOutput("table1")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot1 <- renderPlotly({
  p <- ggplot(Newdata1, aes(x=nat_var, 
                            y=Valuex,
                            # fill=FY17SNUPrioritization,
                            label = PSNU)) +
    geom_jitter(height=0) +
    theme(panel.background=element_rect(fill="white"))
  
  ggplotly(p, source = "first", originalData=F, dragmode =  "select")
    })  
  
  
  output$table1 <- renderTable({
    pl <- event_data("plotly_selected", source = "first")
    idx <- pl$pointNumber + 1
    txc <- Newdata1[idx,]
    
finalx <- txc %>% spread(nat_var, Valuex)
finalx
  })
  
  output$plot2 <- renderPlotly({
    pl2 <- event_data("plotly_selected", source = "first")
    idx2 <- pl2$pointNumber + 1
    txc2 <- Newdata1[idx2,]
    psnus <- txc2$PSNU
    
    subsetdata <- Newdata1 %>% filter(PSNU %in% psnus & nat_var=="TX_CURR_Now_R")
    p2 <- ggplot(subsetdata, aes(x=nat_var, 
                              y=Valuex,
                              # fill=FY17SNUPrioritization,
                              label = PSNU)) +
      geom_jitter(height=0) +
      theme(panel.background=element_rect(fill="white"))
    
    ggplotly(p2, originalData=F)
  })  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

