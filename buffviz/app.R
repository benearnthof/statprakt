
library(shiny)

ui <- fluidPage(

    titlePanel("buffviz"),

    sidebarLayout(
        sidebarPanel(
    
        ),

        mainPanel(
          
        )
    )
)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)
