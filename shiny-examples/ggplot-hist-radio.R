#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
theme_set(theme_minimal(15))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("bins",
                        "Number of bins:",
                        choices = c(1, 5, 20, 50, 100),
                        selected = 20)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ggplot_dist")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$ggplot_dist <- renderPlot({
        ggplot(faithful, aes(waiting)) +
            geom_histogram(bins = as.numeric(input$bins),
                           fill = "cornflowerblue",
                           alpha = 0.7,
                           color = "gray40")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
