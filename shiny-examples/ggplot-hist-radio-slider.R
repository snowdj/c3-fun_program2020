#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
theme_set(theme_minimal(15))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Highway miles per gallon distributions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            radioButtons("var",
                        "Facet distributions by:",
                        choices = c(
                            "None" = "none",
                            "Year" = "year",
                            "Transmission type" = "trans",
                            "Vehicle class" = "class"
                        ),
                        selected = "none")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ggplot_dist")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    mpg <- mpg %>% 
        mutate(year = factor(year))
    
    output$ggplot_dist <- renderPlot({
        
        p <- ggplot(mpg, aes(hwy)) +
            geom_histogram(bins = input$bins,
                           fill = "cornflowerblue",
                           alpha = 0.7,
                           color = "gray40") 
        
        if(input$var != "none") {
            p <- p +
                facet_wrap(input$var)
        }
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
