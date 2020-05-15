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
library(dplyr)
library(DT)
library(forcats)
#data(gss_cat, package = "forcats")

theme_set(theme_minimal(15))

f <- function(data, 
              group_var, 
              outcome_var, 
              .funs = list(n = ~length(.),
                           n_valid = ~length(!is.na(.)),
                           n_miss = ~length(is.na(.)),
                           mean = ~mean(., na.rm = TRUE),
                           sd = ~sd(., na.rm = TRUE),
                           min = ~min(., na.rm = TRUE),
                           max = ~max(., na.rm = TRUE))
) {
    if(!is.data.frame(data)) {
        stop("This is a non-df error. Make your message more informative")
    }
    if(!is.numeric(pull(data, !!enquo(outcome_var)))) {
        stop("This is an error for the outcome variable being non-numeric. Make your message more informative")
    }
    if(is.numeric(pull(data, !!enquo(group_var)))) {
        warning("This is a warning about a numeric grouping variable. Make your message more informative.")
    }
    data %>%
        group_by(!!enquo(group_var)) %>%
        summarize_at(vars(!!enquo(outcome_var)),
                     .funs)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("General Social Science Survey"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("var",
                        "Variable:",
                        choices = c(
                            "None" = "none",
                            "Year" = "year",
                            "Marital status" = "marital",
                            "Age" = "age",
                            "Identified race" = "race",
                            "Reported income" = "rincome",
                            "Political affiliation" = "partyid",
                            "Identified religion" = "relig",
                            "Identified denomination" = "denom"
                        ),
                        selected = "none")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("tv_plot"),
           dataTableOutput("tbl")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$tv_plot <- renderPlot({
        p <- ggplot(gss_cat, aes(tvhours)) +
            geom_histogram(fill = "cornflowerblue",
                           color = "white") 
        if(input$var != "none"){
            p <- p + facet_wrap(input$var)
        }
        p
    })
    output$tbl <- renderDataTable({
        if(input$var != "none") {
            f(gss_cat, !!sym(input$var), tvhours) %>% 
                mutate_if(is.numeric, round, 2) %>% 
                datatable()
        }
        else {
            gss_cat %>% 
                summarize_at("tvhours", 
                             list(n = ~length(.),
                                  n_valid = ~length(!is.na(.)),
                                  n_miss = ~length(is.na(.)),
                                  mean = ~mean(., na.rm = TRUE),
                                  sd = ~sd(., na.rm = TRUE),
                                  min = ~min(., na.rm = TRUE),
                                  max = ~max(., na.rm = TRUE))) %>% 
                mutate_if(is.numeric, round, 2) %>% 
                datatable()
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
