library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
theme_set(theme_minimal(15))

source(here::here("shiny-examples", "shiny-comps", "sidebars.R"))
source(here::here("shiny-examples", "shiny-comps", "outputs.R"))

mpg <- mpg %>% 
    mutate(year = factor(year))

# Define UI
ui <- fluidPage(
    navbarPage("Highway miles per gallon distributions",
      tabPanel("Plot", sidebar_plot),
      tabPanel("Summary Data Tables", sidebar_table)
    )
)

# Define server
server <- function(input, output) {
    output$ggplot_dist <- distributions(input)
    output$dist_smry <- table(input)
}

# Run the application 
shinyApp(ui = ui, server = server)
