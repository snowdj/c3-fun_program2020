sidebar_plot <- sidebarLayout(
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

        mainPanel("Plot",
                  plotOutput("ggplot_dist")
        )
)

sidebar_table <- sidebarLayout(
        sidebarPanel(
            
            radioButtons("var_tab",
                         "Display means by:",
                         choices = c(
                             "Overall" = "none",
                             "Year" = "year",
                             "Transmission type" = "trans",
                             "Vehicle class" = "class"
                         ),
                         selected = "none")
            ),
            
            mainPanel("Table",
                  tabPanel("Table Summary", dataTableOutput("dist_smry"))
            )             
)