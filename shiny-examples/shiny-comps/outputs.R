distributions <- function(input) {
  renderPlot({
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

table <- function(input) {
  renderDataTable({
    if(input$var_tab == "none") {
      mpg %>%
        summarize(Mean = mean(hwy),
                  SD   = sd(hwy),
                  Min  = min(hwy),
                  Max  = max(hwy)) %>%
        mutate_if(is.numeric, round, 2) %>%
        datatable(rownames = FALSE)
    }
    else {
      mpg %>% 
        group_by(!!sym(input$var_tab)) %>% 
        summarize(Mean = mean(hwy),
                  SD   = sd(hwy),
                  Min  = min(hwy),
                  Max  = max(hwy)) %>% 
        mutate_if(is.numeric, round, 2) %>% 
        datatable(rownames = FALSE)
    }
  })
}