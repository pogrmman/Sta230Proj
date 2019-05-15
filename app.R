library(shiny)
library()

knitr::knit("DataProcessing.Rmd", tangle = TRUE, output = "DataProcessing.R")
source("DataProcessing.R")

ui <- fluidPage(
  titlePanel("Internet Usage and Protests"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("SummaryType",
                  label = "Summary Type",
                  choices = list("Sum of Protestors", "Median Protest Size", 
                                 "Mean Protest Size", "Total Protests"),
                  selected = "Sum of Protestors"),
      selectInput("facet",
                  label = "Facet By",
                  choices = list("Half Decade", "Year", "None"),
                  selected = "None")
    ),
    
    mainPanel(plotOutput("InternetUsage", height = "500"))
  )
)

server <- function(input, output) {
  output$InternetUsage <- renderPlot({
    if(input$facet == "None") {
      if(input$SummaryType == "Sum of Protestors") {
        plt <- ggplot(byYear, aes(x = InternetUsers, y = popPctSum)) + 
            geom_point() + geom_smooth(method = "lm")
      } else if(input$SummaryType == "Median Protest Size") {
        plt <- ggplot(byYear, aes(x = InternetUsers, y = popPctMedian)) + 
            geom_point() + geom_smooth(method = "lm")
      } else if(input$SummaryType == "Mean Protest Size") {
        plt <- ggplot(byYear, aes(x = InternetUsers, y = popPctMean)) + 
            geom_point() + geom_smooth(method = "lm")
      } else if(input$SummaryType == "Total Protests") {
        plt <- ggplot(byYear, aes(x = InternetUsers, y = numberProtests)) + 
          geom_point() + geom_smooth(method = "lm")
      }
    } else if(input$facet == "Half Decade") {
      if(input$SummaryType == "Sum of Protestors") {
        plt <- ggplot(byYear, aes(x = InternetUsers, y = popPctSum)) + 
          geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ halfDecade)
      } else if(input$SummaryType == "Median Protest Size") {
        plt <- ggplot(byYear, aes(x = InternetUsers, y = popPctMedian)) + 
          geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ halfDecade)
      } else if(input$SummaryType == "Mean Protest Size") {
        plt <- ggplot(byYear, aes(x = InternetUsers, y = popPctMean)) + 
          geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ halfDecade)
      } else if(input$SummaryType == "Total Protests") {
        plt <- ggplot(byYear, aes(x = InternetUsers, y = numberProtests)) + 
          geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ halfDecade)
      }
    } else if(input$facet == "Year") {
      if(input$SummaryType == "Sum of Protestors") {
        plt <- ggplot(byYear, aes(x = InternetUsers, y = popPctSum)) + 
          geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ year)
      } else if(input$SummaryType == "Median Protest Size") {
        plt <- ggplot(byYear, aes(x = InternetUsers, y = popPctMedian)) + 
          geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ year)
      } else if(input$SummaryType == "Mean Protest Size") {
        plt <- ggplot(byYear, aes(x = InternetUsers, y = popPctMean)) + 
          geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ year)
      } else if(input$SummaryType == "Total Protests") {
        plt <- ggplot(byYear, aes(x = InternetUsers, y = numberProtests)) + 
          geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ year)
      }
    }
    plt
  })
}

shinyApp(ui, server)