library(shiny)
library(ggplot2)

knitr::knit("DataProcessing.Rmd", tangle = TRUE, output = "DataProcessing.R")
source("DataProcessing.R")

ui <- fluidPage(
  titlePanel("Internet Usage and Protests"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("xAxis",
                   label = "By year or by internet access",
                   choices = list("By Year", "By Internet Access"),
                   selected = "By Internet Access"),
      selectInput("SummaryType",
                  label = "Summary Type",
                  choices = list("Sum of Protestors", "Median Protest Size", 
                                 "Mean Protest Size", "Total Protests"),
                  selected = "Sum of Protestors"),
      selectInput("country",
                  label = "Select Country",
                  choices = countryList,
                  multiple = TRUE,
                  selected = NULL)
    ),
    
    mainPanel(plotOutput("InternetUsage", height = "500"))
  )
)

server <- function(input, output, session) {
  output$InternetUsage <- renderPlot({
    if(!is.null(input$country)) {
      data <- byYear %>% filter(country %in% input$country)
      countries = TRUE
    } else {
      data <- byYear
      countries = FALSE
    }
    if(countries) {
      if(input$xAxis == "By Internet Access") {
        if(input$SummaryType == "Sum of Protestors") {
          plt <- ggplot(data, aes(x = InternetUsers, y = popPctSum, color = country)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Total Protestors vs. Internet Access",
                 x = "Internet Access (Percent of Population)",
                 y = "Total Protestors (Percent of Population)")
        } else if (input$SummaryType == "Median Protest Size") {
          plt <- ggplot(data, aes(x = InternetUsers, y = popPctMedian, color = country)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Median Protest Size vs. Internet Access",
                 x = "Internet Access (Percent of Population)",
                 y = "Median Protest Size (Percent of Population)")
        } else if (input$SummaryType == "Mean Protest Size") {
          plt <- ggplot(data, aes(x = InternetUsers, y = popPctMean, color = country)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Mean Protest Size vs. Internet Access",
                 x = "Internet Access (Percent of Population)",
                 y = "Mean Protest Size (Percent of Population)")
        } else if (input$SummaryType == "Total Protests") {
          plt <- ggplot(data, aes(x = InternetUsers, y = numberProtests, color = country)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Number of Protests vs. Internet Access",
                 x = "Internet Access (Percent of Population)",
                 y = "Number of  Protests")
        } 
      } else if(input$xAxis == "By Year") {
        if(input$SummaryType == "Sum of Protestors") {
          plt <- ggplot(data, aes(x = year, y = popPctSum, color = country)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Total Protestors vs. Internet Access",
                 x = "Year",
                 y = "Total Protestors (Percent of Population)")
        } else if (input$SummaryType == "Median Protest Size") {
          plt <- ggplot(data, aes(x = year, y = popPctMedian, color = country)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Median Protest Size vs. Internet Access",
                 x = "Year",
                 y = "Median Protest Size (Percent of Population)")
        } else if (input$SummaryType == "Mean Protest Size") {
          plt <- ggplot(data, aes(x = year, y = popPctMean, color = country)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Mean Protest Size vs. Internet Access",
                 x = "Year",
                 y = "Mean Protest Size (Percent of Population)")
        } else if (input$SummaryType == "Total Protests") {
          plt <- ggplot(data, aes(x = year, y = numberProtests, color = country)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Number of Protests vs. Internet Access",
                 x = "Year",
                 y = "Number of  Protests")
        }
      }
    } else {
      if(input$xAxis == "By Internet Access") {
        if(input$SummaryType == "Sum of Protestors") {
          plt <- ggplot(data, aes(x = InternetUsers, y = popPctSum)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Total Protestors vs. Internet Access",
                 x = "Internet Access (Percent of Population)",
                 y = "Total Protestors (Percent of Population)")
        } else if (input$SummaryType == "Median Protest Size") {
          plt <- ggplot(data, aes(x = InternetUsers, y = popPctMedian)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Median Protest Size vs. Internet Access",
                 x = "Internet Access (Percent of Population)",
                 y = "Median Protest Size (Percent of Population)")
        } else if (input$SummaryType == "Mean Protest Size") {
          plt <- ggplot(data, aes(x = InternetUsers, y = popPctMean)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Mean Protest Size vs. Internet Access",
                 x = "Internet Access (Percent of Population)",
                 y = "Mean Protest Size (Percent of Population)")
        } else if (input$SummaryType == "Total Protests") {
          plt <- ggplot(data, aes(x = InternetUsers, y = numberProtests)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Number of Protests vs. Internet Access",
                 x = "Internet Access (Percent of Population)",
                 y = "Number of  Protests")
        }
      } else if(input$xAxis == "By Year") {
        if(input$SummaryType == "Sum of Protestors") {
          plt <- ggplot(data, aes(x = year, y = popPctSum)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Total Protestors vs. Internet Access",
                 x = "Year",
                 y = "Total Protestors (Percent of Population)")
        } else if (input$SummaryType == "Median Protest Size") {
          plt <- ggplot(data, aes(x = year, y = popPctMedian)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Median Protest Size vs. Internet Access",
                 x = "Year",
                 y = "Median Protest Size (Percent of Population)")
        } else if (input$SummaryType == "Mean Protest Size") {
          plt <- ggplot(data, aes(x = year, y = popPctMean)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Mean Protest Size vs. Internet Access",
                 x = "Year",
                 y = "Mean Protest Size (Percent of Population)")
        } else if (input$SummaryType == "Total Protests") {
          plt <- ggplot(data, aes(x = year, y = numberProtests)) +
            geom_point(size=5) + geom_smooth(method = "lm") + 
            labs(title = "Number of Protests vs. Internet Access",
                 x = "Year",
                 y = "Number of  Protests")
        }
      }
    }
    plt
  })
}

shinyApp(ui, server)