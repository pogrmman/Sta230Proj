library(shiny)
library(ggplot2)

knitr::knit("DataProcessing.Rmd", tangle = TRUE, output = "DataProcessing.R")
source("DataProcessing.R")

ui <- fluidPage(
  titlePanel("Internet Usage and Protests"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xAxis",
                 label = "By year or by internet access",
                 choices = list("By Year", "By Internet Access", "By Internet Censorship",
                                "By Media Censorship", "By Criticism of Government",
                                "By Journalist Harrassment", "By Media Corruption"),
                 selected = "By Internet Access"),
      selectInput("SummaryType",
                  label = "Summary Type",
                  choices = list("Sum of Protestors", "Mean Protest Size", "Total Protests"),
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
    xvar <- switch(input$xAxis,
                 "By Internet Access" = "InternetUsers",
                 "By Year" = "year",
                 "By Internet Censorship" = "internetCensorship",
                 "By Media Censorship" = "censorship",
                 "By Criticism of Government" = "critical",
                 "By Journalist Harrassment" = "harrassJournalists",
                 "By Media Corruption" = "corrupt")
    xLabel <- switch(input$xAxis,
                  "By Internet Access" = "Internet Access (percent of population)",
                  "By Year" = "Year",
                  "By Internet Censorship" = "Internet Censorship Index",
                  "By Media Censorship" = "Media Censorship Index",
                  "By Criticism of Government" = "Media Criticism of Government Index",
                  "By Journalist Harrassment" = "Journalist Harrassment Index",
                  "By Media Corruption" = "Media Corruption Index")
    yvar <- switch(input$SummaryType,
                 "Sum of Protestors" = "popPctSum",
                 "Mean Protest Size" = "popPctMean",
                 "Total Protests" = "numberProtests")
    yLabel <- switch(input$SummaryType,
                   "Sum of Protestors" = "Annual Protestors (percent of population)",
                   "Mean Protest Size" = "Mean Protest Size (percent of population)",
                   "Total Protests" = "Annual Number of Protests")
    if(countries) {
      plt <- ggplot(data, aes(x = get(xvar), y = get(yvar), color = country)) +
        geom_point(size = 4) + geom_smooth(method = "lm") +
        labs(x = xLabel,
             y = yLabel)
    } else {
      plt <- ggplot(data, aes(x = get(xvar), y = get(yvar))) +
        geom_point(size = 4) + geom_smooth(method = "lm") +
        labs(x = xLabel,
             y = yLabel)
    }
    plt
  })
}

shinyApp(ui, server)