### Libraries ###
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

### Load Data from R markdown file ###
knitr::knit("DataProcessing.Rmd", tangle = TRUE, output = "DataProcessing.R")
source("DataProcessing.R")

### UI Setup ###
ui <- fluidPage(
  titlePanel("Protests Across the World"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xAxis",
                 label = "By year or by internet access",
                 choices = list("Year", "Internet Access", "Internet Censorship",
                                "Media Censorship", "Media Criticism of Government",
                                "Journalist Harrassment", "Media Corruption",
                                "Political Party Barriers", 
                                "Freedom of Academic/Cultural Expression",
                                "Political Killings", "State Control of Economy",
                                "Social Equality in Civil Liberties",
                                "Power by Socioeconomic Class"),
                 selected = "By Internet Access"),
      selectInput("SummaryType",
                  label = "Summary Type",
                  choices = list("Sum of Protestors", "Mean Protest Size", "Total Protests"),
                  selected = "Sum of Protestors"),
      selectInput("country",
                  label = "Select Country",
                  choices = countryList,
                  multiple = TRUE,
                  selected = NULL),
      selectInput("region",
                  label = "Region",
                  choices = regionList$names,
                  multiple = TRUE,
                  selected = NULL),
      radioButtons("bestFitLine",
                   label = "Include linear best fit?",
                   choices = list("Yes", "No"),
                   selected = "Yes")
    ),
    
    mainPanel(plotlyOutput("InternetUsage", height = "500"))
  )
)

### Server Setup ###
server <- function(input, output, session) {
  # Allow selection of countries by region
  observe({
    if(!is.null(input$region)) {
      regionsSelected <- (regionList %>% filter(names %in% input$region))$region
      countries <- as.list(regions %>% filter(region %in% regionsSelected) %>% 
                           select(country) %>% arrange(country))
      updateSelectInput(session, inputId = "country", selected = countries$country)
    }
  })
  
  # Plot setup
  output$InternetUsage <- renderPlotly({
    # General control options for plot appearance
    if(!is.null(input$country)) {
      data <- byYear %>% filter(country %in% input$country)
      countries = TRUE
    } else {
      data <- byYear
      countries = FALSE
    }
    if(input$bestFitLine == "Yes") {
      includeLine = TRUE
    } else if(input$bestFitLine == "No") {
      includeLine = FALSE
    }
    
    # Setup plot variables and labels
    xvar <- switch(input$xAxis,
                 "Internet Access" = "InternetUsers",
                 "Year" = "year",
                 "Internet Censorship" = "internetCensorship",
                 "Media Censorship" = "censorship",
                 "Media Criticism of Government" = "critical",
                 "Journalist Harrassment" = "harrassJournalists",
                 "Media Corruption" = "corrupt",
                 "Political Party Barriers" = "partyBarriers",
                 "Freedom of Academic/Cultural Expression" = "academicCulturalExpression",
                 "Political Killings" = "politicalMurder",
                 "State Control of Economy" = "stateEconomyControl",
                 "Social Equality in Civil Liberties" = "socialEquality",
                 "Power by Socioeconomic Class" = "socioEconomicPower")
    xLabel <- switch(input$xAxis,
                  "Internet Access" = "Internet Access (percent of population)",
                  "Year" = "Year",
                  "Internet Censorship" = "Internet Censorship Index",
                  "Media Censorship" = "Media Censorship Index",
                  "Media Criticism of Government" = "Media Criticism of Government Index",
                  "Journalist Harrassment" = "Journalist Harrassment Index",
                  "Media Corruption" = "Media Corruption Index",
                  "Political Party Barriers" = "Barriers to Political Parties Index",
                  "Freedom of Academic/Cultural Expression" = "Freedom of Expression Index",
                  "Political Killings" = "Political Killings Index",
                  "State Control of Economy" = "State Control of Economy Index",
                  "Social Equality in Civil Liberties" = "Social Equality Index",
                  "Power by Socioeconomic Class" = "Socioeconomic Power Index")
    yvar <- switch(input$SummaryType,
                 "Sum of Protestors" = "popPctSum",
                 "Mean Protest Size" = "popPctMean",
                 "Total Protests" = "numberProtests")
    yLabel <- switch(input$SummaryType,
                   "Sum of Protestors" = "Annual Protestors (percent of population)",
                   "Mean Protest Size" = "Mean Protest Size (percent of population)",
                   "Total Protests" = "Annual Number of Protests")
    
    # Generate Plots
    if(countries & includeLine) {
      plt <- ggplot(data, aes(x = !!ensym(xvar), y = !!ensym(yvar), color = country)) +
        geom_point(size = 4) + geom_smooth(method = "lm", se = FALSE) +
        labs(x = xLabel,
             y = yLabel)
    } else if(!countries & includeLine) {
      plt <- ggplot(data, aes(x = !!ensym(xvar), y = !!ensym(yvar))) +
        geom_point(size = 4) + geom_smooth(method = "lm", se = FALSE) +
        labs(x = xLabel,
             y = yLabel)
    } else if(countries & !includeLine) {
      plt <- ggplot(data, aes(x = !!ensym(xvar), y = !!ensym(yvar), color = country)) +
        geom_point(size = 4) +
        labs(x = xLabel,
             y = yLabel)
    } else if(!countries & !includeLine) {
      plt <- ggplot(data, aes(x = !!ensym(xvar), y = !!ensym(yvar))) +
        geom_point(size = 4) +
        labs(x = xLabel,
             y = yLabel)
    }
    ggplotly(plt) %>% config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)