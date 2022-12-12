library(shiny)
library(tidyverse)
library(renv)
library(readxl)

life_exp<- read.csv("Life_Expectancy_00_15.csv", sep = ";")

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Life Expectancy vs. Time"),
    selectInput(inputId = "select_country",
                label = "Choose Country",
                "Names"),
    varSelectInput(inputId = "select_y_variable",
                label = "Choose Y Variable",
                data = life_exp,
                selected = "Life.Expectancy"),
    varSelectInput(inputId = "select_x_variable",
                   label = "Choose X Variable",
                   data = life_exp,
                   selected = "Year"),
    plotOutput("plot")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    data <- reactive({
        req(input$select_country)
        df <- life_exp %>% filter(Country %in% input$select_country)
    })

    observe({
        updateSelectInput(session, "select_country", choices = life_exp$Country)
        updateSelectInput(session, "select_x_variable", choices = c("Life.Expectancy", "Year", "Population", "CO2.emissions", "Health.expenditure", "Electric.power.consumption", "Forest.area", "GDP.per.capita", "Individuals.using.the.Internet", "Military.expenditure", "People.practicing.open.defecation", "People.using.at.least.basic.drinking.water.services", "Obesity.among.adults", "Beer.consumption.per.capita" ))
        updateSelectInput(session, "select_y_variable", choices = c("Life.Expectancy", "Year", "Population", "CO2.emissions", "Health.expenditure", "Electric.power.consumption", "Forest.area", "GDP.per.capita", "Individuals.using.the.Internet", "Military.expenditure", "People.practicing.open.defecation", "People.using.at.least.basic.drinking.water.services", "Obesity.among.adults", "Beer.consumption.per.capita" ))
    })

    #output$plot <- renderPlot({
        #ggplot(data(), aes(x = Year, y = Life.Expectancy)) + geom_line() + geom_point() + theme_bw() + labs( x = "Year", y = "Life Expectancy in Years")
    #})
    output$plot <- renderPlot({
        ggplot(data(), aes_string(x = input$select_x_variable, y = input$select_y_variable)) + geom_line()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
