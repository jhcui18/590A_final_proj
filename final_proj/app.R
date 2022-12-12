#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(renv)
library(readxl)
library(bslib)
thematic::thematic_shiny(font = "auto")

life_exp<- read.csv("Life_Expectancy_00_15.csv", sep = ";")
country_names <- life_exp %>% select(Country) %>% distinct()

# Define UI for application that draws a histogram
ui <- navbarPage("Life Expectancy Visualization Tool",
                 theme = bs_theme(),
                 #data pate
                 tabPanel("Data",
                          titlePanel("Dataset"),
                          
                          # Create a new Row in the UI for selectInputs
                          fluidRow(
                            column(4,
                                   selectInput("Year",
                                               "Year:",
                                               c("All",
                                                 unique(life_exp$Year)))
                            ),
                            column(4,
                                   selectInput("Continent",
                                               "Continent:",
                                               c("All",
                                                 unique(as.character(life_exp$Continent))))
                            ),
                            column(4,
                                   selectInput("Country",
                                               "Country:",
                                               c("All",
                                                 unique(as.character(life_exp$Country))))
                            )
                          ),
                          # Create a new row for the table.
                          DT::dataTableOutput("table")),
                 #end of data page
                 
                 #ranking page
                       tabPanel("Ranking",
                                # Application title
                                titlePanel(h1("Life Expectancy Ranking")),
                                
                                # Sidebar with a slider input for number of bins 
                                sidebarLayout(
                                  sidebarPanel(
                                    p(h5("This function demostrate the life expextancy of the highest and lowest 20 countries in selected year.")),
                                    br(),
                                    p(h3("Check this box to initiate demostration.")),
                                    checkboxInput("ranking", 
                                                  "ranking of selected country"),
                                    p(h4("You can select which year to be demostrated;")),
                                    p(h5("or click the start sign at bottom right of the widge 
              to automatically demostrate the ranking for each year.")),
                                    sliderInput("year_ranking", 
                                                "Input which year (2000 - 2015) you want the ranking in", 
                                                value = 2000,
                                                min = 2000, 
                                                max = 2015, 
                                                step = 1, animate=animationOptions(interval=1000, loop=T)),
                                    p(h3("Select a country name to show its life expectancy from 2000 to 2015")),
                                    selectInput("country_rank", 
                                                "Select country of ranking",
                                                c(" ", country_names), selected = " "), 
                                    helpText("ex. United States")
                                  ),
                                  
                                  # Show a plot of the generated distribution
                                  mainPanel(
                                    plotOutput("Top20_ranking"), 
                                    plotOutput("Bottom20_ranking"),
                                    plotOutput("country_ranking")
                                  )
                                )),
                 #trend page
                       tabPanel("Trend",
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
                                plotOutput("plot")),
                       tabPanel("Component 3")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  bs_themer()
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- life_exp
    if (input$Year != "All") {
      data <- data[data$Year == input$Year,]
    }
    if (input$Continent != "All") {
      data <- data[data$Continent == input$Continent,]
    }
    if (input$Country != "All") {
      data <- data[data$Country == input$Country,]
    }
    data
  }))
  
  #first page: Bar plot coded by Gengtan
  ranking_country <- reactive({
    if (input$year_ranking %in% c(2000:2015)) {
      life_exp %>% filter(Year == input$year_ranking) %>% group_by(Country)
    }
  })
  
  
  output$Top20_ranking <- renderPlot({
    if(isTRUE(input$ranking)){
      top_rank <- ranking_country() %>% arrange(desc(Life.Expectancy)) %>% head(20)
      
      ggplot(top_rank, aes(x = reorder(Country, -Life.Expectancy), y = Life.Expectancy, fill = Country)) + 
        geom_col(show.legend = FALSE, width = 0.5) +
        labs(title = paste("Life expectancy of Top 20 countires in", input$year_ranking),
             x = NULL, y = "Life Expectancy") + theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      
      
    }
    else{
      return(NULL)
    }
    
  })
  
  output$Bottom20_ranking <- renderPlot({
    if(isTRUE(input$ranking)){
      bot_rank <- ranking_country() %>% arrange(Life.Expectancy) %>% head(20)
      
      ggplot(bot_rank, aes(x = reorder(Country, Life.Expectancy), y = Life.Expectancy, fill = Country)) + 
        geom_col(show.legend = FALSE, width = 0.5) + 
        labs(title = paste("Life expectancy of Bottom 20 countires in", input$year_ranking),
             x = NULL, y = "Life Expectancy") + theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
    }
    else{
      return(NULL)
    }
    
  })
  
  output$country_ranking <- renderPlot({
    if(isTRUE(input$ranking) & input$country_rank %in% life_exp$Country){
      country_00_15 <- life_exp %>% filter(Country == input$country_rank)
      
      ggplot(country_00_15, aes(x = Year, y = Life.Expectancy)) + 
        geom_line() + labs(title = paste("Life expectancy of", input$country_rank, "from 2000 to 2015")) + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    }
    else{
      return(NULL)
    }
  })
#first page end here  
  
#second page: time series plot coded by Ryan
  data <- reactive({
    req(input$select_country)
    df <- life_exp %>% filter(Country %in% input$select_country)
  })
  
  observe({
    updateSelectInput(session, "select_country", choices = life_exp$Country)
    updateSelectInput(session, "select_x_variable", choices = c("Life.Expectancy", "Year", "Population", "CO2.emissions", "Health.expenditure", "Electric.power.consumption", "Forest.area", "GDP.per.capita", "Individuals.using.the.Internet", "Military.expenditure", "People.practicing.open.defecation", "People.using.at.least.basic.drinking.water.services", "Obesity.among.adults", "Beer.consumption.per.capita" ))
    updateSelectInput(session, "select_y_variable", choices = c("Life.Expectancy", "Year", "Population", "CO2.emissions", "Health.expenditure", "Electric.power.consumption", "Forest.area", "GDP.per.capita", "Individuals.using.the.Internet", "Military.expenditure", "People.practicing.open.defecation", "People.using.at.least.basic.drinking.water.services", "Obesity.among.adults", "Beer.consumption.per.capita" ))
  })
  
  output$plot <- renderPlot({
    ggplot(data(), aes_string(x = input$select_x_variable, y = input$select_y_variable)) + geom_line()
  })
  #second page end here
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
