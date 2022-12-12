#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(tidyverse)

life_exp <- read.csv(paste("Life_Expectancy_00_15.csv"), sep = ";")
country_names <- life_exp %>% select(Country) %>% distinct()
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(h1("590A final project")),

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
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
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
}

# Run the application 
shinyApp(ui = ui, server = server)
