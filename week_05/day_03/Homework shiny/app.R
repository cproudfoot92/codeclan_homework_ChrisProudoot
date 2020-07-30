library(shiny)
library(dplyr)
library(ggplot2)
library(CodeClanData)
library(shinythemes)




all_teams <- unique(olympics_overall_medals$team)
ui <- fluidPage(
    
    theme = shinytheme("slate"),
    
    titlePanel("Olympic Medals"),
    
    sidebarLayout(
        sidebarPanel(
            radioButtons("season",
                         "Summer or Winter Olympics?",
                         choices = c("Summer", "Winter")
            ),
            selectInput("team",
                        "Which Team?",
                        choices = all_teams
            ),
            tags$a("The Olympics website",
                   href = "https://www.Olympic.org/")
        ),
        mainPanel(
            tabsetPanel(
                
                tabPanel("Plot",
                         plotOutput("medal_plot",
                                    height='600px',
                                    width='1000px')
                ),
                
                tabPanel("You saying? Bolt!",
                         img(src =
                                 "https://media.giphy.com/media/ZhutRFPxdR8Yg/giphy.gif",
                             align = "left",
                             height='600px',
                             width='1000px')
                )
                
            )
        )
    )
)
server <- function(input, output) {
    output$medal_plot <- renderPlot({
        olympics_overall_medals %>%
            filter(team == input$team) %>%
            filter(season == input$season) %>%
            ggplot() +
            aes(x = medal, y = count, fill = medal) +
            geom_col()
    })
}
shinyApp(ui = ui, server = server)