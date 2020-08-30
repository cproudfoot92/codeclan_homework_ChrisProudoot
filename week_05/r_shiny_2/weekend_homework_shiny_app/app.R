library(ggplot2)
library(shiny)
library(CodeClanData)
library(dplyr)
library(shinythemes)
library(shinyWidgets)

ui <- fluidPage(
    
    theme = shinytheme("slate"),
    
    titlePanel("Game Sales"),
    
    sidebarLayout(
        sidebarPanel(
            pickerInput("platform",
                        "Platform",
                        choices = unique(game_sales$platform),
                        options = list(
                            `actions-box` = TRUE), 
                        multiple = TRUE
            ),
            
            
            pickerInput("genre",
                        "Genre",
                        choices = unique(game_sales$genre),
                        options = list(
                            `actions-box` = TRUE), 
                        multiple = TRUE
            ),
            
            
            sliderInput("year",
                        "Year",
                        min = as.numeric(min(gsub("\\,", "", game_sales$year_of_release))),
                        max = as.numeric(max(gsub("\\,", "", game_sales$year_of_release))), 
                        value = c(min = as.numeric(min(gsub("\\,", "", game_sales$year_of_release))), 
                                  max = as.numeric(max(gsub("\\,", "", game_sales$year_of_release))))
            ),
            # cant get rid of this comma!!!
            sliderInput("score",
                        "Critics Score",
                        min = 0, max = 100, value = c(50, 80)
            ),
            
            
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Title",
                         fluidRow(
                         column(6,
                                imageOutput("image1",width = 100, height = 50)
                         ),
                         column(6,
                                imageOutput("image2", height = 100)
                         ))
                ),
                # tabPanel("Line",
                #          plotOutput("line_plot")
                #          ),
                # 
                # tabPanel("Bar",
                #          plotOutput("bar_plot")
                #          ),
                
                tabPanel("Data",
                         dataTableOutput("table_output")
                         )
            )
        )
    ) 
)

server <- function(input, output, session) {
    
    games <- reactive( {
    
        game_sales %>%
            filter(platform == input$platform)  %>%
            filter(genre == input$genre) %>%
            filter(year_of_release == input$year) %>%
            filter(critic_score == input$score) 
        
    })
    
  
    
    
    
    
    output$image1 <- renderImage({
        if (input$platform %in% c("Wii", "DS", "3DS", "WiiU", "GC", "GBA")) {
            return(list(
                src = "nintendo-logo.png",
                contentType = "png"
            ))
        } 
        
    }, deleteFile = FALSE)
    
    output$image2 <- renderImage({
        if (input$platform %in% c("XB", "X360", "XOne")) {
            return(list(
                src = "xbox-xbox-360-green-gamers-wallpaper-preview.jpg",
                contentType = "jpg",
                alt = "xbox"
            ))
        } 
        
    }, deleteFile = FALSE)
    
    
    output$table_output <- renderDataTable({
        games()
    })
}
shinyApp(ui = ui, server = server)