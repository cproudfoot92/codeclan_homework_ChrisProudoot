library(ggplot2)
library(shiny)
library(CodeClanData)
library(dplyr)
library(shinythemes)

ui <- fluidPage(
    
    theme = shinytheme("slate"),
    
    titlePanel("Game Sales"),
    
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("platform",
                               "Platform",
                               choices = unique(game_sales$platform)
            ),
            actionLink("selectall","Select All"),
            
            checkboxGroupInput("genre",
                               "Genre",
                               choices = unique(game_sales$genre)
            ),
            actionLink("selectall2","Select All"),
            
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
                         column(6,
                                imageOutput("image1", height = 150)
                         ),
                         column(6,
                                imageOutput("image2", height = 150)
                         )
                ),
                # tabPanel("Line",
                #          plotOutput("line_plot")
                #          ),
                # 
                # tabPanel("Bar",
                #          plotOutput("bar_plot")
                #          ),
                
                tabPanel("Data",
                         tableOutput("table_output")
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
            filter(critic_score == input$score) %>%
            slice(1:10)
        
    })
    observe({
        if(input$selectall == 0) return(NULL) 
        else if (input$selectall %% 2 == 0)
        {
            updateCheckboxGroupInput(session,
                                     "platform",
                                     "Platform",
                                     choices = unique(game_sales$platform))
        }
        else
        {
            updateCheckboxGroupInput(session,
                                     "platform",
                                     "Platform",
                                     choices = unique(game_sales$platform), selected = unique(game_sales$platform))
        }
    })
    observe({
        if(input$selectall2 == 0) return(NULL) 
        else if (input$selectall2 %% 2 == 0)
        {
            updateCheckboxGroupInput(session,
                                     "genre",
                                     "Genre",
                                     choices = unique(game_sales$genre))
        }
        else
        {
            updateCheckboxGroupInput(session,
                                     "genre",
                                     "Genre",
                                     choices = unique(game_sales$genre), selected = unique(game_sales$genre))
        }
    })
    # output$line_plot <- renderPlot({
    #     ggplot() +
    #         aes()
    # })
    
    
    
    
    
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
    
    
    output$table_output <- renderTable({
        games()
    })
}
shinyApp(ui = ui, server = server)