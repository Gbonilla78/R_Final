library(tidyverse)
library(shinyjs)
install.packages(shinyjs)

Stress<-c(1,2,3,4,6,7,8,9,7,6,3,5,6,7,8)
Time_Spent<-c(5,6,7,8,4,2,2,5,6,7,8,6,5,6,5)
data1 = data.frame(x=Stress, y=Time_Spent)
    

ui <- fluidPage(
    titlePanel("R Mental Evaluation"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("This is a Drew Steen motivational survey"),
            actionButton("button", "Click me for R Stress levels"),
            useShinyjs(),
            actionLink("button", "Show additional"),
            hidden(div(id='text_div', verbatimTextOutput("text"))),
        
            
            selectInput("var", 
                        label = "Stress level from R",
                        choices = c("Very low", 
                                    "Low",
                                    "Medium", 
                                    "High",
                                    "Very High"),
                        selected = "High"),
            
            sliderInput("range", 
                        label = "Numeric level of stress",
                        min = 0, max = 100, value = 50)
        ),
        
        
        
        mainPanel(
            textOutput("selected_var"),
            plotOutput("distPlot"),
        )
    )

)

server <- function(input, output) {
    
    output$selected_var <- renderText({ 
        paste("You have selected", input$var)
        
    })
    
}

server <- function(input, output) {
    observeEvent(input$button, {
        output$distPlot <- renderPlot({
            plot(data1, mapping = aes(x="Stress", y="Time_Spent") 
                   + geom_count())
            server = function(input, output, session){
                observeEvent(input$button, {
                    toggle('text_div')
                    output$text <- renderText({"Additional"})
                    
                    if (input$button %% 2 == 1) {
                        txt <- "Hide Additional"
                    } else {
                        txt <- "Show Additional"
                    }
                    updateActionButton(session, "button", label = txt)
                })
                
            }
            
            server = function(input, output, session){
                observeEvent(input$button, {
                    toggle('text_div')
                    output$text <- renderText({"Additional"})
                    
                    if (input$button %% 2 == 1) {
                        txt <- "Hide Additional"
                    } else {
                        txt <- "Show Additional"
                    }
                    updateActionButton(session, "button", label = txt)
                })
                
            }
            
            
            
        })
    })
}
shinyApp(ui = ui, server = server)
