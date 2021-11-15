library(shiny)

ui <- fluidPage(
    titlePanel("R Mental Evaluation"),
    
        sidebarPanel(
            helpText("This is a Drew Steen motivational survey"),
            
            actionButton("button", "Click me if you are stressed"),
            
            selectInput("var", 
                        label = "Stress level from R",
                        choices = c("Very low", 
                                    "Low",
                                    "Medium", 
                                    "High",
                                    "Very High"),
                        selected = "High"),
            
    sliderInput("x", "If time in R", min = 1, max = 50, value = 30),
    sliderInput("y", "Projects unfinished", min = 1, max = 50, value = 5),
    "then, (Stress level) is", textOutput("product"),
    

    mainPanel(
    textOutput("selected_var"),
    plotOutput("distPlot"),
    textOutput("text")
)
)
)
server <- function(input, output, session) {
    output$product <- renderText({ 
        product <- input$x * input$y
        product
        
        
        })
    }
    
    
#error lies somewhere in here
#when adding function here other function doesnt work
server <- function(input, output, session){
    observeEvent(input$button, {
    output$text <- renderText({"ahh you pressed it"})

})
    
}

     


shinyApp(ui, server)