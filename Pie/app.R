library(shiny)
library(ggplot2)
x <-c(1,2,3,4,6,7,8,9,7,6,3,5,6,7,8)
y <-c(5,6,7,8,4,2,2,5,6,7,8,6,5,6,5)
data1 = data.frame(x=x, y=y)

ui <- fluidPage(
    titlePanel("R Mental Evaluation"),
    
    sidebarPanel(
        helpText("This is a Drew Steen motivational survey"),
        
        actionButton("button", "Click me if you are stressed"),
        actionButton("button", "Click me for R Stress levels"),
        selectInput("var", 
                    label = "Stress level from R",
                    choices = c("Very low", 
                                "Low",
                                "Medium", 
                                "High",
                                "Very High"),
                    selected = "High"),
        
        sliderInput("x", " time in R", min = 1, max = 50, value = 30),
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
    output$selected_var <- renderText({ 
        paste("You have selected", input$var)
    })   
    observeEvent(input$button, {
        output$text <- renderText({"You arent stressed, back in my day we had to write R code on a calculator and use it to power jets. Get back to work"})    
    })
    output$distPlot <- renderPlot({
        plot(data1, mapping = aes(x="Stress", y="Time_Spent")  
             
             + geom_count())    
        #error lies somewhere in here
        #when adding function here other function doesnt work
        
        
        
    })
    
}
shinyApp(ui = ui, server = server)   