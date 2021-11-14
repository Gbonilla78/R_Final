#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#loading the libraries
library(shiny)
library(ggplot2)
library(magrittr)
## Only run examples in interactive R sessions


ui <- fluidPage(
    titlePanel("R mental Check"),
    textInput("name" , "What's your name?"),
    textAreaInput("story", "How much sleep at night do you lose because of R?"),
    sidebarLayout(
        sidebarPanel(
            actionButton("button", "Click to set everything on fire"),
        ),
        mainPanel(
            plotOutput("distPlot"),
            ui <- fluidPage(    
                radioButtons("radio", 
                             label = HTML('<FONT color="red"><FONT size="5pt">Welcome</FONT></FONT><br> <b>Your favorite color is red ?</b>'),
                             choices = list("TRUE" = 1, "FALSE" = 2),
                             selected = 1,
                             inline = T,
                             width = "100%"),      
                fluidRow(column(3, textOutput("value")))) 
        )
    )
)




x <-c(1,2,3,4,6,7,8,9,7,6,3,5,6,7,8)
y <-c(5,6,7,8,4,2,2,5,6,7,8,6,5,6,5)
data1 = data.frame(x=x, y=y)
server <- function(input, output) {
    observeEvent(input$button, {
        output$distPlot <- renderPlot({
            hist(rnorm(9))
            

server <- function(input, output){
                output$value <- renderPrint({
                    if(input$radio == 1){return('Great !')}
                    else{return("Sorry !")}})}
            
            

            
            
        })
    })
}


shinyApp(ui = ui, server = server)



Hello

