library(shiny)
library(plotly)
library(ggplot2)



# Define server logic for random distribution application
server <- function(input, output) {
  
  output$myplot <- renderPlot({
    a = input$a
    m = input$m
    c = input$c
    k = input$k
    q0 = input$q0
    res = NULL
    x = seq(0,12,0.1)
    for (qi in x) {
      res = c(res,( a + m/(1+c*exp(-(qi-q0)/k))))
    }
    df = data.frame(x,res)
    ggplot(df,aes(x=x,y=res)) + geom_line() + xlab("q") + ylab("Q")
  }, height = 500, width = 600)
 
}

##### UI #####

ui <- shinyUI(fluidPage(
  
  titlePanel("Logistic Funcion"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput('q0','q0',min=-1,max=10,value=0,step=0.01),
      sliderInput('k','k',min=0.1,max=10,value=1,step=0.01),
      sliderInput('a','a',min=-5,max=5,value=-0.5,step=0.01),
      sliderInput('m','m',min=0.0,max=5,value=1.5,step=0.01),
      sliderInput('c','c',min=0.0,max=5,value=2,step=0.01)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Logistic Function", plotOutput("myplot",width="100%"))
      )
    )
  )
))

##### Run #####
shinyApp(ui = ui, server = server)