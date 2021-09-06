library(shiny)
#Define UI for application
shinyUI(fluidPage(
  #Header or Title Panel
  titlePanel(title = h3("The location of customers at every second")),
  sidebarLayout(
    #Sidebar panel
    sidebarPanel(
      sliderInput("time","time",min = 0, max = 3600,value = 0, animate=animationOptions(interval=200, loop=TRUE) )
    ),
    mainPanel(
      plotOutput("mystore"),
      ## plotOutput("legend")
    )
  )
)
)