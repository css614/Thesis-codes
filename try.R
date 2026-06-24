library(shiny)
#Define UI for application
ui3 <-fluidPage(
  #Header or Title Panel
  titlePanel(title = h3("The location of customers at every second")),
  sidebarLayout(
    #Sidebar panel
    sidebarPanel(
      sliderInput("time","time",min = 0, max = 20000,value = 0, animate=animationOptions(interval=1000, loop=TRUE) )
    ),
    mainPanel(
      plotOutput("mystore3"),
      ## plotOutput("legend")
    )
  )
)



server3<-function(input,output){
  output$mystore3 <- renderPlot({
    plot(x=NA,y=NA,xlim = c(0,50),ylim = c(0,30),xlab = NA,ylab = NA,pch=19,cex=2, bty = "n", xaxt = "n", yaxt ="n", asp=1)
    text(x=c(48,50,15,35), y=c(25,5,15,15), labels=c("entrance","exit","zone 1","zone 2"), cex=1.2, col="red")
    polygon(x=c(0,0,50,50),y=c(0,30,30,0))
    polygon(x=corner_x[1:4],y=corner_y[1:4])
    polygon(x=corner_x[5:8],y=corner_y[5:8])
    
    l<-numeric(customer.numeber)
    for(i in 1:customer.numeber)
    {
      l[i]<-length(which(reserve_time[,i]>0))-1
    }
    
    for(i in 1:customer.numeber)
    {
      points(time_all[input$time,(2*i-1)],time_all[input$time,(2*i)],col=i,pch=19,cex=2)
      
      if(l[i]>0)
      {
        for(j in 1:(l[i]))
        {
          if(input$time<reserve_time[(j+1),i]&input$time>=reserve_time[j,i])
          {
            points(reserve_all[j:l,(2*i-1)],reserve_all[j:l,(2*i)],pch=4,cex=2,col=i)
          }
        }
      }
      
    }
    
    options(warn = -1)
    
    
    
  })
  
}

# Run the app
shinyApp(ui = ui3, server = server3)