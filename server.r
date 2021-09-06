library(shiny)
shinyServer(function(input,output){
  output$mystore <- renderPlot({
    plot(x=NA,y=NA,xlim = c(0,30),ylim = c(0,30),xlab = NA,ylab = NA,pch=19,cex=2,asp = 1, bty = "n", xaxt = "n", yaxt ="n")
    text(x=c(29.5,30),y=c(25,5),c("entrance","exit"))
    polygon(x=c(0,0,30,30),y=c(0,30,30,0))
    polygon(x=corner_x,y=corner_y)
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
  
})