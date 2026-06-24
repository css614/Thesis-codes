A1<-0.2108
M<-0.148 #delta*(c*A1)*log(20/(c*A1)+1), using delta=42/3600
c<-70.58419

#########################################################################
####### set the initial number of customers that the system starting with
## for example, for the boxing day or black Friday
## there will be lots of people waiting outside of the store
## as long as the store starts to be open
## there will dozens of customers enter the store at the same time.
pre_customer<-40

######
#########################################################################
result<-J_left[,1]
t<-0
while(t<3)
{
  J<-matrix(data=rep(100000000,360000),nrow = 3600,ncol=100)
  J_left<-matrix(data=NA,nrow = 3600,ncol=100)
  error<-matrix(data=NA,nrow = 3600,ncol=100)
  
  #entry_frequency<-(1/3600)*c(10:60)
  #entry_frequency<-(1/3600)*c(30,35,41,42,45,50,55,60,65,70,75,80,85,90,95,100)
  entry_frequency<-(1/3600)*c(35,80)
  
  M_exp<-matrix(data = NA, nrow = 3600,ncol = 3600)
  M_f<-matrix(data = NA, nrow = 3600,ncol = 3600)
  
  for(delta in entry_frequency)
  {
    #keep the expenditure for each customer as the same 
    #M_num<-rep(15,3600)
    ## change the target expenditure for each customer according to the distribution
    #M_num<-ceiling(rgamma(3600,3,0.2)) 
    M_num<-rgamma(3600,150,10)
    M_exp[,delta*3600]<-M*M_num/15
    
    for(r in 1:pre_customer)
    {
      M_f[r,1]<-M_exp[r,delta*3600]/delta
    }
    
    for(r in (pre_customer+1):3600)
    {
      M_f[r,1:(r-pre_customer+1)]<-M_exp[r,delta*3600]/delta
    }
    #M_f[,1]<-M_exp[,1]/delta
    #####################################################
    
    r<-2
    
    while(r <= (3600-pre_customer+1))
    {
      J_left[r-1,delta*3600]<-(r-1)+(pre_customer-1)-length(which(J[,delta*3600]<=(r-1))) 
      # record # of customers at time r-1
      # since now we are considering the expenditure during [r-1,r]
      i<-1
      while(i %in% which(J[,delta*3600]<=r-1)){i<-i+1}
      ## skip the customers who has completed the shopping 
      ## since now there is the distribution, so we can not just skip the first several customers
      ## maybe the latter customer leave the store at first
      ## so here we are using %in% function
      
      while(i<(r+pre_customer-1)) 
      ## Just calculate the customers who has entered before time r, 
      ## but at time 1, there were already pre_customer entering
      ## so at time r, there are (r+pre_customer-1) in the store
      {
        record<-M_f[i,r-1]-1/(1+(J_left[r-1,delta*3600]-1)/(c*A1))
        
        if(M_f[i,r-1]>0 & record<=0 )
        {
          # max function refer to the customer who has left the store at the latest time
          # if let J[i,delta*3600]=r-2, then there will be 2 customers leave the store at the same time
          # since r has increased by 1 automatically.
          
          #if(abs(record)+0.035<M_f[i,r-1] | (r-max(J[which(J[,delta*3600]<10000),delta*3600]))==2)
          #{
          #  J[i,delta*3600]<-r
          #  M_f[i,r]<-record
          #  i<-i+1
          #  while(i %in% which(J[,delta*3600]<=r-1)){i<-i+1}
          #}else{
          #  J[i,delta*3600]<-r-1
          #  M_f[i,r]<-record
          #  r<-r-1 ## since r will add 1 automatically w.r.t the loop, but I need to go back to time r-1
          #  break
          #}
          
          J[i,delta*3600]<-r
          M_f[i,r]<-record
          i<-i+1
          while(i %in% which(J[,delta*3600]<=r-1)){i<-i+1}
          
        }else if(M_f[i,r-1]>0 & record>0 ){
          M_f[i,r]<-record
          i<-i+1
          while(i %in% which(J[,delta*3600]<=r-1)){i<-i+1}
        }else{ # skip the the customer who has left (i.e. with negative M_f)
          i<-i+1
          while(i %in% which(J[,delta*3600]<=r-1)){i<-i+1}
        }
        
        
      }
      
      r<-r+1
      
    }
  
    print(delta*3600)  
    
  }
  result<-cbind(result,J_left[,entry_frequency*3600])
  t<-t+1
  print(t)
}




##############

J_left_pre1_3_fix<-J_left
J_left_pre1_150_fix<-J_left
J_left_pre20_3_fix<-J_left
J_left_pre20_150_fix<-J_left
J_left_pre40_3_fix<-J_left 
J_left_pre40_150_fix<-J_left
write.csv(J_left_pre1_3_fix,"J_left_pre1_3_fix")

plot(x=c(30*(1:round(20000/30))),y=J_left_pre40_150_fix[1:round(20000/30),30],col=30,cex=0.2
     ,xlim=c(0,20000),ylim=c(0,150),xlab = " time (seconds)"
     ,ylab = expression(J[r]-K[r]),main = "# of customers in the store Gamma(150,10)")
for(i in c(35,41,45,50,60,80))
{
  lines(x=c(i*(1:round(20000/i))),y=J_left_pre40_150_fix[1:round(20000/i),i],col=i,lwd = 2)
}
legend(-400,150,c("35s","41s","45s","50s","60s","80s")
       ,bty = "n",cex = 0.8,text.col = c(35,41,45,50,60,80),pch = 19,col = c(35,41,45,50,60,80))

##############

plot(x=c(50*(1:round(40000/50))),y=result[1:round(40000/50),1],col="black",cex=0.2
     ,xlim=c(0,40000),ylim=c(0,60),xlab = " time (seconds)",type= "l"
     ,ylab = expression(J[r]-K[r]),main = "# of customers in the store Gamma(150,10)")

lines(x=c(50*(1:round(40000/50))),y=result[1:round(40000/50),5],col="blue")

lines(x=c(50*(1:round(40000/50))),y=J_left_pre40_150_draw[1:round(40000/50),50],col="blue")

a<-50
plot(x=c(a*(1:round(80000/a))),y=J_left[1:round(80000/a),a],col="black",cex=0.2
     ,xlim=c(0,80000),ylim=c(0,120),xlab = " time (seconds)",type= "l"
     ,ylab = expression(J[r]-K[r]),main = "# of customers in the store Gamma(150,10)")


plot(x=c(80*(1:round(20000/80))),y=J_left[1:round(20000/80),80],col="black",cex=0.2
     ,xlim=c(0,20000),ylim=c(0,60),xlab = " time (seconds)",type= "l"
     ,ylab = expression(J[r]-K[r]),main = "# of customers in the store Gamma(150,10)")

lines(x=c(80*(1:round(20000/80))),y=J_left_pre40_150_draw[1:round(20000/80),80],col="blue",lwd=2)


######################################################################
J_left_pre20_150_draw<-read.csv("J_left_pre20_150_draw.csv")[,-1]
M_exp_pre20_150_draw<-M_exp
write.csv(J_left_pre20_150_draw,"J_left_pre20_150_draw.csv")
M_f__pre20_150<-M_f
plot(x=c(80*(1:round(20000/80))),y=J_left_pre20_150_draw[1:round(20000/80),80],col="black",cex=0.2
     ,xlim=c(0,20000),ylim=c(0,40),xlab = " time (seconds)",type= "l"
     ,ylab = expression(J[r]-K[r]),main = "# of customers in the store with 20 in advance")
plot(x=c(50*(1:round(40000/50))),y=J_left_pre20_150_draw[1:round(40000/50),50],col="black",cex=0.2
     ,xlim=c(0,40000),ylim=c(0,70),xlab = " time (seconds)",type= "l"
     ,ylab = expression(J[r]-K[r]),main = "# of customers in the store with 20 in advance")



J_left_pre40_150_draw[,42]<-J_left[,42]
write.csv(J_left_pre40_150_draw,"J_left_pre40_150_draw.csv")
plot(x=c(80*(1:round(20000/80))),y=J_left_pre40_150_draw[1:round(20000/80),80],col="black",cex=0.2
     ,xlim=c(0,20000),ylim=c(0,70),xlab = " time (seconds)",type= "l"
     ,ylab = expression(J[r]-K[r]),main = "# of customers in the store with 20 in advance")
plot(x=c(50*(1:round(40000/50))),y=J_left_pre40_150_draw[1:round(40000/50),50],col="black",cex=0.2
     ,xlim=c(0,40000),ylim=c(0,100),xlab = " time (seconds)",type= "l"
     ,ylab = expression(J[r]-K[r]),main = "# of customers in the store with 20 in advance")

lines(x=c(50*(1:round(40000/50))),y=J_left_pre40_3_draw[1:round(40000/50),50],col="red")
lines(x=c(80*(1:round(20000/80))),y=J_left_pre40_3_draw[1:round(20000/80),80],col="red")

legend(10000,100,c("gamma(3,0.2)","gamma(150,10)"),col = c("red","black")
       ,text.col = c("red","black"),bty = "n",cex = 1,pch = 19)
legend(25000,100,"delta=50",bty="n",cex=1)


############## test csv file
a<-read.csv("J_left_pre40_150_draw.csv")[,-1]
plot(x=c(80*(1:round(20000/80))),y=a[1:round(20000/80),80],col="black",cex=0.2
     ,xlim=c(0,20000),ylim=c(0,70),xlab = " time (seconds)",type= "l"
     ,ylab = expression(J[r]-K[r]),main = "# of customers in the store Gamma(150,10)")
b<-read.csv("J_left_pre40_3_draw.csv")[,-1]
plot(x=c(50*(1:round(40000/50))),y=b[1:round(40000/50),50],col="black",cex=0.2
     ,xlim=c(0,40000),ylim=c(0,100),xlab = " time (seconds)",type= "l"
     ,ylab = expression(J[r]-K[r]),main = "# of customers in the store Gamma(150,10)")
#############


J_left_pre20_3_draw[,c(42,45)]<-J_left[,1]
M_exp_pre20_3_draw<-M_exp
write.csv(J_left_pre20_3_draw,"J_left_pre20_3_draw.csv")
M_f__pre20_3<-M_f
plot(x=c(80*(1:round(20000/80))),y=J_left_pre20_3_draw[1:round(20000/80),80],col="black",cex=0.2
     ,xlim=c(0,20000),ylim=c(0,40),xlab = " time (seconds)",type= "l"
     ,ylab = expression(J[r]-K[r]),main = "# of customers in the store Gamma(150,10)")
plot(x=c(50*(1:round(40000/50))),y=J_left_pre20_3_draw[1:round(40000/50),50],col="black",cex=0.2
     ,xlim=c(0,40000),ylim=c(0,60),xlab = " time (seconds)",type= "l"
     ,ylab = expression(J[r]-K[r]),main = "# of customers in the store Gamma(150,10)")


J_left_pre40_3_draw[,c(42,45)]<-J_left[,c(42,45)]
write.csv(J_left_pre40_3_draw,"J_left_pre40_3_draw.csv")
plot(x=c(80*(1:round(20000/80))),y=J_left_pre40_3_draw[1:round(20000/80),80],col="black",cex=0.2
     ,xlim=c(0,20000),ylim=c(0,70),xlab = " time (seconds)",type= "l"
     ,ylab = expression(J[r]-K[r]),main = "# of customers in the store Gamma(150,10)")
plot(x=c(50*(1:round(40000/50))),y=J_left_pre40_3_draw[1:round(40000/50),50],col="black",cex=0.2
     ,xlim=c(0,40000),ylim=c(0,60),xlab = " time (seconds)",type= "l"
     ,ylab = expression(J[r]-K[r]),main = "# of customers in the store Gamma(150,10)")






