A1<-0.2108
M<-0.148 #delta*(c*A1)*log(20/(c*A1)+1)
c<-70.58419

J_left_record<-matrix(data=100000000,nrow = 1,ncol=100)

J_spend_record<-matrix(data=NA,nrow = 1,ncol=100)

M_exp_record<-matrix(data = NA, nrow = 1,ncol = 3600)
#M_f_record<-matrix(data = NA, nrow = 1,ncol = 3600)

for(number in 1:1)
{
  J<-matrix(data=100000000,nrow = 3600,ncol=100)
  J_left<-matrix(data=NA,nrow = 3600,ncol=100)
  
  entry_frequency<-(1/3600)*c(10:60)
  entry_frequency<-(1/3600)*c(30,35,41,42,45,50,55,60,65,70,75,80,85,90,95,100)
  entry_frequency<-(1/3600)*45
  
  M_exp<-matrix(data = NA, nrow = 3600,ncol = 3600)
  M_f<-matrix(data = NA, nrow = 3600,ncol = 3600)
  
  for(delta in entry_frequency)
  {
    #keep the expenditure for each customer as the same 
    #M_num<-rep(15,3600)
    ## change the target expenditure for each customer according to the distribution
    M_num<-ceiling(rgamma(3600,3,0.2)) 
    M_exp[,delta*3600]<-M*M_num/15
    for(r in 1:3600)
    {
      M_f[r,1:r]<-M_exp[r,delta*3600]/delta
    }
    #M_f[,1]<-M_exp[,1]/delta
    #####################################################
    
    r<-2
    
    while(r <= 3600)
    {
      J_left[r,delta*3600]<-r-length(which(J[,delta*3600]<=r)) 
      # record # of customers at time r
      i<-1
      while(i %in% which(J[,delta*3600]<=r)){i<-i+1} 
      ## skip the customers who has completed the shopping 
      ## since now there is the distribution, so we can not just skip the first several customers
      ## maybe the latter customer leave the store at first
      ## so here we are using %in% function
      while(i<r)
      {
        record<-M_f[i,r-1]-1/(1+(J_left[r,delta*3600]-1)/(c*A1))
        
        if(M_f[i,r-1]>0 & record<=0 )
        {
          if(abs(record)+0.035<M_f[i,r-1] | (r-max(J[which(J[,delta*3600]<10000),delta*3600]))==2)
          {
            J[i,delta*3600]<-r-1
            M_f[i,r]<-record
            r<-r-2
            break
            
          }else{
            J[i,delta*3600]<-r-2
            M_f[i,r]<-record
            r<-r-3 ## since r will add 1 automatically w.r.t the loop, but I need to go back to time r-1
            break
          }
          
        }else if(M_f[i,r-1]>0 & record>0 ){
          M_f[i,r]<-record
          i<-i+1
          while(i %in% which(J[,delta*3600]<=r)){i<-i+1}
        }else{
          i<-i+1
          while(i %in% which(J[,delta*3600]<=r)){i<-i+1}
        }
        
        
      }
      
      r<-r+1
      
    }
    
   
  }
  
  J_spend<-matrix(data=rep(0,360000),nrow = 3600,ncol=100)
  for(i in c(30,35,41,42,45,60,65,70,75,80,85,90,95,100))
  {
    for(j in 1:3600)
    {
      J_spend[j,i]<-J[j,i]-j
    }
  }
  
  
  J_left_record<-rbind(J_left_record,J_left[-c(1:100),])
  
  J_spend_record<-rbind(J_spend_record,J_spend[-c(1:100),])
  
  M_exp_record<-rbind(M_exp_record,M_exp)
  #M_f_record<-rbind(M_f_record,M_f)
  
  print(number)
}













