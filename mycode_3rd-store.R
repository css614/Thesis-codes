for(entrance_interval in entrance_interval_sequence)
{
  while(success[1,entrance_interval]<1) ## 成功运行的次数，相当于monte-carlo，最后以平均值作为最后的估计值
  {
    customer.numeber<-1
    
    freezing<-"no freezing"
    
    ################    Create the initial matrix
    type_1_matrix<-matrix(data=0,ncol=1,nrow=1)
    type_2_matrix<-matrix(data=NA,ncol=1,nrow=100000)
    type_3_matrix<-matrix(data=NA,ncol=2,nrow=100000)
    
    ################    Record the number of customers in the store every second
    customer_number_in_store<-type_2_matrix
    ################
    
    ################     Create the initial shopping list of the first customer
    customer.all<-shopping_list("count",3,0.2,15)
    ###Record the customer's next shopping point
    #######
    #next_shopping_point<-matrix(data=nearest_point(customer.all,entrance[1],entrance[2]),nrow = 2,ncol=1)
    #######
    
    next_shopping_point<-matrix(data=shopping_list("count",15,2,1)[1,],nrow = 2,ncol=1)
    ################
    
    ################     coordinates of the corners of vegetable zone 
    #corner_x<-c(10,10,20,20,30,30,40,40)
    #corner_y<-c(10,20,20,10,10,20,20,10)
    #corner<-cbind(corner_x,corner_y)
    ################ 
    
    ########################     Initial value setting
    shopping_time<-type_1_matrix
    change<-type_1_matrix
    change_time<-type_2_matrix
    reserve_all<-type_3_matrix
    reserve_number<-type_1_matrix
    reserve_time<-type_2_matrix
    
    time_all<-type_3_matrix
    time_all[1,1:2]<-entrance
    
    
    B<-type_1_matrix
    L.B<-type_2_matrix
    
    type_4_matrix<-matrix(data=c(10000,10000),nrow=1,ncol=2)
    risktime<-matrix(data=c(1.5,2),byrow = T,ncol=2,nrow=1)
    
    type_5_matrix<-matrix(data=16-entrance_interval ,ncol=1,nrow=1)
    ## since the time start from 1, for the latter customer,they can not move
    ## so we need to set their "fix" smaller than 15, then they will not move
    ## keep as the initial point
    ## since after each time fix<fix+1, so after enough time, this customer could enter
    ## and at that time, their "fix" has been added to larger than 15
    ## this means the cycle for this customer could start
    
    fix<-matrix(data=16,ncol=1,nrow=1)
    ## "fix" for 1st customer has already been 16 which is larger than 15
    ## so the cycle for 1st customer will start immediately.
    ######################## 
    
    
    
    ######################## cycle begins, which means customers start to enter the store for shopping 
    for(i in 1:14399)
    {
      
      if(i==1)
      {
        customer_number_in_store[1]<-1
      }else
      {
        customer_number_in_store[i]<-customer_number_in_store[i-1]
      }
      
      ########################  Matrix changes after new customers entering
      
      if (floor((i-1)/entrance_interval)>=0&((i-1)/entrance_interval)==floor((i-1)/entrance_interval))
      {
        ## Record the total number of customers after adding new customers
        customer.numeber<-customer.numeber+1
        
        customer_number_in_store[i]<-customer_number_in_store[i]+1  
        
        ###################  Judge if it is freezing at the door
        if(customer.numeber>=3)
        {
          if(time_all[i,(2*(customer.numeber-2)-1)]==entrance[1]&time_all[i,2*(customer.numeber-2)]==entrance[2]) ## 上一个顾客已经被堵在门口不能动了
          {
            freezing<-"stuck"
            print("stuck")
            break
          }
        }
        
        ###################  Record of some data indicators
        shopping_time<-cbind(shopping_time,type_1_matrix)
        change<-cbind(change,type_1_matrix)
        change_time<-cbind(change_time,type_2_matrix)
        reserve_all<-cbind(reserve_all,type_3_matrix)
        reserve_number<-cbind(reserve_number,type_1_matrix)
        reserve_time<-cbind(reserve_time,type_2_matrix)
        #########################
        
        ###################  Main route change data 
        time_all<-cbind(time_all,type_3_matrix)
        time_all[1:((customer.numeber-1)*entrance_interval+1),(2*customer.numeber-1)]<-entrance[1]
        time_all[1:((customer.numeber-1)*entrance_interval+1),(2*customer.numeber)]<-entrance[2]
        
        B<-cbind(B,type_1_matrix)
        L.B<-cbind(L.B,type_2_matrix)
        ###################
        
        ###################  Indicators for judging whether to enter the loop
        risktime<-rbind(risktime,type_4_matrix)
        
        fix<-cbind(fix,type_5_matrix)
        
        ###################  Changes in customer shopping lists 
        x<-shopping_list("count",3,0.2,15)
        customer.all<-cbind(customer.all,x) ## 将新进入的顾客的信息合并到之前的矩阵里面
        
        #####
        #xx<-matrix(data=nearest_point(x,entrance[1],entrance[2]),nrow = 2,ncol=1)
        #####
        xx<-matrix(data=shopping_list("count",15,2,1)[1,],nrow=2,ncol=1)
        
        next_shopping_point<-cbind(next_shopping_point,xx)
        ##################   risktime also changes
        if(i>1)
        {
          risktime<-my_min_distance_change(i-1,i,time_all)
        }
        
      }
      
      ########################
      
      
      for(j in 1:customer.numeber)
      {
        if(fix[j]>15) ## customer will stay at each shopping point for 15 seconds, so we don't need to consider collision within these 15 seconds
        {
          ##customers who may collide with other people or need to be back again
          if(risktime[j,1]<=i|B[j]>0)  
          {
            ##For the convenience, change the coordinates of the customers who did not enter the store 
            if(risktime[j,1]<i&j<customer.numeber)
            {
              for(t in (j+1):customer.numeber)
              {
                time_all[i,(2*t-1):(2*t)]<-10000*t
              }
            }
            ###########################################
            ###########################################
            
            ## Determine the position of other customers relative to this customer
            dd<-numeric(customer.numeber)
            other_customer<-matrix(data=NA,nrow=2,ncol=customer.numeber-1)
            for(k in 1:customer.numeber)
            {
              if(k>j)
              {
                dd[k]<-sqrt((time_all[i,2*k-1]-time_all[i,2*j-1])^2+(time_all[i,2*k]-time_all[i,2*j])^2)
                other_customer[1:2,(k-1)]<-time_all[i,(2*k-1):(2*k)]
              }else if(k<j)
              {
                dd[k]<-sqrt((time_all[i+1,2*k-1]-time_all[i,2*j-1])^2+(time_all[i+1,2*k]-time_all[i,2*j])^2)
                other_customer[1:2,k]<-time_all[i+1,(2*k-1):(2*k)]
              }else
              {
                dd[k]<-1000000000
              }
              nearest_customer<-which.min(dd)
            }
            ###########################################
            ###########################################
            
            
            ## Determine whether the customer could continue to go back
            if(B[j]>0)
            {
              back_x<-2*time_all[i,2*j-1]-time_all[i-1,2*j-1]
              back_y<-2*time_all[i,2*j]-time_all[i-1,2*j]
              dis_back<-sqrt((other_customer[1,]-back_x)^2+(other_customer[2,]-back_y)^2)
              if(setequal(which(dis_back<2),integer(0))&in_the_zone(back_x,back_y)==1)
              {back<-1}else
              {back<-0}
            }else
            {back<-0}
            
            ##Back is 1 means that the customer can continue to go back, 
            ##and 0 means that the customer cannot continue to go back
            
            ###########################################
            ###########################################
            
            ## Judge the direction of the customer
            if(back==1)
            {
              time_all[i+1,2*j-1]<-back_x
              time_all[i+1,2*j]<-back_y
              change[j]<-change[j]+1
              change_time[change[j],j]<-i
              B[j]<-B[j]-1
              L.B[change[j],j]<-i
            }else
            {
              if(j<nearest_customer)
              {
                judge_change<-judgment_direction(time_all[i,2*j-1],time_all[i,2*j],next_shopping_point[1,j],next_shopping_point[2,j],time_all[i,2*nearest_customer-1],time_all[i,2*nearest_customer],next_shopping_point[1,nearest_customer],next_shopping_point[2,nearest_customer],other_customer)
              }else
              {
                judge_change<-judgment_direction(time_all[i,2*j-1],time_all[i,2*j],next_shopping_point[1,j],next_shopping_point[2,j],time_all[i+1,2*nearest_customer-1],time_all[i+1,2*nearest_customer],next_shopping_point[1,nearest_customer],next_shopping_point[2,nearest_customer],other_customer)
              }
              
              time_all[(i+1),(2*j-1):(2*j)]<-judge_change[2:3]
              if(judge_change[1]==0)
              {
                change[j]<-change[j]+1
                change_time[change[j],j]<-i
              }
              B[j]<-judge_change[4]+2   ##prevent premature blockage 
            }
            ###########################################
            ###########################################
            
            ## Change back the coordinates that were changed for calculation convenience
            
            if(risktime[j,1]<i&j<customer.numeber)
            {
              for(t in (j+1):customer.numeber)
              {
                time_all[i,(2*t-1)]<-entrance[1]
                time_all[i,(2*t)]<-entrance[2]
              }
            }
            
            risktime<-my_min_distance_change(i,j,time_all)
            
            ###########################################
            
          }else   
            ##If it is found that this customer will not collide with other customers 
            ##and does not need to continue backing, then walk normally
          {
            if(judgment(time_all[i,2*j-1],time_all[i,2*j],next_shopping_point[1,j],next_shopping_point[2,j])==1)
            {
              really_nc1<-judgment_corner(time_all[i,2*j-1],time_all[i,2*j],next_shopping_point[1,j],next_shopping_point[2,j])
              aa<-really_nc1[2]
              bb<-really_nc1[3]
            }else
            {
              aa<-next_shopping_point[1,j]
              bb<-next_shopping_point[2,j]
            }
            
            time_all[(i+1),(2*j-1):(2*j)]<-movement(time_all[i,2*j-1],time_all[i,2*j],aa,bb)
          }
          
          ########################################### 
          ###########################################
          
          ###########################################  Delete the shopping point that has arrived
          delete<-abs(next_shopping_point[1,j]-time_all[(i+1),(2*j-1)])+abs(next_shopping_point[2,j]-time_all[(i+1),(2*j)])
          #delete等于0，表示已经抵达这个购物点（或者是这个目的地）
          if(delete==0)
          {
            ## since the 1st shopping point is random and we didn't collect it into customer.all
            ## so we need to find whether now this customer has arrived 1st point
            delete_shopping_point<-abs(customer.all[,(2*j-1)]-next_shopping_point[1,j])+abs(customer.all[,(2*j)]-next_shopping_point[2,j])
            ## if delete_shopping_point == 0 is integer(0), means this customer arrives 1st point or the exit
            ## we don't need to do anything to our customer.all
            ## otherwise, we need to delete this point.
            if(!setequal(which(delete_shopping_point==0),integer(0)))
            {
              customer.all[which(delete_shopping_point==0),(2*j-1)]<-10000
              customer.all[which(delete_shopping_point==0),(2*j)]<-10000
            }
            ## After the delete, we need to check whether there is any more shopping points left
            
            
            if(setequal(which(customer.all[,(2*j-1)]<10000),integer(0))&(abs(time_all[(i+1),(2*j-1)]-exit[1])+abs(time_all[(i+1),(2*j)]-exit[2]))!=0)
            {
              ## If the shopping list has been emptied and the customer is not at the exit at this time, 
              ## then add the exit to the shopping list and let the customer continue to walk out of the store
              next_shopping_point[1,j]<-exit[1]
              next_shopping_point[2,j]<-exit[2]
              
              fix[j]<-1 ## the customer still need to reach the exit, so "fix" back to 1 is enough
              time_all[(i+2):(i+15),(2*j-1)]<-time_all[(i+1),(2*j-1)]
              time_all[(i+2):(i+15),(2*j)]<-time_all[(i+1),(2*j)]
              
              reserve_number[j]<-reserve_number[j]+1
              reserve_all[reserve_number[j],(2*j-1):(2*j)]<-time_all[(i+1),(2*j-1):(2*j)]
              reserve_time[reserve_number[j]+1,j]<-i+15
              
            }else if((setequal(which(customer.all[,(2*j-1)]<10000),integer(0))&(abs(time_all[(i+1),(2*j-1)]-exit[1])+abs(time_all[(i+1),(2*j)]-exit[2]))==0))
            {
              ## If the shopping list has been emptied and the exit has been reached,
              ## the customer cycle ends
              fix[j]<--500000 ## the customer has left the store and will never enter the cycle, so set "fix" to be small enough
              time_all[(i+2):100000,(2*j-1)]<-10000*j
              time_all[(i+2):100000,(2*j)]<-10000*j
              
              ## When the customer arrives at the exit, clear the coordinates of the entrance
              next_shopping_point[1:2,j]<-10000
              
              ## The number of customers in the store should be -1
              customer_number_in_store[i]<-customer_number_in_store[i]-1
              
            }else
            {
              ## If the shopping list has not been emptied yet, delete the arrived ones first, 
              ## and then assign a new point to next_shopping_point
              
              ## If it is a point in the shopping list, 
              ## delete the point in the shopping list at the same time
              
              
              next_shopping_point[1:2,j]<-nearest_point(customer.all[,(2*j-1):(2*j)],time_all[i+1,2*j-1],time_all[i+1,2*j])
              
              
              fix[j]<-1
              time_all[(i+2):(i+15),(2*j-1)]<-time_all[(i+1),(2*j-1)]
              time_all[(i+2):(i+15),(2*j)]<-time_all[(i+1),(2*j)] 
              
              reserve_number[j]<-reserve_number[j]+1
              reserve_all[reserve_number[j],(2*j-1):(2*j)]<-time_all[(i+1),(2*j-1):(2*j)]
              reserve_time[reserve_number[j]+1,j]<-i+15
            }
          }
          
          
          ########################################### 
          
          ## If customer stay in one place for 30 seconds and still haven’t left, 
          ## change to another shopping point
          if(i>(29+(j-1)*entrance_interval)&time_all[i+1,(2*j-1)]<10000)
          {
            if((sum(abs(time_all[i+1,(2*j-1)]-time_all[(i-29):i,(2*j-1)])+abs(time_all[i+1,(2*j)]-time_all[(i-29):i,(2*j)])))==0)
            {
              choose_random_point<-function(data)
              {
                if((setequal(which(data[,1]<10000),integer(0))))
                {
                  return("no more shopping point")
                }else if(length(which(data[,1]<10000))<=1)
                {
                  return("no more shopping point")
                }else
                {
                  return(sample(which(data[,1]<10000),1)) #随机抽取一个购物点出来
                }
              }
              
              random_number<-choose_random_point(customer.all[,(2*j-1):(2*j)])
              if(random_number!="no more shopping point")
              {
                #random_shopping_point<-customer.all[random_number,(2*j-1):(2*j)] #存储新选中的下个购物点
                #customer.all[random_number,(2*j-1):(2*j)]<-next_shopping_point[1:2,j] #将原本决定去的购物点放回来，因为还没去买呢
                #next_shopping_point[1:2,j]<-random_shopping_point #现在将下一个购物点设置为接下来移动的目的地
                next_shopping_point[1:2,j]<-customer.all[random_number,(2*j-1):(2*j)]
              }
            }
          }
          
          ##########################################
          
          
        }else  
          ## Here is the situation of a certain customer staying and shopping at the shopping point,
          ## but still need to update the risktime data
        {
          if(risktime[j,1]==i)
          {
            risktime<-my_min_distance_change(i,j,time_all)
          }
        }
        ###########################################
        ###########################################
        
        ## Judge whether there will be congestion elsewhere in the store
        freezing<-"no freezing"
        if(i>(199+(j-1)*entrance_interval)&time_all[i+1,(2*j-1)]<10000)
        {
          if(((time_all[i+1,(2*j-1)]-time_all[i-199,(2*j-1)])+(time_all[i+1,(2*j)]-time_all[i-199,(2*j)]))==0)
          {
            #since customers maybe arrive at the vegetable zones corner a lot of times
            #so maybe this situation occurs by chance, eg.customer arrive (10,10) now and 200 seconds before
            #but it is not a freezing case
            if(sum(abs(time_all[(i-199):i,(2*j-1)]-time_all[i+1,(2*j-1)])+abs(time_all[(i-199):i,(2*j)]-time_all[i+1,(2*j)]))==0)
            {
              freezing<-"freezing"
              break
            }
            
          }
        }
        ###########################################
        ###########################################
        
      }
      
      ###########################################
      if(freezing=="freezing")
      {
        print(freezing)
        break
      }
      
      fix<-fix+1
    }
    
    
    ######################   The end of customer shopping cycle 
    
    
    ######################################################################################
    ######################################################################################
    
    ######################################################################################
    if(freezing=="stuck")
    {                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
      success[2,entrance_interval]<-success[2,entrance_interval]+1
      print(c(freezing,success[2,entrance_interval],i))
    }else if(freezing=="freezing")
    {
      success[3,entrance_interval]<-success[3,entrance_interval]+1
      print(c(freezing,success[3,entrance_interval],i))
    }else
    {
      success[1,entrance_interval]<-success[1,entrance_interval]+1
      print(c(freezing,success[1,entrance_interval],i))
      
      ########################################
      ########################################
      
      customer.numeber<-customer.numeber-1
      complete<-numeric(customer.numeber)
      
      enter_time<-numeric(customer.numeber)
      for(i in 1:customer.numeber)              
      {
        time_all[1:((i-1)*entrance_interval+1),(2*i-1):(2*i)]<-10000*i
        time_all[((i-1)*entrance_interval+1),(2*i-1):(2*i)]<-entrance
        enter_time[i]<-(i-1)*entrance_interval+1
        shopping_time[i]<-length(which(time_all[,(2*i)]<10000*i))
        
        ## Mark the customers who have completed the entire shopping process
        ## 1 means completed, 0 means not completed
        
        complete_number<-which(time_all[dim(time_all)[1],]>=10000)
        if(setequal(which(complete_number==2*i),integer(0)))
        {
          complete[i]<-0
        }else
        {
          complete[i]<-1
        }
        
        ##########################
      }
      reserve_time[1,1:customer.numeber]<-enter_time
      total_spending_rate<-matrix(data=NA,ncol=customer.numeber,nrow=20000)
      for(s in 1:customer.numeber)
      {
        if(reserve_number[s]>0)
        {        
          total_spending_rate[(1:(enter_time[s])),s]<-0
          for(t in 1:(reserve_number[s]))
          {
            total_spending_rate[(reserve_time[t,s]+1):(reserve_time[t+1,s]),s]<-1/(reserve_time[t+1,s]-reserve_time[t,s])
          }
          total_spending_rate[(reserve_time[(reserve_number[s]+1),s]+1):20000,s]<-0
        }else
        {
          total_spending_rate[(1:20000),s]<-0
        }
      }
      
      #average_total_spending_rate[1:20000,entrance_interval]<-average_total_spending_rate[1:20000,entrance_interval]+apply(total_spending_rate,MARGIN = 1,FUN = sum)
      
      average_customer_number[success[1,entrance_interval],entrance_interval]<-sum(complete)
      
      #customer_number_in_store_save[1:14399,entrance_interval]<-customer_number_in_store[1:14399]
      
      if(sum(complete)>0)
      {
        average_shopping_time[success[1,entrance_interval],entrance_interval]<-sum(shopping_time[which(complete==1)])/sum(complete)
        
        #shopping_time_store[success[1,entrance_interval],1:length(which(complete==1))]<-shopping_time[which(complete==1)]
        shopping_time_store[success[1,entrance_interval],]<-shopping_time[which(complete==1)]
        
      }else   
        ## Even if it has not been frozen, no one has completed the shopping,
        ## so the number of people is 0. 
        ## And shopping time is set to 0 means there is no data yet
      {
        average_shopping_time[success[1,entrance_interval],entrance_interval]<-0
      }
    }
    
    #customer_number_in_store_save[1:14399,entrance_interval]<-customer_number_in_store[1:14399]
    #a<-cbind(a,customer_number_in_store[1:14399])
  }
  
  
}
