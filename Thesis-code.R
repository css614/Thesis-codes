############## Pre-function setting

####### function for choosing shopping list for each customer randomly
shopping_list<-function(miu,sigma,method,number)
{
  if(method=="normal")## choose the orginal shopping list
  {
    n<-round(rnorm(1,miu,sigma))
  }else ## choose the first shopping point
  {
    n<-number
  }
  
  length_edge<-(4*(max(corner_x)-min(corner_x))+100)
  epsilon<-sample(c(1,2,3,4,5,6,7,8),n,replace = TRUE, prob=c(10/length_edge,10/length_edge,10/length_edge,10/length_edge,30/length_edge,20/length_edge,20/length_edge,30/length_edge))
  
  point_number<-numeric(8)
  for (i in 1:8)
  {
    point_number[i]<-length(which(epsilon==i))
  }
  
  x<-numeric(n)
  y<-numeric(n)
  if(point_number[1]!=0)
  {
    x[1:point_number[1]]<-corner_x[1]
    y[1:point_number[1]]<-runif(point_number[1],corner_y[1],corner_y[2])
  }
  if(point_number[2]!=0)
  {
    x[min((point_number[1]+1),sum(point_number[1:2])):sum(point_number[1:2])]<-runif(point_number[2],corner_x[2],corner_x[3])
    y[min((point_number[1]+1),sum(point_number[1:2])):sum(point_number[1:2])]<-corner_y[2]
  }
  if(point_number[3]!=0)
  {
    x[min((sum(point_number[1:2])+1),sum(point_number[1:3])):sum(point_number[1:3])]<-corner_x[3]
    y[min((sum(point_number[1:2])+1),sum(point_number[1:3])):sum(point_number[1:3])]<-runif(point_number[3],corner_y[4],corner_y[3])
  }
  if(point_number[4]!=0)
  {
    x[min((sum(point_number[1:3])+1),sum(point_number[1:4])):sum(point_number[1:4])]<-runif(point_number[4],corner_x[1],corner_x[4])
    y[min((sum(point_number[1:3])+1),sum(point_number[1:4])):sum(point_number[1:4])]<-corner_y[4]
  }
  if(point_number[5]!=0)
  {
    x[min((sum(point_number[1:4])+1),sum(point_number[1:5])):sum(point_number[1:5])]<-0
    y[min((sum(point_number[1:4])+1),sum(point_number[1:5])):sum(point_number[1:5])]<-runif(point_number[5],0,30)
  }
  if(point_number[6]!=0)
  {
    x[min((sum(point_number[1:5])+1),sum(point_number[1:6])):sum(point_number[1:6])]<-runif(point_number[6],0,20)
    y[min((sum(point_number[1:5])+1),sum(point_number[1:6])):sum(point_number[1:6])]<-30
  }
  if(point_number[7]!=0)
  {
    x[min((sum(point_number[1:6])+1),sum(point_number[1:7])):sum(point_number[1:7])]<-30
    y[min((sum(point_number[1:6])+1),sum(point_number[1:7])):sum(point_number[1:7])]<-runif(point_number[7],0,20)
  }
  if(point_number[8]!=0)
  {
    x[min((sum(point_number[1:7])+1),sum(point_number[1:8])):sum(point_number[1:8])]<-runif(point_number[8],0,30)
    y[min((sum(point_number[1:7])+1),sum(point_number[1:8])):sum(point_number[1:8])]<-0
  }
  
  
  data<-cbind(x,y)
  
  entrance<-c(30,25)
  exit<-c(30,5)
  ##If it coincides with the exit, delete this point 
  if(setequal(which(data[,1]==exit[1]&data[,2]==exit[2]),integer(0))) 
  {data.new<-data}else 
  {
    row<-which(data[,1]==exit[1]&data[,2]==exit[2])
    data<-data[-row,]
  }
  data.new<-matrix(data=NA,ncol=2,nrow=10000)
  
  k<-dim(data)[1]
  
  data.new[1:k,1:2]<-data
  
  return(data.new)
  
}
########################################################################





####### function for judging if the next step of customer will accross vegetable zone
judgment<-function(x1,y1,x2,y2)
{
  corner_x<-c(10,10,20,20)
  corner_y<-c(10,20,20,10)
  corner<-cbind(corner_x,corner_y)
  a<-y1-y2
  b<-x2-x1
  c<-x1*y2-x2*y1
  if(length(which(a*corner_x+b*corner_y+c<=0))!=4&length(which(a*corner_x+b*corner_y+c>=0))!=4)
  {
    if((x1<=min(corner_x)&x2<=min(corner_x))
       |(x1>=max(corner_x)&x2>=max(corner_x))
       |(y1<=min(corner_y)&y2<=min(corner_y))
       |(y1>=max(corner_y)&y2>=max(corner_y)) )
    {
      value<-0
    }else
    {
      value<-1
    }
  }else
  {
    value<-0
  }
  
  return(value)
}
## 0 means not across the vegetable zone, 1 means across
########################################################################



####### function for judging how could customer do next step without across vegetable zone by corner
judgment_corner<-function(a,b,c,d)
{
  ## customer may need to arrive 2 corners before get to the destination shopping point
  output<-numeric(5)
  point<-matrix(data=NA,ncol=4,nrow=4)
  distance_corner<-numeric(4)
  first_judge1<-numeric(4)
  first_judge2<-numeric(4)
  for(s in 1:4)
  {
    first_judge1[s]<-judgment(corner[s,1],corner[s,2],a,b)
    first_judge2[s]<-judgment(corner[s,1],corner[s,2],c,d)
  }
  first_judge<-numeric(4)
  first_judge<-first_judge1+first_judge2
  first<-which(first_judge==0)
  if(setequal(first,integer(0)))
  {
    for(s in 1:4)
    {
      second_judge1<-numeric(4)
      second_judge2<-numeric(4)
      if(first_judge1[s]==1)
      {
        for(m in 1:4)
        {
          second_judge1[m]<-judgment(corner[s,1],corner[s,2],corner[m,1],corner[m,2])
          second_judge2[m]<-judgment(corner[m,1],corner[m,2],a,b)
        }
        second_judge<-numeric(4)
        second_judge<-second_judge1+second_judge2
        second<-which(second_judge==0)
        distance_corner[s]<-sqrt((corner[s,1]-c)^2+(corner[s,2]-d)^2)+sqrt((corner[second,1]-a)^2+(corner[second,2]-b)^2)+sqrt((corner[s,1]-corner[second,1])^2+(corner[s,2]-corner[second,2])^2)
        point[s,1]<-corner[second,1]
        point[s,2]<-corner[second,2]
        point[s,3]<-corner[s,1]
        point[s,4]<-corner[s,2]
        
      }else
      {
        for(m in 1:4)
        {
          second_judge1[m]<-judgment(corner[s,1],corner[s,2],corner[m,1],corner[m,2])
          second_judge2[m]<-judgment(corner[m,1],corner[m,2],c,d)
        }
        second_judge<-numeric(4)
        second_judge<-second_judge1+second_judge2
        second<-which(second_judge==0)
        distance_corner[s]<-sqrt((corner[s,1]-a)^2+(corner[s,2]-b)^2)+sqrt((corner[second,1]-c)^2+(corner[second,2]-d)^2)+sqrt((corner[s,1]-corner[second,1])^2+(corner[s,2]-corner[second,2])^2)
        point[s,1]<-corner[s,1]
        point[s,2]<-corner[s,2]
        point[s,3]<-corner[second,1]
        point[s,4]<-corner[second,2]
      }
    }
    min_point<-which.min(distance_corner)
    output[1]<-min(distance_corner)## record the minimum distance of this route
    output[2]<-point[min_point,1]
    output[3]<-point[min_point,2]
    output[4]<-point[min_point,3]
    output[5]<-point[min_point,4]
  }else
  {
    output[1]<-sqrt((corner[first[1],1]-a)^2+(corner[first[1],2]-b)^2)+sqrt((corner[first[1],1]-c)^2+(corner[first[1],2]-d)^2)
    output[2]<-corner[first[1],1]
    output[3]<-corner[first[1],2]
    output[4]<--10000
    output[5]<--10000
  }
  return(output)
}
########################################################################



####### function for judging the risk number for each customer
my_min_distance<-function(data)
{
  distance<-matrix(data=NA,ncol = dim(data)[1] ,nrow = dim(data)[1])
  min_distance<-matrix(data = NA,ncol=2,nrow = dim(data)[1])
  for(i in 1:(dim(data)[1]))
  {
    for(j in 1:(dim(data)[1]))
    {
      distance[i,j]<-sqrt((data[i,1]-data[j,1])^2+(data[i,2]-data[j,2])^2)
    }
    dis<-distance[i,-i]
    min_distance[i,1]<-dis[which.min(dis)]
    if(which(distance[i,]==min_distance[i,1])[1]==i)
    {
      min_distance[i,2]<-which(distance[i,]==min_distance[i,1])[2]
    }else
    {
      min_distance[i,2]<-which(distance[i,]==min_distance[i,1])[1]
    }
  }
  min_distance[,1]<-floor((min_distance[,1]-2)/0.6)+1
  
  return(min_distance)
}


#######
my_min_distance_change<-function(i,j,data)
{
  risktime_data<-matrix(data=NA,nrow=dim(data)[2]/2,ncol=2)
  
  ## Build the data set required for my_min_distance 
  for(k in 1:customer.numeber)
  {
    if(k>j)
    {
      risktime_data[k,1:2]<-data[i,(2*k-1):(2*k)]
    }else
    {
      risktime_data[k,1:2]<-data[i+1,(2*k-1):(2*k)]
    }
  }
  
  ## recore the result of my_min_distance
  risktime<-my_min_distance(risktime_data)
  
  ##Follow up risktime according to the order of customer movement
  for(k in 1:customer.numeber)
  {
    if(k>j)
    {
      risktime[k,1]<-i+risktime[k,1]-1
    }else
    {
      risktime[k,1]<-i+risktime[k,1]
    }
  }
  
  ## Output the final result 
  return(risktime)
  
}

########################################################################



####### function for judging the the direction when customers are approaching collision
##If the customer is approaching to the shopping point 
##and the distance between him and the shopping point is smaller than 0.3 meters, 
##then we set his next step is on this shopping point
movement<-function(a,b,c,d)
{
  if(sqrt((c-a)^2+(d-b)^2)>0.3)
  {
    cos_theta<-(c-a)/sqrt((c-a)^2+(d-b)^2)
    sin_theta<-(d-b)/sqrt((c-a)^2+(d-b)^2)
    return(c(a+0.3*cos_theta,b+0.3*sin_theta))
  }else
  {
    return(c(c,d))
  }
}
####################
##choose the nearest shopping point from unfinished shopping list to be next shopping point
nearest_point<-function(data,a,b) 
{
  distance<-sqrt((data[,1]-a)^2+(data[,2]-b)^2)
  min<-which.min(distance)
  judge<-judgment(a,b,data[min,1],data[min,2])
  
  if(judge==0)
  {return(c(data[min,1],data[min,2]))}else
  {
    while(judge==1)
    {
      output<-judgment_corner(a,b,data[min,1],data[min,2])
      distance[min]<-output[1]
      min_0<-which.min(distance)
      if(min_0==min)
      {
        judge<-0;
        return(c(data[min,1],data[min,2]))
      }else
      {
        judge<-judgment(a,b,data[min_0,1],data[min_0,2]);
        if(judge==0)
        {return(c(data[min_0,1],data[min_0,2]))}else
        {min<-min_0}
      }
    }
  }
}

###### Judge if this is point is not only in the store but also not in the vegetable zone
in_the_zone<-function(a,b)
{
  if(a>30|a<0|b>30|b<0)
  {value<-0}else if(a<max(corner_x)&a>min(corner_x)&b<max(corner_y)&b>min(corner_y))
  {value<-0}else
  {value<-1}
  return(value)
}

###### Judge the direction when customers is approaching to collide
judgment_direction<-function(a,b,aa,bb,c,d,cc,dd,data3)
{
  direction<-0  ### Set an initial value 
  
  output<-numeric(4)
  ## Adjusted from the nearest shopping point to the actual first corner for 1st customer
  if(judgment(a,b,aa,bb)==1)
  {
    really_nc1<-judgment_corner(a,b,aa,bb)
    aa<-really_nc1[2]
    bb<-really_nc1[3]
  }
  next_point<-movement(a,b,aa,bb)
  
  
  dis_next<-sqrt((data3[1,]-next_point[1])^2+(data3[2,]-next_point[2])^2)
  if(setequal(which(dis_next<2),integer(0)))
  {
    output[1]<-1
    output[2:3]<-next_point
  }else
  {
    ## Adjusted from the nearest shopping point to the actual first corner for 2nd customer
    if(judgment(c,d,cc,dd)==1)
    {
      really_nc2<-judgment_corner(c,d,cc,dd)
      cc<-really_nc2[2]
      dd<-really_nc2[3]
    }
    ##Calculate the angle between the headings of two customers
    cos_between<-(a-c)/sqrt((c-a)^2+(d-b)^2)
    sin_between<-(b-d)/sqrt((c-a)^2+(d-b)^2)
    cos_first<-(cc-c)/sqrt((cc-c)^2+(dd-d)^2)
    sin_first<-(dd-d)/sqrt((cc-c)^2+(dd-d)^2)
    cos_target<-cos_between*cos_first+sin_between*sin_first
    sin_target<-cos_between*sin_first-sin_between*cos_first
    
    ##Calculate the position after moving left or right or backward 
    right_x<-a+0.3*(-sin_between)
    right_y<-b+0.3*cos_between
    left_x<-a+0.3*sin_between
    left_y<-b+0.3*(-cos_between)
    back_x<-a+0.3*cos_between
    back_y<-b+0.3*sin_between
    
    dis_right<-sqrt((data3[1,]-right_x)^2+(data3[2,]-right_y)^2)
    dis_left<-sqrt((data3[1,]-left_x)^2+(data3[2,]-left_y)^2)
    dis_back<-sqrt((data3[1,]-back_x)^2+(data3[2,]-back_y)^2)
    
    ## Judge if move left, right or back would cross a wall
    if(setequal(which(dis_right<2),integer(0))&in_the_zone(right_x,right_y)==1)
    {judge_right<-1}else
    {judge_right<-0}
    if(setequal(which(dis_left<2),integer(0))&in_the_zone(left_x,left_y)==1)
    {judge_left<-1}else
    {judge_left<-0}
    if(setequal(which(dis_back<2),integer(0))&in_the_zone(back_x,back_y)==1)
    {judge_back<-1}else
    {judge_back<-0}
    
    ##Choose direction according to the probability to move left, right or back based on angle
    if(cos_target>=0&sin_target>=0)
    {
      if(judge_left==0&judge_back==0)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0,0,1))}else if(judge_left==1&judge_back==0)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0.5,0,0.5))}else if(judge_left==0&judge_back==1)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0,0.25,0.75))}else
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0.5,0.25,0.25))}
      point<-switch(direction,c(right_x,right_y),c(left_x,left_y),c(back_x,back_y),c(a,b))
      output[1]<-0
      output[2:3]<-point
    }else if(cos_target<0&sin_target>=0)
    {
      if(judge_left==0)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0,0,1))}else
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0.5,0,0.5))}
      point<-switch(direction,c(right_x,right_y),c(left_x,left_y),c(back_x,back_y),c(a,b))
      output[1]<-0
      output[2:3]<-point
    }else if(cos_target<0&sin_target<0)
    {
      if(judge_right==0)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0,0,1))}else
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0.5,0,0,0.5))}
      point<-switch(direction,c(right_x,right_y),c(left_x,left_y),c(back_x,back_y),c(a,b))
      output[1]<-0
      output[2:3]<-point
    }else
    {
      if(judge_right==0&judge_back==0)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0,0,1))}else if(judge_right==1&judge_back==0)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0.5,0,0,0.5))}else if(judge_right==0&judge_back==1)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0,0.25,0.75))}else
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0.5,0,0.25,0.25))}
      point<-switch(direction,c(right_x,right_y),c(left_x,left_y),c(back_x,back_y),c(a,b))
      output[1]<-0
      output[2:3]<-point
    }
  }
  ##If the customer choose back,
  ##the number of steps to be back will be randomly taken out according to the probability of 1/2^n
  if(output[1]==0&direction==3)
  {
    c<-1
    u<-runif(1)
    y<-rgeom(1,0.5)
    while (u>(1/2^(y+1))/(c*dunif(y,0,1)))
    {
      u<-runif(1)
      y<-rgeom(1,0.5)
    }
    output[4]<-y
  }else
  {output[4]<-0}
  
  
  return(output)
}



########################################################################




####### The complete code for simulation of the movement of customers within 3600 seconds

success<-matrix(data=rep(0,300),nrow=3,ncol=10000)
average_shopping_time<-matrix(data=NA,nrow=100,ncol=10000)
average_customer_number<-matrix(data=NA,nrow=100,ncol=10000)
average_total_spending_rate<-matrix(data = rep(0,1000000),nrow = 10000,ncol = 100)
customer_number_in_store_save<-matrix(data = NA,nrow = 3600,ncol = 100)


for(entrance_interval in c(28,30,31,32,33,34,35,37,40,42,45,47,50,55,60,65))
{
  while(success[1,entrance_interval]<100)
  {
    customer.numeber<-1
    
    freezing<-"no freezing"
    
    ################    Create the initial matrix
    type_1_matrix<-matrix(data=0,ncol=1,nrow=1)
    type_2_matrix<-matrix(data=NA,ncol=1,nrow=10000)
    type_3_matrix<-matrix(data=NA,ncol=2,nrow=10000)
    
    ################    Record the number of customers in the store every second
    customer_number_in_store<-type_2_matrix
    ################
    
    ################     Create the initial shopping list of the first customer
    customer.all<-shopping_list(15,2,"normal",1)
    ###Record the customer's next shopping point
    next_shopping_point<-matrix(data=shopping_list(15,2,"count",1)[1,],nrow = 2,ncol=1)
    ################
    
    ################     coordinates of the corners of vegetable zone 
    corner_x<-c(10,10,20,20)
    corner_y<-c(10,20,20,10)
    corner<-cbind(corner_x,corner_y)
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
    fix<-matrix(data=16,ncol=1,nrow=1)
    ######################## 
    
    
    
    ######################## cycle begins, which meanns customers start to enter the store for shopping 
    for(i in 1:3599)
    {
      
      if(i==1)
      {
        customer_number_in_store[1,1]<-1
      }else
      {
        customer_number_in_store[i,1]<-customer_number_in_store[i-1,1]
      }
      
      ########################  Matrix changes after new customers entering
      
      if (floor((i-1)/entrance_interval)>=0&((i-1)/entrance_interval)==floor((i-1)/entrance_interval))
      {
        ## Record the total number of customers after adding new customers
        customer.numeber<-customer.numeber+1
        
        customer_number_in_store[i,1]<-customer_number_in_store[i,1]+1  
        
        ###################  Judge if it is freezing at the door
        if(customer.numeber>=3)
        {
          if(time_all[i,(2*(customer.numeber-2)-1)]==30&time_all[i,2*(customer.numeber-2)]==25)
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
        x<-shopping_list(15,2,"normal",1)
        customer.all<-cbind(customer.all,x)
        
        xx<-matrix(data=shopping_list(15,2,"count",1)[1,],nrow=2,ncol=1)
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
        if(fix[j]>15)
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
                time_all[i,(2*t-1)]<-30
                time_all[i,(2*t)]<-25
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
          if(delete==0)
          {
            if(setequal(which(customer.all[,(2*j-1):(2*j)]<10000),integer(0))&(abs(time_all[(i+1),(2*j-1)]-exit[1])+abs(time_all[(i+1),(2*j)]-exit[2]))!=0)
            {
              ## If the shopping list has been emptied and the customer is not at the exit at this time, 
              ## then add the exit to the shopping list and let the customer continue to walk out of the store
              next_shopping_point[1,j]<-exit[1]
              next_shopping_point[2,j]<-exit[2]
              
              fix[j]<-1
              time_all[(i+2):(i+15),(2*j-1)]<-time_all[(i+1),(2*j-1)]
              time_all[(i+2):(i+15),(2*j)]<-time_all[(i+1),(2*j)]
              
              reserve_number[j]<-reserve_number[j]+1
              reserve_all[reserve_number[j],(2*j-1):(2*j)]<-time_all[(i+1),(2*j-1):(2*j)]
              reserve_time[reserve_number[j]+1,j]<-i+15
              
            }else if((setequal(which(customer.all[,(2*j-1):(2*j)]<10000),integer(0))&(abs(time_all[(i+1),(2*j-1)]-exit[1])+abs(time_all[(i+1),(2*j)]-exit[2]))==0))
            {
              ## If the shopping list has been emptied and the exit has been reached,
              ## the customer cycle ends
              fix[j]<--50000
              time_all[(i+2):10000,(2*j-1)]<-10000*j
              time_all[(i+2):10000,(2*j)]<-10000*j
              
              ## When the customer arrives at the exit, clear the coordinates of the entrance
              next_shopping_point[1:2,j]<-10000
              
              ## The number of customers in the store should be -1
              customer_number_in_store[i,1]<-customer_number_in_store[i,1]-1
              
            }else
            {
              ## If the shopping list has not been emptied yet, delete the arrived ones first, 
              ## and then assign a new point to next_shopping_point
              next_shopping_point[1:2,j]<-nearest_point(customer.all[,(2*j-1):(2*j)],time_all[i,2*j-1],time_all[i,2*j])
              delete_shopping_point<-abs(customer.all[,(2*j-1)]-next_shopping_point[1,j])+abs(customer.all[,(2*j)]-next_shopping_point[2,j])
              
              ## If it is a point in the shopping list, 
              ## delete the point in the shopping list at the same time
              customer.all[which(delete_shopping_point==0),(2*j-1)]<-10000
              customer.all[which(delete_shopping_point==0),(2*j)]<-10000
              
              
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
            if(((time_all[i+1,(2*j-1)]-time_all[i-29,(2*j-1)])+(time_all[i+1,(2*j)]-time_all[i-29,(2*j)]))==0)
            {
              random_number<-choose_randomm_point(customer.all[,(2*j-1):(2*j)])
              if(random_number!="no more shopping point")
              {
                random_shopping_point<-customer.all[random_number,(2*j):(2*j-1)]
                customer.all[random_number,(2*j-1):(2*j)]<-next_shopping_point[1:2,j]
                next_shopping_point[1:2,j]<-random_shopping_point
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
            freezing<-"freezing"
            break
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
      print(c(freezing,success[2,entrance_interval]))
    }else if(freezing=="freezing")
    {
      success[3,entrance_interval]<-success[3,entrance_interval]+1
      print(c(freezing,success[3,entrance_interval]))
    }else
    {
      success[1,entrance_interval]<-success[1,entrance_interval]+1
      print(c(freezing,success[1,entrance_interval]))
      
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
      total_spending_rate<-matrix(data=NA,ncol=customer.numeber,nrow=10000)
      for(s in 1:customer.numeber)
      {
        if(reserve_number[s]>0)
        {
          total_spending_rate[(1:(enter_time[s])),s]<-0
          for(t in 1:(reserve_number[s]))
          {
            total_spending_rate[(reserve_time[t,s]+1):(reserve_time[t+1,s]),s]<-1/(reserve_time[t+1,s]-reserve_time[t,s])
          }
          total_spending_rate[(reserve_time[(reserve_number[s]+1),s]+1):10000,s]<-0
        }else
        {
          total_spending_rate[(1:10000),s]<-0
        }
      }
      average_total_spending_rate[1:10000,entrance_interval]<-average_total_spending_rate[1:10000,entrance_interval]+apply(total_spending_rate,MARGIN = 1,FUN = sum)
      
      average_customer_number[success[1,entrance_interval],entrance_interval]<-sum(complete)
      
      if(sum(complete)>0)
      {
        average_shopping_time[success[1,entrance_interval],entrance_interval]<-sum(shopping_time[which(complete==1)])/sum(complete)
        
      }else   
        ## Even if it has not been frozen, no one has completed the shopping,
        ## so the number of people is 0. 
        ## And shopping time is set to 0 means there is no data yet
      {
        average_shopping_time[success[1,entrance_interval],entrance_interval]<-0
      }
    }
  }
  ## record the total spending rate at every second for customers in the store
  average_total_spending_rate[1:3600,entrance_interval]<-average_total_spending_rate[1:3600,entrance_interval]/100
  ## record the number of customers in the store at every second
  customer_number_in_store_save[1:3599,entrance_interval]<-customer_number_in_store[1:3599]
}
########################################################################




####### Draw the plots for visual analysis

###### Draw the plots for the proportion of three situation during the simulations
y<-success[1,]/(apply(success,MARGIN = 2,FUN = sum))
y2<-success[2,]/(apply(success,MARGIN = 2,FUN = sum))
y3<-success[3,]/(apply(success,MARGIN = 2,FUN = sum))
plot(x=c(28,30,31,32,33,34,35,37,40,42,45,47,50,55,60,65)
     ,y=c(y[28],y[30],y[31],y[32],y[33],y[34],y[35],y[37],y[40],y[42],y[45],y[47],y[50],y[55],y[60],y[65]),type = "b",
     xlim=c(25,70),ylim=c(0,1)
     ,main="proportion of three situation during the simulations",xlab="time interval",ylab = "")

points(x=c(28,30,31,32,33,34,35,37,40,42,45,47,50,55,60,65)
       ,y=c(y2[28],y2[30],y2[31],y2[32],y2[33],y2[34],y2[35],y2[37],y2[40],y2[42],y2[45],y2[47],y2[50],y2[55],y2[60],y2[65]),type = "b",col="blue")
points(x=c(28,30,31,32,33,34,35,37,40,42,45,47,50,55,60,65)
       ,y=c(y3[28],y3[30],y3[31],y3[32],y3[33],y3[34],y3[35],y3[37],y3[40],y3[42],y3[45],y3[47],y3[50],y3[55],y3[60],y3[65]),type = "b",col="red")

legend(53,0.6,c("no freezing","freezing_entrance","freezing_other part")
       ,text.col=c("black","blue","red"),bty = "n",cex = 1,pch = 19
       ,col=c("black","blue","red"))

###### Draw the plot of the #of customers who has completed shopping
y_number<-apply(average_customer_number,MARGIN = 2,FUN = mean)
plot(x=c(28,30,31,32,33,34,35,37,40,42,45,47,50,55,60,65)
     ,y=c(y_number[28],y_number[30],y_number[31],y_number[32],y_number[33],y_number[34],y_number[35],y_number[37],y_number[40],y_number[42],y_number[45],y_number[47],y_number[50],y_number[55],y_number[60],y_number[65]), type = "b"
     ,xlim=c(25,65),ylim=c(30,50),ylab = "",xlab = "time interval",main = "Average #of customers who completed shopping")

###### Draw the plot of the #of customers who has entered the store within 3600 seconds
plot(x=c(28,30,31,32,33,34,35,37,40,42,45,47,50,55,60,65)
     ,y=c(floor(3598/28+1),floor(3598/30+1),floor(3598/31+1),floor(3598/32+1),
          floor(3598/33+1),floor(3598/34+1),floor(3598/35+1),floor(3598/37+1),
          floor(3598/40+1),floor(3598/42+1),floor(3598/45+1),
          floor(3598/47+1),floor(3598/50+1),floor(3598/55+1), 
          floor(3598/60+1),floor(3598/65+1)), type = "b",ylab = "",xlab = "time interval"
     ,main = "number of total customers",ylim=c(50,140),xlim=c(25,65))

####### Draw the plot of the average shopping time for customers who has completed the shopping
y_time<-apply(average_shopping_time,MARGIN = 2,FUN = mean)
plot(x=c(28,30,31,32,33,34,35,37,40,42,45,47,50,55,60,65)
     ,y=c(y_time[28],y_time[30],y_time[31],y_time[32],y_time[33],y_time[34],y_time[35],y_time[37],y_time[40],y_time[42],y_time[45],y_time[47],y_time[50],y_time[55],y_time[60],y_time[65]), type = "b"
     ,ylab = "second(s)",xlab = "time interval",main = "Average shopping time of each customer"
     ,ylim = c(900,1600),xlim = c(25,65))
legend(29.5,1610,c("2A1=1517.56 seconds","(30,1516.907)"),text.col = c("red","black"),bty = "n",cex = 1,pch = 19,col=c("red","black"))

###### Draw the plot for #of the customers who stay in store at each second within 3600 seconds
plot(customer_number_in_store_save[1:3599,28],xlab = "time",ylab = "number"
     ,main = "#of customers in the store wrt. different entrence frequency",col=28,ylim = c(0,100))
for(i in c(30,31,32,33,34,35,37,40,42,45,47,50,55,60,65))
{
  points(x=c(1:3599),y=customer_number_in_store_save[1:3599,i],col=i)
}
legend(0,100,c("the top blue line is 28 seconds","the bottom black line is 65 seconds"),bty = "n",text.col=c(28,65),col=c(28,65),pch = 19)

##### Draw the plot for the total spending rate at every second
plot(x=1:3600,y=average_total_spending_rate[1:3600,65],col=65,ylim=c(0,0.7),type = "l",
     xlab = "time (seoncds)",ylab = NA, main = "shopping rate for total customer in store")
for(i in c(28,30,31,32,33,34,35,37,40,42,45,47,50,55,60))
{
  lines(x=1:3600,y=average_total_spending_rate[1:3600,i],col=i)
}

plot(x=1:3600,y=average_total_spending_rate[1:3600,65],col=65,ylim=c(0,0.7),type = "l",
     xlab = "time (seoncds)",ylab = NA, main = "shopping rate for total customer in store")
for(i in c(28,45))
{
  lines(x=1:3600,y=average_total_spending_rate[1:3600,i],col=i)
}
legend(0,0.70,c("28 seconds","45 seconds","65seonds"),col = c(28,45,65)
       ,text.col = c(28,45,65),bty = "n",cex = 1,pch = 19)
